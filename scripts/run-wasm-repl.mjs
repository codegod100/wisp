import { Wisp, WASD } from '../web/wisp.js';
import WASI from '../web/wasi.js';
import fs from 'node:fs';
import path from 'node:path';
import readline from 'node:readline';
import { fileURLToPath } from 'node:url';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const rootDir = path.resolve(__dirname, '..');
process.chdir(rootDir);

async function loadWasmModule() {
  const candidates = [
    'web/dist/wisp.wasm',
    'web/wisp.wasm',
    'core/zig-out/bin/wisp.wasm',
  ];

  for (const candidate of candidates) {
    const full = path.resolve(rootDir, candidate);
    if (fs.existsSync(full)) {
      const wasmBytes = fs.readFileSync(full);
      const wasmModule = await WebAssembly.compile(wasmBytes);
      return { wasmModule, wasmPath: full };
    }
  }

  throw new Error(`Could not find wisp.wasm (looked in ${candidates.join(', ')})`);
}

async function initContext() {
  console.log('Initializing Wisp...');
  const { wasmModule, wasmPath } = await loadWasmModule();
  console.log(`Loaded WASM from: ${wasmPath}`);

  const wasi = new WASI();
  const wasd = new WASD();

  const instance = await WebAssembly.instantiate(wasmModule, {
    wasi_snapshot_preview1: wasi.exports(),
    dom: wasd.exports(),
  });

  wasi.setMemory(instance.exports.memory);
  const ctx = new Wisp(instance);
  wasd.setWisp(ctx);

  return ctx;
}

function formatValue(ctx, value) {
  const uvalue = value >>> 0;
  const uzap = ctx.sys.zap >>> 0;
  const unil = ctx.sys.nil >>> 0;
  const ut = ctx.sys.t >>> 0;
  const unah = ctx.sys.nah >>> 0;
  const utop = ctx.sys.top >>> 0;

  if (uvalue === uzap) return 'zap';
  if (uvalue === unil) return 'nil';

  if (uvalue === ut) return 't';
  if (uvalue === unah) return 'nah';
  if (uvalue === utop) return 'top';

  value = uvalue;

  if ((value & 0x80000000) === 0) {
    if (value & 0x40000000) {
      return (value | 0x80000000).toString();
    }
    return value.toString();
  }

  const tags = {
    v08: 0x1a,
    v32: 0x19,
    duo: 0x15,
    sym: 0x16,
    fun: 0x17,
    mac: 0x18,
    sys: 0x11,
    chr: 0x12,
    jet: 0x13,
    pin: 0x1f,
  };

  let tag = (value >>> 27) & 0x1f;

  if (tag === tags.v08) {
    try {
      return `"${ctx.loadString(value)}"`;
    } catch {
      return `[string: 0x${value.toString(16)}]`;
    }
  } else if (tag === tags.v32) {
    try {
      const vec = ctx.loadVector(value);
      return `[${vec.map((v) => formatValue(ctx, v)).join(', ')}]`;
    } catch {
      return `[vector: 0x${value.toString(16)}]`;
    }
  } else if (tag === tags.duo) {
    try {
      let result = [];
      let current = value;
      let depth = 0;

      while (depth < 100) {
        const h = ctx.api.wisp_heap_get_duo_head(ctx.heap, current) >>> 0;
        const t = ctx.api.wisp_heap_get_duo_tail(ctx.heap, current) >>> 0;
        result.push(formatValue(ctx, h));

        if (t === ctx.sys.nil) {
          return `(${result.join(' ')})`;
        }

        const tailTag = (t >>> 27) & 0x1f;
        if (tailTag === tags.duo) {
          current = t;
          depth += 1;
        } else {
          return `(${result.join(' ')} . ${formatValue(ctx, t)})`;
        }
      }
      return `(${result.join(' ')} ...)`;
    } catch {
      return `[cons: 0x${value.toString(16)}]`;
    }
  } else if (tag === tags.sym) {
    try {
      if (ctx.api.wisp_heap_get_sym_str) {
        const strPtr = ctx.api.wisp_heap_get_sym_str(ctx.heap, value) >>> 0;
        if (strPtr !== ctx.sys.zap && strPtr !== ctx.sys.nil) {
          const strTag = (strPtr >>> 27) & 0x1f;
          if (strTag === tags.v08) {
            return ctx.loadString(strPtr);
          }
        }
      }
    } catch {
      // fall through
    }
    return `[symbol: 0x${value.toString(16)}]`;
  } else if (tag === tags.sys) {
    const sysIdx = value & 0x7ffffff;
    const sysNames = ['nil', 't', 'nah', 'zap', 'top'];
    if (sysIdx < sysNames.length) {
      return sysNames[sysIdx];
    }
    return `[sys:${sysIdx}: 0x${value.toString(16)}]`;
  } else if (tag === tags.fun || tag === tags.mac) {
    return `[${tag === tags.fun ? 'function' : 'macro'}: 0x${value.toString(16)}]`;
  }

  return `[tag:0x${tag.toString(16)}: 0x${value.toString(16)}]`;
}

async function evalForms(ctx, code) {
  const forms = ctx.readMany(code);
  if (forms === ctx.sys.zap) throw new Error('read error');

  let current = forms;
  let lastResult = ctx.sys.nil;
  let formIndex = 0;

  while (current !== ctx.sys.nil) {
    formIndex += 1;
    const form = ctx.api.wisp_heap_get_duo_head(ctx.heap, current) >>> 0;
    const run = ctx.api.wisp_run_init(ctx.heap, form);
    lastResult = ctx.api.wisp_run_eval(ctx.heap, run, 1_000_000) >>> 0;

    if (lastResult === ctx.sys.zap) {
      const err = ctx.api.wisp_run_err(ctx.heap, run);
      const exp = ctx.api.wisp_run_exp(ctx.heap, run);
      const val = ctx.api.wisp_run_val(ctx.heap, run);
      throw new Error(
        `form ${formIndex} signaled zap: err=${formatValue(ctx, err)} exp=${formatValue(ctx, exp)} val=${formatValue(ctx, val)}`,
      );
    }

    current = ctx.api.wisp_heap_get_duo_tail(ctx.heap, current) >>> 0;
  }

  return lastResult;
}

async function loadFile(ctx, filename) {
  const fullPath = path.resolve(rootDir, filename);
  if (!fs.existsSync(fullPath)) return false;
  const code = fs.readFileSync(fullPath, 'utf8');
  await evalForms(ctx, code);
  return true;
}

async function bootstrap(ctx) {
  const candidates = ['web/structs.wisp', './web/structs.wisp', 'structs.wisp'];
  for (const candidate of candidates) {
    try {
      if (await loadFile(ctx, candidate)) {
        console.log(`Loaded ${candidate}`);
        return;
      }
    } catch (err) {
      console.warn(`Failed to load ${candidate}: ${err.message}`);
    }
  }
  console.warn('Warning: could not load structs.wisp');
}

async function repl() {
  const ctx = await initContext();
  await bootstrap(ctx);

  const rl = readline.createInterface({
    input: process.stdin,
    output: process.stdout,
    prompt: '> ',
  });

  rl.prompt();

  rl.on('line', async (line) => {
    const input = line.trim();
    if (input.length === 0) {
      rl.prompt();
      return;
    }

    if (input === '(quit)' || input === '(exit)') {
      rl.close();
      return;
    }

    try {
      const result = await evalForms(ctx, input);
      console.log(formatValue(ctx, result));
    } catch (err) {
      console.error(err.message);
    }

    rl.prompt();
  });

  rl.on('close', () => {
    process.stdout.write('\n');
    process.exit(0);
  });
}

await repl();

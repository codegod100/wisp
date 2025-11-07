#!/usr/bin/env node
// Test runner for Node.js - can be run from command line
// Usage: node test-runner-node.js [test-name]

import { Wisp, WASD } from './wisp.js';
import WASI from './wasi.js';
import { runTests } from './test-harness.js';
import fs from 'fs';

async function initWisp() {
  // Load WASM module - try multiple possible locations
  const possiblePaths = [
    './wisp.wasm',
    './dist/wisp.wasm',
    '../core/zig-out/bin/wisp.wasm'
  ];
  
  let wasmBytes = null;
  let wasmPath = null;
  for (const path of possiblePaths) {
    try {
      wasmBytes = fs.readFileSync(path);
      wasmPath = path;
      break;
    } catch (e) {
      // Try next path
    }
  }
  
  if (!wasmBytes) {
    throw new Error(`Could not find wisp.wasm in any of: ${possiblePaths.join(', ')}`);
  }
  
  console.log(`Loading WASM from: ${wasmPath}`);
  const wasmModule = await WebAssembly.compile(wasmBytes);
  
  // Set up WASI and DOM interfaces
  const wasi = new WASI();
  const wasd = new WASD();
  
  // Create instance with required imports
  const instance = await WebAssembly.instantiate(wasmModule, {
    wasi_snapshot_preview1: wasi.exports(),
    dom: wasd.exports()
  });
  
  wasi.setMemory(instance.exports.memory);
  const ctx = new Wisp(instance);
  wasd.setWisp(ctx);
  
  return ctx;
}

async function loadWispFile(ctx, filename) {
  try {
    const code = fs.readFileSync(filename, 'utf8');
    const forms = ctx.readMany(code);
    if (forms === ctx.sys.zap) {
      console.error(`${filename} failed to parse`);
      return false;
    }
    
    let current = forms;
    let formIndex = 0;
    while (current !== ctx.sys.nil) {
      formIndex++;
      const form = ctx.api.wisp_heap_get_duo_head(ctx.heap, current) >>> 0;
      const run = ctx.api.wisp_run_init(ctx.heap, form);
      const result = ctx.api.wisp_run_eval(ctx.heap, run, 4_000_000) >>> 0;
      if (result === ctx.sys.zap) {
        const runErr = ctx.api.wisp_run_err(ctx.heap, run);
        const runExp = ctx.api.wisp_run_exp(ctx.heap, run);
        console.error(`${filename} form ${formIndex} failed:`, runErr, runExp);
        return false;
      }
      current = ctx.api.wisp_heap_get_duo_tail(ctx.heap, current) >>> 0;
    }
    
    console.log(`${filename} loaded successfully`);
    return true;
  } catch (e) {
    console.error(`Could not load ${filename}:`, e);
    return false;
  }
}

async function main() {
  console.log('Initializing Wisp...');
  const ctx = await initWisp();
  
  // Load required files - try multiple possible locations
  console.log('Loading base files...');
  const wispFiles = [
    'structs.wisp',
    './structs.wisp',
    './dist/structs.wisp'
  ];
  
  let loaded = false;
  for (const file of wispFiles) {
    try {
      if (await loadWispFile(ctx, file)) {
        loaded = true;
        break;
      }
    } catch (e) {
      // Try next path
    }
  }
  
  if (!loaded) {
    console.warn('Warning: Could not load structs.wisp, tests may fail');
  }
  
  // Get test name from command line if provided
  const testName = process.argv[2];
  
  // Run tests
  console.log('\n=== Running Tests ===');
  const summary = await runTests(ctx, {
    verbose: true,
    stopOnError: false,
    onTestError: (result, error) => {
      console.error(`\nDetailed error for ${result.name}:`);
      console.error(error);
    }
  });
  
  // Exit with appropriate code
  process.exit(summary.failed > 0 ? 1 : 0);
}

main().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});

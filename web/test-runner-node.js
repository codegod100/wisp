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
      // Reduced step limit for faster failure on hanging loads
      const result = ctx.api.wisp_run_eval(ctx.heap, run, 1_000_000) >>> 0;
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
  
  // Load test helpers first - provides symbol-name and other utilities
  console.log('Loading test helpers...');
  const helperFiles = [
    'test-helpers.wisp',
    './test-helpers.wisp',
    './dist/test-helpers.wisp'
  ];
  
  let helpersLoaded = false;
  for (const file of helperFiles) {
    try {
      if (await loadWispFile(ctx, file)) {
        helpersLoaded = true;
        break;
      }
    } catch (e) {
      // Try next path
    }
  }
  
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
  
  // Load generics.wisp for defmethod support
  console.log('Loading generics...');
  const genericsFiles = [
    'generics.wisp',
    './generics.wisp',
    './dist/generics.wisp'
  ];
  
  let genericsLoaded = false;
  for (const file of genericsFiles) {
    try {
      if (await loadWispFile(ctx, file)) {
        genericsLoaded = true;
        break;
      }
    } catch (e) {
      // Try next path
    }
  }
  
  if (!helpersLoaded) {
    console.warn('Warning: Could not load test-helpers.wisp, some tests may fail');
  }
  if (!loaded) {
    console.warn('Warning: Could not load structs.wisp, tests may fail');
  }
  if (!genericsLoaded) {
    console.warn('Warning: Could not load generics.wisp, defmethod tests may fail');
  }
  
  // Get test name from command line if provided
  const testName = process.argv[2];
  
  // Run tests with timeout protection
  console.log('\n=== Running Tests ===');
  const testPromise = runTests(ctx, {
    verbose: true,
    stopOnError: false,
    onTestError: (result, error) => {
      console.error(`\nDetailed error for ${result.name}:`);
      console.error(error);
    }
  });
  
  // Add timeout to prevent hanging (4 seconds to leave 1 second buffer for Makefile timeout)
  const timeoutPromise = new Promise((_, reject) => {
    setTimeout(() => reject(new Error('Test suite timed out after 4 seconds')), 4000);
  });
  
  try {
    const summary = await Promise.race([testPromise, timeoutPromise]);
    // Exit with appropriate code
    process.exit(summary.failed > 0 ? 1 : 0);
  } catch (error) {
    console.error('\n' + error.message);
    process.exit(124); // Exit code 124 is used by timeout command
  }
}

main().catch(error => {
  console.error('Fatal error:', error);
  process.exit(1);
});

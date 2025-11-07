// Test harness for Wisp - can run in browser or Node.js
// Usage in browser: import { runTests } from './test-harness.js'
// Usage in Node: node test-harness.js

export const tests = [
  {
    name: "check helper functions exist",
    code: `
      (list
        (symbol-function (quote set-struct-def))
        (symbol-function (quote build-constructor-name))
        (symbol-function (quote build-predicate-name))
        (symbol-function (quote create-all-accessors))
      )
    `,
    expected: null, // Just check they exist
    description: "Verify all helper functions are defined"
  },
  {
    name: "manual set-struct-def",
    code: "(set-struct-def (quote foo) (quote (a b)))",
    expected: null,
    description: "Test set-struct-def directly"
  },
  {
    name: "test quote returns symbol",
    code: "(quote foo)",
    expected: null, // Should return the symbol FOO
    description: "Test that quote returns a symbol"
  },
  {
    name: "test SYMBOL-NAME",
    code: "(SYMBOL-NAME (quote foo))",
    expected: null,
    description: "Test SYMBOL-NAME jet"
  },
  {
    name: "test STRING-APPEND",
    code: '(STRING-APPEND "make-" "foo")',
    expected: '"make-foo"',
    description: "Test STRING-APPEND jet"
  },
  {
    name: "test SYMBOL-NAME then STRING-APPEND",
    code: '(let ((name-str (SYMBOL-NAME (quote foo)))) (STRING-APPEND "make-" name-str))',
    expected: '"make-FOO"',
    description: "Test SYMBOL-NAME + STRING-APPEND"
  },
  {
    name: "manual build-constructor-name",
    code: "(build-constructor-name (quote foo))",
    expected: null, // Should return a symbol
    description: "Test build-constructor-name directly"
  },
  {
    name: "defstruct basic",
    code: "(defstruct foo a b)",
    expected: "FOO", // defstruct should return the struct name
    description: "Test basic defstruct definition"
  },
  {
    name: "defstruct and make instance",
    code: `
      (defstruct foo a b)
      (make-foo :a 32 :b 15)
    `,
    expected: null, // We'll check the structure
    description: "Test defstruct and creating an instance"
  },
  {
    name: "defstruct accessor",
    code: `
      (defstruct foo a b)
      (foo-a (make-foo :a 32 :b 15))
    `,
    expected: "32",
    description: "Test struct accessor function"
  },
  // Add more tests here
];

export async function runTests(ctx, options = {}) {
  const {
    verbose = true,
    stopOnError = false,
    onTestStart = null,
    onTestComplete = null,
    onTestError = null
  } = options;

  const results = [];
  let passed = 0;
  let failed = 0;

  for (const test of tests) {
    if (onTestStart) onTestStart(test);
    
    const result = {
      name: test.name,
      description: test.description,
      passed: false,
      error: null,
      actual: null,
      expected: test.expected,
      code: test.code.trim()
    };

    try {
      if (verbose) {
        console.log(`\n=== Running: ${test.name} ===`);
        console.log(`Code: ${test.code.trim()}`);
      }

      // Read and evaluate the code
      const forms = ctx.readMany(test.code);
      if (forms === ctx.sys.zap) {
        throw new Error("Failed to parse code");
      }

      let lastResult = ctx.sys.nil;
      let current = forms;
      let formCount = 0;

      while (current !== ctx.sys.nil) {
        formCount++;
        const form = ctx.api.wisp_heap_get_duo_head(ctx.heap, current) >>> 0;
        
        if (verbose && formCount > 1) {
          console.log(`  Evaluating form ${formCount}...`);
        }

        const run = ctx.api.wisp_run_init(ctx.heap, form);
        lastResult = ctx.api.wisp_run_eval(ctx.heap, run, 4_000_000) >>> 0;

        if (lastResult === ctx.sys.zap) {
          // Get error details
          const runErr = ctx.api.wisp_run_err(ctx.heap, run);
          const runExp = ctx.api.wisp_run_exp(ctx.heap, run);
          const runVal = ctx.api.wisp_run_val(ctx.heap, run);
          
          const errorDetails = {
            error: runErr,
            expression: runExp,
            value: runVal,
            errorStr: formatValue(ctx, runErr),
            expStr: formatValue(ctx, runExp),
            valStr: formatValue(ctx, runVal)
          };
          
          throw new Error(`Evaluation failed: ${JSON.stringify(errorDetails, null, 2)}`);
        }

        current = ctx.api.wisp_heap_get_duo_tail(ctx.heap, current) >>> 0;
      }

      result.actual = formatValue(ctx, lastResult);
      
      // Check if result matches expected
      if (test.expected !== null) {
        if (result.actual === test.expected || 
            result.actual === `"${test.expected}"` ||
            result.actual === test.expected.toString()) {
          result.passed = true;
          passed++;
        } else {
          result.passed = false;
          failed++;
          result.error = `Expected: ${test.expected}, Got: ${result.actual}`;
        }
      } else {
        // No expected value, just check it didn't error
        result.passed = true;
        passed++;
      }

      if (verbose) {
        if (result.passed) {
          console.log(`✓ PASSED: ${test.name}`);
          console.log(`  Result: ${result.actual}`);
        } else {
          console.log(`✗ FAILED: ${test.name}`);
          console.log(`  ${result.error}`);
        }
      }

      if (onTestComplete) onTestComplete(result);
      
      if (!result.passed && stopOnError) {
        break;
      }

    } catch (error) {
      result.passed = false;
      result.error = error.message;
      failed++;
      
      if (verbose) {
        console.log(`✗ ERROR: ${test.name}`);
        console.log(`  ${error.message}`);
      }

      if (onTestError) onTestError(result, error);
      
      if (stopOnError) {
        break;
      }
    }

    results.push(result);
  }

  // Summary
  if (verbose) {
    console.log(`\n=== Test Summary ===`);
    console.log(`Total: ${tests.length}`);
    console.log(`Passed: ${passed}`);
    console.log(`Failed: ${failed}`);
  }

  return {
    results,
    passed,
    failed,
    total: tests.length
  };
}

function formatValue(ctx, value) {
  // Convert to unsigned for comparison
  const uvalue = value >>> 0;
  const uzap = (ctx.sys.zap >>> 0);
  const unil = (ctx.sys.nil >>> 0);
  const ut = (ctx.sys.t >>> 0);
  const unah = (ctx.sys.nah >>> 0);
  const utop = (ctx.sys.top >>> 0);
  
  // Check for special system values first (before tag check)
  if (uvalue === uzap) return "zap";
  if (uvalue === unil) return "nil";
  if (uvalue === ut) return "t";
  if (uvalue === unah) return "nah";
  if (uvalue === utop) return "top";
  
  value = uvalue;
  
  // Integers
  if ((value & 0x80000000) === 0) {
    if (value & 0x40000000) {
      return (value | 0x80000000).toString();
    }
    return value.toString();
  }
  
  // Tagged pointers
  const tags = { 
    v08: 0x1a, v32: 0x19, duo: 0x15, sym: 0x16,
    fun: 0x17, mac: 0x18, sys: 0x11, chr: 0x12,
    jet: 0x13, pin: 0x1f,
  };
  
  let tag = (value >>> 27) & 0x1f;
  
  if (tag === tags.v08) {
    try { 
      return '"' + ctx.loadString(value) + '"'; 
    } catch (e) { 
      return `[string: 0x${value.toString(16)}]`; 
    }
  } else if (tag === tags.v32) {
    try {
      const vec = ctx.loadVector(value);
      return `[${vec.map(v => formatValue(ctx, v)).join(", ")}]`;
    } catch (e) { 
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
          return `(${result.join(" ")})`;
        }
        
        const tailTag = (t >>> 27) & 0x1f;
        if (tailTag === tags.duo) {
          current = t;
          depth++;
        } else {
          return `(${result.join(" ")} . ${formatValue(ctx, t)})`;
        }
      }
      return `(${result.join(" ")} ...)`;
    } catch (e) { 
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
    } catch (e) {
      // Fall through
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
  } else {
    return `[tag:0x${tag.toString(16)}: 0x${value.toString(16)}]`;
  }
}

// For Node.js usage
if (typeof module !== 'undefined' && module.exports) {
  module.exports = { tests, runTests };
}


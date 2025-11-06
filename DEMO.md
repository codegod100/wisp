# Wisp Lisp - Simple Usage Demo

## Running Lisp Files Natively

Wisp can run Lisp files directly as a native executable:

```bash
# Build the native executable first
make core-fast

# Run a Lisp file
./core/zig-out/bin/wisp run test-simple.wisp
# Output: 10

# Evaluate code directly
./core/zig-out/bin/wisp eval "(+ 1 2 3)"
# Output: 6

# Start a REPL
./core/zig-out/bin/wisp repl
```

## Running Lisp in WebAssembly (Browser)

1. Build the web version:
```bash
make web
```

2. Open `web/dist/demo-simple.html` in your browser

This simple demo shows:
- Running basic arithmetic: `(+ 1 2 3 4)`
- Creating lists: `(list 1 2 3)`
- Defining functions: `(defun square (x) (* x x))`
- Running your own code in the textarea

## Example Lisp Files

### test-simple.wisp
```lisp
(+ 1 2 3 4)
```

### More Examples

**Arithmetic:**
```lisp
(* 2 3 4)
(/ 100 5)
(- 10 3)
```

**Lists:**
```lisp
(list 1 2 3)
(cons 1 (list 2 3))
```

**Functions:**
```lisp
(defun add (x y) (+ x y))
(add 5 3)
```

**Conditionals:**
```lisp
(if (> 5 3) "yes" "no")
```

## What's the Difference?

- **Native (`wisp run`)**: Runs Lisp code as a native executable, fastest execution
- **WASM (`demo-simple.html`)**: Runs Lisp code in the browser via WebAssembly, good for web apps
- **IDE (`index.html`)**: Full interactive development environment (what you saw before)

For just running Lisp code, use the native `wisp run` command or the simple web demo!


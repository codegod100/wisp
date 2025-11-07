# Wisp Test Harness

A test system for debugging Wisp code that works both in the browser and from the command line.

## Browser Usage

1. Open `widget-demo.html` in your browser
2. Click the **"Run Tests"** button
3. Tests will run and results will be displayed in the result area and console

## Command Line Usage

```bash
cd web
node test-runner-node.js
```

To run a specific test:
```bash
node test-runner-node.js "test-name"
```

## Adding Tests

Edit `test-harness.js` and add test cases to the `tests` array:

```javascript
{
  name: "my test name",
  code: "(your-lisp-code-here)",
  expected: "expected-result",  // or null to just check it doesn't error
  description: "What this test does"
}
```

## Test Structure

Each test has:
- `name`: Unique test identifier
- `code`: Lisp code to execute (can be multiple expressions)
- `expected`: Expected result (string) or `null` to just check no error
- `description`: Human-readable description

## Debugging

The test harness provides detailed error information:
- Error values and expressions
- Formatted error details
- Step-by-step evaluation results

Check the browser console or terminal output for detailed debugging information.


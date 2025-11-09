#!/bin/bash
# Helper script to run Wisp code with structs.wisp loaded
# Usage: ./run-with-structs.sh "(your code here)"

cd "$(dirname "$0")/.."

# Create combined file
cat web/structs.wisp > /tmp/wisp-with-structs.wisp
echo "" >> /tmp/wisp-with-structs.wisp
echo "$1" >> /tmp/wisp-with-structs.wisp

# Run with wasmtime
cd core
mise exec -- wasmtime wisp-wasi.wasm run /tmp/wisp-with-structs.wisp





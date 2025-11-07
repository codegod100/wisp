# Default: fast build for development/testing
all: core-debug web

# Debug build (slower, for debugging)
core-debug:; cd core && zig build

# Fast build (optimized, for testing)
core-fast:; cd core && zig build -Doptimize=ReleaseFast

# Native tests (fast, no WASM)
test:; cd core && zig build test

# WASM tests (requires web build, 5 second timeout)
test-wasm: web
	@timeout 5 sh -c 'cd web && node test-runner-node.js' || \
		(exit_code=$$?; \
		if [ $$exit_code -eq 124 ]; then \
			echo "Tests timed out after 5 seconds"; \
		fi; \
		exit $$exit_code)

# Web build (incremental)
web:; cd web && ./build

# Clean build artifacts
clean:; rm -rf web/dist/* core/zig-* web/lib/*.js web/lib/*.js.map

# Deployment targets
deploy:; cp web/dist/* /restless/www/wisp/
deploy-nodetown: web; scp web/dist/* wisp.town:/restless/www/wisp/

# WASM sanity check
wasm-sanity:
	cd core && zig build -Dtarget=wasm32-wasi && \
	  wasmtime zig-out/bin/wisp.wasm eval "(+ 1 1)"

.PHONY: all core-debug core-fast test test-wasm web clean deploy deploy-nodetown wasm-sanity

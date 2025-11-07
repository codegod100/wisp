import { lezerPlugin } from "@nota-lang/esbuild-lezer"
import esbuild from "esbuild"
import { statSync, existsSync } from "fs"

const startTime = Date.now()

// Skip sourcemaps for faster builds during testing
// Set SOURCEMAP=1 to enable sourcemaps
const enableSourcemap = process.env.SOURCEMAP === "1"

// Check if grammar needs regeneration
const grammarFile = "lib/wisplang.grammar"
const grammarOutput = "lib/wisplang.js"
let needsGrammarRegen = true

if (existsSync(grammarFile) && existsSync(grammarOutput)) {
  try {
    const grammarTime = statSync(grammarFile).mtimeMs
    const outputTime = statSync(grammarOutput).mtimeMs
    needsGrammarRegen = grammarTime > outputTime
  } catch (e) {
    // If we can't check, regenerate to be safe
    needsGrammarRegen = true
  }
}

const entryPoints = [
  "lib/codemirror.ts",
  "lib/git.ts",
  "lib/idom.ts",
]

// Only include grammar if it needs regeneration
if (needsGrammarRegen) {
  entryPoints.push(grammarFile)
} else {
  console.log("Skipping grammar generation (grammar file unchanged)")
}

const buildStart = Date.now()
console.log(`Starting esbuild at ${buildStart - startTime}ms`)

esbuild.build({
  entryPoints,
  outdir: "lib",
  bundle: true,
  sourcemap: enableSourcemap,
  format: "esm",
  loader: {
    ".wisp": "text",
    ".wasm": "file",
  },
  plugins: [
    lezerPlugin(),
  ],
  // Incremental builds - esbuild will cache and only rebuild changed files
  incremental: true,
}).then((result) => {
  const buildEnd = Date.now()
  console.log(`esbuild completed in ${buildEnd - buildStart}ms`)
  console.log(`Total build time: ${buildEnd - startTime}ms`)
  // Store result for next incremental build
  if (result.rebuild) {
    process.rebuild = result.rebuild
  }
  process.exit(0)
}).catch((err) => {
  const buildEnd = Date.now()
  console.error(`esbuild failed after ${buildEnd - buildStart}ms`)
  console.error(err)
  process.exit(1)
})

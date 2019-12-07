module Setup
  ( main
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Options (Options, (:=))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console (log)
import Node.Execa (ExecaOpt, cwd, execa, inherit, preferLocal, stderr, stdout)
import Node.FsExtra (remove)
import Node.Path (concat)


backends ∷ Array String
backends =
  [ "core"
  ]


main ∷ Effect Unit
main = launchAff_ do
  buildServer
  traverse_ setupBackend backends


buildServer ∷ Aff Unit
buildServer = void do
  log "* Setup Server\n"
  execa "pulpunte" ["b", "-c", "-t", "index.js"] defaultOptions

setupBackend ∷ String → Aff Unit
setupBackend backend = void do
  let workDir = concat ["backend", backend]
      options = defaultOptions <> cwd := workDir

  log $ "* Setup backend: " <> backend <> "\n"
  _ ← execa "pulpunte" ["i", "-c"] options
  remove $ concat [workDir, "output"]
  _ ← execa "purs" ["compile", ".pulpunte/*/*/src/**/*.purs", "lib/*/src/**/*.purs"] options
  _ ← execa "purs" ["bundle", "output/*/*.js", "-o", "bundle.raw.js"] options
  execa "uglifyjs" ["-c", "-m", "-o", "bundle.js", "bundle.raw.js"] options

--

defaultOptions ∷ Options ExecaOpt
defaultOptions
   = preferLocal := true
  <> stdout := inherit
  <> stderr := inherit

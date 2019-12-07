module Node.Execa
  ( ExecaOpt
  , ExecaResult
  , execa
  , preferLocal
  , cwd
  , StdioOption
  , pipe
  , ignore
  , inherit
  , stdin
  , stdout
  , stderr
  ) where

import Prelude

import Control.Promise (Promise, toAff)
import Data.Function.Uncurried (Fn3, runFn3)
import Data.Options (Option, Options, opt, options)
import Effect.Aff (Aff)
import Foreign (Foreign)

data ExecaOpt

type ExecaResult =
  { exitCode ∷ Int
  , stdout ∷ String
  , stderr ∷ String
  }

execa ∷ String → Array String → Options ExecaOpt → Aff ExecaResult
execa command args opts = toAff $ runFn3 execaImpl command args $ options opts

foreign import execaImpl ∷ Fn3 String (Array String) Foreign (Promise ExecaResult)

-- Execa Options

preferLocal ∷ Option ExecaOpt Boolean
preferLocal = opt "preferLocal"

cwd ∷ Option ExecaOpt String
cwd = opt "cwd"

newtype StdioOption = StdioOption String

pipe ∷ StdioOption
pipe = StdioOption "pipe"

ignore ∷ StdioOption
ignore = StdioOption "ignore"

inherit ∷ StdioOption
inherit = StdioOption "inherit"

stdin ∷ Option ExecaOpt StdioOption
stdin = opt "stdin"

stdout ∷ Option ExecaOpt StdioOption
stdout = opt "stdout"

stderr ∷ Option ExecaOpt StdioOption
stderr = opt "stderr"

module Purs
 ( compile
 , CompileErrorPosition
 , CompileError
 , CompileErrors
 ) where

import Prelude

import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Exception (error)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Foreign (renderForeignError)
import Simple.JSON (readJSON)

type CompileErrorPosition =
  { startLine ∷ Int
  , startColumn ∷ Int
  , endLine ∷ Int
  , endColumn ∷ Int
  }

type CompileError =
  { position ∷ CompileErrorPosition
  , message ∷ String
  , errorCode ∷ String
  , errorLink ∷ String
  , filename ∷ String
  , moduleName ∷ Nullable String
  , suggestion ∷ Nullable
      { replaceRange ∷ CompileErrorPosition
      , replacement ∷ String
      }
  , allSpans ∷ Array
      { start ∷ Array Int
      , end ∷ Array Int
      , name ∷ String
      }
  }

type CompileErrors =
  { warnings ∷ Array CompileError
  , errors ∷ Array CompileError
  }

compile ∷ String → Effect CompileErrors
compile backend = do
  let args =
        [ "compile"
        , "backend/src/Main.purs"
        , "backend/" <> backend <> "/.pulpunte/*/*/src/**/*.purs"
        , "--json-errors"
        , "--no-prefix"
        , "--output", "backend/" <> backend <> "/output"
        ]
  result ← runEffectFn1 pursImpl args
  case readJSON result.stderr of
    Right compileErrors → pure compileErrors
    Left errors → throwError $ error $ foldMap renderForeignError errors

foreign import pursImpl ∷ EffectFn1 (Array String) {stdout ∷ String, stderr ∷ String}

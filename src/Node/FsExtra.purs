module Node.FsExtra
  ( pathExists
  , readFile
  , outputFileSync
  , readFileSync
  , removeSync
  , module Node.Path
  ) where

import Prelude

import Control.Promise (Promise, toAff)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn1, EffectFn2, runEffectFn1, runEffectFn2)
import Node.Path (FilePath)
import Prelude.Unicode ((∘))

pathExists ∷ FilePath → Aff Boolean
pathExists = toAff ∘ pathExistsImpl

readFile ∷ FilePath → Aff String
readFile = toAff ∘ readFileImpl

outputFileSync ∷ FilePath → String → Effect Unit
outputFileSync = runEffectFn2 outputFileSyncImpl

readFileSync ∷ FilePath → Effect String
readFileSync = runEffectFn1 readFileSyncImpl

removeSync ∷ FilePath → Effect Unit
removeSync = runEffectFn1 removeSyncImpl

foreign import pathExistsImpl ∷ FilePath → Promise Boolean
foreign import readFileImpl ∷ FilePath → Promise String
foreign import outputFileSyncImpl ∷ EffectFn2 FilePath String Unit
foreign import readFileSyncImpl ∷ EffectFn1 FilePath String
foreign import removeSyncImpl ∷ EffectFn1 FilePath Unit

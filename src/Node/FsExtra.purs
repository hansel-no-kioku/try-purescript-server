module Node.FsExtra
  ( pathExists
  , outputFileSync
  , module Node.Path
  ) where

import Prelude

import Control.Promise (Promise, toAff)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Uncurried (EffectFn2, runEffectFn2)
import Node.Path (FilePath)
import Prelude.Unicode ((∘))

pathExists ∷ FilePath → Aff Boolean
pathExists = toAff ∘ pathExistsImpl

foreign import pathExistsImpl ∷ FilePath → Promise Boolean

outputFileSync ∷ FilePath → String → Effect Unit
outputFileSync = runEffectFn2 outputFileSyncImpl

foreign import outputFileSyncImpl ∷ EffectFn2 FilePath String Unit

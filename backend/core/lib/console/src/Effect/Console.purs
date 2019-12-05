module Effect.Console
  ( log
  , logShow
  , error
  , errorShow
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Prelude.Unicode ((∘))

log ∷ String → Effect Unit
log = runEffectFn1 logImpl

logShow ∷ ∀ a. Show a ⇒ a → Effect Unit
logShow = log ∘ show

error ∷ String → Effect Unit
error = runEffectFn1 errorImpl

errorShow ∷ ∀ a. Show a ⇒ a → Effect Unit
errorShow = error ∘ show

foreign import logImpl ∷ EffectFn1 String Unit
foreign import errorImpl ∷ EffectFn1 String Unit

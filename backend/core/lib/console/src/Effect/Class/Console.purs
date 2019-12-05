module Effect.Class.Console
  ( log
  , logShow
  , error
  , errorShow
  ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as C
import Prelude.Unicode ((∘))

log ∷ ∀ m. MonadEffect m ⇒ String → m Unit
log = liftEffect ∘ C.log

logShow ∷ ∀ a m. Show a ⇒ MonadEffect m ⇒ a → m Unit
logShow = liftEffect ∘ C.logShow

error ∷ ∀ m. MonadEffect m ⇒ String → m Unit
error = liftEffect ∘ C.error

errorShow ∷ ∀ a m. Show a ⇒ MonadEffect m ⇒ a → m Unit
errorShow = liftEffect ∘ C.errorShow

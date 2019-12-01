module Text.Smolder.TryPureScript
  ( render
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Text.Smolder.Markup (Markup)
import Text.Smolder.Renderer.String as S

render ∷ ∀ e. Markup e → Effect Unit
render = runEffectFn1 renderImpl <<< S.render

foreign import renderImpl ∷ EffectFn1 String Unit

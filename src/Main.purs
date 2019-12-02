module Main
  ( main
  ) where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Data.Array (elem, null)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Lazy (force)
import Data.Maybe (Maybe(..))
import Data.Tuple.Unicode (type (×), (×))
import Effect (Effect)
import Effect.Aff (Aff, message, try)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Foreign.Object (lookup)
import Hyper.Conn (Conn)
import Hyper.Header (Header)
import Hyper.Middleware (Middleware, lift')
import Hyper.Node.Server (HttpRequest, HttpResponse, defaultOptionsWithLogging, runServer)
import Hyper.Request (RequestData, getRequestData, readBody)
import Hyper.Response (ResponseEnded, StatusLineOpen, headers, respond, writeHeader, writeStatus)
import Hyper.Status (Status, status, statusNotFound, statusOK)
import Node.FsExtra (outputFileSync, readFile, readFileSync, removeSync)
import Node.Path (concat)
import Prelude.Unicode ((∘))
import Purs as P
import Simple.JSON (writeJSON)


backends ∷ Array String
backends =
  [ "core"
  ]

type ResponseData =
  { status ∷ Status
  , headers ∷ Array Header
  , body ∷ String
  }

data Action = Compile | Bundle


main :: Effect Unit
main = runServer defaultOptionsWithLogging {} Ix.do
  request ← getRequestData
  body ← readBody
  lift' $ logShow request

  let cors = case lookup "host" request.headers of
              Just "localhost:3000" → true
              _ → false
      res404 = { status: statusNotFound
               , headers: []
               , body: "Not Found"
               }
      res500 = { status: status 500 "Internal Server Error"
               , headers: []
               , body: "Internal Server Error"
               }

  result ← lift' $ try case parseRequest request of
    Just (backend × Compile) → compile backend body
    Just (backend × Bundle) → bundle backend
    _ → pure res404

  res ← lift' $ either (const (pure res500) ∘ log ∘ message) pure result
  respond' cors res


parseRequest ∷ RequestData → Maybe (String × Action)
parseRequest request
  | ["api", backend, "compile"] ← (force request.parsedUrl).path
  , Left POST ← request.method
  , backend `elem` backends = Just $ backend × Compile
  | ["api", backend, "bundle"] ← (force request.parsedUrl).path
  , Left GET ← request.method
  , backend `elem` backends = Just $ backend × Bundle
  | otherwise = Nothing


compile ∷ String → String → Aff ResponseData
compile backend src = liftEffect do
  outputFileSync (concat ["backend", "src", "Main.purs"]) src
  compileErrors ← P.compile backend
  result ← if null compileErrors.errors
    then do
      js ← readFileSync $ concat ["backend", backend, "output", "Main", "index.js"]
      pure $ writeJSON {warnings: compileErrors.warnings, js}
    else
      pure $ writeJSON {error: {tag: "CompilerErrors", contents: compileErrors.errors}}

  removeSync $ concat ["backend", backend, "output", "Main"]
  removeSync $ concat ["backend", "src"]

  pure {status: statusOK, headers: [], body: result}


bundle ∷ String → Aff ResponseData
bundle backend = do
  body ← readFile $ concat ["backend", backend, "bundle.js"]
  pure { status: statusOK
       , headers: [ "Cache-Control" × "max-age=600" ]
       , body
       }


respond'
  ∷ Boolean
  → ResponseData
  → Middleware Aff (Conn HttpRequest (HttpResponse StatusLineOpen) {}) (Conn HttpRequest (HttpResponse ResponseEnded) {}) Unit
respond' cors res = Ix.do
  writeStatus res.status
  when cors $ writeHeader $ "Access-Control-Allow-Origin" × "*"
  headers res.headers
  respond res.body

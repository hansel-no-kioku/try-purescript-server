module Main
  ( main
  ) where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Data.Array (null)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Lazy (force)
import Data.Maybe (Maybe(..))
import Data.Tuple.Unicode ((×))
import Effect (Effect)
import Effect.Aff (Aff, error, throwError, try)
import Effect.Aff.Class (liftAff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Foreign.Object (lookup)
import Hyper.Conn (Conn)
import Hyper.Middleware (Middleware)
import Hyper.Node.Server (HttpRequest, HttpResponse, defaultOptionsWithLogging, runServer)
import Hyper.Request (RequestData, getRequestData, readBody)
import Hyper.Response (ResponseEnded, StatusLineOpen, closeHeaders, respond, writeHeader, writeStatus)
import Hyper.Status (statusNotFound, statusOK)
import Node.FsExtra (outputFileSync, pathExists, readFileSync, removeSync)
import Node.Path (concat)
import Purs (bundle, compile)
import Simple.JSON (writeJSON)

main :: Effect Unit
main = runServer defaultOptionsWithLogging {} Ix.do
  request ← getRequestData
  let cors = case lookup "host" request.headers of
              Just "localhost:3000" → true
              _ → false
  body ← readBody
  result ← liftAff $ try $ app request body
  case result of
    Right response → respondOK cors response
    Left error → respondNG


app ∷ RequestData → String → Aff String
app request body = do
  logShow request
  backend ← getBackend request
  response ← liftEffect $ build backend body
  liftEffect $ log response
  pure response

  where
    getBackend req =
      if req.method == Left POST
        then
          case (force req.parsedUrl).path of
            ["api", backend, "compile"] → checkBackend backend
            _ → throwError $ error $ "Invalid path: " <> req.url
        else
          throwError $ error $ "Invalid method: " <> show req.method

    checkBackend backend = do
      exists ← pathExists $ concat ["backend", backend, ".pulpunte"]
      if exists
        then pure backend
        else throwError $ error $ "Invalid backend: " <> backend


build ∷ String → String → Effect String
build backend src = do
  let mainPursPath = concat ["backend", "src", "Main.purs"]

  outputFileSync mainPursPath src
  compileErrors ← compile backend
  result ← if null compileErrors.errors
    then do
      js ← readFileSync $ concat ["backend", backend, "output", "Main", "index.js"]
      bundled ← bundle backend
      pure $ writeJSON
        { warnings: compileErrors.warnings
        , js
        , bundled
        }
    else
      pure $ writeJSON
        { error:
          { tag: "CompilerErrors"
          , contents: compileErrors.errors
          }
        }

  removeSync $ concat ["backend", backend, "output", "Main"]
  removeSync mainPursPath
  pure result


respondOK
  ∷ Boolean
  → String
  → Middleware Aff (Conn HttpRequest (HttpResponse StatusLineOpen) {}) (Conn HttpRequest (HttpResponse ResponseEnded) {}) Unit
respondOK cors res = Ix.do
  writeStatus statusOK
  when cors $ writeHeader $ "Access-Control-Allow-Origin" × "*"
  closeHeaders
  respond res


respondNG
  ∷ Middleware Aff (Conn HttpRequest (HttpResponse StatusLineOpen) {}) (Conn HttpRequest (HttpResponse ResponseEnded) {}) Unit
respondNG = Ix.do
  writeStatus statusNotFound
  closeHeaders
  respond "Not Found"

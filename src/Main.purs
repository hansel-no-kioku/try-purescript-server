module Main
  ( main
  ) where

import Prelude

import Control.Monad.Indexed.Qualified as Ix
import Data.Array as A
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Lazy (force)
import Data.Maybe (Maybe(..))
import Data.MediaType.Common (applicationJSON, applicationJavascript)
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, null)
import Data.String.Regex (regex, test)
import Data.String.Regex.Flags (multiline)
import Data.Tuple.Unicode (type (×), (×))
import Effect (Effect)
import Effect.Aff (Aff, error, message, throwError, try)
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
import Node.Encoding (Encoding(..), byteLength)
import Node.FsExtra (outputFileSync, pathExists, readFile, readFileSync, removeSync)
import Node.Path (concat)
import Prelude.Unicode ((∘))
import Purs as P
import Simple.JSON (writeJSON)


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
      on = \f backend → do
        valid ← validateBackend backend
        if valid then f backend else pure res404

  result ← lift' $ try case parseRequest request of
    Just (backend × Compile) → compile body `on` backend
    Just (backend × Bundle) → bundle `on` backend
    _ → pure res404

  res ← lift' $ either (const (pure res500) ∘ log ∘ message) pure result
  respond' cors res


parseRequest ∷ RequestData → Maybe (String × Action)
parseRequest request
  | ["api", backend, "compile"] ← (force request.parsedUrl).path
  , Left POST ← request.method = Just $ backend × Compile
  | ["api", backend, "bundle"] ← (force request.parsedUrl).path
  , Left GET ← request.method = Just $ backend × Bundle
  | otherwise = Nothing


validateBackend ∷ String → Aff Boolean
validateBackend backend = pathExists $ concat ["backend", backend, "output"]


respond'
  ∷ Boolean
  → ResponseData
  → Middleware Aff (Conn HttpRequest (HttpResponse StatusLineOpen) {}) (Conn HttpRequest (HttpResponse ResponseEnded) {}) Unit
respond' cors res = Ix.do
  let contentLength = byteLength res.body UTF8
  writeStatus res.status
  when (contentLength > 0) $ writeHeader $ "Content-Length" × show contentLength
  when cors $ writeHeader $ "Access-Control-Allow-Origin" × "*"
  headers res.headers
  respond res.body


compile ∷ String → String → Aff ResponseData
compile src backend = liftEffect do
  valid ← checkModule src
  body ← if valid
    then compile' src backend
    else pure errInvalidModule
  pure { status: statusOK
       , headers: ["Content-Type" × unwrap applicationJSON]
       , body
       }

compile' ∷ String → String → Effect String
compile' src backend = do
  outputFileSync (concat ["backend", "src", "Main.purs"]) src
  compileErrors ← P.compile backend
  body ← if A.null compileErrors.errors
    then do
      js ← readFileSync $ concat ["backend", backend, "output", "Main", "index.js"]
      pure $ writeJSON {warnings: compileErrors.warnings, js}
    else
      pure $ writeJSON {error: {tag: "CompilerErrors", contents: compileErrors.errors}}

  removeSync $ concat ["backend", backend, "output", "Main"]
  removeSync $ concat ["backend", "src"]

  pure body


bundle ∷ String → Aff ResponseData
bundle backend = do
  body ← readFile $ concat ["backend", backend, "bundle.js"]
  pure { status: statusOK
       , headers: [ "Cache-Control" × "max-age=600"
                  , "Content-Type" × unwrap applicationJavascript
                  ]
       , body
       }


checkModule ∷ String → Effect Boolean
checkModule src = do
  either (const $ throwError $ error "Invalid Regex")
         (\reg → pure $ test reg src)
         $ regex "\\s*module\\s+Main[\\s|\\(|$]" multiline

errInvalidModule ∷ String
errInvalidModule = writeJSON
  { error:
    { tag: "CompilerErrors"
    , contents:
      [ { "suggestion": null ∷ Nullable String
        , "position":
          { "startLine": 1
          , "startColumn": 1
          , "endLine": 1
          , "endColumn": 1
          }
        , "moduleName": null ∷ Nullable String
        , "message": "  The code must start with \"module Main\"\n"
        , "filename": "backend\\src\\Main.purs"
        , "errorLink": ""
        , "errorCode": "ErrorParsingModule"
        , "allSpans":
            [ { "start": [1, 1]
              , "end": [1, 1]
              , "name": "backend\\src\\Main.purs"
              }
            ]
        }
      ]
    }
  }

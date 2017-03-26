{-# LANGUAGE OverloadedStrings #-}

import           Charlotte.App
import           Charlotte.Wai

import           Control.Monad (join)

import qualified Network.HTTP.Types as HTTP
import           Network.Wai (Application, Middleware)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)

main :: IO ()
main =
  Warp.runSettings
    (Warp.setPort 8080 Warp.defaultSettings) $
      logStdout $
        application

--------

application :: Application
application =
  routes notFound

routes :: Middleware
routes next req resp =
  case Wai.pathInfo req of
    [] ->
      secure home req resp
    ["login"] ->
      login req resp
    ["profile", user] ->
      secure (profile (User user)) req resp
    _ ->
     next req resp

login :: Application
login req resp =
  case Wai.requestMethod req of
    "POST" -> do
      -- TODO Content-type
      form <- fmap HTTP.parseQueryText . Wai.requestBody $ req
      resp $
        case (join . lookup "username") form of
          Nothing ->
            html HTTP.status400 $
              loginView (Just "Missing user")
          Just name ->
            addHeader (setCookie $ makeSimpleCookie "session" name) $
              redirect [""]
    _ ->
      resp $
        html HTTP.status200 $
          loginView Nothing

home :: Session -> Application
home session _req resp =
  resp $
    redirect ["profile", renderUser (sessionUser session)]

profile :: User -> Session -> Application
profile user session _req resp =
  if sessionUser session /= user then
    resp $
      html HTTP.status403 $
        htmlTemplate "Unauthorized"
  else
    resp $
      html HTTP.status200 $
        htmlTemplate "Authorized"

notFound :: Application
notFound _req resp =
  resp $
     html HTTP.status404 $
       htmlTemplate "Not found"

secure :: (Session -> Application) -> Application
secure f req resp =
  case getCookie "session" req of
    Nothing ->
      resp $ redirect ["login"]
    Just user ->
      f (Session (User user)) req resp

{-# LANGUAGE OverloadedStrings #-}

import           Charlotte.App
import           Charlotte.Wai

import           Control.Monad (join)

import qualified Network.HTTP.Types as HTTP
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)

main :: IO ()
main =
  Warp.runSettings
    (Warp.setPort 8080 Warp.defaultSettings) $
      logStdout $
        (\req resp -> application req >>= resp)

--------

application :: Application'
application =
  routes notFound

routes :: Application' -> Application'
routes next req =
  case Wai.pathInfo req of
    [] ->
      secure home req
    ["login"] ->
      login req
    ["profile", user] ->
      secure (profile (User user)) req
    _ ->
     next req

login :: Application'
login req =
  case Wai.requestMethod req of
    "POST" -> do
      -- TODO Content-type
      form <- fmap HTTP.parseQueryText . Wai.requestBody $ req
      return $
        case (join . lookup "username") form of
          Nothing ->
            html HTTP.status400 $
              loginView (Just "Missing user")
          Just name ->
            addHeader (setCookie $ makeSimpleCookie "session" name) $
              redirect [""]
    _ ->
      return $
        html HTTP.status200 $
          loginView Nothing

home :: Session -> Application'
home session _req =
  return $
    redirect ["profile", renderUser (sessionUser session)]

profile :: User -> Session -> Application'
profile user session _req =
  if sessionUser session /= user then
    return $
      html HTTP.status403 $
        htmlTemplate "Unauthorized"
  else
    return $
      html HTTP.status200 $
        htmlTemplate "Authorized"

notFound :: Application'
notFound req =
  return $
    html HTTP.status404 $
      htmlTemplate "Not found"

secure :: (Session -> Application') -> Application'
secure f req =
  case getCookie "session" req of
    Nothing ->
      return $ redirect ["login"]
    Just user ->
      f (Session (User user)) req

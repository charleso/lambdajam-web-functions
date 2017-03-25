{-# LANGUAGE OverloadedStrings #-}
module Charlotte.Wai (
    application
  ) where

import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           Network.Wai (Application, Middleware, Request, Response)
import qualified Network.Wai as Wai
import           Network.HTTP.Types (Header)
import qualified Network.HTTP.Types as HTTP

import qualified Web.Cookie as Cookie

newtype User =
  User {
      renderUser :: Text
    } deriving (Eq, Show)

application :: Application
application =
  routes notFound

routes :: Middleware
routes next req resp =
  case Wai.pathInfo req of
    [] ->
      home req resp
    ["login"] ->
      login req resp
    ["profile", user] ->
      profile (User user) req resp
    _ ->
     next req resp

login :: Application
login req resp =
  case Wai.requestMethod req of
    "POST" ->
      resp $ redirect [setCookie $ makeSimpleCookie "session" "charles"] []
    _ ->
      resp $
        Wai.responseLBS HTTP.status200 [] $
          mconcat [
              "<html>"
            , "  <body>"
            , "    <form method=\"POST\" action=\"/login\">"
            , "      <input name=\"username\" />"
            , "    </form>"
            , "  </body>"
            , "</html>"
            ]


home :: Application
home req resp =
  case getCookie "session" req of
    Nothing ->
      resp $ redirect [] []
    Just user ->
      resp $ redirect [] ["profile", user]

profile :: User -> Application
profile username req resp =
  undefined

notFound :: Application
notFound req resp =
  resp $ Wai.responseLBS HTTP.status404 [] "<html><body>Not found</body></html>"

------

redirect :: [Header] -> [Text] -> Response
redirect headers loc =
  Wai.responseLBS
    HTTP.status302
    ((HTTP.hLocation, (BSL.toStrict . toLazyByteString . HTTP.encodePathSegments) loc) : headers)
    ""

------

getCookie :: Text -> Request -> Maybe Text
getCookie name req =
  (=<<) (fmap T.decodeUtf8 . lookup (T.encodeUtf8 name) . Cookie.parseCookies)
    . lookup "Cookie"
    . Wai.requestHeaders
    $ req

setCookie :: Cookie.SetCookie -> HTTP.Header
setCookie c =
  ("Set-Cookie", BSL.toStrict . toLazyByteString . Cookie.renderSetCookie $ c)

-- https://hackage.haskell.org/package/scotty-cookie-0.1.0.3/docs/Web-Scotty-Cookie.html
makeSimpleCookie :: Text -> Text -> Cookie.SetCookie
makeSimpleCookie name value =
  Cookie.def { Cookie.setCookieName = T.encodeUtf8 name, Cookie.setCookieValue = T.encodeUtf8 value }

{-# LANGUAGE OverloadedStrings #-}
module Charlotte.Wai (
    Application'
  , html
  , redirect
  , addHeader
  , getCookie
  , setCookie
  , makeSimpleCookie
  ) where

import           Charlotte.App

import           Data.ByteString.Builder (toLazyByteString)
import qualified Data.ByteString.Lazy as BSL
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

import           Network.Wai (Request, Response)
import qualified Network.Wai as Wai
import           Network.HTTP.Types (Header, Status)
import qualified Network.HTTP.Types as HTTP

import qualified Web.Cookie as Cookie

type Application' = Request -> IO Response

------

html :: Status -> Html -> Response
html status =
  Wai.responseLBS status [(HTTP.hContentType, "text/html; charset=utf-8")] . BSL.fromStrict . T.encodeUtf8

------

redirect :: [Text] -> Response
redirect loc =
  Wai.responseLBS
    HTTP.status302
    [(HTTP.hLocation, (BSL.toStrict . toLazyByteString . HTTP.encodePathSegments) loc)]
    ""

addHeader :: Header -> Response -> Response
addHeader h =
  Wai.mapResponseHeaders ((:) h)

------

getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 cm <- lookup "Cookie" . Wai.requestHeaders $ req
 fmap T.decodeUtf8 . lookup (T.encodeUtf8 name) . Cookie.parseCookies $ cm

setCookie :: Cookie.SetCookie -> HTTP.Header
setCookie c =
  ("Set-Cookie", BSL.toStrict . toLazyByteString . Cookie.renderSetCookie $ c)

-- https://hackage.haskell.org/package/scotty-cookie-0.1.0.3/docs/Web-Scotty-Cookie.html
makeSimpleCookie :: Text -> Text -> Cookie.SetCookie
makeSimpleCookie name value =
  Cookie.def { Cookie.setCookieName = T.encodeUtf8 name, Cookie.setCookieValue = T.encodeUtf8 value }

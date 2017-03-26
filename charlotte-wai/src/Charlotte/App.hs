{-# LANGUAGE OverloadedStrings #-}
module Charlotte.App (
    User (..)
  , Session (..)
  , Html
  , loginView
  , htmlTemplate
  ) where

import qualified Data.ByteString.Lazy as BSL
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text.Encoding as T

newtype User =
  User {
      renderUser :: Text
    } deriving (Eq, Show)

data Session =
  Session {
      sessionUser :: User
    } deriving (Eq, Show)

-- TODO Use blaze I guess
type Html = BSL.ByteString

loginView :: Maybe Text -> Html
loginView e =
  htmlTemplate . mconcat $ [
      maybe "" (\e' -> BSL.fromStrict . T.encodeUtf8 $ "<div style=\"color: red;\">" <> e' <> "</div") e
    , "<form method=\"POST\" action=\"/login\">"
    , "  <input name=\"username\" />"
    , "</form>"
    ]

htmlTemplate :: Html -> Html
htmlTemplate body =
  "<html><body>" <> body <> "</body></html>"

{-# LANGUAGE OverloadedStrings #-}

import           Charlotte.App

import           Data.Monoid ((<>))
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Network.HTTP.Types as HTTP

import           Web.Scotty (ActionM, ScottyM, get, post, redirect, param, status, html, scotty)
import           Web.Scotty.Cookie (getCookie, setCookie, makeSimpleCookie)

main :: IO ()
main =
  scotty 8081 routes

--------

routes :: ScottyM ()
routes = do
  get "/" $
    secure homeGet
  get "/login" $
    loginGet
  post "/login" $
    loginPost
  get "/profile/:user" $ do
    name <- User <$> param "user"
    secure (profileGet name)

homeGet :: Session -> ActionM ()
homeGet session =
  redirect . TL.fromStrict $
    "/profile/" <> (renderUser . sessionUser) session

loginGet :: ActionM ()
loginGet =
  html' $
    loginView Nothing

loginPost :: ActionM ()
loginPost = do
  user <- param "username"
  setCookie $
    makeSimpleCookie "session" user
  redirect . TL.fromStrict $
    "/profile/" <> user

profileGet :: User -> Session -> ActionM ()
profileGet user session =
  if sessionUser session /= user then do
    status HTTP.status403
    html' $ htmlTemplate "Unauthorized"
  else do
    status HTTP.status200
    html' $ htmlTemplate "Authorized"

secure :: (Session -> ActionM ()) -> ActionM ()
secure f = do
  c <- getCookie "session"
  case c of
    Nothing ->
      redirect "/login"
    Just s ->
      f (Session (User s))

html' :: Html -> ActionM ()
html' =
  html . TL.fromStrict

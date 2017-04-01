class: center, bottom, heading-black
background-image: url(images/ambiata-chopsticks.png)

# Caught in a web of functions

---

<img src="images/recommended_haskell.png" width="700" />

<img src="images/recommended_haskell_2.png" width="700" />

---

## Haskell Web Frameworks

- Happstack
- Scotty
- Snap
- Yesod
- Airship
- Spock
- Servant

---

class: center, middle, section-aqua, heading-white

# Scotty

---

## Scotty

```haskell
main :: IO ()
main =
  scotty 8080 routes

routes :: ScottyM ()
routes = do
  get "/" $
    --
  get "/login" $
    --
  post "/login" $
    --
  get "/profile/:user" $
    --
```

---

## Scotty

```haskell
get "/" $
  secure $ session ->
    redirect ("/profile/" <> session)

get "/login" $
  html $ loginView Nothing

post "/login" $ do
  user <- param "username"
  setCookie (makeSimpleCookie "session" user)
  redirect ("/profile/" <> user)

get "/profile/:user" $
  secure $ session -> do
    user <- param "user"
    if session /= user then do
      status status403
      html (page "Unauthorized")
    else
      html (page "Authorized")
```

???

- Split into multiple page

---

## Scotty

```haskell
secure :: (Session -> ActionM ()) -> ActionM ()
secure f = do
  c <- getCookie "session"
  case c of
    Nothing ->
      redirect "/login"
    Just s ->
      f (Session s)
```












---

class: center, middle, section-aqua, heading-white

# What is a web framework?

???

- "Let's take a step back"
- TODO Image?

---

class: center, middle, section-yellow, heading-black

# Request -> Response

---

```java
interface HttpServlet {

  void service(
    HttpServletRequest req,
    HttpServletResponse resp
  );
}
```

---

class: center, middle, section-yellow, heading-black

# Request -> IO Response

???

- "But this is haskell and we need to be able to do side effects"

---

class: center, middle, section-aqua, heading-white

# Web Application Interface

---

class: center, middle, section-aqua, heading-white

# WAI


---


TODO Request Response



---

## Home

```haskell
get "/" $
  ...
    redirect ("/profile/" <> session)
```

```haskell
redirect :: Text -> ActionM ()
```

---

## WAI: Create Response

``` haskell
responseLBS ::
  Status ->
  [Header] ->
  ByteString ->
  Response
```

---

```haskell
myRedirect :: Text -> Response
myRedirect location =
  responseLBS status302 [("Location", location)] ""
```

???

- Ignore the body for now - will discuss later

---

```haskell
get "/" $
  ...
    setResponse (myRedirect ("/profile/" <> session))
```






---

```haskell
post "/login" $ do
  user <- param "username"
  setCookie (makeSimpleCookie "session" user)
  redirect ("/profile/" <> user)
```

---

```haskell
setCookie :: Cookie -> ActionM ()
```

---

```haskell
mapResponseHeaders ::
  ([Header] -> [Header]) ->
  Response ->
  Response
```

```haskell
setMyCookie :: Cookie -> Response -> Response
setMyCookie c =
  mapResponseHeaders
    (\hs -> ("Set-Cookie", renderSetCookie c) : hs)
```

---

```haskell
post "/login" $ do
  user <- param "username"
  setResponse $
    setMyCookie (makeSimpleCookie "session" user $
      redirect ("/profile/" <> user)
```

???

TODO Param










---

```haskell
get "/profile/:user" $
  ...
    user <- param "user"
    if session /= user then
      setResponse (responseLBS status403 [] "")
    else
      setResponse (responseLBS status200 [] "")
```

---

```haskell
secure :: (Session -> ActionM ()) -> ActionM ()
secure f = do
  c <- getCookie "session"
  case c of
    Nothing ->
      redirect "/login"
    Just s ->
      f (Session s)
```

---

```haskell
getCookie :: Text -> ActionM (Maybe Text)
```

---

```haskell
getMyCookie :: Request -> Text -> Maybe Text
getMyCookie request name = do
 cm <- lookup "Cookie" (requestHeaders request)
 lookup name (parseCookies cm)
```

---

```haskell
secure ::
  Request ->
  (Session -> IO Response) ->
  IO Response
secure request f =
  case getMyCookie request "session" of
    Nothing ->
      return (redirect "/login")
    Just s ->
      f (Session s)
```











---

class: center, middle, section-aqua, heading-white

# Routing

---

```haskell
routes :: ScottyM ()
routes = do
  get "/" $
    ...
  get "/login" $
    ...
  post "/login" $
    ...
  get "/profile/:user" $
    ...
```

---

```haskell
pathInfo :: Request -> [Text]
```

```haskell
requestMethod :: Request -> Method
```

---

```haskell
myRoutes :: Request -> IO Response
myRoutes request =
  case pathOf request of
    [] ->
      ...
    ["login"] ->
      ...
    ["profile", user] ->
      ...
    _ ->
      ???
```

---

```haskell
myRoutes :: Request -> IO Response
myRoutes request =
  case pathOf request of
    [] ->
      ...
    ["login"] ->
      ...
    ["profile", user] ->
      ...
    _ ->
      responseLBS status404 [] "Not found"
```

---

```haskell
myRoutes :: Request -> IO Response
myRoutes request =
  case pathOf request of
    [] ->
      ...
    ["login"] ->
      ...
    ["profile", user] ->
      ...
    _ ->
      responseLBS status404 [] "Not found"
```

???

- Less than ideal, what does the 404 look like?

---

```haskell
myRoutes :: (Request -> IO Response) -> Request -> IO Response
myRoutes notFound request =
  case pathOf request of
    [] ->
      ...
    ["login"] ->
      ...
    ["profile", user] ->
      ...
    _ ->
      notFound request
```

---

```haskell
myRoutes :: Request -> IO (Maybe Response)
myRoutes request =
  case pathOf request of
    [] ->
      Just <$> ...
    ["login"] ->
      Just <$> ...
    ["profile", user] ->
      Just <$> ...
    _ ->
      return Nothing
```

---

```haskell
myRoutes :: Request -> IO Response
myRoutes request =
  case pathOf request of
    ...
    ["login"] ->
      case requestMethod request of
        "GET" ->
           ...
        "POST" ->
           ...
        _ ->
          responseLBS status405 [] ""
```

---

```haskell
routes :: ScottyM ()
routes = do
  ...
  get "/profile/:user" $
    user <- getParam "username"
```

???

- Needs type safety

---

class: center, middle, section-yellow, heading-black

# Bidirectional

???

- TODO This slide feels out of place

---

## Haskell Routing

- web-routes
- snap-web-routes
- web-routes-boomerang
- reroute

???

- I'm not suggesting pattern matching is way to do routing
- Cottage Industry





















---

class: center, middle, section-aqua, heading-white

# How do I run it?

---

```haskell
main :: IO ()
main =
  scotty 8080 $ do
    r <- request
    setResponse (myRoutes r)
```

```haskell
request :: ActionM Request
```

---

class: center, middle, section-yellow, heading-black

# scottyApp :: ScottyM () -> IO Application

---

class: center, middle, section-yellow, heading-black

# type Application = Request -> IO Response

---

class: center, middle, section-aqua, heading-white

# Warp

???

- One of the most common web servers

---

class: center, middle, section-yellow, heading-black

# run :: Port -> Application -> IO ()

---

```haskell
main :: IO ()
main = do
  app <- scottyApp $ do
    r <- request
    setResponse (myRoutes r)
  run 8080 app
```

---

class: center, middle, section-yellow, heading-black

# WAI Not

---

```haskell
main :: IO ()
main =
  run 8080 myRoutes
```







---

class: center, middle, section-aqua, heading-white

# White Lie

---

```haskell
type Application =
  Request -> IO Response
```

---

```haskell
type Application =
  Request ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
```

???

- See resources at the end

---

```haskell
main :: IO ()
main =
  run 8080 myApp
```

```haskell
myApp ::
  Request ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
myApp request respond = do
  response <- myRoutes request
  respond response
```

```haskell
myRoutes :: Request -> IO Response
```



















---

class: center, middle, section-aqua, heading-white

# What did the web framework give us?

---

class: center, middle, section-yellow, heading-black

# Request Handling

---

class: center, middle, section-yellow, heading-black

# Response Building

---

class: center, middle, section-yellow, heading-black

# Do we need web frameworks?

---

class: center, middle, section-yellow, heading-black

# Just Functions + Data

---

## Resources

- http://blog.infinitenegativeutility.com/2016/8/resources--laziness--and-continuation-passing-style
- https://github.com/charleso/lambdajam-web-functions
- "Build yourself a Haskell web framework"
  - https://www.youtube.com/watch?v=etuSnom2v2M

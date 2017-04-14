class: center, bottom, heading-black
background-image: url(images/ambiata-chopsticks.png)

# Caught in a web of functions

---

class: middle

<img src="images/recommended_haskell.png" width="700" />

---

class: middle

<img src="images/recommended_haskell_2.png" width="700" />

---

class: center, middle, section-aqua, heading-white

# Why?

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

# &lt;insert contrived example&gt;

---

class: center, middle, section-aqua, heading-white

# Scotty

---

class: code

```haskell
routes = do
  get "/login" $
    ...
  post "/login" $
    ...
  get "/profile/:user" $
    ...
```

---

class: code

```haskell
get "/login" $
  html $
    "<form method="POST" action="/login">" <>
    "  <input name="username" />" <>
    "</form>"
```

---

class: code

```haskell
post "/login" $ do
  user <- param "username"
  setCookie (makeCookie "session" user)
  redirect ("/profile/" <> user)
```

???

- Content-Type

---

class: code

```haskell
get "/profile/:user" $ do
  c <- getCookie "session"
  case c of
    Nothing ->
      redirect "/login"
    Just session -> do
      user <- param "user"
      if session /= user then do
        status status403
        html "<body>Unauthorized"
      else
        html "<body>Authorized"
```

???

- Split into multiple page












---

class: center, middle, section-aqua, heading-white

# What is a web application?

???

- "Let's take a step back"
- TODO Image?

---

class: code

```haskell







type Application = ???
```

---

class: code

```haskell







type Application = Request ->    Response
```

---

class: code

```haskell







type Application = Request -> IO Response
```

???

- "But this is haskell and we need to be able to do side effects"

---

class: center, middle, section-aqua, heading-white

# Web Application Interface

---

class: center, middle, section-aqua, heading-white

# WAI

---

class: code

```haskell
data Request =
  Request {
      pathInfo :: [Text]
    , requestMethod :: Method
    , requestHeaders :: [Header]
    , ...
    }

data Response =
  Response {
      responseStatus :: Status
    , responseHeaders :: [Header]
    , ...
    }
```

---

class: center, middle, section-yellow, heading-black

# Data + Functions

---

background-image: url(https://georgebrock.github.io/talks/command-line-ruby/images/lex.jpg)

???

- We're functional programmers - we know this












---

class: center, middle, section-aqua, heading-white

# What can I do with it?

---

class: code

```haskell
get "/login" $

  html $
    "<form method="POST" action="/login">" <>
    "  <input name="username" />" <>
    "</form>"
```

---

class: code

``` haskell
get "/login" $

  html $
    "<form method="POST" action="/login">" <>
    "  <input name="username" />" <>
    "</form>"

responseLBS ::
  Status -> [Header] -> ByteString -> Response
```

---

class: code

```haskell
loginGet :: Response
loginGet =
  responseLBS status200 [] $
    "<form method="POST" action="/login">" <>
    "  <input name="username" />" <>
    "</form>"

responseLBS ::
  Status -> [Header] -> ByteString -> Response
```

---

class: code

```haskell
post "/login" $ do
  user <- param "username"
  setCookie (makeCookie "session" user)
  redirect ("/profile/" <> user)
```

---

class: code

```haskell
requestBody :: Request -> IO ByteString

parseQueryText ::
  ByteString -> [(Text, Maybe Text)]
```

---

class: code

```haskell
requestBody :: Request -> IO ByteString

parseQueryText ::
  ByteString -> [(Text, Maybe Text)]


parseBody :: Request -> IO [(Text, Maybe Text)]
parseBody =
  requestBody >>= parseQueryText
```

---

class: code

```haskell
loginPost :: Request -> IO Response
loginPost request = do
  b <- parseBody request
    ...


parseBody :: Request -> IO [(Text, Maybe Text)]
```

---

class: code

```haskell
loginPost :: Request -> IO Response
loginPost request = do
  b <- parseBody request
  case lookup "username" b of
    Nothing ->
      ...


    Just ->
      ...
```

---

class: code

```haskell
loginPost :: Request -> IO Response
loginPost request = do
  b <- parseBody request
  case lookup "username" b of
    Nothing ->
      return $
        responseLBS status400 []
          "<body>Bad request"
    Just user ->
      ...
```

---

class: code

```haskell
    Just user ->
```

---

class: code

```haskell
    Just user ->

        myRedirect ("/profile/" <> user)

myRedirect :: Text -> Response
```

---

class: code

```haskell
    Just user ->
      setMyCookie (makeCookie "session" user) $
        ...



setMyCookie :: Cookie -> Response -> Response
```
---

class: code

```haskell
    Just user ->
      setMyCookie (makeCookie "session" user) $
        myRedirect ("/profile/" <> user)

myRedirect :: Text -> Response

setMyCookie :: Cookie -> Response -> Response
```










---

class: code

```haskell
get "/profile/:user" $ do
  c <- getCookie "session"
  case c of
    Nothing ->
      redirect "/login"
    Just session -> do
      user <- param "user"
      if session /= user then do
        status status403
        html "<body>Not allowed"
      else
        html "<body>Hello"
```

---

class: code

```haskell
userGet :: User -> Request -> Response
userGet user request -> do
  case getMyCookie request "session" of
    Nothing ->
      ....
    Just session ->
      ...


getMyCookie :: Request -> Text -> Maybe Text
getMyCookie request name = do
  ...
```

---

class: code

```haskell
userGet :: User -> Request -> Response
userGet user request -> do
  case getMyCookie request "session" of
    Nothing ->
      ....
    Just session ->
      ...


getMyCookie :: Request -> Text -> Maybe Text
getMyCookie request name = do
 cs <- lookup "Cookie" (requestHeaders request)
 lookup name (parseCookies cs)
```

---

class: code

```haskell
userGet :: User -> Request -> Response
userGet user request -> do
  case getMyCookie request "session" of
    Nothing ->
      myRedirect "/login"
    Just session ->
      if session /= user then
        responseLBS status403 []
          "<body>Not allowed"
      else
        responseLBS status200 []
          "<body>Hello"
```











---

class: center, middle, section-aqua, heading-white

# Routing

---

class: code

```haskell
routes = do
  get "/login" $
    ...
  post "/login" $
    ...
  get "/profile/:user" $
    ...
```

---

class: code

```haskell








pathInfo :: Request -> [Text]
```

---

class: code

```haskell
myRoutes :: Request -> IO Response
myRoutes request =
  case pathOf request of
    ["login"] ->
      ...
    ["profile", user] ->
      ...

pathInfo :: Request -> [Text]
```

---

class: center, middle, section-yellow, heading-black

#### Warning: Pattern match(es) are non-exhaustive

---

class: code

```haskell
myRoutes :: Request -> IO Response
myRoutes request =
  case pathOf request of
    ["login"] ->
      ...
    ["profile", user] ->
      ...
    _ ->
      responseLBS status404 []
        "<body>Not found"
```

???

- "Interesting, now _we_ have to deal with the 404"
- Less than ideal, what does the 404 look like?

---

class: code

```haskell











requestMethod :: Request -> Method
```

---

class: code

```haskell
myRoutes :: Request -> IO Response
myRoutes request =
  case pathOf request of
    ["login"] ->
      case requestMethod request of
        "GET" ->
           ...
        "POST" ->
           ...


requestMethod :: Request -> Method
```

---

class: center, middle, section-yellow, heading-black

#### Warning: Pattern match(es) are non-exhaustive

---

class: code

```haskell
myRoutes :: Request -> IO Response
myRoutes request =
  case pathOf request of
    ["login"] ->
      case requestMethod request of
        "GET" ->
           ...
        "POST" ->
           ...
        _ ->
          responseLBS status405 []
            "<body>Not Allowed"
```

---

class: code

```haskell
routes :: ScottyM ()
routes = do
  ...
  get "/profile/:user" $
    user <- getParam "user"
```

---

class: code

```haskell
routes :: ScottyM ()
routes = do
  ...
  get "/profile/:foo" $
    user <- getParam "bar"
```

???

- Needs type safety

---

## Haskell Routing

- [web-routes](http://hackage.haskell.org/package/web-routes)
- [web-routes-boomerang](http://hackage.haskell.org/package/web-routes-boomerang)
- [snap-web-routes](http://hackage.haskell.org/package/snap-web-routes)
- [reroute](http://hackage.haskell.org/package/reroute)

???

- I'm not suggesting pattern matching is way to do routing
- Cottage Industry





















---

class: center, middle, section-aqua, heading-white

# How do I run it?

---

class: code

```haskell
type Application = Request -> IO Response
```

---

class: code

```haskell
type Application = Request -> IO Response

run :: Port -> Application -> IO ()
```

---

class: center, middle, section-aqua, heading-white

# Warp

???

- One of the most common web servers

---

class: code

```haskell
type Application = Request -> IO Response

run :: Port -> Application -> IO ()
```

---

class: code

```haskell
type Application = Request -> IO Response

run :: Port -> Application -> IO ()

main :: IO ()
main =
  run 8080 $ \request ->
    case pathOf request of
      ["login"] ->
        ...
      ["profile", user] ->
        ...
```







---

class: center, middle, section-aqua, heading-white

# White Lie

---

class: code

```haskell
type Application =
  Request ->
  IO Response
```

---

class: code

```haskell
type Application =
  Request ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
```

???

- See resources at the end

---

class: code

```haskell
type Application =
  Request ->
  IO Response


main :: IO ()
main =
  run 8080 $ \request         ->
    case pathOf request of
      ["login"] ->
        return $
          responseLBS status404 [] ...
```

---

class: code

```haskell
type Application =
  Request ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived

main :: IO ()
main =
  run 8080 $ \request respond ->
    case pathOf request of
      ["login"] ->
        respond $
          responseLBS status404 [] ...
```



















---

class: center, middle, section-aqua, heading-white

# What is a web application?

---

class: center, middle, section-aqua, heading-white

# WAI = Data + Functions

---

## Resources

- http://blog.infinitenegativeutility.com/2016/8/resources--laziness--and-continuation-passing-style
- https://github.com/charleso/lambdajam-web-functions
- "Build yourself a Haskell web framework"
  - https://www.youtube.com/watch?v=etuSnom2v2M

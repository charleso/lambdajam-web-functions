class: center, bottom, heading-black
background-image: url(images/ambiata-chopsticks.png)

# Caught in a web of functions

---

class: middle

<a href="https://www.reddit.com/r/haskell/comments/16kqe0/recommended_haskell_web_framework_for_beginners/">
  <img src="images/recommended_haskell.png" width="700" />
</a>

---

class: middle

<a href="https://www.reddit.com/r/haskell/comments/1y3eff/the_simplest_haskell_web_framework_is/">
  <img src="images/recommended_haskell_3.png" width="700" />
</a>

---

class: middle

<a href="https://www.reddit.com/r/haskell/comments/2wfap0/web_development_using_haskell/">
  <img src="images/recommended_haskell_4.png" width="700" />
</a>

---

class: middle

<a href="https://www.reddit.com/r/haskell/comments/5gu7op/haskell_equivalent_of_spring_framework/">
  <img src="images/recommended_haskell_2.png" width="700" />
</a>

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

## &lt;insert contrived example&gt;

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

-- http://hackage.haskell.org/package/wai
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

-- http://hackage.haskell.org/package/wai
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
-- http://hackage.haskell.org/package/wai
requestBody :: Request -> IO ByteString

-- http://hackage.haskell.org/package/http-types
parseQueryText ::
  ByteString -> [(Text, Maybe Text)]
```

---

class: code

```haskell
-- http://hackage.haskell.org/package/wai
requestBody :: Request -> IO ByteString

-- http://hackage.haskell.org/package/http-types
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
loginPost :: Request -> IO Response
loginPost request = do
  b <- parseBody request
  case lookup "username" b of
    Just user ->
```

---

class: code

```haskell
loginPost :: Request -> IO Response
loginPost request = do
  b <- parseBody request
  case lookup "username" b of
    Just user ->

        myRedirect ("/profile/" <> user)


myRedirect :: Text -> Response
```

---

class: code

```haskell
loginPost :: Request -> IO Response
loginPost request = do
  b <- parseBody request
  case lookup "username" b of
    Just user ->

        myRedirect ("/profile/" <> user)


myRedirect :: Text -> Response
myRedirect uri =
  responseLBS
    status302
    [("Location", uri)]
    "<body>Redirect"
```

---

class: code

```haskell
loginPost :: Request -> IO Response
loginPost request = do
  b <- parseBody request
  case lookup "username" b of
    Just user ->
      setMyCookie (makeCookie "session" user) $
        myRedirect ("/profile/" <> user)


setMyCookie :: Cookie -> Response -> Response

-- http://hackage.haskell.org/package/cookie
makeCookie :: ByteString -> ByteString -> Cookie
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
      setMyCookie (makeCookie "session" user) $
        myRedirect ("/profile/" <> user)
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

-- http://hackage.haskell.org/package/cookie
parseCookies : ByteString -> Cookies
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








-- http://hackage.haskell.org/package/wai
pathInfo :: Request -> [Text]
```

---

class: code

```haskell
myApp :: Request -> IO Response
myApp request =
  case pathInfo request of
    ["login"] ->
      ...
    ["profile", user] ->
      ...

-- http://hackage.haskell.org/package/wai
pathInfo :: Request -> [Text]
```

---

class: code

```haskell
myApp :: Request -> IO Response
myApp request =
  case pathInfo request of
    ["login"] ->
      ...
    ["profile", user] ->
      ...

-- http://hackage.haskell.org/package/wai
pathInfo :: Request -> [Text]
```

<pre><code class="hljs"><span style="color: #FFC039;">Warning: Pattern match(es) are non-exhaustive</span>
</code></pre>

---

class: code

```haskell
myApp :: Request -> IO Response
myApp request =
  case pathInfo request of
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










-- http://hackage.haskell.org/package/wai
requestMethod :: Request -> Method
```

---

class: code

```haskell
myApp :: Request -> IO Response
myApp request =
  case pathInfo request of
    ["login"] ->
      case requestMethod request of
        "GET" ->
           ...
        "POST" ->
           ...

-- http://hackage.haskell.org/package/wai
requestMethod :: Request -> Method
```

---

class: code

```haskell
myApp :: Request -> IO Response
myApp request =
  case pathInfo request of
    ["login"] ->
      case requestMethod request of
        "GET" ->
           ...
        "POST" ->
           ...

-- http://hackage.haskell.org/package/wai
requestMethod :: Request -> Method
```

<pre><code class="hljs"><span style="color: #FFC039;">Warning: Pattern match(es) are non-exhaustive</span>
</code></pre>

---

class: code

```haskell
myApp :: Request -> IO Response
myApp request =
  case pathInfo request of
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

class: center, middle, section-yellow, heading-black

# Complete

---

class: code

```haskell
myApp :: Request -> IO Response
myApp request =
  case pathInfo request of
    ["login"] ->
      case requestMethod request of
        "GET" ->
           loginGet
        "POST" ->
           loginPost request
    ["profile", user] ->
      case requestMethod request of
        "GET" ->
           userGet user request
```

---

class: code

```haskell
myApp :: Request -> IO Response
myApp request =
  case (requestMethod request, pathInfo request) of
    ("GET", ["login"]) ->
      loginGet
    ("POST", ["login"]) ->
      loginPost request
    ("GET", ["profile", user]) ->
      userGet user request
```

---

class: center, middle, section-yellow, heading-black

# Type Safe Routing?

---

class: code

```haskell
routes = do
  ...
  get "/profile/:user" $
    user <- getParam "user"
```

---

class: code

```haskell
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

-- http://hackage.haskell.org/package/warp
run :: Port -> Application -> IO ()
```

---

class: code

```haskell
type Application = Request -> IO Response

-- http://hackage.haskell.org/package/warp
run :: Port -> Application -> IO ()

main :: IO ()
main =
  run 8080 $ \request ->
    case pathInfo request of
      ["login"] ->
        ...
      ["profile", user] ->
        ...
```







---

class: image, bottom

<img src="http://berkreviews.com/wp-content/uploads/2016/12/untitled1.png" />

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


myApp :: Application
myApp request         =
  case pathInfo request of
    ["login"] ->
      return $
        responseLBS status200 [] ...
```

---

class: code

```haskell
type Application =
  Request ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived

myApp :: Application
myApp request respond =
  case pathInfo request of
    ["login"] ->
      respond $
        responseLBS status200 [] ...
```










---

class: center, middle, section-aqua, heading-white

# More?

???

- Brief look at some other things




---

class: center, middle, section-yellow, heading-black

# Content negotation

---

class: code

```haskell
homeGet ::            Response
homeGet         =
  responseLBS status200 [] $
    "<body>Hello"
```

---

class: code

```haskell
homeGet ::            Response
homeGet         =
  responseLBS status200 [] $
    "<body>Hello"




-- http://hackage.haskell.org/package/http-media
mapAcceptMedia ::
  Accept -> [(MediaType, b)] -> Maybe b
```

---

class: code

```haskell
homeGet :: Request -> Response
homeGet request =
  responseLBS status200 [] $
    mapAcceptMedia (acceptHeader request) $ [
        ("text/html", "<body>Hello")
      , ("application/json", "{}")
      ]

-- http://hackage.haskell.org/package/http-media
mapAcceptMedia ::
  Accept -> [(MediaType, b)] -> Maybe b
```











---

class: center, middle, section-yellow, heading-black

# Error Handling

---

class: code

```haskell
myApp :: Request -> Response
myApp =
  case pathInfo request of
    ["profile", user] ->
      ...



    _ ->
      responseLBS status404 []
        "<body>Not found"
```

---

class: code

```haskell
myApp :: Request -> Response
myApp =
  case pathInfo request of
    ["profile", user] ->
      case getUser request of
        Nothing ->
          responseLBS status404 []
            "<body>Not found"
    _ ->
      responseLBS status404 []
        "<body>Not found"
```

---

class: code

```haskell
myApp :: Request -> Response
myApp =
  case pathInfo request of
    ["profile", user] ->
      case getUser request of
        Nothing ->
          notFound

    _ ->
      notFound

notFound :: Response
notFound =
  responseLBS status404 []
    "<body>Not found"
```

---

class: code

```haskell
myApp :: Request -> Either Error Response
myApp =
  case pathInfo request of
    ["profile", user] ->
      case getUser request of
        Nothing ->
          Left NotFound

    _ ->
      Left NotFound

data Error =
    NotFound
```

---

class: code

```haskell
myApp :: Request -> Either Error Response

errorResp :: Either Error Response -> Response
errorResp e =
  case e of
    Left NotFound ->
      responseLBS status404 []
        "<body>Not found"
    Right r ->
      r

data Error =
    NotFound
```

---

class: code

```haskell
data Error =
    NotFound
```

---

class: code

```haskell
data Error =
    NotFound
  | NotAllowed
```

---

class: code

```haskell
data Error =
    NotFound
  | NotAllowed
  | PermissionDenied
```












---

class: center, middle, section-aqua, heading-white

# What web framework should I use?

---

class: center, middle, section-aqua, heading-white

# What is a web application?

---

class: center, middle, section-aqua, heading-white

# WAI

---

class: center, middle, section-aqua, heading-white

# WAI = Data + Functions

---

## Web Libraries

- [wai](https://hackage.haskell.org/package/wai)
- [http-types](https://hackage.haskell.org/package/http-types)
- [cookie](https://hackage.haskell.org/package/cookie)
- [http-media](https://hackage.haskell.org/package/http-media)
- [wai-extra](https://hackage.haskell.org/package/wai-extra)

---

## Resources

- http://blog.infinitenegativeutility.com/2016/8/resources--laziness--and-continuation-passing-style
- https://github.com/charleso/lambdajam-web-functions
- "Build yourself a Haskell web framework"
  - https://www.youtube.com/watch?v=etuSnom2v2M

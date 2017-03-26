class: center, bottom, heading-black
background-image: url(images/ambiata-chopsticks.png)

# Caught in a web of functions

---

<img src="images/recommended_haskell.png" width="700" />

<img src="images/recommended_haskell_2.png" width="700" />

---

## What do we expect from a web framework?

- Routing
- Response Building
- Error handling?
- Content Type Negotiation?

---

## Basic Example

- `/`
  If logged in then redirect to profile,
  otherwise redirect to login
- `/login`
  Require username for login
- `/profile/{user}`
  Show username if matches current user,
  otherwise an unauthorised page

---

## Wai

```haskell
type Application =
  Request ->
  (Response -> IO ResponseReceived) ->
  IO ResponseReceived
```

---

## [Servlet](https://docs.oracle.com/javaee/7/api/javax/servlet/http/HttpServlet.html)

```java
interface HttpServlet {

  void service(
    HttpServletRequest req,
    HttpServletResponse resp
  );
}
```

---

## Wai Simplified

```haskell
type Application = Request -> IO Response
```









---

## Routing

```
routes :: (Request -> IO Response) -> Request -> IO Response
routes notFound req =
  case Wai.pathInfo req of
    [] ->
      home req
    ["login"] ->
      login request
    ["profile", user] ->
      profile user req
    _ ->
      notFound req
```

---

## Scotty

> A Haskell web framework inspired by Ruby's Sinatra, using WAI and Warp.

https://hackage.haskell.org/package/scotty

???

This is _not_ about critising Scotty, it's just one example

---

## Routing: Scotty

```haskell
routes :: ScottyM ()
routes = do
  get "/" $
    homeGet
  get "/login" $
    loginGet
  post "/login" $
    loginPost
  get "/profile/:user" $ do
    name <- param "user2"
    profileGet name
```

---

## Routing: Scotty (Bug)

```haskell
routes :: ScottyM ()
routes = do
  get "/" $
    homeGet
  get "/login" $
    loginGet
  post "/login" $
    loginPost
  get "/profile/:user" $ do
    name <- param "user2"
    profileGet name
```

???

TODO highlight error

---

## Routing: Scotty (Fix)

```haskell
routes :: ScottyM ()
routes = do
  get "/" $
    homeGet
  get "/login" $
    loginGet
  post "/login" $
    loginPost
  get "/profile/:user" $ do
    name <- param "user"
    profileGet name
```

---

## More Routing Features

- Type-safe
- Bi-directional

???

This is not a tutorial about routing.
Prefer to show other examples of web frameworks.










---

## Scotty

```haskell
type ActionT m a =
  ReaderT Request (StateT Response m) a
```

???

TODO When/Should you should this in the presentation?

---

## Scotty

```haskell
type ActionT m a =
  ReaderT Request (StateT Response m) a
```

- `ReaderT`: Global arguments
- `StateT`: Global state










---

class: center, middle, section-aqua, heading-white

# Request handling

---

## Get Cookie (Wai)

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 c <- lookup "Cookie" . Wai.requestHeaders $ req
 lookup name $ parseCookies c
```

---

## Get Cookie (Scotty)

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 c <- lookup "Cookie" . Wai.requestHeaders $ req
 lookup name $ parseCookies c
```

```haskell
getCookie :: Monad m => Text -> ActionT m (Maybe Text)
getCookie name = do
  c <- header "Cookie"
  return . lookup name $ parseCookies c
```

---

## Get Cookie (Scotty)

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 c <- lookup "Cookie" . Wai.requestHeaders $ req
 lookup name $ parseCookies c
```

```haskell
getCookie :: Monad m => Text -> ActionT m (Maybe Text)
getCookie name = do
  c <- header "Cookie"
  return . lookup name $ parseCookies c
```

```haskell
getCookie "foo"
```


---

## Get Cookie (Scotty unrolled)

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 c <- lookup "Cookie" . Wai.requestHeaders $ req
 lookup name $ parseCookies c
```

```haskell
getCookie :: Monad m => Text -> RequestT (StateT Response m) (Maybe Text)
getCookie name = do
  c <- header "Cookie"
  return . lookup name $ parseCookies c
```

---

## Get Cookie (Scotty unrolled)

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 c <- lookup "Cookie" . Wai.requestHeaders $ req
 lookup name $ parseCookies c
```

```haskell
getCookie :: Monad m => Text -> Request -> StateT Response m (Maybe Text)
getCookie name req = do
  c <- lookup "Cookie" . Wai.requestHeaders $ req
  return . lookup name $ parseCookies c
```

---

## Get Cookie (Scotty unrolled)

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 c <- lookup "Cookie" . Wai.requestHeaders $ req
 lookup name $ parseCookies c
```

```haskell
getCookie :: Monad m => Text -> Request -> Response -> m (Response, Maybe Text)
getCookie name req resp = do
  c <- lookup "Cookie" . Wai.requestHeaders $ req
  return (resp, lookup name $ parseCookies c)
```

---

## Get Cookie (Scotty unrolled)

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 c <- lookup "Cookie" . Wai.requestHeaders $ req
 lookup name $ parseCookies c
```

```haskell
getCookie :: Monad m => Text -> Request -> m (Maybe Text)
getCookie name req = do
  c <- lookup "Cookie" . Wai.requestHeaders $ req
  return (lookup name $ parseCookies c)
```

---

## Get Cookie (Scotty unrolled)

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 c <- lookup "Cookie" . Wai.requestHeaders $ req
 lookup name $ parseCookies c
```

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
  c <- lookup "Cookie" . Wai.requestHeaders $ req
  lookup name $ parseCookies c
```

---

## Get Cookie (Wai + Scotty)

```haskell
getCookie :: Text -> Request -> Maybe Text
getCookie name req = do
 c <- lookup "Cookie" . Wai.requestHeaders $ req
 lookup name $ parseCookies c
```

```haskell
getCookie "foo" <$> request
```

```haskell
request :: Monad m => ActionT e m Request
```

---

class: center, middle, section-yellow, heading-black

# Be precise
















---

class: center, middle, section-aqua, heading-white

# Response Building

---

## Set Cookie (Wai)

```haskell
setCookie :: SetCookie -> Response -> Response
setCookie c =
  addHeader ("Set-Cookie", renderSetCookie c)
```

```haskell
addHeader :: Header -> Response -> Response
```

---

## Add Headers (Wai)

```haskell
addHeader :: Header -> Response -> Response
addHeader header response =
  resp {
      responseHeaders =
        header : responseHeaders response
    }
```

```haskell
data Response {
  ...
  , responseHeaders :: [Header]
  }
```


---

## Set Cookie (Scotty)

```haskell
setCookie :: Monad m => SetCookie -> ActionT e m ()
setCookie c =
  addHeader ("Set-Cookie", renderSetCookie c)
```

```haskell
addHeader :: Monad m => Header -> ActionT e m ()
```

???

TODO

- How would you test?

---

## Add Header (Scotty)

```haskell
addHeader :: Header -> Response -> Response
addHeader h r =
  response { responseHeaders = r : responseHeaders r }
```

```haskell
addHeader :: Monad m => ActionT m ()
addHeader header =
  modify (\response -> addHeaderWai header response)
```

---

## Add Header (Scotty unrolled)

```haskell
addHeader :: Header -> Response -> Response
addHeader h r =
  response { responseHeaders = r : responseHeaders r }
```

```haskell
addHeader :: Monad m => Header -> ReaderT Request (StateT Response m) ()
addHeader header =
  modify (\response -> addHeaderWai header response)
```

---

## Add Header (Scotty unrolled)

```haskell
addHeader :: Header -> Response -> Response
addHeader h r =
  response { responseHeaders = r : responseHeaders r }
```

```haskell
addHeader :: Monad m => Header -> Request -> StateT Response m ()
addHeader header req =
  modify (\response -> addHeaderWai header response)
```

---

## Add Header (Scotty unrolled)

```haskell
addHeader :: Header -> Response -> Response
addHeader h r =
  response { responseHeaders = r : responseHeaders r }
```

```haskell
addHeader :: Monad m => Header -> Request -> StateT Response m ()
addHeader header req =
  modify (\response -> addHeaderWai header response)
```

```sh
main/application.hs:59:14:
  Warning: Defined but not used: ‘req’

<no location info>:
Failing due to -Werror.
```

---

## Add Header (Scotty unrolled)

```haskell
addHeader :: Header -> Response -> Response
addHeader h r =
  response { responseHeaders = r : responseHeaders r }
```

```haskell
addHeader :: Monad m => Header -> StateT Response m ()
addHeader header =
  modify (\response -> addHeaderWai header response)
```

---

## Add Header (Scotty unrolled)

```haskell
addHeader :: Header -> Response -> Response
addHeader h r =
  response { responseHeaders = r : responseHeaders r }
```

```haskell
addHeader :: Header -> Response -> Response
addHeader h r =
  response { responseHeaders = r : responseHeaders r }
```

---

## Add Header (Wai + Scotty)

```haskell
addHeader :: Header -> Response -> Response
```

```haskell
modifyResponse :: (Response -> Response) -> ActionT m ()
```

```haskell
modifyResponse (addHeader cookieHeader)
```


---

class: center, middle, section-yellow, heading-black

# Be precise!

???

- Compiler errors are you friend












---

# Good Functions

---

## Good Functions

- Be precise
- Compiler warings should be errors
- Composition

---

## Resources

- https://github.com/charleso/lambdajam-web-functions
- "Build yourself a Haskell web framework"
  - https://www.youtube.com/watch?v=etuSnom2v2M

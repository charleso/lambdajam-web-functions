class: center, bottom, heading-black
background-image: url(images/ambiata-chopsticks.png)

# Caught in a web of functions

---

<img src="images/recommended_haskell.png" width="700" />

<img src="images/recommended_haskell_2.png" width="700" />

---

## What do we expect from a web framework?

- Routing
  - Path
  - Method
- Request Parsing
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

## Wai Simplified

```haskell
type Application =
  Request -> IO Response
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
  get "/profile/:user" $
    profileGet
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
  get "/profile/:user" $
    profileGet
```

```
500 Internal Server Error

Param: username not found!
```

???

TODO highlight error

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
  get "/profile/:user" $
    profileGet

profileGet :: ActionT m ()
profileGet =
  name <- param "username"
  ...
```

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

???

- What tradeoffs are we making?
- Nice syntax doesn't make it safe

---

## Routing

```haskell
routes :: Application
routes req =
  case pathInfo req of
    [] ->
      home req
    ["login"] ->
      login request
    ["profile", user] ->
      profile user req
    _ ->
      error "Not found"
```

---

## Routing

```haskell
routes :: Request -> IO Response
routes req =
  case pathInfo req of
    [] ->
      home req
    ["login"] ->
      login request
    ["profile", user] ->
      profile user req
    _ ->
      error "Not found"
```

```haskell
pathInfo :: Request -> [Text]
```

---

## Routing

```haskell
routes :: (Request -> IO Response) ->
  Request -> IO Response
routes notFound req =
  case pathInfo req of
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

## Get Header (Scotty)

```haskell
getHeader :: Monad m => Text ->
  ActionT m (Maybe Text)
```

---

## Get Header (Scotty)

```haskell
getHeader :: Monad m => Text ->
  ActionT m (Maybe Text)
getHeader name = do
  req <- request
  return . lookup name . requestHeaders $ req
```

```haskell
request :: Monad m => ActionT e m Request
```

---

## Get Header (Scotty unrolled)

```haskell
getHeader :: Monad m => Text ->
  ReaderT Request (StateT Response m) (Maybe Text)
getHeader name = do
  req <- request
  return . lookup name . requestHeaders $ req
```

---

## Get Header (Scotty unrolled)

```haskell
getHeader :: Monad m => Text ->
  Request -> StateT Response m (Maybe Text)
getHeader name req =
  return . lookup name . requestHeaders $ req
```

---

## Get Header (Scotty unrolled)

```haskell
getHeader :: Monad m => Text ->
  Request -> Response -> m (Response, Maybe Text)
getHeader name req resp =
  return (resp, lookup name . requestHeaders $ req)
```

---

## Get Header (Scotty unrolled)

```haskell
getHeader :: Monad m => Text ->
  Request -> m (Maybe Text)
getHeader name req =
  return . lookup name . requestHeaders $ req
```

---

## Get Header (Scotty unrolled)

```haskell
getHeader :: Text ->
  Request -> Maybe Text
getHeader name req =
  lookup name . requestHeaders $ req
```

---

## Get Header (Wai + Scotty)

```haskell
getHeader :: Text ->
  Request -> Maybe Text
getHeader name req =
 lookup name . requestHeaders $ req
```

```haskell
getHeader "foo" <$> request
```

---

class: center, middle, section-yellow, heading-black

# Be precise
















---

class: center, middle, section-aqua, heading-white

# Response Building

---

## Set Cookie (Scotty)

```haskell
addHeader :: Monad m => Header ->
  ActionT e m ()
```

---

## Add Header (Scotty)

```haskell
addHeader :: Monad m =>
  ActionT m ()
addHeader h =
  modify (\resp ->
    resp { responseHeaders = h : responseHeaders resp }
  )
```

```haskell
modify :: (Response -> Response) -> ActionT m ()
```

```haskell
responseHeaders :: Response -> [Header]
```



---

## Add Header (Scotty unrolled)

```haskell
addHeader :: Monad m => Header ->
  ReaderT Request (StateT Response m) ()
addHeader h =
  modify (\resp ->
    resp { responseHeaders = h : responseHeaders resp }
  )
```

---

## Add Header (Scotty unrolled)

```haskell
addHeader :: Monad m => Header ->
  Request -> StateT Response m ()
addHeader h req =
  modify (\resp ->
    resp { responseHeaders = h : responseHeaders resp }
  )
```

---

## Add Header (Scotty unrolled)

```haskell
addHeader :: Monad m => Header ->
  Request -> StateT Response m ()
addHeader h req =
  modify (\resp ->
    resp { responseHeaders = h : responseHeaders resp }
  )
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
addHeader :: Monad m => Header ->
  StateT Response m ()
addHeader h =
  modify (\resp ->
    resp { responseHeaders = h : responseHeaders resp }
  )
```

---

## Add Header (Scotty unrolled)

```haskell
addHeader :: Header ->
  Response -> Response
addHeader h resp =
  resp { responseHeaders = h : responseHeaders resp }
```

---

## Add Header (Wai + Scotty)

```haskell
addHeader :: Header ->
  Response -> Response
addHeader h resp =
  resp { responseHeaders = h : responseHeaders resp }
```

```haskell
modifyResponse (addHeader ("Set-Cookie", "abc"))
```


---

class: center, middle, section-yellow, heading-black

# Be precise!

???

- Compiler errors are you friend












---

class: center, middle, section-aqua, heading-white

# Good Functions

---

## Good Functions

- Be precise
- Compiler warnings should be errors
- Composition

---

## Resources

- https://github.com/charleso/lambdajam-web-functions
- "Build yourself a Haskell web framework"
  - https://www.youtube.com/watch?v=etuSnom2v2M

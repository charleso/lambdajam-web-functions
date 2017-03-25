import           Charlotte.Wai

import qualified Network.Wai.Handler.Warp as Warp
import           Network.Wai.Middleware.RequestLogger (logStdout)

main :: IO ()
main =
  Warp.runSettings
    (Warp.setPort 8080 Warp.defaultSettings) $
      logStdout $
        application

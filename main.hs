import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (makeApplication)
import Control.Applicative  (pure)

main :: IO ()
main = defaultMain (fromArgs $ \_ _ -> pure ()) makeApplication

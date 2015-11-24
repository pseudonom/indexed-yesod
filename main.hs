{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Control.Monad.Reader
import           Yesod

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
|]

instance Yesod HelloWorld

type Handler' = ReaderT () (HandlerT HelloWorld IO)

getHomeR :: Handler' Html
getHomeR = lift $ defaultLayout [whamlet|Hello World!|]

main :: IO ()
main = warp 3000 HelloWorld

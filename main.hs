{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE PolyKinds             #-}

import           Prelude hiding ((>>=))
import qualified Prelude as P
import           Data.String (fromString)

import           Yesod
import           Yesod.Core.Internal (mkYesodGeneral, ToHandler(..))


ifThenElse :: Bool -> a -> a -> a
ifThenElse True t _ = t
ifThenElse False _ f = f


data HelloWorld = HelloWorld

uncurry (++) <$> mkYesodGeneral "HelloWorld" [] False True [parseRoutes|
/sensitive SensitiveR GET
|]

instance Yesod HelloWorld


newtype IndexedHandler (i :: k) (o :: k) site m a
  = IndexedHandler { runIndexedHandler :: HandlerT site m a }
instance ToHandler (IndexedHandler 'No 'Yes) where
  unwrap = runIndexedHandler

iReturn :: (Monad m) => a -> IndexedHandler c c site m a
iReturn = IndexedHandler . P.return

iBind
  :: (Monad m)
  => IndexedHandler c1 c2 site m a -> (a -> IndexedHandler c2 c3 site m b)
  -> IndexedHandler c1 c3 site m b
IndexedHandler m `iBind` f = IndexedHandler $ runIndexedHandler . f =<< m


data AuthChecked = Yes | No

getSensitiveR :: (Monad m) => IndexedHandler ('No :: AuthChecked) ('Yes :: AuthChecked) site m ()
getSensitiveR = do
  -- authed <- iReturn True
  authed <- checkAuth
  if authed
    then sensitiveData
    else iReturn ()
  where
    (>>=) = iBind

checkAuth :: (Monad m) => IndexedHandler ('No :: AuthChecked) ('Yes :: AuthChecked) site m Bool
checkAuth = IndexedHandler . P.return $ True

sensitiveData :: (Monad m) => IndexedHandler ('Yes :: AuthChecked) ('Yes :: AuthChecked) site m ()
sensitiveData = iReturn ()


main :: IO ()
main = warp 3000 HelloWorld

{-# LANGUAGE JavaScriptFFI #-}
module Jwt
where

import Data.JSString
import qualified Data.JSString as JSS
import Data.JSString.Text
import Data.Text
import GHCJS.Marshal
import Language.Javascript.JSaddle.Value
import System.IO (IO)
import Data.Maybe
import Protolude

foreign import javascript unsafe
  "window['localStorage']['getItem']('jwt')"
  js_getJwt :: IO JSVal

lookupJwtToken :: IO (Maybe Text)
lookupJwtToken = js_getJwt >>= GHCJS.Marshal.fromJSValUnchecked

foreign import javascript unsafe
  "window['localStorage']['setItem']('jwt', $1)"
  js_setJwt :: JSS.JSString -> IO ()

setJwtToken :: Text -> IO ()
setJwtToken t = js_setJwt $ Data.JSString.Text.textToJSString t

foreign import javascript unsafe
  "window['localStorage']['removeItem']('jwt');"
  clearJwtToken :: IO ()

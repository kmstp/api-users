{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | An @CSS@ empty data type with `MimeRender` instances for @clay@'s
-- `Css` datatype.
-- You should only need to import this module for it's instances and the
-- `CSS` datatype.:
--
-- >>> type Eg = Get '[CSS] Css
module Common.Clay where

import Clay (Css, compact, renderWith)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.Typeable (Typeable)
import Servant.API (Accept(..), MimeRender(..))

import qualified Network.HTTP.Media as M
import Protolude hiding (encodeUtf8)

data CSS deriving Typeable

-- | @text/css;charset=utf-8@
instance Accept CSS where
  contentType _ = "text" M.// "css" M./: ("charset", "utf-8")

instance MimeRender CSS Css where
  mimeRender _ = encodeUtf8 . renderWith compact []

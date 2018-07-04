{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
module Hack
where

import Protolude
import qualified GHC.Show
import Data.Typeable

data Zero 
data Succ n

type Three = Succ (Succ (Succ Zero))

data True
data False

class Even n b | n -> b where even_ :: n -> b
class Odd n b | n -> b where odd_ :: n -> b

instance Even Zero True
instance Odd n b => Even (Succ n) b
instance Odd Zero False
instance Even n b => Odd (Succ n) b

three = undefined :: Three

u = undefined

print_ x = print $ typeOf  x

test3 :: IO ()
test3 = print_ (even_ three) -- False

class Add a b c | a b -> c where add_ :: a -> b -> c
instance Add Zero b b
instance Add a b c => Add (Succ a) b (Succ c)

test4 :: IO ()
test4 = print_ $ add_ (u :: Three) (u :: Three)

class Mul a b c | a b -> c where mul_ :: a -> b -> c
instance Mul Zero b Zero
instance (Mul a b c, Add b c d) => Mul (Succ a) b d

class Pow a b c | a b -> c where pow_ :: a -> b -> c
instance Pow a Zero (Succ Zero)
instance (Pow a b c, Mul a c d) => Pow a (Succ b) d
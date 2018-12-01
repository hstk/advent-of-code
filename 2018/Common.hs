module Common where

import Text.Trifecta (Result(..))

fromResult :: a -> Result a -> a
fromResult base res = case res of
  Success a -> a
  _ -> base
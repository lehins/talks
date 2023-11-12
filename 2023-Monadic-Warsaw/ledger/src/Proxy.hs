{-# LANGUAGE ScopedTypeVariables #-}

module Proxy where

import Data.Proxy

printMempty :: forall m. (Monoid m, Show m) => Proxy m -> IO ()
printMempty _ = print (mempty :: m)

-- printMempty :: IO ()
-- printMempty = print mempty

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
module Hash (Hash, HashAlgorithm (..), Blake2b224, Blake2b256) where

--import GHC.TypeLits
import Data.ByteString

ffiHashBlake2b224 :: ByteString -> ByteString
ffiHashBlake2b224 = error "FFI binding to Libsodium"

ffiHashBlake2b256 :: ByteString -> ByteString
ffiHashBlake2b256 = error "FFI binding to Libsodium"

--- No type safety. Easy to mix them up. Length is not statically known.
---

newtype HashBlake2b224 = HashBlake2b224 ByteString
newtype HashBlake2b256 = HashBlake2b256 ByteString

hashBlake2b224 :: ByteString -> HashBlake2b224
hashBlake2b224 = HashBlake2b224 . ffiHashBlake2b224

hashBlake2b256 :: ByteString -> HashBlake2b256
hashBlake2b256 = HashBlake2b256 . ffiHashBlake2b256

--

newtype Hash h = Hash ByteString
  deriving (Eq, Ord, Show)

type role Hash nominal

class HashAlgorithm h where
  hash :: ByteString -> Hash h

data Blake2b224

instance HashAlgorithm Blake2b224 where
  hash = Hash . ffiHashBlake2b224

data Blake2b256

instance HashAlgorithm Blake2b256 where
  hash = Hash . ffiHashBlake2b256

-- newtype Hash h = Hash ByteString
--   deriving (Eq, Ord, Show)

-- class KnownNat (HashSize h) => HashAlgorithm h where
--   type HashSize h :: Nat
--   hash :: ByteString -> Hash h
--   hashSize :: Proxy h -> Integer
--   hashSize _ = natVal (Proxy @(HashSize h))

-- data Blake2b224

-- instance HashAlgorithm Blake2b224 where
--   type HashSize Blake2b224 = 28
--   hash = Hash . ffiHashBlake2b224

-- data Blake2b256

-- instance HashAlgorithm Blake2b256 where
--   type HashSize Blake2b256 = 32
--   hash = Hash . ffiHashBlake2b256

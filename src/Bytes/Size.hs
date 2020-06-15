{-# LANGUAGE TypeFamilyDependencies #-}

module Bytes.Size
  ( ByteSz (..),
    IncByteSz (..),
  )
where

data ByteSz
  = B
  | KB
  | MB
  | GB
  | TB
  deriving (Eq, Show)

class IncByteSz a where
  type Next a = r | r -> a
  next :: a -> Next a
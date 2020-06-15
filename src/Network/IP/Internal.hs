{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Network.IP.Internal
  ( IPType (..),
    IP (..),
    combineIPs,
  )
where

data IPType
  = Local
  | Global
  deriving Show

newtype IP (a :: IPType) = MkIP String
  deriving Show

combineIPs :: IP 'Local -> IP 'Global -> String
combineIPs l g =
  dispLocalIP l
    <> "\n"
    <> dispGlobalIP g

dispLocalIP :: IP 'Local -> String
dispLocalIP (MkIP s) = "Local  IP: " <> s

dispGlobalIP :: IP 'Global -> String
dispGlobalIP (MkIP s) = "Global IP: " <> s
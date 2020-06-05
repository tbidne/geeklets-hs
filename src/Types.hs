{-# LANGUAGE GADTs #-}

module Types
  ( CLI (..),
    ParseResult (..),
    RunResult (..),
    RunResultStr,
    runCLI,
  )
where

import Freer

data ParseResult
  = PSuccess String
  | PFailure String
  deriving (Eq, Show)

data RunResult a
  = RSuccess a
  | RFailure String
  deriving (Eq, Show)

type RunResultStr = RunResult String

instance Functor RunResult where
  fmap f (RSuccess x) = RSuccess $ f x
  fmap _ (RFailure x) = RFailure x

instance Applicative RunResult where
  pure = RSuccess

  (RFailure f) <*> _ = RFailure f
  _ <*> (RFailure x) = RFailure x
  (RSuccess f) <*> (RSuccess x) = RSuccess $ f x

instance Monad RunResult where
  (RFailure x) >>= _ = RFailure x
  (RSuccess x) >>= f = f x

extractRResult :: RunResultStr -> String
extractRResult (RFailure s) = "Error: " <> s
extractRResult (RSuccess s) = s

data CLI a where
  GetArgs :: CLI [String]
  ParseArgs :: [String] -> CLI ParseResult
  RunCmd :: String -> CLI RunResultStr
  PutStrLn :: String -> CLI ()

runCLI :: Freer CLI ()
runCLI = do
  args <- etaF GetArgs
  cmd <- (etaF . ParseArgs) args
  case cmd of
    PFailure s -> etaF $ PutStrLn s
    PSuccess s -> do
      res <- etaF $ RunCmd s
      etaF $ PutStrLn $ extractRResult res

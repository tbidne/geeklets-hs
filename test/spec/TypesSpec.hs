{-# LANGUAGE GADTs #-}

module TypesSpec
  ( dslSpec,
  )
where

import Freer
import Test.Hspec
import qualified Test.Tasty as T
import qualified Test.Tasty.Hspec as TH
import Types

dslSpec :: IO [T.TestTree]
dslSpec = TH.testSpecs $ do
  describe "Types" $ do
    it "DSL parse and run succeeds" $
      foldFreer interpretSuccess runCLI `shouldSatisfy` verifyCLI
    it "DSL parse failure" $
      foldFreer interpretParseFail runCLI `shouldSatisfy` verifyParseFail
    it "DSL cmd failure" $
      foldFreer interpretCmdFail runCLI `shouldSatisfy` verifyCmdFail

verifyCLI :: Identity () -> Bool
verifyCLI (MkIdentity [Retrieved, Parsed, Ran, Put Success] ()) = True
verifyCLI _ = False

verifyParseFail :: Identity () -> Bool
verifyParseFail (MkIdentity [Retrieved, Parsed, Put ParseFail] ()) = True
verifyParseFail _ = False

verifyCmdFail :: Identity () -> Bool
verifyCmdFail (MkIdentity [Retrieved, Parsed, Ran, Put CmdFail] ()) = True
verifyCmdFail _ = False

data Result
  = ParseFail
  | CmdFail
  | Success
  deriving (Eq, Show)

data Action
  = Retrieved
  | Parsed
  | Ran
  | Put Result
  deriving (Eq, Show)

data Identity a = MkIdentity [Action] a
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (MkIdentity as l) <> (MkIdentity bs r) = MkIdentity (as <> bs) (l <> r)

instance Monoid a => Monoid (Identity a) where
  mempty = MkIdentity [] mempty

instance Functor Identity where
  fmap f (MkIdentity as x) = MkIdentity as $ f x

instance Applicative Identity where
  pure = MkIdentity []
  (MkIdentity as f) <*> (MkIdentity bs x) = MkIdentity (as <> bs) $ f x

instance Monad Identity where
  (MkIdentity as x) >>= f =
    let (MkIdentity bs x') = f x
     in MkIdentity (as <> bs) x'

interpretSuccess :: CLI a -> Identity a
interpretSuccess GetArgs = MkIdentity [Retrieved] ["good arg"]
interpretSuccess (ParseArgs xs) = pureParseArgs xs
interpretSuccess (RunCmd cmd) = pureRunCmd cmd
interpretSuccess (PutStrLn s) = purePutStrLn s

interpretParseFail :: CLI a -> Identity a
interpretParseFail GetArgs = MkIdentity [Retrieved] []
interpretParseFail (ParseArgs xs) = pureParseArgs xs
interpretParseFail (RunCmd cmd) = pureRunCmd cmd
interpretParseFail (PutStrLn s) = purePutStrLn s

interpretCmdFail :: CLI a -> Identity a
interpretCmdFail GetArgs = MkIdentity [Retrieved] ["other arg"]
interpretCmdFail (ParseArgs xs) = pureParseArgs xs
interpretCmdFail (RunCmd cmd) = pureRunCmd cmd
interpretCmdFail (PutStrLn s) = purePutStrLn s

pureParseArgs :: [String] -> Identity ParseResult
pureParseArgs ["good arg"] = MkIdentity [Parsed] $ PSuccess "good cmd"
pureParseArgs ["other arg"] = MkIdentity [Parsed] $ PSuccess "bad cmd"
pureParseArgs _ = MkIdentity [Parsed] $ PFailure "bad arg"

pureRunCmd :: String -> Identity RunResultStr
pureRunCmd "good cmd" = MkIdentity [Ran] $ RSuccess "ran"
pureRunCmd _ = MkIdentity [Ran] $ RFailure "cmd died"

purePutStrLn :: String -> Identity ()
purePutStrLn "ran" = MkIdentity [Put Success] ()
purePutStrLn "Error: cmd died" = MkIdentity [Put CmdFail] ()
purePutStrLn "bad arg" = MkIdentity [Put ParseFail] ()
purePutStrLn x = error $ "unexpected string to purePutStrLn: " <> x
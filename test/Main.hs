{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}

module Main where

import Prelude hiding (ioError)

import qualified Control.Carrier.State.IORef  as IOState
import qualified Control.Carrier.State.Strict as State
import           Control.Carrier.Lift (runM)
import           Control.Effect.Exception
import           Control.Effect.State
import qualified Test.Tasty                   as Tasty
import qualified Test.Tasty.HUnit             as HUnit

problematic :: (Has (Lift IO) sig m, Has (State Char) sig m) => m ()
problematic =
  let throws = modify @Char succ *> throwIO (userError "should explode") `finally` put @Char 'x'
  in throws `catch` (\(_ :: IOException) -> pure ())

testStateDropsWrites :: Tasty.TestTree
testStateDropsWrites = HUnit.testCase "State.Strict drops writes" $ do
  result <- State.execState 'a' problematic
  result HUnit.@?= 'a' -- writes are lost

testIOStatePreservesWrites :: Tasty.TestTree
testIOStatePreservesWrites = HUnit.testCase "State.IORef preserves writes" $ do
  result <- IOState.execState 'a' problematic
  result HUnit.@?= 'x'

tests :: Tasty.TestTree
tests = Tasty.testGroup "Control.Carrier.Exception"
  [ Tasty.testGroup "finally"
    [ testStateDropsWrites
    , testIOStatePreservesWrites
    ]
  ]

main :: IO ()
main = Tasty.defaultMain tests


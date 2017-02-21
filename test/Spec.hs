{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Text (Text)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [xmlTests]

xmlTests :: TestTree
xmlTests = testGroup "xml" [foo]

foo :: TestTree
foo = testGroup "foo" [bar]

bar :: TestTree
bar = testCase "works correctly" $ 1 @?= 1

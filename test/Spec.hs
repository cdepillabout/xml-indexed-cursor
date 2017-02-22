{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Default (def)
import Data.Monoid ((<>))
import Data.Text (Text)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.XML (parseLBS_, Element(..), Name(..), Node(..))
import Text.XML.Cursor.Generic (($.//), child, descendant, node)

import Text.XML.Cursor.Indexed
       (IndexedCursor, IndexedNode(..), check, fromDocument)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [xmlCursorTests]

xmlCursorTests :: TestTree
xmlCursorTests =
  testGroup
    "XML Cursors"
    [ cursorCheck
    , cursorPredicate
    , cursorCheckNode
    , cursorCheckElement
    , cursorCheckName
    , cursorAnyElement
    , cursorElement
    , cursorLaxElement
    , cursorContent
    , cursorAttribute
    , cursorLaxAttribute
    , cursorDeep
    , cursorForce
    , cursorForceM
    , cursorHasAttribute
    , cursorAttributeIs
    ]

cursorCheck :: TestTree
cursorCheck =
  testCase "has correct check" $ null (cursor $.// check (const False)) @?= True

cursorPredicate :: TestTree
cursorPredicate =
  testCase "has correct check with lists" $
  (name $ cursor $.// check descendant) @?= ["foo", "bar2", "baz3", "bar3"]

cursorCheckNode :: TestTree
cursorCheckNode = testCase "has correct checkNode" $ undefined

cursorCheckElement :: TestTree
cursorCheckElement = testCase "has correct checkElement" $ undefined

cursorCheckName :: TestTree
cursorCheckName = testCase "has correct checkName" $ undefined

cursorAnyElement :: TestTree
cursorAnyElement = testCase "has correct anyElement" $ undefined

cursorElement :: TestTree
cursorElement = testCase "has correct element" $ undefined

cursorLaxElement :: TestTree
cursorLaxElement = testCase "has correct laxElement" $ undefined

cursorContent :: TestTree
cursorContent = testCase "has correct content" $ undefined

cursorAttribute :: TestTree
cursorAttribute = testCase "has correct attribute" $ undefined

cursorLaxAttribute :: TestTree
cursorLaxAttribute = testCase "has correct laxAttribute" $ undefined

cursorDeep :: TestTree
cursorDeep = testCase "has correct &* and $* operators" $ undefined

cursorForce :: TestTree
cursorForce = testCase "has correct force" $ undefined

cursorForceM :: TestTree
cursorForceM = testCase "has correct forceM" $ undefined

cursorHasAttribute :: TestTree
cursorHasAttribute = testCase "has correct hasAttribute" $ undefined

cursorAttributeIs :: TestTree
cursorAttributeIs = testCase "has correct attributeIs" $ undefined

name :: [IndexedCursor] -> [Text]
name [] = []
name (c:cs) =
  ($ name cs) $
  case indexedNodeNode $ node c of
    NodeElement e -> ((nameLocalName $ elementName e) :)
    _ -> id

cursor :: IndexedCursor
cursor =
    fromDocument $ parseLBS_ def input
  where
    input :: ByteString
    input =
        "<foo attr=\"x\">" <>
           "<bar1/>" <>
           "<bar2>" <>
              "<baz1/>" <>
              "<baz2 attr=\"y\"/>" <>
              "<baz3>a</baz3>" <>
           "</bar2>" <>
           "<bar3>" <>
              "<bin1/>" <>
              "b" <>
              "<bin2/>" <>
              "<bin3/>" <>
           "</bar3>" <>
           "<Bar1 xmlns=\"http://example.com\" Attr=\"q\"/>" <>
        "</foo>"

bar2, baz2, bar3, bin2 :: IndexedCursor
bar2 = child cursor !! 1
baz2 = child bar2 !! 1

bar3 = child cursor !! 2
bin2 = child bar3 !! 1

-- cursorPredicate = (name $ cursor $.// Cu.check Cu.descendant) @?= T.words "foo bar2 baz3 bar3"
-- cursorCheckNode = (name $ cursor $// Cu.checkNode f) @?= T.words "bar1 bar2 bar3"
--     where f (Res.NodeElement e) = "bar" `T.isPrefixOf` Res.nameLocalName (Res.elementName e)
--           f _               = False
-- cursorCheckElement = (name $ cursor $// Cu.checkElement f) @?= T.words "bar1 bar2 bar3"
--     where f e = "bar" `T.isPrefixOf` Res.nameLocalName (Res.elementName e)
-- cursorCheckName = (name $ cursor $// Cu.checkName f) @?= T.words "bar1 bar2 bar3"
--     where f n = "bar" `T.isPrefixOf` nameLocalName n
-- cursorAnyElement = (name $ cursor $// Cu.anyElement) @?= T.words "bar1 bar2 baz1 baz2 baz3 bar3 bin1 bin2 bin3 Bar1"
-- cursorElement = (name $ cursor $// Cu.element "bar1") @?= ["bar1"]
-- cursorLaxElement = (name $ cursor $// Cu.laxElement "bar1") @?= ["bar1", "Bar1"]
-- cursorContent = do
--   Cu.content cursor @?= []
--   (cursor $.// Cu.content) @?= ["a", "b"]
-- cursorAttribute = Cu.attribute "attr" cursor @?= ["x"]
-- cursorLaxAttribute = (cursor $.// Cu.laxAttribute "Attr") @?= ["x", "y", "q"]

-- cursorHasAttribute = (length $ cursor $.// Cu.hasAttribute "attr") @?= 2
-- cursorAttributeIs = (length $ cursor $.// Cu.attributeIs "attr" "y") @?= 1

-- cursorDeep = do
--   (Cu.element "foo" &/ Cu.element "bar2" &// Cu.attribute "attr") cursor @?= ["y"]
--   (return &.// Cu.attribute "attr") cursor @?= ["x", "y"]
--   (cursor $.// Cu.attribute "attr") @?= ["x", "y"]
--   (cursor $/ Cu.element "bar2" &// Cu.attribute "attr") @?= ["y"]
--   (cursor $/ Cu.element "bar2" &/ Cu.element "baz2" >=> Cu.attribute "attr") @?= ["y"]
--   null (cursor $| Cu.element "foo") @?= False
-- cursorForce = do
--   Cu.force DummyEx [] @?= (Nothing :: Maybe Integer)
--   Cu.force DummyEx [1] @?= Just (1 :: Int)
--   Cu.force DummyEx [1,2] @?= Just (1 :: Int)
-- cursorForceM = do
--   Cu.forceM DummyEx [] @?= (Nothing :: Maybe Integer)
--   Cu.forceM DummyEx [Just 1, Nothing] @?= Just (1 :: Int)
--   Cu.forceM DummyEx [Nothing, Just (1 :: Int)] @?= Nothing


{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

{- |
Module      :  Text.XML.Cursor.Indexed

Copyright   :  Dennis Gosnell 2017
License     :  BSD3

Maintainer  :  Dennis Gosnell (cdep.illabout@gmail.com)
Stability   :  experimental
Portability :  unknown

This module provides indexed 'Cursor's.  It has a very similar API to
"Text.XML.Cursor".

The big difference is in the 'Cursor' type.  'Text.XML.Cursor.Cursor' wraps
around a 'Node', while this module's 'Cursor' type wraps around an
'IndexedNode'.

An 'IndexedNode' is a data type that contains both a 'Node' and a 'NodeIndex'.
The 'NodeIndex' gives a way to figure out how two 'IndexedNode's compare to
each other in the 'Document'.  It gives the ability to figure out which
'IndexedNode' comes earlier in the 'Document'.  This gives the ability to sort
lists of 'IndexedNode's, based on their location in the 'Document'.  See
'NodeIndex' for more information.
-}

module Text.XML.Cursor.Indexed
  ( -- * Cursor
    IndexedCursor
  , IndexedAxis
    -- * 'NodeIndex' and 'IndexedNode'
  , NodeIndex(..)
  , HasNodeIndex(..)
  , rootIndex
  , IndexedNode(..)
  , nodeToRootIndexedNode
  , toChildIndex
  , nodeToIndexedNode
  , childNodeToIndexedNode
  , childNodesToIndexedNodes
    -- * Converting
  , fromDocument
  , fromNode
    -- * \"check\" functions
  , check
  , checkIndexedNode
  , checkElement
  , checkName
    -- * XPath-style functions
  , element
  , content
  , attribute
  , attributeMay
  , laxAttribute
  , hasAttribute
  , attributeIs
  , descendantElementsNamed
  , ancestorElementsNamed
  , descendantElementsNamedWithAttr
  , descendantContent
  , attrValForElemCursor
    -- * Parse directly into 'IndexedCursor'
  , indexedCursorFromByteString_
  , indexedCursorFromByteString
  , indexedCursorFromText_
  , indexedCursorFromText
  , indexedCursorFromByteStringWithOpts_
  , indexedCursorFromByteStringWithOpts
  , indexedCursorFromTextWithOpts_
  , indexedCursorFromTextWithOpts
    -- * Patterns
  , pattern IndexedNodeContent
  , pattern IndexedNodeElement
  ) where

import Control.Exception (SomeException)
import Control.Monad ((>=>), guard)
import Data.ByteString.Lazy (ByteString)
import Data.Data (Data)
import Data.Default (def)
import Data.Function (on)
import Data.Map (toList)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Sequence (Seq, (|>), fromList)
import Data.Text (Text, toCaseFold)
import qualified Data.Text.Lazy as LText
import Data.Typeable (Typeable)
import Text.XML
       (Document, Element(Element), Name, Node(NodeContent, NodeElement),
        ParseSettings, documentRoot, elementAttributes, elementName,
        nameLocalName, parseLBS, parseLBS_, parseText, parseText_)
import Text.XML.Cursor (Boolean(bool))
import Text.XML.Cursor.Generic
       (Axis, Cursor, ancestor, descendant, node, toCursor)

-- | Index for a 'Node' in a 'Document'.
--
-- The root element has a value of '[]'.  Every child element is given an
-- 'Int' index as the first element of the list, and the grandchild elements
-- are given an 'Int' index as the second element of the list, and so on.  If
-- there are multiple root elements, then '[]' acts as a \"virtual\" root
-- element that contains all actual root elements.
--
-- The index of the first child of the root be @[0]@
--
-- The index of the second child of the root would be @[1]@.
--
-- The index of the third child of the root would be @[2]@.
--
-- The index of the first child of the first child of the root would be
-- @[0, 0]@.
--
-- The index of the second child of the first child of the root would be
-- @[0, 1]@ (since the @[Int]@ is stored reversed).
--
-- The index of the third child of the fifth child of the root would be
-- @[4, 2]@.
newtype NodeIndex = NodeIndex
  { unNodeIndex :: Seq Int
  } deriving (Data, Eq, Ord, Read, Show, Typeable)

class HasNodeIndex a where
  -- | This is basically @'Control.Lens.Lens'' a 'NodeIndex'@.
  nodeIndexLens :: Functor f => (NodeIndex -> f NodeIndex) -> a -> f a

instance HasNodeIndex NodeIndex where
  nodeIndexLens = id
  {-# INLINE nodeIndexLens #-}

-- | Index to use for the root 'NodeIndex'.  Should be '[]'.
rootIndex :: NodeIndex
rootIndex = NodeIndex $ fromList []

-- | 'IndexedNode' just wraps together a 'Node' and a 'NodeIndex' for that
-- 'Node'.
data IndexedNode = IndexedNode
  { indexedNodeIndex :: NodeIndex
  , indexedNodeNode :: Node
  } deriving (Data, Eq, Show, Typeable)

instance HasNodeIndex IndexedNode where
  nodeIndexLens
    :: Functor f
    => (NodeIndex -> f NodeIndex) -> IndexedNode -> f IndexedNode
  nodeIndexLens =
    lens indexedNodeIndex (\indexedNode x -> indexedNode {indexedNodeIndex = x})
    where
      lens
        :: forall f s a b t.
           Functor f
        => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
      lens sa sbt afb s = sbt s <$> afb (sa s)

-- | This is similar to 'Text.XML.Cursor.Cursor' except for 'IndexedNode'.
type IndexedCursor = Cursor IndexedNode

-- | This is similar to 'Text.XML.Cursor.Axis except for 'IndexedNode'.
type IndexedAxis = Axis IndexedNode

-- | Convert a 'Node' to a root 'IndexedNode'.
nodeToRootIndexedNode :: Node -> IndexedNode
nodeToRootIndexedNode = IndexedNode rootIndex
{-# INLINE nodeToRootIndexedNode #-}

-- | Create a 'NodeIndex' for the 'Int' child below the input parent
-- 'NodeIndex'.
toChildIndex
  :: NodeIndex -- ^ Parent 'NodeIndex'.
  -> Int -- ^ Child index.
  -> NodeIndex
toChildIndex (NodeIndex seq') = NodeIndex . (seq' |>)
{-# INLINE toChildIndex #-}

-- | Given a 'NodeIndex', create an 'IndexedNode' for a 'Node'.
nodeToIndexedNode :: NodeIndex -> Node -> IndexedNode
nodeToIndexedNode = IndexedNode
{-# INLINE nodeToIndexedNode #-}

-- | In @'childNodeToIndexedNode' parentIndex childIndexInt childNode@, create
-- an 'IndexedNode' out of @childNode@, creating its 'NodeIndex' using
-- 'toChildIndex'.
childNodeToIndexedNode :: NodeIndex -> Int -> Node -> IndexedNode
childNodeToIndexedNode parentIndex childIndexInt =
  nodeToIndexedNode (toChildIndex parentIndex childIndexInt)
{-# INLINE childNodeToIndexedNode #-}

-- | In @'childNodesToIndexedNodes@ parentIndex childNodes@ convert a list of
-- 'Node' @childNodes@ to a list of 'IndexNode's using the 'NodeIndex'
-- @parentIndex@.
childNodesToIndexedNodes :: NodeIndex -> [Node] -> [IndexedNode]
childNodesToIndexedNodes parentIndex childNodes = go <$> zip [0 ..] childNodes
  where
    go :: (Int, Node) -> IndexedNode
    go (childIndexInt, childNode) =
      childNodeToIndexedNode parentIndex childIndexInt childNode

-- | Convert a 'Document' to a 'Cursor'. It will point to the document root.
fromDocument :: Document -> IndexedCursor
fromDocument = fromNode . NodeElement . documentRoot
{-# INLINE fromDocument #-}

-- | Convert a 'Node' to a root 'IndexedCursor'.
fromNode :: Node -> IndexedCursor
fromNode = toCursor cs . nodeToRootIndexedNode
  where
    cs :: IndexedNode -> [IndexedNode]
    cs (IndexedNode curIndex (NodeElement (Element _ _ childNodes))) =
      childNodesToIndexedNodes curIndex childNodes
    cs _ = []

-- | Filter cursors that don't pass a check.
check
  :: Boolean b
  => (Cursor a -> b) -> Axis a
check f c = [c | bool $ f c]

-- | Filter nodes that don't pass a check.
checkIndexedNode
  :: Boolean b
  => (IndexedNode -> b) -> IndexedAxis
checkIndexedNode f = check (f . node)
{-# INLINE checkIndexedNode #-}

-- | Filter elements that don't pass a check, and remove all non-elements.
checkElement
  :: Boolean b
  => (Element -> b) -> IndexedAxis
checkElement f c =
  case node c of
    IndexedNodeElement e -> [c | bool $ f e]
    _ -> []

-- | Filter elements that don't pass a name check, and remove all non-elements.
checkName :: Boolean b => (Name -> b) -> IndexedAxis
checkName f = checkElement (f . elementName)
{-# INLINE checkName #-}

-- | Select only those elements with a matching tag name.
--
-- XPath: /A node test that is a QName is true if and only if the type of the
-- node (see [5 Data Model]) is the principal node type and has an
-- expanded-name equal to the expanded-name specified by the QName./
element :: Name -> IndexedAxis
element n = checkName (== n)
{-# INLINE element #-}

-- | Select only text nodes, and directly give the 'Content' values.
--
-- XPath: /The node test text() is true for any text node./
--
-- Note that this is not strictly an 'Axis', but will work with most combinators.
content :: IndexedCursor -> [Text]
content (node -> IndexedNodeContent v) = [v]
content _ = []
{-# INLINE content #-}

-- | Select attributes on the current element (or nothing if it is not an element). XPath:
-- /the attribute axis contains the attributes of the context node; the axis will be empty unless the context node is an element/
--
-- Note that this is not strictly an 'Axis', but will work with most combinators.
--
-- The return list of the generalised axis contains as elements lists of 'Content'
-- elements, each full list representing an attribute value.
attribute :: Name -> IndexedCursor -> [Text]
attribute name = maybeToList . attributeMay name
{-# INLINE attribute #-}

-- | Similar to 'attribute' but return a 'Maybe' instead of a list.
attributeMay :: Name -> IndexedCursor -> Maybe Text
attributeMay n (node -> IndexedNodeElement (Element _ as _)) = Map.lookup n as
attributeMay _ _ = Nothing
{-# INLINE attributeMay #-}

-- | Select attributes on the current element (or nothing if it is not an
-- element).  Namespace and case are ignored.
--
-- XPath: /the attribute axis contains the attributes of the context node;
-- the axis will be empty unless the context node is an element/
--
-- Note that this is not strictly an 'Axis', but will work with most combinators.
--
-- The return list of the generalised axis contains as elements lists of 'Content'
-- elements, each full list representing an attribute value.
laxAttribute :: Text -> IndexedCursor -> [Text]
laxAttribute n (node -> IndexedNodeElement e) = do
  (n', v) <- toList $ elementAttributes e
  guard $ (on (==) toCaseFold) n (nameLocalName n')
  pure v
laxAttribute _ _ = []

-- | Select only those element nodes with the given attribute.
hasAttribute :: Name -> IndexedAxis
hasAttribute n c =
  case node c of
    IndexedNodeElement (Element _ as _) -> maybeToList $ c <$ Map.lookup n as
    _ -> []

-- | Select only those element nodes containing the given attribute key/value
-- pair.
attributeIs :: Name -> Text -> IndexedAxis
attributeIs name v c =
  case node c of
    IndexedNodeElement (Element _ as _) -> [c | Just v == Map.lookup name as]
    _ -> []

-- | For a given 'Name', find all 'descendant' 'Element's with that 'Name'.
descendantElementsNamed :: Name -> IndexedAxis
descendantElementsNamed elemName = descendant >=> element elemName

-- | For a given 'Name', find all 'ancestor' 'Element's. with that 'Name'.
ancestorElementsNamed :: Name -> IndexedAxis
ancestorElementsNamed elemName = ancestor >=> element elemName

-- | In @'descendantElementsNamedWithAttr' elemName attrKey attrVal@, find all
-- 'descendant' 'Element's with @elemName@ that have an attribute called
-- 'attrKey' with a value of 'attrVal'.
descendantElementsNamedWithAttr :: Name -> Name -> Text -> IndexedAxis
descendantElementsNamedWithAttr elemName attrKey attrVal =
  descendantElementsNamed elemName >=> attributeIs attrKey attrVal

-- | Find all 'content' in all 'descendant's.
descendantContent :: IndexedCursor -> [Text]
descendantContent = descendant >=> content

-- | Find 'attribute' with 'Name' on the element 'IndexedCursor' is pointing to.
attrValForElemCursor :: Name -> IndexedCursor -> Maybe Text
attrValForElemCursor attrName = listToMaybe . attribute attrName

-- | This reads a 'Document' from a 'ByteString' with 'parseLBS_', and then
-- converts that 'Document' to an 'IndexedCursor'.
indexedCursorFromByteString_ :: ByteString -> IndexedCursor
indexedCursorFromByteString_ = fromDocument . parseLBS_ def

-- | Similar to 'indexedCursorFromByteString_' but uses 'parseLBS' instead of
-- 'parseLBS_'.
indexedCursorFromByteString :: ByteString -> Either SomeException IndexedCursor
indexedCursorFromByteString = fmap fromDocument . parseLBS def

-- | Similar to 'indexedCursorFromByteString_' but uses 'parseText_' instead of
-- 'parseLBS_'.
indexedCursorFromText_ :: LText.Text -> IndexedCursor
indexedCursorFromText_ = fromDocument . parseText_ def

-- | Similar to 'indexedCursorFromByteString_' but uses 'parseText' instead of
-- 'parseLBS_'.
indexedCursorFromText :: LText.Text -> Either SomeException IndexedCursor
indexedCursorFromText = fmap fromDocument . parseText def

-- | Similar to 'indexedCursorFromByteString_' but also takes 'ParseSettings'.
indexedCursorFromByteStringWithOpts_ :: ParseSettings
                                     -> ByteString
                                     -> IndexedCursor
indexedCursorFromByteStringWithOpts_ parseSettings =
  fromDocument . parseLBS_ parseSettings

-- | Similar to 'indexedCursorFromByteString' but also takes 'ParseSettings'.
indexedCursorFromByteStringWithOpts :: ParseSettings
                                    -> ByteString
                                    -> Either SomeException IndexedCursor
indexedCursorFromByteStringWithOpts parseSettings =
  fmap fromDocument . parseLBS parseSettings

-- | Similar to 'indexedCursorFromText_' but also takes 'ParseSettings'.
indexedCursorFromTextWithOpts_ :: ParseSettings -> LText.Text -> IndexedCursor
indexedCursorFromTextWithOpts_ parseSettings =
  fromDocument . parseText_ parseSettings

-- | Similar to 'indexedCursorFromText' but also takes 'ParseSettings'.
indexedCursorFromTextWithOpts :: ParseSettings
                              -> LText.Text
                              -> Either SomeException IndexedCursor
indexedCursorFromTextWithOpts parseSettings =
  fmap fromDocument . parseText parseSettings

-------------
-- Helpers --
-------------

pattern IndexedNodeContent :: Text -> IndexedNode
pattern IndexedNodeContent c <- IndexedNode _ (NodeContent c)

pattern IndexedNodeElement :: Element -> IndexedNode
pattern IndexedNodeElement e <- IndexedNode _ (NodeElement e)


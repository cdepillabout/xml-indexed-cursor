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

-}

module Text.XML.Cursor.Indexed
  where

import Control.Monad ((>=>), guard)
import Data.Data (Data)
import Data.Function (on)
import Data.Map (toList)
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, maybeToList)
import Data.Sequence (Seq, (|>), fromList)
import Data.Text (Text, toCaseFold)
import Data.Typeable (Typeable)
import Text.XML
       (Document, Element(Element), Name, Node(NodeContent, NodeElement),
        documentRoot, elementAttributes, elementName, nameLocalName)
import Text.XML.Cursor (Boolean(bool))
import Text.XML.Cursor.Generic
       (Axis, Cursor, ancestor, descendant, node, toCursor)

-- $setup
-- >>>


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
  nodeIndexLens :: Functor f => (NodeIndex -> f NodeIndex) -> a -> f a

instance HasNodeIndex NodeIndex where
  nodeIndexLens = id
  {-# INLINE nodeIndexLens #-}

-- | Index to use for the root 'NodeIndex'.  Should be '[]'.
rootIndex :: NodeIndex
rootIndex = NodeIndex $ fromList []

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

type IndexedCursor = Cursor IndexedNode
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

-- -- | Filter elements that don't pass a name check, and remove all non-elements.
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

descendantElementsNamed :: Name -> IndexedAxis
descendantElementsNamed elemName = descendant >=> element elemName

ancestorElementsNamed :: Name -> IndexedAxis
ancestorElementsNamed elemName = ancestor >=> element elemName

descendantElementsNamedWithAttr :: Name -> Name -> Text -> IndexedAxis
descendantElementsNamedWithAttr elemName attrKey attrVal =
  descendantElementsNamed elemName >=> attributeIs attrKey attrVal

descendantContent :: IndexedCursor -> [Text]
descendantContent = descendant >=> content

-- | Find 'attribute' with 'Name' on the element 'IndexedCursor' is pointing to.
attrValForElemCursor :: Name -> IndexedCursor -> Maybe Text
attrValForElemCursor attrName = listToMaybe . attribute attrName

-------------
-- Helpers --
-------------

pattern IndexedNodeContent :: Text -> IndexedNode
pattern IndexedNodeContent c <- IndexedNode _ (NodeContent c)

pattern IndexedNodeElement :: Element -> IndexedNode
pattern IndexedNodeElement e <- IndexedNode _ (NodeElement e)

lens
  :: forall f s a b t.
     Functor f
  => (s -> a) -> (s -> b -> t) -> (a -> f b) -> s -> f t
lens sa sbt afb s = sbt s <$> afb (sa s)

{-# LANGUAGE ImportQualifiedPost #-}

{-
ToDo:
    Need to verify FEN output and input with Dendroscope (errors/non-parse reported)
-}

{- |
Description :  module witb interconversion functions for commonly used
phylogentic graph formats (newick. dot, fgl) graphs parsed to fgl types.


Forest Extended Newick defined here as a series of ENewick representations
within '<' ans '>'. Nodes can be shared among consituent ENewick representations
(';' from enewick itself, just for illustration, not doubled)

<EN1;EN2>

ExtendedNewick from Cardona et al. 2008.  BMC Bioinformatics 2008:9:532

    The labels and comments are as in Olsen Newick formalization below, except
    that underscores in unquoted label are NOT converted to spaces and quoted labels
    ar left as is with quotes and all.
    Other elements as in Cardona et all ENewick.



Gary Olsen's Interpretation of the "Newick's 8:45" Tree Format Standard
https://evolution.genetics.washington.edu/phylip/newick_doc.html

Conventions:
   Items in { } may appear zero or more times.
   Items in [ ] are optional, they may appear once or not at all.
   All other punctuation marks (colon, semicolon, parentheses, comma and
         single quote) are required parts of the format.

              tree ==> descendant_list [ root_label ] [ : branch_length ] ;

   descendant_list ==> ( subtree { , subtree } )

           subtree ==> descendant_list [internal_node_label] [: branch_length]
                   ==> leaf_label [: branch_length]

            root_label ==> label
   internal_node_label ==> label
            leaf_label ==> label

                 label ==> unquoted_label
                       ==> quoted_label

        unquoted_label ==> string_of_printing_characters
          quoted_label ==> ' string_of_printing_characters '

         branch_length ==> signed_number
                       ==> unsigned_number

Notes:
   Unquoted labels may not contain blanks, parentheses, square brackets,
        single_quotes, colons, semicolons, or commas.
   Underscore characters in unquoted labels are converted to blanks.
   Single quote characters in a quoted label are represented by two single
        quotes.
   Blanks or tabs may appear anywhere except within unquoted labels or
        branch_lengths.
   Newlines may appear anywhere except within labels or branch_lengths.
   Comments are enclosed in square brackets and may appear anywhere
        newlines are permitted.

Other notes:
   PAUP (David Swofford) allows nesting of comments.
   TreeAlign (Jotun Hein) writes a root node branch length (with a value of
        0.0).
   PHYLIP (Joseph Felsenstein) requires that an unrooted tree begin with a
        trifurcation; it will not "uproot" a rooted tree.

Example:
   (((One:0.2,Two:0.3):0.3,(Three:0.5,Four:0.3):0.2):0.3,Five:0.7):0.0;

           +-+ One
        +--+
        |  +--+ Two
     +--+
     |  | +----+ Three
     |  +-+
     |    +--+ Four
     +
     +------+ Five
-}
module GraphFormatUtilities (
    forestEnhancedNewickStringList2FGLList,
    fgl2FEN,
    fglList2ForestEnhancedNewickString,
    component2Newick,
    checkIfLeaf,
    stringGraph2TextGraph,
    textGraph2StringGraph,
    stringGraph2TextGraphDouble,
    showGraph,
    relabelFGL,
    convertGraphToStrictText,
    splitVertexList,
    relabelFGLEdgesDouble,
    getDistToRoot,
    fgl2DotString,
    modifyVertexEdgeLabels,
    relabelGraphLeaves,
    checkGraphsAndData,
    cyclic,
    reIndexLeavesEdges,
) where

import Control.Parallel.Strategies
import Cyclic qualified as C
import Data.Char (isSpace)
import Data.Foldable (fold, toList)
import Data.Graph.Inductive.Graph qualified as G
import Data.Graph.Inductive.PatriciaTree qualified as P
import Data.GraphViz qualified as GV
import Data.GraphViz.Attributes.Complete (Attribute (Label), Attributes, Label (..))
import Data.GraphViz.Printing qualified as GVP
import Data.List qualified as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Set qualified as Set
import Data.Text qualified as Strict
import Data.Text.Lazy (Text)
import Data.Text.Lazy qualified as T
import GeneralUtilities
import ParallelUtilities
import Text.Read


-- | showGraph a semi-formatted show for Graphs
showGraph ∷ (Show a, Show b) ⇒ P.Gr a b → String -- BV.BV (BV.BV, BV.BV) -> String
showGraph inGraph =
    if G.isEmpty inGraph
        then "Empty Graph"
        else
            let nodeString = show $ G.labNodes inGraph
                edgeString = show $ G.labEdges inGraph
            in  ("Nodes:" <> nodeString <> "\n" <> "Edges: " <> edgeString)


{- | getForestEnhancedNewickList takes String file contents and returns a list
of fgl graphs with Text labels for nodes and edges or error if not ForestEnhancedNewick or Newick formats.
-}
forestEnhancedNewickStringList2FGLList ∷ Text → [P.Gr Text Double]
forestEnhancedNewickStringList2FGLList fileText =
    if T.null fileText
        then []
        else
            let feNewickList = fmap (removeNewickSpaces . removeNewickComments) (divideGraphText fileText)
            in  -- trace ("There are " <> (show $ length feNewickList) <> " graphs to convert: " <> (show feNewickList))
                parmap rdeepseq text2FGLGraph feNewickList


{- | divideGraphText splits multiple Text representations of graphs (Newick styles)
and returns a list of Text graph descriptions
-}
divideGraphText ∷ Text → [Text]
divideGraphText inText = case fst <$> T.uncons inText of
    Nothing → []
    Just '<' →
        let (firstPart, restPart) = T.span (/= '>') inText
        in  case T.uncons restPart of
                Nothing → [firstPart]
                Just (c, rest) → T.snoc firstPart c : divideGraphText rest
    Just '(' →
        let (firstPart, restPart) = T.span (/= ';') inText
        in  case T.uncons restPart of
                Nothing → [firstPart]
                Just (c, rest) → T.snoc firstPart c : divideGraphText rest
    Just firstChar →
        error $
            unwords
                [ "First character in graph representation"
                , T.unpack inText
                , ":"
                , show firstChar
                , "is not either '<' or '('"
                ]


-- | removeNewickComments take text and removes all "[...]"
removeNewickComments ∷ Text → Text
removeNewickComments inString
    | T.null inString = T.empty
    | not (T.any (== ']') inString) = inString
    | otherwise =
        let firstPart = T.takeWhile (/= '[') inString
            secondPart = T.tail $ T.dropWhile (/= ']') inString
        in  T.append firstPart (removeNewickComments secondPart)


{- | convertQuotedText takes single quoted Text and removes single quotes and converts spaces
to underscores
-}
convertQuotedText ∷ Text → (Text, Text)
convertQuotedText inText =
    if T.null inText
        then error "Emmpty Text in convertQuotedText"
        else
            let firstPart = T.replace (T.pack " ") (T.pack "_") $ T.takeWhile (/= '\'') (T.tail inText)
                restPart = T.tail $ T.dropWhile (/= '\'') (T.tail inText)
            in  (firstPart, restPart)


{- | removeNewickSpaces removes spaces and converts single quoted strings
with spaces to unquoted strings with underscores replacing spaces: ' blah bleh ' => blah_bleh
-}
removeNewickSpaces ∷ Text → Text
removeNewickSpaces inText = case T.uncons inText of
    Nothing → mempty
    Just ('\'', _) →
        let (newText, restText) = convertQuotedText inText
        in  fold [newText, removeNewickSpaces restText]
    Just (firstChar, otherText) | isSpace firstChar → removeNewickSpaces otherText
    Just (firstChar, otherText) → T.cons firstChar $ removeNewickSpaces otherText


{- | text2FGLGraph takes Text of newick (forest or enhanced or OG) and
retns fgl graph representation
-}
text2FGLGraph ∷ Text → P.Gr Text Double
text2FGLGraph inGraphText = case T.uncons inGraphText >>= traverse T.unsnoc of
    Nothing → error "Empty graph text in text2FGLGraph"
    Just ('<', (_, '>')) → fENewick2FGL inGraphText
    Just ('(', (_, ';')) → mergeNetNodesAndEdges . makeGraphFromPairList $ eNewick2FGL [] [] [(inGraphText, (-1, mempty))]
    _ → error "Graph text not in ForestEnhancedNewick or (Enhanced)Newick format"


{- | fENewick2FGL takes a Forest Extended Newick (Text) string and returns FGL graph
breaks up forest and parses seprate eNewicks then modifes for any
common network nodes in the sub-graphs
-}
fENewick2FGL ∷ Text → P.Gr Text Double
fENewick2FGL inText
    | T.null inText = error "Empty graph text in fENewick2FGL"
    | otherwise -- split eNewicks
        =
        let eNewickTextList = splitForest inText
            startNodeList = replicate (length eNewickTextList) (-1, T.empty)
            textNodeList = zip eNewickTextList startNodeList
            -- init to remove trailing ';' from eNewick
            eNewickGraphList = fmap ((mergeNetNodesAndEdges . makeGraphFromPairList) . (eNewick2FGL [] [] . (: []))) textNodeList
        in  case eNewickGraphList of
                [x] → x
                _ → mergeNetNodesAndEdges $ mergeFGLGraphs G.empty eNewickGraphList


{- | splitForest takes a Text (string) Forest Enhanced Newick representation and splits into
its consituent Extended Newick representations
-}
splitForest ∷ Text → [Text]
splitForest inText
    | T.null inText = []
    | (T.head inText /= '<') || (T.last inText /= '>') =
        error
            ( "Invalid Forest Extended Newick representation,"
                <> " must begin with \'<\'' and end with \'>\' : "
                <> T.unpack inText
            )
    | otherwise =
        let partsList = filter (not . T.null) $ T.splitOn (T.singleton ';') (T.init $ T.tail inText)
            eNewickList = fmap (`T.append` T.singleton ';') partsList
        in  eNewickList


{- | makeGraphFromPairList takes pair of node list and edge list and returns Graph
| filters to remove place holder node and edges creted during eNewick pass
-}
makeGraphFromPairList ∷ [(G.LNode Text, G.LEdge Double)] → P.Gr Text Double
makeGraphFromPairList pairList =
    if null pairList
        then G.empty
        else
            let (nodeList, edgeList) = unzip pairList
            in  G.mkGraph (filter ((> (-1)) . fst) nodeList) (filter ((> (-1)) . fst3) edgeList)


{- | getBranchLength extracts branch length from Text label and puts in '1' if there is no
branch length--makes sure after last ')'
-}
getBranchLength ∷ Text → Double
getBranchLength inText =
    -- trace ("Getting branch length of " <> show inText) (
    if T.null inText
        then error "Null text in getBranchLength"
        else
            let a = ((T.dropWhile (/= ':') . T.reverse) . T.takeWhile (/= ')') $ T.reverse inText)
            in  if T.null a
                    then 1
                    else
                        if T.length a == 1
                            then error "Need branch length after \':\')"
                            else (read (T.unpack $ T.tail a) ∷ Double)


-- )

{- | getNodeLabel get--or makes--a label for a node
after last ')' before any ':', without ',' after last ')'
-}
getNodeLabel ∷ Int → Text → Text
getNodeLabel nodeNumber inText =
    -- trace ("Getting node label of " <> show inText) (
    if T.null inText
        then error "Null text in getNodeLabel"
        else
            let a = ((T.takeWhile (/= ':') . T.reverse) . T.takeWhile (/= ')') $ T.reverse inText)
            in  if T.any (== ',') a || T.null a then T.append (T.pack "HTU") (T.pack $ show nodeNumber) else a


-- )

{- | getLeafInfo takes Text of teminal (no ',') and parses to yeild
either a single leaf label, edge, and edge weight, or two
leaves with labels and costs if there is a network node as parent
need to merge network nodes later
-}
getLeafInfo ∷ Text → G.LNode Text → [G.LNode Text] → [(G.LNode Text, G.LEdge Double)]
getLeafInfo leafText parentNode nodeList
    | T.null leafText = error "Empty leaf text in getLeafInfo"
    | not (T.any (== '(') leafText) =
        let leafLabel = T.takeWhile (/= ':') leafText
            edgeWeight = getBranchLength leafText
            -- CHECK FOR EXISTING
            thisNode = (length nodeList, leafLabel)
            thisEdge = (fst parentNode, length nodeList, edgeWeight)
        in  -- preexistingNode = checkForExistingNode leafLabel nodeList

            [(thisNode, thisEdge)]
    | otherwise =
        let -- leaf parent info
            -- (leafLabel)leafParentLabel:leafParentBranchLength
            leafParentEdgeWeight = getBranchLength leafText
            leafParentLabel = getNodeLabel (length nodeList) leafText
            leafParentNode = (length nodeList, leafParentLabel)
            leafParentEdge = (fst parentNode, fst leafParentNode, leafParentEdgeWeight)

            -- leaf info
            -- (leafLabel)X#H:000 => leafLabel
            leafLabelText = T.takeWhile (/= ')') $ T.tail leafText
            -- check for existing
            leafLabel = T.takeWhile (/= ':') leafLabelText
            leafEdgeWeight = getBranchLength leafLabelText
            leafNode = (1 + length nodeList, leafLabel)
            leafEdge = (fst leafParentNode, fst leafNode, leafEdgeWeight)
        in  [(leafNode, leafEdge), (leafParentNode, leafParentEdge)]


{- | getBodyParts takes a Text of a subTree and splits out the group description '(blah)', any node label
and any branch length
-}
getBodyParts ∷ Text → Int → (Text, Text, Double)
getBodyParts inRep nodeNumber =
    if T.null inRep
        then error "No group to parse in getBodyParts"
        else -- trace ("In body parts") (
            let subGraphPart = (T.reverse . T.dropWhile (/= ')') $ T.reverse inRep)
                branchLength = getBranchLength inRep
                subGraphLabel = getNodeLabel nodeNumber inRep
            in  -- trace (show (subGraphPart, subGraphLabel, branchLength))
                (subGraphPart, subGraphLabel, branchLength)


-- )

{- | getParenBoundedGraph tkaes a Text String and returns  the first graph component
with balanced parens and remainder of Text
-}
getParenBoundedGraph ∷ Int → Int → Text → Text → (Text, Text)
getParenBoundedGraph leftParenCounter rightParenCounter curText inText =
    -- trace ("GB " <> show curText <> " " <> show inText) (
    if T.null inText
        then (curText, inText)
        else
            let firstChar = T.head inText
            in  if firstChar == '('
                    then getParenBoundedGraph (leftParenCounter + 1) rightParenCounter (T.snoc curText firstChar) (T.tail inText)
                    else
                        if firstChar /= ')'
                            then getParenBoundedGraph leftParenCounter rightParenCounter (T.snoc curText firstChar) (T.tail inText)
                            else -- right paren
                                if rightParenCounter + 1 == leftParenCounter -- closing matrched paren
                                    then
                                        let restOfComponent = T.takeWhile (/= ',') inText
                                            remainderText = T.dropWhile (/= ',') inText
                                        in  (curText `T.append` restOfComponent, remainderText)
                                    else getParenBoundedGraph leftParenCounter (rightParenCounter + 1) (T.snoc curText firstChar) (T.tail inText)


-- )

{- | getSubComponents takes a Text String and reurns 1 or more subcomponents of graph
scenarios include leaf, leaf in parens, subgraph in parens
-}
getSubComponents ∷ Text → [Text]
getSubComponents inText
    | T.null inText = []
    | T.head inText == ',' = getSubComponents (T.tail inText)
    | T.head inText /= '(' -- simple leaf (no net node labels)
        =
        let subGraph = T.takeWhile (/= ',') inText
            restGraph = T.dropWhile (/= ',') inText
        in  subGraph : getSubComponents restGraph
    | otherwise -- "regular" paren defined element
        =
        let (subGraph, restGraph) = getParenBoundedGraph 0 0 T.empty inText
        in  subGraph : getSubComponents restGraph


-- | getChildren splits a subGraph Text '(blah, blah)' by commas, removing outer parens
getChildren ∷ Text → [Text]
getChildren inText
    | T.null inText = []
    | (T.head inText /= '(') || (T.last inText /= ')') =
        error
            ( "Invalid Extended Newick component,"
                <> " must begin with \'(\'' and end with \')\' : "
                <> T.unpack inText
                <> "\nPerhaps missing commas ',' in F/E/Newick format?"
            )
    | otherwise =
        -- modify for indegree 1 outdegree 1 and print warning.
        let guts = T.init $ T.tail inText -- removes leading and training parens
            subComponents = filter (not . T.null) $ getSubComponents guts
        in  subComponents


{- | checkForExistingNode takes a node label and checs the node list for the first
node with the same label and returns a Maybe node, else Nothing
-}
checkForExistingNode ∷ Text → [G.LNode Text] → Maybe (G.LNode Text)
checkForExistingNode nodeLabel nodeList =
    if null nodeList
        then Nothing
        else
            let matchList = filter ((== nodeLabel) . snd) nodeList
            in  case matchList of
                    [] → Nothing
                    x : _ → Just x


{- | checkIfLeaf checks text to see if leaf.
if the number of left parens is 1 and right parens 1 and no ',' then leaf
if no left parens and no right paren then leaf
then its a leaf
either "bleh", "bleh:00", or "(bleh)label:00"
-}
checkIfLeaf ∷ Text → Bool
checkIfLeaf inText =
    if T.null inText
        then error "Null text to check if leaf in checkIfLeaf"
        else
            let leftParenCount = T.count (T.pack "(") inText
                rightParenCount = T.count (T.pack ")") inText
                commaCount = T.count (T.pack ",") inText
            in  ((leftParenCount == 0) && (rightParenCount == 0) && (commaCount == 0))
                    || ( if (leftParenCount == 0) && (rightParenCount == 0) && (commaCount > 0)
                            then error ("Comma within leaf label" <> show inText)
                            else (leftParenCount == 1) && (rightParenCount == 1) && (commaCount == 0)
                       )


{- | eNewick2FGL takes a single Extended Newick (Text) string and returns FGL graph
allows arbitrary in and out degree except for root and leaves
-}
eNewick2FGL ∷ [G.LNode Text] → [G.LEdge Double] → [(Text, G.LNode Text)] → [(G.LNode Text, G.LEdge Double)]
eNewick2FGL nodeList edgeList = \case
    [] → []
    (inTextFirst, parentNode) : xs →
        let inText = case nodeList of
                -- see if initial call and check format
                []
                    | T.head inTextFirst /= '(' || T.last inTextFirst /= ';' →
                        error $
                            unwords
                                [ "Invalid Extended Newick component,"
                                , " must begin with \'(\'' and end with \';\' : "
                                , T.unpack inTextFirst
                                ]
                -- not first call and/or format OK
                [] → T.takeWhile (/= ';') inTextFirst -- remove trailing ';' if first (a bit wasteful--but intial check on format
                _ → inTextFirst

            -- is a single leaf
            -- need better could be  series of indegree `1 outdegree 1 nodes to a single leaf with no ','
            -- like (a(b(c(d))))
            leafCase =
                -- parse label ala Gary Olsen formalization
                -- since could have reticulate label yeilding two edges and two nodes
                -- Cardona et al 2008  Extended Newick
                let newLeafList = getLeafInfo inText parentNode nodeList
                    newNodeList = fmap fst newLeafList
                    newEdgeList = fmap snd newLeafList
                in  newLeafList <> eNewick2FGL (newNodeList <> nodeList) (newEdgeList <> edgeList) xs

            -- is subtree assumes start and end with parens '(blah)'
            nodeCase =
                let (subTree, nodeLabel, edgeWeight) = getBodyParts inText (length nodeList)
                    thisNode = (length nodeList, nodeLabel)
                    thisEdge = (fst parentNode, length nodeList, edgeWeight)
                    childTextList = getChildren subTree
                    parentNodeList = replicate (length childTextList) thisNode
                    childParentList = zip childTextList parentNodeList
                in  (thisNode, thisEdge) : eNewick2FGL (thisNode : nodeList) (thisEdge : edgeList) (childParentList <> xs)
        in  if checkIfLeaf inText
                then leafCase
                else nodeCase


{- | reindexNode takes an offset and adds to the node index
returning new node
-}
reindexNode ∷ Int → G.LNode Text → G.LNode Text
reindexNode offSet (index, label) = (index + offSet, label)


{- | reindexEdge takes an offset and adds to the two indices of the edge
returning the new edge
-}
reindexEdge ∷ Int → G.LEdge Double → G.LEdge Double
reindexEdge offSet (e, u, label) = (e + offSet, u + offSet, label)


{- | mergeFGLGraphs takes multiple graphs and merges
nodes and edges via reindexing
just adds progessive offsets from graph node indices as added
-}
mergeFGLGraphs ∷ P.Gr Text Double → [P.Gr Text Double] → P.Gr Text Double
mergeFGLGraphs curGraph = \case
    [] → curGraph
    firstGraph : otherGraphs | G.isEmpty curGraph → mergeFGLGraphs firstGraph otherGraphs
    firstGraph : otherGraphs →
        let firstNodes = G.labNodes firstGraph
            firstEdges = G.labEdges firstGraph
            curNodes = G.labNodes curGraph
            curEdges = G.labEdges curGraph
            newNodes = reindexNode (length curNodes) <$> firstNodes
            newEdges = reindexEdge (length curNodes) <$> firstEdges
        in  mergeFGLGraphs (G.mkGraph (curNodes <> newNodes) (curEdges <> newEdges)) otherGraphs


{- | getNodeIndexPair take a list of unique nodes and checks successive nodes and
adds to unique list, also creating a full list of pairs of indicess for non-unique that
can be used as an index map for edges
length of unique list to keep the node indices sequential
-}
getNodeIndexPair ∷ [G.LNode Text] → [(Int, Int)] → [G.LNode Text] → ([G.LNode Text], [(Int, Int)])
getNodeIndexPair uniqueList pairList = \case
    [] → (reverse uniqueList, reverse pairList)
    firstNode@(index, label) : otherNodes → case checkForExistingNode label uniqueList of
        Nothing → getNodeIndexPair (firstNode : uniqueList) ((index, length uniqueList) : pairList) otherNodes
        Just (value, _) →
            let newPair = (index, value)
            in  getNodeIndexPair uniqueList (newPair : pairList) otherNodes


{- | mergeNetNodesAndEdges takes a single graph and merges
nodes and edges due to network nodes and edges
uses checkForExistingNode and creates a map from nodes to reindex edges
needs to be merged first if graphs are combined--or indices will be wrong
-}
mergeNetNodesAndEdges ∷ P.Gr Text Double → P.Gr Text Double
mergeNetNodesAndEdges inGraph =
    -- trace (showGraph inGraph) (
    if G.isEmpty inGraph
        then G.empty
        else
            let nodeList = G.labNodes inGraph
                graphDelta = getMergeNodeEdgeDelta inGraph nodeList
            in  -- nothing to do (no repeated node labels)
                if isNothing graphDelta
                    then -- need to reindex nodes and edges so nodes are sequential
                        let (_, nodeIndexPairs) = getNodeIndexPair [] [] nodeList
                            nodeMap = Map.fromList nodeIndexPairs
                            reindexedNodeList = fmap (reIndexLNode nodeMap) (G.labNodes inGraph)
                            reIndexedEdgeList = fmap (reIndexLEdge nodeMap) (G.labEdges inGraph)
                        in  -- to make nodes sequencentioal required later
                            G.mkGraph reindexedNodeList reIndexedEdgeList
                    else -- modifications were made to graph
                        let (nodesToDelete, edgesToDelete, edgesToCreate) = fromJust graphDelta
                            newGraph = (G.insEdges edgesToCreate . G.delNodes (fmap fst nodesToDelete) $ G.delEdges (fmap G.toEdge edgesToDelete) inGraph)
                        in  -- trace (showGraph newGraph)
                            mergeNetNodesAndEdges newGraph


-- )

{- | getMergeNodeEdgeDelta takes a graph and list of labelled nodes
merges the nodes with identical labels, dfeltes all but the first
and creates new edges to and from first node, deleting the others
works recursively to update graph to keep node and edge indices in synch
this is n^2 in number of network nodes--prob could be made linear
-}
getMergeNodeEdgeDelta ∷ P.Gr Text Double → [G.LNode Text] → Maybe ([G.LNode Text], [G.LEdge Double], [G.LEdge Double])
getMergeNodeEdgeDelta inGraph
    | G.isEmpty inGraph = const $ error "Empty graph in getMergeNodeEdgeDelta"
    | otherwise = \case
        [] → Nothing
        -- Nodes to examine
        (key, val) : moreNodes → case filter ((== val) . snd) moreNodes of
            -- node label not repeated
            [] → getMergeNodeEdgeDelta inGraph moreNodes
            -- node label repeated
            nodesToDelete →
                let inEdgesToDelete = foldMap (G.inn inGraph . fst) nodesToDelete
                    outEdgesToDelete = foldMap (G.out inGraph . fst) nodesToDelete

                    -- Make new in-edges
                    (newInEdgeUs, _, newInEdgeLabels) = unzip3 inEdgesToDelete
                    newInEdgeVs = replicate (length inEdgesToDelete) key
                    inEdgesToAdd = zip3 newInEdgeUs newInEdgeVs newInEdgeLabels

                    -- make new out-edges
                    newOutEdgeUs = replicate (length outEdgesToDelete) key
                    (_, newOutEdgeVs, newOutEdgeLabels) = unzip3 outEdgesToDelete
                    outEdgesToAdd = zip3 newOutEdgeUs newOutEdgeVs newOutEdgeLabels
                in  Just (nodesToDelete, inEdgesToDelete <> outEdgesToDelete, inEdgesToAdd <> outEdgesToAdd)


{- | subTreeSize takes a nodeList and retuns the number of leaves that can be
traced back to those nodes (for single just pass list of 1 node)
this used for ordering of groups left (smaller) right (larger)
-}
subTreeSize ∷ P.Gr a b → Int → [G.LNode a] → Int
subTreeSize inGraph counter = \case
    [] → counter
    firstNode : otherNodes →
        let children = G.suc inGraph $ fst firstNode
            -- assumes all childrfen have a label (if not--problems)
            labelList = fmap (fromJust . G.lab inGraph) children
            labChildren = zip children labelList
        in  subTreeSize inGraph (counter + length labChildren) (otherNodes <> labChildren)


{- | getRoot takes a greaph and list of nodes and returns vertex with indegree 0
so assumes a connected graph--with a single root--not a forest
-}
getRoots ∷ P.Gr a b → [G.LNode a] → [G.LNode a]
getRoots inGraph = \case
    [] → [] -- error "Root vertex not found in getRoot"
    firstNode@(index, _) : otherNodes →
        let continuation = getRoots inGraph otherNodes
        in  if G.indeg inGraph index == 0
                then firstNode : continuation
                else continuation


{-
-- | removeDuplicateSubtreeText removes duplicate subtree textx that come from indegree > 1 nodes
-- there should be at least two of each network texts.
-- for each case, the first instance is kept, and the remainders are replaced with the node label
-- and edge weight if specified (:000)
removeDuplicateSubtreeText :: (Show b) => Text -> [G.LNode Text] -> P.Gr Text b -> Bool -> Bool -> Text
removeDuplicateSubtreeText inRep netNodeList fglGraph writeEdgeWeight writeNodeLable =
  if null netNodeList then inRep
  else
    let netNodeText = T.init $ component2Newick fglGraph writeEdgeWeight writeNodeLable (head netNodeList)
        -- edge weight already removed or may not match all occurences
        -- I have no idea why--but there are extraneous double quotes that have to be removed.
        nodeText' = T.filter (/= '\"') netNodeText
        -- checks to see if leaf is indegree > 1
        nodeText = if not (checkIfLeaf nodeText') then nodeText' else T.filter (/= '(') $ T.filter (/= ')') nodeText'
        nodeLabel = T.reverse $ T.takeWhile (/= ')') $ T.reverse nodeText
        textList = T.splitOn nodeText inRep
        -- isFound = T.isInfixOf nodeText inRep -- (T.pack "(4:1.0)Y#H1") inRep
    in
    -- trace ("Removing ? " <> show isFound <> " " <> show nodeText <> " " <> show nodeLabel <> " from " <> show inRep <> " in list (" <> show (length textList) <> ") " <> show textList) (
    -- since root cannot be network neither first nor last pieces should be empty
    if T.null (head textList) || T.null (last textList) then error ("Text representation of graph is incorrect with subtree:\n" <> T.unpack nodeText
      <> " first or last in representation:\n " <> T.unpack inRep)
    --else if length textList == 1 then error ("Text representation of graph is incorrect with subtree:\n" <> T.unpack nodeText
    --  <> " not found in representation:\n " <> T.unpack inRep)
    else if length textList == 2 then
        trace "Warning: Network subtree present only once--extraneous use of \'#\' perhaps--"
        inRep
    else -- should be minimum of 3 pieces (two occurences of subtree text removed) is subtree found 2x and not sister to itself (which should never happen)
      -- edge weights (if they occur) remain the beginning of each text list (after the first)
      let firstPart = head textList `T.append` nodeText
          secondPart = T.intercalate nodeLabel (tail textList)
      in
      removeDuplicateSubtreeText (firstPart `T.append` secondPart) (tail netNodeList) fglGraph writeEdgeWeight writeNodeLable
      --)
-}

{- | getDistToRoot takes a node and a graph and gets the shortes path to root
and returns the number of links
-}
getDistToRoot ∷ P.Gr Text b → Int → G.Node → Int
getDistToRoot fglGraph counter inNode =
    if counter > length (G.nodes fglGraph)
        then error "Cycle likely in graph, path to root larger than number of nodes"
        else
            let parents = G.pre fglGraph inNode
            in  if null parents
                    then counter
                    else
                        let parentPaths = fmap (getDistToRoot fglGraph (counter + 1)) parents
                        in  minimum parentPaths


{- | modifyInDegGT1Leaves operates on the leaf nodes with indegree > 1 to prepare them for enewick representation
leaves with indegree greater than one are modified such that:
  1) a new node is created as parent to the leaf and added to the "add" list
  2) edges incident on the leaf are put in the "delete" list
  3) edges are created from teh new node to the leaf, and edges are "added" from teh leaf's parent to the new node
-}
modifyInDegGT1Leaves
    ∷ P.Gr Text Double
    → Int
    → [G.LNode Text]
    → ([G.LNode Text], [G.LEdge Double], [G.LEdge Double])
    → ([G.LNode Text], [G.LEdge Double], [G.LEdge Double])
modifyInDegGT1Leaves origGraph totalNumberNodes
    | G.isEmpty origGraph = error "Empty graph in modifyInDegGT1Leaves"
    | otherwise = \case
        [] → id
        (label, _) : otherLeaves → case G.indeg origGraph label of
            1 → modifyInDegGT1Leaves origGraph totalNumberNodes otherLeaves
            -- in a "network leaf"
            _ → \(nodesToAdd, edgesToAdd, edgesToDelete) →
                let inEdgeList = G.inn origGraph label
                    (parentNodeList, _, inEdgeLabels) = unzip3 inEdgeList
                    newNode = (totalNumberNodes, T.pack $ "HTU" <> show totalNumberNodes)
                    newEdge = (totalNumberNodes, label, 0 ∷ Double)
                    newEdgeList = newEdge : zip3 parentNodeList (repeat totalNumberNodes) inEdgeLabels
                in  modifyInDegGT1Leaves
                        origGraph
                        (totalNumberNodes + 1)
                        otherLeaves
                        (newNode : nodesToAdd, newEdgeList <> edgesToAdd, inEdgeList <> edgesToDelete)


{- | modifyInDegGT1HTU operates on the HTU nodes with indegree > 1 to prepare them for enewick representation
HTUs with indegree greater than one are modified such that:
  1) the original HTU is maintained the first edge to that HTU is Maintained also
  2) other edges to that HTU are put in "delete" list
  3) a new HTU node is created for each deleted edge with same label as original HTU
  4) edges are created from the parents (except for the firt one) to the new nodes such thayt each one is indegree=1 and outdegree=0
  5) ne edges are put in teh "add list"
Graphs are remade at eash recursivfe step tpo keep node/edge indexing correct
-}
modifyInDegGT1HTU ∷ P.Gr Text Double → Int → [G.LNode Text] → P.Gr Text Double
modifyInDegGT1HTU origGraph nodeIndex
    | G.isEmpty origGraph = error "Empty graph in modifyInDegGT1HTU"
    | otherwise = \case
        [] → origGraph
        firstHTU@(label, value) : otherHTUs → case G.indeg origGraph label of
            1 → modifyInDegGT1HTU origGraph nodeIndex otherHTUs
            -- in a "network leaf"
            _ →
                let inEdgeList = G.inn origGraph label
                    outEdgeList = G.out origGraph label
                    (parentNodeList, _, inEdgeLabels) = unzip3 inEdgeList
                    (_, childNodeList, _) = unzip3 outEdgeList

                    -- Create new nodes and edges
                    numNewNodes = length inEdgeList
                    nodeIndexList = [nodeIndex .. nodeIndex + numNewNodes - 1]
                    newNodeList = zip nodeIndexList . repeat $ (T.pack "#") <> value

                    -- Edges to new nodes and edges from first new node ot children
                    newEdgeList = zip3 parentNodeList nodeIndexList inEdgeLabels <> zip3 (repeat nodeIndex) childNodeList inEdgeLabels
                    newGraph =
                        ( (G.insEdges newEdgeList . G.insNodes newNodeList) . G.delNode (fst firstHTU) $
                            G.delEdges (fmap G.toEdge (inEdgeList <> outEdgeList)) origGraph
                        )
                in  modifyInDegGT1HTU newGraph (nodeIndex + numNewNodes) otherHTUs


-- )

{- | modifyFGLForEnewick takes an FGl graphs and modified for enewick output
1) makes leaves that are indegree > 1 indegree 1 by creationg a new parent node with
   the leafs parents as parents and teh leaf as single child
2) convertes each indegree > 1 node (non--leaf) to a series of indegree 1 nodes
   the first of which gets the first parent and all the children. Each subsequent
   parent gets a node with the name label and no children
-}
modifyFGLForEnewick ∷ P.Gr Text Double → P.Gr Text Double
modifyFGLForEnewick inGraph =
    if G.isEmpty inGraph
        then G.empty
        else
            let (_, leafNodes, nonLeafNodes) = splitVertexList inGraph

                -- leaf nodes
                (nodesToAdd, edgesToAdd, edgesToDelete) = modifyInDegGT1Leaves inGraph (G.order inGraph) leafNodes ([], [], [])
                leafModGraph = (G.insEdges edgesToAdd . G.insNodes nodesToAdd $ G.delEdges (fmap G.toEdge edgesToDelete) inGraph)

                -- HTU nodes
                -- (nodesToAddHTU, edgesToAddHTU, nodesToDeleteHTU, edgesToDeleteHTU) = modifyInDegGT1HTU leafModGraph (G.order leafModGraph) (nonLeafNodes <> nodesToAdd)
                -- htuModGraph =  G.insEdges edgesToAddHTU $ G.insNodes nodesToAddHTU $ G.delNodes (fmap fst nodesToDeleteHTU) $ G.delEdges (fmap G.toEdge edgesToDeleteHTU) leafModGraph
                htuModGraph = modifyInDegGT1HTU leafModGraph (G.order leafModGraph) (nonLeafNodes <> nodesToAdd)
            in  -- trace (showGraph leafModGraph <> "\n" <> showGraph htuModGraph)
                htuModGraph


{- | fgl2FEN take a fgl graph and returns a Forest Enhanced Newick Text
  Can be simplified along lines of Cardona with change to graph before
  generating the rep bu splitting Hybrid nodes.
  enewick requires (it seems) indegree 1 for leaves
  so need to creates nodes for indegree > 1 leaves
these are not issues for dot files
-}
fgl2FEN ∷ Bool → Bool → P.Gr Text Double → Text
fgl2FEN writeEdgeWeight writeNodeLable inFGLGraph =
    if G.isEmpty inFGLGraph
        then T.empty
        else -- Modify greaph for enewick stuff (leaves -> indegree 1, 'split' network nodes)
        -- trace ("Original:\n" <> showGraph inFGLGraph) (
            let fglGraph = modifyFGLForEnewick inFGLGraph
            in  -- get forest roots
                -- trace ("Final:\n" <> showGraph fglGraph) (
                let numRoots = getRoots fglGraph (G.labNodes fglGraph)
                    rootGraphSizeList = fmap (subTreeSize fglGraph 0 . (: [])) numRoots
                    rootAndSizes = zip rootGraphSizeList numRoots
                    rootOrder = L.sortOn fst rootAndSizes
                    fenTextList = fmap (component2Newick fglGraph writeEdgeWeight writeNodeLable . snd) rootOrder
                    wholeRep = T.concat $ (`T.append` T.singleton '\n') <$> fenTextList
                in  -- trace ("fgl2FEN " <> (show $ length numRoots) <> " " <> show rootOrder <> "->" <> show fenTextList) (
                    if length fenTextList == 1
                        then wholeRep -- just a single tree/network
                        else T.snoc (T.cons '<' wholeRep) '>' -- forest
                        -- ))


{- | fglList2ForestEnhancedNewickString takes FGL representation of forest and returns
list of Forest Enhanced Newick as a single String
-}
fglList2ForestEnhancedNewickString ∷ [P.Gr Text Double] → Bool → Bool → String
fglList2ForestEnhancedNewickString inFGLList writeEdgeWeight writeNodeLable =
    if null inFGLList
        then "\n"
        else
            let forestTextList =
                    (`T.append` T.singleton '\n') <$> parmap rdeepseq (fgl2FEN writeEdgeWeight writeNodeLable) (filter (not . G.isEmpty) inFGLList)
                forestListString = T.unpack $ T.concat forestTextList
            in  forestListString


-- | component2Newick take a graph and root and creates enhanced newick from that root
component2Newick ∷ (Show a) ⇒ P.Gr Text a → Bool → Bool → G.LNode Text → Text
component2Newick fglGraph writeEdgeWeight writeNodeLable (index, label)
    | G.isEmpty fglGraph = mempty
    | otherwise =
        -- start with root (no in edge weight) issue at root not seeing multiple components properly
        let -- preorder traversal
            middlePartList = foldMap (getNewick fglGraph writeEdgeWeight writeNodeLable . replicate 1) $ G.out fglGraph index
            label'
                | writeNodeLable = label
                | otherwise = mempty -- trivial trees or write node name
        in  -- "naked" root
            let firstText = case middlePartList of
                    [] → fold [T.pack "(", label, T.pack ")", T.pack ";"]
                    [x] → fold [T.pack "(", x, T.pack ")", label', T.pack ";"]
                    x : xs → fold [T.pack "(", T.intercalate (T.pack ",") $ x : xs, T.pack ")", label', T.pack ";"]
            in  removeTrailingComma $ deduplicateCommas firstText


-- | makeLabel takes Maybe Text and retuns T.empty if Nothing, Text otherwise
makeLabel ∷ Maybe Text → Text
makeLabel = fold


-- | fix for newick lack of paren in specific situation--inelegant
endStart ∷ Text
endStart = T.pack ")("


newEndStart ∷ Text
newEndStart = T.pack "),("


{- | getNewick takes an edge of a graph and either creates the text if a leaf
or recurses down tree if has descendents, adding  commas, outer parens, labels, and edge weights if they exist.
need to filter redundant subtrees later at the forest level (Can have shared node between rooted components)
-}
getNewick ∷ (Show a) ⇒ P.Gr Text a → Bool → Bool → [G.LEdge a] → [Text]
getNewick fglGraph writeEdgeWeight writeNodeLable
    | G.isEmpty fglGraph = const [mempty]
    | otherwise = \case
        [] → []
        firstEdge@(_, curNodeIndex, edgeLabel) : otherEdges →
            let continuation = getNewick fglGraph writeEdgeWeight writeNodeLable otherEdges
            in  case G.out fglGraph curNodeIndex of
                    -- is a leaf, no children
                    [] → case G.lab fglGraph curNodeIndex of
                        Nothing →
                            error $
                                fold
                                    [ "Leaf without label in getNewick: node "
                                    , show curNodeIndex
                                    , " edge: "
                                    , show firstEdge
                                    , "\n"
                                    , G.prettify fglGraph
                                    ]
                        Just leafLabel →
                            let newLabelList
                                    | writeEdgeWeight = [fold [leafLabel, T.pack ":", T.pack $ show edgeLabel]]
                                    | otherwise = [leafLabel]
                            in  case otherEdges of
                                    [] → newLabelList
                                    _ → [fold $ newLabelList <> [T.pack ","] <> continuation]
                    -- is HTU recurse
                    _ →
                        let nodeLabel
                                | not writeNodeLable = mempty
                                | otherwise = makeLabel $ G.lab fglGraph curNodeIndex
                            prefix x = [T.pack "(", x, T.pack ")", nodeLabel]
                        in  case getNewick fglGraph writeEdgeWeight writeNodeLable $ G.out fglGraph curNodeIndex of
                                -- outdegree 1
                                [x] →
                                    let middleText = T.replace endStart newEndStart x
                                        suffix
                                            | not writeEdgeWeight = [T.pack ","]
                                            | otherwise = [T.pack ":", T.pack $ show edgeLabel, T.pack ","]
                                        expansion = fold $ prefix middleText <> suffix
                                    in  expansion : continuation
                                -- multiple children, outdegree > 1
                                middlePartList →
                                    let middleText = T.intercalate (T.pack ",") middlePartList
                                        cleanText = removeTrailingComma . deduplicateCommas
                                        suffix
                                            | not writeEdgeWeight = []
                                            | otherwise = [T.pack ":", T.pack $ show edgeLabel]
                                        expansion = cleanText . fold $ prefix middleText <> suffix
                                    in  expansion : continuation


-- |  stringGraph2TextGraph take P.Gr String Doble and converts to P.Gr Text a
stringGraph2TextGraph ∷ P.Gr String Double → P.Gr Text Double
stringGraph2TextGraph inStringGraph =
    let (indices, labels) = unzip $ G.labNodes inStringGraph
        edges = G.labEdges inStringGraph
        textLabels = fmap T.pack labels
        newNodes = zip indices textLabels
    in  G.mkGraph newNodes edges


{- |  stringGraph2TextGraphDouble take P.Gr String a and converts to P.Gr Text Double
ignores the edge label and reurns "0.0"
-}
stringGraph2TextGraphDouble ∷ P.Gr String String → P.Gr Text Double
stringGraph2TextGraphDouble inStringGraph =
    let (indices, labels) = unzip $ G.labNodes inStringGraph
        textLabels = fmap T.pack labels
        newNodes = zip indices textLabels
        origEdges = G.labEdges inStringGraph
        newEdges = fmap dummyRelabelEdges origEdges
    in  G.mkGraph newNodes newEdges
    where
        dummyRelabelEdges ∷ (a, b, String) → (a, b, Double)
        dummyRelabelEdges (a, b, c) =
            let newC = readMaybe c ∷ Maybe Double
            in  if isJust newC
                    then (a, b, fromJust newC)
                    else (a, b, 0.0)


-- |  textGraph2StringGraph take P.Gr String a and converts to P.Gr Text a
textGraph2StringGraph ∷ P.Gr Text b → P.Gr String b
textGraph2StringGraph inTextGraph =
    let (indices, labels) = unzip $ G.labNodes inTextGraph
        edges = G.labEdges inTextGraph
        stringLabels = fmap T.unpack labels
        newNodes = zip indices stringLabels
    in  G.mkGraph newNodes edges


{-
Fucntions to relabel Dot greaph to RawGraph format
-}

{- | findStrLabel checks Attributes (list f Attribute) from Graphvz to extract the String label of node
returns Maybe Text
-}
findStrLabel ∷ Attributes → Maybe Text
findStrLabel = getFirst . foldMap getStrLabel


-- | getStrLabel takes an Attribute and reurns Text if StrLabel found, mempty otherwise
getStrLabel ∷ Attribute → First Text
getStrLabel (Label (StrLabel txt)) = First . Just $ txt
getStrLabel _ = mempty


{- | getLeafText takes a pairs (node vertex number, graphViz Attributes)
and returns Text name of leaf of Stringified nude number if unlabbeled
-}
getLeafText ∷ (Int, Attributes) → Text
getLeafText (nodeIndex, nodeLabel) =
    let maybeTextLabel = findStrLabel nodeLabel
    in  fromMaybe (T.pack $ show nodeIndex) maybeTextLabel


-- | splitVertexList splits the vertices of a graph into ([root], [leaf], [non-leaf-non-root])
splitVertexList ∷ P.Gr a b → ([G.LNode a], [G.LNode a], [G.LNode a])
splitVertexList inGraph
    | G.isEmpty inGraph = ([], [], [])
    | otherwise =
        let -- leaves
            degOutList = G.outdeg inGraph <$> G.nodes inGraph
            newNodePair = zip degOutList (G.labNodes inGraph)
            leafPairList = filter ((== 0) . fst) newNodePair
            (_, leafList) = unzip leafPairList

            -- roots
            degInList = G.indeg inGraph <$> G.nodes inGraph
            newRootPair = zip degInList (G.labNodes inGraph)
            rootPairList = filter ((== 0) . fst) newRootPair
            (_, rootList) = unzip rootPairList

            -- non-leaves, non-root
            nodeTripleList = zip3 degOutList degInList (G.labNodes inGraph)
            nonLeafTripleList = filter ((> 0) . fst3) $ filter ((> 0) . snd3) nodeTripleList
            (_, _, nonLeafList) = unzip3 nonLeafTripleList
        in  (rootList, leafList, nonLeafList)


-- | getVertexList returns vertex complement of graph from DOT file
getVertexList ∷ P.Gr Attributes Attributes → [G.LNode Text]
getVertexList inGraph
    | G.isEmpty inGraph = mempty
    | otherwise =
        let (nodeVerts, _) = unzip $ G.labNodes inGraph
            newLabels = (getLeafText <$> G.labNodes inGraph)
            vertexList' = zip nodeVerts newLabels
        in  vertexList'


{- |
'relabelFGL' takes P.Gr Attributes Attributes and converts to P.Gr Text Double
-}
relabelFGL ∷ P.Gr Attributes Attributes → P.Gr Text Double
relabelFGL inGraph
    | G.isEmpty inGraph = G.empty
    | otherwise =
        let newVertexList = getVertexList inGraph
            newEdgeList = fmap relabeLEdge (G.labEdges inGraph)
        in  G.mkGraph newVertexList newEdgeList


-- | relabeLEdge convertes edhe labels to Double
relabeLEdge ∷ G.LEdge b → G.LEdge Double
relabeLEdge (u, v, _) = (u, v, 0.0 ∷ Double)


{- |
'relabelFGL' takes P.Gr Attributes Attributes and converts to P.Gr Text Double
-}
relabelFGLEdgesDouble ∷ P.Gr a b → P.Gr a Double
relabelFGLEdgesDouble inGraph
    | G.isEmpty inGraph = G.empty
    | otherwise =
        let newEdgeList = fmap relabeLEdge (G.labEdges inGraph)
        in  G.mkGraph (G.labNodes inGraph) newEdgeList


-- | convertGraphToStrictText take a graphs with laze Text and makes it strict.
convertGraphToStrictText ∷ P.Gr Text Double → P.Gr Strict.Text Double
convertGraphToStrictText inGraph
    | G.isEmpty inGraph = G.empty
    | otherwise =
        let nodeList = G.labNodes inGraph
            nodesStrictText = fmap (T.toStrict . snd) nodeList
            nodeIndices = fmap fst nodeList
        in  G.mkGraph (zip nodeIndices nodesStrictText) (G.labEdges inGraph)


-- | fgl2DotString takes an FGL graph and returns a String
fgl2DotString ∷ (GV.Labellable a, GV.Labellable b) ⇒ P.Gr a b → String
fgl2DotString inGraph
    | G.isEmpty inGraph = mempty
    | otherwise = T.unpack . GVP.renderDot . GVP.toDot $ GV.graphToDot GV.quickParams inGraph


-- | modifyVertexEdgeLabels keeps or removes vertex and edge labels
modifyVertexEdgeLabels ∷ (Show b) ⇒ Bool → Bool → P.Gr String b → P.Gr String String
modifyVertexEdgeLabels keepVertexLabel keepEdgeLabel inGraph
    | G.isEmpty inGraph = G.empty
    | otherwise =
        let inLabNodes = G.labNodes inGraph
            degOutList = G.outdeg inGraph <$> G.nodes inGraph
            nodeOutList = zip degOutList inLabNodes
            leafNodeList = snd <$> filter ((== 0) . fst) nodeOutList
            nonLeafNodeList = snd <$> filter ((> 0) . fst) nodeOutList
            showLabel ∷ (Show c) ⇒ (a, b, c) → (a, b, String)
            showLabel (e, u, l) = (e, u, show l)
            newNonLeafNodes =
                if keepVertexLabel
                    then nonLeafNodeList
                    else zip (fmap fst nonLeafNodeList) (replicate (length nonLeafNodeList) "")
            inLabEdges = G.labEdges inGraph
            inEdges = fmap G.toEdge inLabEdges
            newEdges =
                if keepEdgeLabel
                    then fmap showLabel inLabEdges
                    else fmap (`G.toLEdge` "") inEdges
        in  G.mkGraph (leafNodeList <> newNonLeafNodes) newEdges


{- | relabelLeaf takes list of pairs and if current leaf label
is snd in a pair, it replaces the label with the first of the pair
-}
relabelLeaf ∷ [(Text, Text)] → G.LNode Text → G.LNode Text
relabelLeaf namePairList leafNode =
    if null namePairList
        then leafNode
        else
            let foundName = L.find ((== snd leafNode) . snd) namePairList
            in  if isNothing foundName
                    then leafNode
                    else (fst leafNode, fst $ fromJust foundName)


{- | relabelGraphLeaves takes and FGL graph Text Double and renames based on pair of Text
old name second, new name first in pair
-}
relabelGraphLeaves ∷ [(Text, Text)] → P.Gr Text Double → P.Gr Text Double
relabelGraphLeaves namePairList inGraph
    | null namePairList = inGraph
    | G.isEmpty inGraph = inGraph
    | otherwise =
        let (rootVerts, leafList, otherVerts) = splitVertexList inGraph
            edgeList = G.labEdges inGraph
            newLeafList = fmap (relabelLeaf namePairList) leafList
        in  G.mkGraph (newLeafList <> rootVerts <> otherVerts) edgeList


{- | checkGraphsAndData leaf names (must be sorted) and a graph
nedd to add other sanity checks
does not check for cycles becasue that is done on input
-}
checkGraphsAndData ∷ (Foldable f) ⇒ f Text → P.Gr Text Double → P.Gr Text Double
checkGraphsAndData leafNameList inGraph
    | G.isEmpty inGraph = inGraph
    | otherwise = case toList leafNameList of
        [] → error "Empty leaf name list"
        x : xs →
            let (_, leafList, _) = splitVertexList inGraph
                dataLeafSet = Set.fromList $ x : xs
            in  case leafList of
                    [] → inGraph
                    y : ys →
                        let graphLeafNames = NE.sort $ snd <$> y :| ys
                            graphLeafSet = Set.fromAscList $ toList graphLeafNames
                            nameGroupsGT1 = NE.filter ((> 1) . length) $ NE.group1 graphLeafNames
                        in  case nameGroupsGT1 of
                                -- check for repeated terminals
                                z : zs → errorWithoutStackTrace $ "Input graph has repeated leaf labels" <> show (NE.head <$> z : zs)
                                -- check for leaf complement identity
                                []
                                    | dataLeafSet /= graphLeafSet →
                                        let inBoth = dataLeafSet `Set.intersection` graphLeafSet
                                            onlyInData = dataLeafSet Set.\\ inBoth
                                            onlyInGraph = graphLeafSet Set.\\ inBoth
                                        in  errorWithoutStackTrace $
                                                unwords
                                                    [ "Data leaf list does not match graph leaf list: \n\tOnly in data :"
                                                    , show onlyInData
                                                    , "\n\tOnly in Graph :"
                                                    , show onlyInGraph
                                                    , "(concatenated names could be due to lack of commas ',' or unbalanced parentheses '()') in grap[h specification"
                                                    ]
                                [] → inGraph


-- | cyclic maps to cyclic funcitn in moduel Cyclic.hs
cyclic ∷ (G.DynGraph g) ⇒ g a b → Bool
cyclic = C.cyclic


-- | makeHTULabel take HTU index and amkes into HTU#
makeHTULabel ∷ Int → Text
makeHTULabel index = T.pack $ "HTU" <> show index


{- | getLeafLabelMatches tyakes the total list and looks for elements in the smaller local leaf set
retuns int index of the match or (-1) if not found so that leaf can be added in orginal order
-}
getLeafLabelMatches ∷ [G.LNode Text] → G.LNode Text → (Int, Int)
getLeafLabelMatches = \case
    [] → \(idx, _) → (-1, idx)
    (index, leafString) : otherLeaves → \case
        (value, str) | leafString == str → (index, value)
        totNode → getLeafLabelMatches otherLeaves totNode


{- | reIndexLeavesEdges Leaves takes input fgl graph and total input leaf sets and reindexes node, and edges
such that leaves are nodes 0-n-1, then roots and then other htus and edges are reindexed based on that via a map
-}
reIndexLeavesEdges ∷ (Foldable f) ⇒ f Text → P.Gr Text Double → P.Gr Text Double
reIndexLeavesEdges leafList inGraph
    | G.isEmpty inGraph = G.empty
    | otherwise =
        -- reindex nodes and edges and add in new nodes (total leaf set + local HTUs)
        -- create a map between inputLeafSet and graphLeafSet which is the canonical enumeration
        -- then add in local HTU nodes and for map as well
        let numLeaves = length leafList
            canonicalLeafOrder = zip [0 ..] $ toList leafList
            (rootList, leafVertexList, nonRootHTUList) = splitVertexList inGraph
            correspondanceList = fmap (getLeafLabelMatches leafVertexList) canonicalLeafOrder
            matchList = filter ((/= (-1)) . fst) correspondanceList
            htuList = fmap fst $ rootList <> nonRootHTUList
            htuNumber = length htuList
            newHTUNumbers = [numLeaves .. (numLeaves + htuNumber - 1)]
            newHTULabels = fmap makeHTULabel newHTUNumbers
            htuMatchList = zip htuList newHTUNumbers
            -- remove order dependancey
            vertexMap = Map.fromList (matchList <> htuMatchList)
            reIndexedEdgeList = fmap (reIndexLEdge vertexMap) (G.labEdges inGraph)
            newNodeList = canonicalLeafOrder <> zip newHTUNumbers newHTULabels
        in  G.mkGraph newNodeList reIndexedEdgeList


-- | reIndexEdge takes an (Int, Int) map, labelled edge, and returns a new labelled edge with new e,u vertices
reIndexLEdge ∷ Map.Map Int Int → G.LEdge Double → G.LEdge Double
reIndexLEdge vertexMap inEdge =
    if Map.null vertexMap
        then error "Null vertex map"
        else
            let (e, u, label) = inEdge
                newE = Map.lookup e vertexMap
                newU = Map.lookup u vertexMap
            in  -- trace ((show $ Map.size vertexMap) <> " " <> (show $ Map.toList vertexMap)) (
                if isNothing newE
                    then error ("Edge error looking up vertex " <> show e <> " in " <> show (e, u))
                    else
                        if isNothing newU
                            then error ("Edge error looking up vertex " <> show u <> " in " <> show (e, u))
                            else (fromJust newE, fromJust newU, label)


-- )

-- | reIndexNode takes an (Int, Int) map, labelled node, and returns a new labelled node with new vertex
reIndexLNode ∷ Map.Map Int Int → G.LNode Text → G.LNode Text
reIndexLNode vertexMap inNode =
    if Map.null vertexMap
        then error "Null vertex map"
        else
            let (index, label) = inNode
                newIndex = Map.lookup index vertexMap
            in  if isNothing newIndex
                    then error ("Error looking up vertex " <> show index <> " in " <> show inNode)
                    else (fromJust newIndex, label)


deduplicateCommas ∷ Text → Text
deduplicateCommas = T.replace (T.pack ",,") (T.pack ",")


removeTrailingComma ∷ Text → Text
removeTrailingComma = T.replace (T.pack ",)") (T.pack ")")

{- |
Module      :  genRandTree
Description :  Generates a "random" tree specifying leaf number and tree distributin (Uniform, Yule)
Copyright   :  (c) 2025 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
License     :  

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met: 

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer. 
2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution. 

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation are those
of the authors and should not be interpreted as representing official policies, 
either expressed or implied, of the FreeBSD Project.

Maintainer  :  Ward Wheeler <wheeler@amnh.org>
Stability   :  unstable
Portability :  portable (I hope)

-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.IO
import System.Process
import System.Environment
import Data.List
import Data.Maybe
import Data.Text.Lazy qualified as T
import Debug.Trace
import GeneralUtilities
import GraphFormatUtilities qualified as GFU
import LocalGraph qualified as LG
import System.Random
import Text.Read
import Debug.Trace

-- | Graph Type
data GraphType = Tree | Network
    deriving stock (Show, Eq)

-- | Tree distribution  variety
data DistributionType = Uniform | Yule 
    deriving stock (Show, Eq)

-- | output as Graphviz/Dot or newick
data OutputFormat = GraphViz | Newick 
    deriving stock (Show, Eq)

-- | Brnach length distribution
data BranchDistribution = None | Exponential | UniformD | Constant 
    deriving stock (Show, Eq)

-- changeDotPreamble takes an input string to search for and a new one to add in its place
-- searches through dot file (can have multipl graphs) replacing teh search string each time.
changeDotPreamble ∷ String → String → String → String
changeDotPreamble findString newString inDotString =
    if null inDotString
        then []
        else changePreamble' findString newString [] (lines inDotString)


-- changeDotPreamble' internal process for changeDotPreamble
changePreamble' ∷ String → String → [String] → [String] → String
changePreamble' findString newString accumList inLineList =
    if null inLineList
        then unlines $ reverse accumList
        else -- trace ("CP':" <> (head inLineList) <> " " <>  findString <> " " <> newString) (

            let firstLine = head inLineList
            in  if firstLine == findString
                    then changePreamble' findString newString (newString : accumList) (tail inLineList)
                    else changePreamble' findString newString (firstLine : accumList) (tail inLineList)

-- | removeLabel00 removes the [label=0.0] field from edge specification in dot format
removeLabel00 :: String -> String
removeLabel00 inString = 
    if null inString then []
    else 
        let inLines = fmap words $ lines inString
            newLines = fmap deleteLabel0 inLines
        in
        unlines newLines

-- | deleteLabel0 removes [label=0.0] from list of words
deleteLabel0 :: [String] -> String
deleteLabel0 inWordList =
    if null inWordList then []
    else 
        if "[label=0.0];" `notElem` inWordList then unwords inWordList
        else 
            let newWordList = (filter (/=  "[label=0.0];") inWordList) <> [";"]
            in
            unwords newWordList

-- | chooseRandomElement selects man edge at random from list
chooseRandomElement :: StdGen -> [a] -> (StdGen,a)
chooseRandomElement inGen elementList =
   if null elementList then error "Null edge list to split"
   else 
      let (index, newGen) = randomR  (0, (length elementList) - 1) inGen
      in
      (newGen, elementList !! index)

-- | genRandTreeFGL generates a random gfl tree with leaf label list and distribution
genRandTreeFGL :: StdGen -> Int -> Int -> [String] -> DistributionType -> LG.Gr String String -> (StdGen, LG.Gr String String)
genRandTreeFGL inGen numLeaves htuCounter leafList distribution inGraph =
   let inList = uncons leafList
   in
   if isNothing inList then (inGen, inGraph)
   else 
      let (firstTerminal, restList) = fromJust inList
          edgeList = LG.labEdges inGraph
          nonOutgroupEgdeList = filter ((/= 0) . snd3) edgeList

          edgesAvailableToSplit = if distribution == Uniform then 
                                       nonOutgroupEgdeList
                                  else 
                                       filter ((<  numLeaves) . snd3) nonOutgroupEgdeList

          (newGen, edgeToSplit@(e,v,_)) = chooseRandomElement inGen edgesAvailableToSplit
          newNodeIndex = numLeaves + htuCounter
          addedTerminalIndex = read (drop 1 firstTerminal) :: Int
          
          newNodeList = [(newNodeIndex, "HTU" <> (show newNodeIndex)), (addedTerminalIndex, firstTerminal)]
          
          edgessToAdd = [(e, newNodeIndex, "Edge" <> (show newNodeIndex)), (newNodeIndex, v, "Edge" <> (show v)), (newNodeIndex, addedTerminalIndex, "Edge" <> (show addedTerminalIndex))]

          newGraph = LG.insEdges edgessToAdd $ LG.insNodes newNodeList $ LG.delLEdge edgeToSplit inGraph
      in 
      genRandTreeFGL newGen numLeaves (htuCounter + 1) restList distribution newGraph
     

-- uniform2Exponential takes the branchParam and uniform random value and converts uniform random double values on [0.0, branchParam]
-- and retuns exponential random values with parameter branchParam
uniform2Exponential ::  Double -> Double -> Double
uniform2Exponential branchParam uniVal =
    if not (branchParam > 0.0) then 
        errorWithoutStackTrace ("Branch length parameter (double) must be > 0.0: " <> (show branchParam))
    else
        (-1.0 / branchParam) * (log (uniVal / branchParam))

-- relabelWDist erlabels graph edges with values from infinite list
relabelWDist :: [Double] -> String -> String
relabelWDist edgeValList inString =
    if null inString then []
    else 
        let inLines = fmap words $ lines inString
            newLines = fmap changeLabel $ zip edgeValList inLines
        in
        unlines newLines

-- | changeLabel changes [label=0.0] from to [label=X] list of words
changeLabel :: (Double, [String]) -> String
changeLabel (newLableVal, inWordList) =
    if null inWordList then []
    else 
        -- trace (show ((concat inWordList), isInfixOf  "Edge" (concat inWordList),   isInfixOf "Digraph" (concat inWordList) )) $
        if isInfixOf  "Edge" (concat inWordList) == False then (concat inWordList)
        else if  isInfixOf "Digraph" (concat inWordList)  == True then (concat inWordList)
        else 
            let newWordList = (takeWhile (/=  '[') $ concat inWordList) <> ("[label=\"" <> (show newLableVal) <>  "\"];")
            in
            newWordList

-- relabelEdges relabels edges for newick output (could generalize)
relabelEdges :: [(Double, LG.LEdge String)] -> [LG.LEdge String]
relabelEdges inPairList =
    if null inPairList then []
    else 
        let (firstWieght', firstEdge') = head inPairList
            firstEdge =  LG.toEdge firstEdge'
            firstWieght = show firstWieght'
        in
        (LG.toLEdge firstEdge firstWieght) : relabelEdges (tail inPairList) 


-- | relabelNode  relabels node with string cersion ofg node index
relabelNode :: LG.LNode a -> LG.LNode String
relabelNode (index, _) = (index, "HTU"<> (show index))



{- | isEdgePairPermissible takes a graph and two edges, coeval contraints, and tests whether a
pair of edges can be linked by a new edge and satify three consitions:
   1) neither edge is a network edge
   2) one edge cannot be "before" while the other is "after" in any of the constraint pairs
   3) neither edge is an ancestor or descendent edge of the other (tested via bv of nodes)
the result should apply to a new edge in either direction
new edge to be creted is edge1 -> ege2
Could change to LG.isPhylogeneticGraph
-}
isEdgePairPermissible
    ∷ (Eq a, Eq b, Show a) ⇒ LG.Gr a b
    → [(LG.LNode a, LG.LNode a, [LG.LNode a], [LG.LNode a], [LG.LNode a], [LG.LNode a])]
    → (LG.LEdge b, LG.LEdge b)
    → Bool
isEdgePairPermissible inGraph constraintList (edge1@(u, v, _), edge2@(u', v', _)) =
    if LG.isEmpty inGraph
        then error "Empty input graph in isEdgePairPermissible"
        else
            if u == u'
                then False
                else
                    if v == v'
                        then False
                        else -- equality implied in above two
                        -- else if LG.toEdge edge1 == LG.toEdge edge2 then False

                            if (LG.isNetworkNode inGraph u) || (LG.isNetworkNode inGraph u')
                                then False
                                else
                                    if (LG.isNetworkLabEdge inGraph edge1) || (LG.isNetworkLabEdge inGraph edge2)
                                        then False
                                        else
                                            if not (LG.meetsAllCoevalConstraintsNodes (fmap removeNodeLabels constraintList) edge1 edge2)
                                                then False
                                                else
                                                    -- need to make sure source edge not ancestor to target
                                                    let (ancestralNodes, ancestralEdges) = LG.nodesAndEdgesBefore inGraph [(u', fromJust $ LG.lab inGraph u')]
                                                        (ancestralNodesR, ancestralEdgesR) = LG.nodesAndEdgesBefore inGraph [(u, fromJust $ LG.lab inGraph u)]
                                                        isAncestor = edge1 `elem` ancestralEdges
                                                        isAncestor' = u `elem` (fmap fst $ ancestralNodes)
                                                        isAncestorR = edge2 `elem` ancestralEdgesR
                                                        isAncestorR' = u' `elem` (fmap fst $ ancestralNodesR)
                                                    in
                                                    --trace ("iEP: " <> (show (u,fmap fst ancestralNodes)) <> "->" <> (show isAncestor') <> " " <> (show (LG.toEdge edge1, LG.toEdge  edge2, fmap LG.toEdge ancestralEdges)) <> "->" <> (show isAncestor) ) $ 
                                                    -- if LG.parentsInChain inGraph
                                                    -- if (isAncDescEdge inGraph edge1 edge2)
                                                    if isAncestor' || isAncestorR' || isAncestor || isAncestorR then False
                                                    else -- get children of u' to make sure no net children

                                                            if (not . null) $ filter (== True) $ fmap (LG.isNetworkNode inGraph) $ LG.descendants inGraph u'
                                                                then False
                                                                else True
    where
        {-
        removeNodeLabels :: forall {f1 :: * -> *} {f2 :: * -> *}
                                   {f3 :: * -> *} {f4 :: * -> *} {a1} {a2} {a3} {a4} {a5} {a6}.
                            (Functor f1, Functor f2, Functor f3, Functor f4) =>
                            (LG.LNode a1, LG.LNode a2, f1 (LG.LNode a3), f2 (LG.LNode a4),
                             f3 (LG.LNode a5), f4 (LG.LNode a6))
                            -> (LG.Node, LG.Node, f1 LG.Node, f2 LG.Node, f3 LG.Node,
                                f4 LG.Node)
        -}
        removeNodeLabels (a, b, c, d, e, f) = (LG.toNode a, LG.toNode b, fmap LG.toNode c, fmap LG.toNode d, fmap LG.toNode e, fmap LG.toNode f)

{- | getPermissibleEdgePairs takes a DecoratedGraph and returns the list of all pairs
of edges that can be joined by a network edge and meet all necessary conditions
-}
-- add in other conditions
--   reproducable--ie not tree node with two net node children--other stuff
getPermissibleEdgePairs ∷ (Eq a, Eq b, Show a) ⇒ LG.Gr a b → [(LG.LEdge b, LG.LEdge b)]
getPermissibleEdgePairs inGraph =
    if LG.isEmpty inGraph
        then error "Empty input graph in isEdgePairPermissible"
        else
            let rootIndex = fst $ head $ LG.getRoots inGraph
                edgeList' = LG.labEdges inGraph

                edgeList = filter ((/= rootIndex) . fst3) edgeList'


                -- edges to potentially conenct
                edgePairs = cartProd edgeList edgeList

                -- get coeval node pairs in existing grap
                coevalNodeConstraintList = LG.coevalNodePairs inGraph

                coevalNodeConstraintList' = fmap (LG.addBeforeAfterToPair inGraph) coevalNodeConstraintList
                
                edgeTestList = fmap (isEdgePairPermissible inGraph coevalNodeConstraintList') edgePairs
                    
                pairList = fmap fst $ filter ((== True) . snd) $ zip edgePairs edgeTestList

                in
                if null edgeTestList then []
                else pairList


-- | addRandNetworkNodes randGen3 randTreeFGL' randGen2
-- edges (a,b) and (u,v) yeild two new nodes indexed beyond what exist (i) -> (i, i+1)
-- input edges are deleted (a,b) and (u,v)
-- new edges created as new edge directed from (a,b) to (u,v)
-- new edges (a,i) (i,b), (i, i+1), (u,i+1), (i+1,v)
addRandNetworkNodes ::  (Eq a, Eq b, Show a) => StdGen -> LG.Gr a b ->  Int -> Int -> (StdGen, LG.Gr a b)
addRandNetworkNodes randGen inputGraph networkNodeNumber counter =
    if LG.isEmpty inputGraph then (randGen, inputGraph)
    else if networkNodeNumber == 0 then (randGen, inputGraph)
    else if counter == networkNodeNumber then (randGen, inputGraph)
    else 
        let edgePairs = getPermissibleEdgePairs inputGraph
            (newRandGen, randEdgePair) = chooseRandomElement randGen edgePairs
            ((aE,bE,_), (uE,vE, _)) = randEdgePair

            --add two new  nodes (one network), deleting two edgses and creating 5 new edges, and making new graph
            inNodes = LG.labNodes inputGraph
            inEdges = LG.labEdges inputGraph

            -- thes relabelled later or will screw up types
            newI = (length inNodes, snd $ head inNodes)
            newI1 = (1 + length inNodes, snd $ head inNodes)

            dummyEdgeLabel = thd3 $ head inEdges

            newEdgeList = [(aE, fst newI, dummyEdgeLabel),(fst newI, bE, dummyEdgeLabel),(fst newI, fst newI1, dummyEdgeLabel),(uE, fst newI1, dummyEdgeLabel),(fst newI1, vE, dummyEdgeLabel)]

            newGraph = LG.insEdges newEdgeList $ LG.insNodes [newI, newI1] $ LG.delLEdges [fst randEdgePair, snd randEdgePair] inputGraph
        in
        if null edgePairs then (randGen, LG.empty)
        else 
            --trace ("ARNE: " <> (show ((aE,bE),(uE,vE))) <> " from " <> (show $ fmap LG.toEdge inEdges)) $ 
            addRandNetworkNodes newRandGen newGraph networkNodeNumber (counter + 1)

-- | Main function for conversion
main :: IO ()
main = 
  do 
     --get input command filename, ouputs to stdout
    args <- getArgs
    if (length args < 4) 
      then errorWithoutStackTrace "Require five (or six) arguments: graphType (tree or network), number of leaves in tree (Integer), tree distribution (Uniform/Yule), output format (GraphViz/Newick), branch length distribution (None, Exponential, Uniform, Constant), and branch length parameter (Float > 0.0)"
      else hPutStrLn stderr "Inputs: "
    mapM_ (hPutStrLn stderr) args
    hPutStrLn stderr ""
    
    let (graphTypeString, otherArgs) = fromJust $ uncons args
    let numLeavesString = T.unpack $ T.toLower $ T.pack $ fst $ fromJust $ uncons otherArgs
    let distributionText = T.toLower $ T.pack $ (snd $ fromJust $ uncons otherArgs) !! 0
    let outputFormatText = T.toLower $ T.pack $ (snd $ fromJust $ uncons otherArgs) !! 1
    let branchDistText = T.toLower $ T.pack $ (snd $ fromJust $ uncons otherArgs) !! 2
    let branchParamString = T.unpack $ T.toLower $ T.pack $ last $ (snd $ fromJust $ uncons otherArgs)

    let numLeavesMaybe = (readMaybe numLeavesString) :: Maybe Int
    let branchParamMaybe = (readMaybe branchParamString) :: Maybe Double

    let graphTypeText = T.toLower $ T.pack graphTypeString


    let graphType = if T.head graphTypeText == 't' then Tree
                    else if T.head graphTypeText == 'n' then Network
                    else errorWithoutStackTrace ("First argument needs to be an 'Tree' or 'Network' : " <> (T.unpack graphTypeText))


    let networkNodeNumber = if graphType == Tree then 0
                                  else
                                      let paramPart = dropWhile (/= ':') graphTypeString
                                          netNodeString = drop 1 paramPart
                                          netNodeNumberMaybe = (readMaybe netNodeString) :: Maybe Int
                                      in
                                      if null paramPart || null netNodeString || isNothing netNodeNumberMaybe then
                                            errorWithoutStackTrace ("'Network' must be followed by a network node number (:Integer) ie 'network:2' " <> (T.unpack graphTypeText))
                                      else fromJust netNodeNumberMaybe


    let numLeaves = if isJust numLeavesMaybe then fromJust numLeavesMaybe
                    else errorWithoutStackTrace ("Second argument needs to be an integer (e.g. 10): " <> numLeavesString)

    let distribution = if T.head distributionText == 'u' then Uniform
                       else if T.head distributionText == 'y' then Yule
                       else errorWithoutStackTrace ("Third argument needs to be 'Uniform' or 'Yule': " <> (args !! 1))

    let outputFormat = if T.head outputFormatText == 'g' then GraphViz
                       else if T.head outputFormatText == 'n' then Newick
                       else errorWithoutStackTrace ("Fourth argument needs to be 'Graphviz' or 'Newick': " <> last args)

    let (branchDistribution, branchParam) = if T.head branchDistText == 'n' then (None, 0.0)
                                            else 
                                                if isNothing branchParamMaybe then errorWithoutStackTrace ("Need a branch length parameter (float)--perhaps missing: " <> branchParamString)
                                                else if (not (fromJust branchParamMaybe > 0.0)) then errorWithoutStackTrace ("Branch length parameter (float) must be > 0.0: " <> branchParamString)
                                                else 
                                                    if T.head branchDistText == 'e' then (Exponential, fromJust branchParamMaybe)
                                                    else if T.head branchDistText == 'u' then (UniformD, fromJust branchParamMaybe)
                                                    else (Constant, fromJust branchParamMaybe)

   
    hPutStrLn stderr $ "Creating random "<> (show graphType) <> " with " <> (show networkNodeNumber) <> " network nodes and " <> (show numLeaves) <> " leaves via a " <> (show distribution) <> " distribution" 
    if branchDistribution /= None then 
        hPutStrLn stderr $ "\tBranch/edge lengths/weights drawn from " <> (show branchDistribution)  <> " with parameter " <> (show branchParam)
    else 
        hPutStrLn stderr $ "\tWithout branch/edge lengths/weights"

    if branchDistribution == Exponential then
        hPutStrLn stderr $ "\tMean edge  :" <> (show $ 1.0 / branchParam)
    else if branchDistribution == UniformD then
        hPutStrLn stderr $ "\tMean edge rate :" <> (show (branchParam/2.0))
    else if branchDistribution == Constant then
        hPutStrLn stderr $ "\tAll edge rates :" <> (show branchParam)
    else hPutStrLn stderr $ "\tEdges without rates"


    -- create leaf labels for tree
    let leafLabelList = drop 3 $ fmap ('T' :) $ fmap show $ [0.. numLeaves - 1] 

    -- create initial 3 taxon tree
    let firstThreeNodes = [(0, "T0"), (1, "T1"),(2, "T2"), (numLeaves, "HTU" <> (show numLeaves)), (numLeaves + 1, "HTU" <> (show $ numLeaves + 1))]
    let firstThreeEdges = [(numLeaves, 0, "Edge" <> (show 0)), (numLeaves, numLeaves + 1, "Edge" <> (show $ numLeaves + 1)), (numLeaves + 1, 1, "Edge" <> (show 1)), (numLeaves + 1, 2, "Edge" <> (show 2))]
    let firstThreeGraph = LG.mkGraph firstThreeNodes firstThreeEdges
   
    -- random initialization
    randomGen <- initStdGen
    
    -- generatge random tree in fgl
    let (newRandGen, randTreeFGL') = genRandTreeFGL randomGen numLeaves (2 :: Int) leafLabelList distribution firstThreeGraph

    let (randGen2, randGen3) = splitGen newRandGen 


    let randTreeFGL = if graphType == Tree then randTreeFGL'
                      else snd $ addRandNetworkNodes randGen2  randTreeFGL' networkNodeNumber 0

    if LG.isEmpty randTreeFGL then 
        hPutStrLn stderr "Warning--Could not create a valid phylogenetic network"
    else hPutStrLn stderr ""


    let branchLengthsUniform =  randomRs (0.0, branchParam) randGen3

    let branchLengthsExp =  fmap (uniform2Exponential branchParam) branchLengthsUniform

    let newBranchLengths =  if branchDistribution == Exponential then 
                                branchLengthsExp
                            else if branchDistribution == UniformD then 
                                branchLengthsUniform
                            else repeat branchParam


    -- relabel nodes and edge weights if needed
    let (fglRoots, fglLeaves, fglTree, fglNet) = LG.splitVertexList randTreeFGL
    let relabeledHTUs = fmap relabelNode (fglRoots <> fglTree <> fglNet)

    let fglNodes = LG.labNodes randTreeFGL
    let fglEdges = LG.labEdges randTreeFGL

    let fglNodes' = fglLeaves <> relabeledHTUs

    let newEdges = if branchDistribution == None then fglEdges
                   else relabelEdges $ zip newBranchLengths fglEdges

    let relabelledGraph = LG.mkGraph fglNodes' newEdges

    -- output trees in formats (newick, dot)
    -- dot format
    -- relabelling edges if required
    let outGraphStringDot = if branchDistribution == None then 
                                removeLabel00 $ GFU.fgl2DotString relabelledGraph
                            else GFU.fgl2DotString relabelledGraph

    let outGraphStringDot' = changeDotPreamble "digraph {" "digraph G {\n\trankdir = LR;\tedge [colorscheme=spectral11];\tnode [shape = none];\n" outGraphStringDot

    -- newick format
    --let graphTD = GFU.stringGraph2TextGraphDouble relabelledGraph
    let outGraphStringNewick = if branchDistribution == None then 
                                    T.unpack $ GFU.fgl2FEN False True (GFU.stringGraph2TextGraphDouble relabelledGraph)
                                else 
                                    T.unpack $ GFU.fgl2FEN True True (GFU.stringGraph2TextGraphDouble relabelledGraph)

    if outputFormat == GraphViz then hPutStrLn stdout outGraphStringDot'
    else hPutStrLn stdout outGraphStringNewick


    -- output displaytrees if network--True for contact edges/nodes
    if graphType /= Network then  
        hPutStrLn stdout ""
    else do
        let displayTreeList = LG.generateDisplayTrees True relabelledGraph
        let outGraphStringDotList = if branchDistribution == None then 
                                        fmap (changeDotPreamble "digraph {" "digraph G {\n\trankdir = LR;\tedge [colorscheme=spectral11];\tnode [shape = none];\n") $ fmap removeLabel00 $ fmap GFU.fgl2DotString displayTreeList
                                    else fmap (changeDotPreamble "digraph {" "digraph G {\n\trankdir = LR;\tedge [colorscheme=spectral11];\tnode [shape = none];\n") $ fmap GFU.fgl2DotString displayTreeList

        let outGraphStringNewickList =  if branchDistribution == None then 
                                            fmap T.unpack $ fmap (GFU.fgl2FEN False False) $ fmap GFU.stringGraph2TextGraphDouble displayTreeList
                                        else 
                                            fmap T.unpack $ fmap (GFU.fgl2FEN True False) $ fmap GFU.stringGraph2TextGraphDouble displayTreeList

        hPutStrLn stdout "\nDisplay trees:\n"
        if outputFormat == GraphViz then 
           hPutStrLn stdout $ concat $ fmap (<> "\n") outGraphStringDotList
        else 
           hPutStrLn stdout $ concat $ fmap (<> "\n") outGraphStringNewickList
    
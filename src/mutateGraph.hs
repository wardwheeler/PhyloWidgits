{- |
Module      :  mutateGraph
Description :  Takes a newick or Graphviz formatted graph and mutates within NNI, SPR, or TBR neighborhood.
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
import Data.Char
import Data.List
import Data.List qualified as L
import Data.Maybe
import Data.Text.Lazy qualified as T
import Debug.Trace
import GeneralUtilities
import GraphFormatUtilities qualified as GFU
import LocalGraph qualified as LG
import System.Random
import Text.Read

-- | output as Graphviz/Dot or newick
data OutputFormat = GraphViz | Newick 
    deriving stock (Show, Eq)

-- | mutation neighborhood
data Neighborhood = NNI | SPR | TBR
   deriving stock (Show, Eq)

-- | maxTries for relooping is random not a good choice-this limts so no unending loops
maxTries :: Int
maxTries = 1000


-- | 'trim' trim removes leading and trailing white space
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

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
-- | chooseRandomEdge selects man edge at random from list
chooseRandomEdge :: StdGen -> [LG.LEdge b] -> (StdGen, LG.LEdge b)
chooseRandomEdge inGen edgesAvailable =
   if null edgesAvailable then error "Null edge list to choose"
   else 
      let (index, newGen) = randomR  (0, (length edgesAvailable) - 1) inGen
      in
      (newGen, edgesAvailable !! index)

-- | findEdge get labelled edges from graph and retue=rns LEdge with input indices
findEdge :: Show b => LG.Gr a b -> Int -> Int -> LG.LEdge b
findEdge inGraph e v =
   if LG.isEmpty inGraph then error ("No edge in graph with indices " <> (show (e, v)))
   else 
      let edgeList = LG.labEdges inGraph
      in
      getEdge e v edgeList

-- | getEdge checks first 2 elements to match and return triple
getEdge :: Show b => Int -> Int -> [(Int, Int, b)] -> (Int, Int, b)
getEdge e v edgeList =
   if null edgeList then error ("Indices not found in edge list: " <> (show (e,v,edgeList)))
   else 
      let (inE, inV, _) = head edgeList
      in
      if (inE == e) && (inV == v) then  head edgeList
      else getEdge e v (drop 1 edgeList)

-- | mutateGraphFGL generates a random gfl tree with leaf label list and distribution
--    1) edge (e,v) removed uniformly at random (other that one that leads to output)
--    2) edges with e are contracted
--    3) if NNI or SPR new edge added to v from edge in graph partition that contained e
--    4) if TBR then new edge between edges in both partitions (contracting and redirecting edges)
--    5) recurse tiulle nu,nber of mutations accomplished

mutateGraphFGL :: StdGen -> Int ->  Int -> Int -> LG.LEdge Double -> Int -> Neighborhood -> LG.Gr String Double -> (StdGen, LG.Gr String Double)
mutateGraphFGL inGen mutationCounter maxMutations rootIndex outgroupEdge maxTriesLocal neighborhood inGraph =
   if mutationCounter >= maxMutations then (inGen, inGraph)
   else if maxTriesLocal >= maxTries then errorWithoutStackTrace ("Maximum number of randomization tries to mutate graph exceeded: " <> (show (maxTriesLocal >= maxTries)))
   else if LG.isEmpty inGraph then error "Empty graph in mutateGraphFGL"
   else 
      let edgeList = LG.labEdges inGraph 
          -- this uses root index is --should be 0 can't use edges as constrant since change with mutation
          nonOutgroupEgdeList = filter ((/=rootIndex) . fst3) edgeList 

          -- only delete edges that are not connceted to root
          (newGen, edgeToDelete) = chooseRandomEdge inGen nonOutgroupEgdeList

          (splitGraph, baseGraphRoot, prunedGraphRoot, originalConnectionRoot, newEdge, deletedEdgeList) = LG.splitGraphOnEdge' inGraph edgeToDelete

          -- For SPR/TBR can't add back to outgroup edge or original split edge (newEdge) but others are OK.
          edgeListBaseGraph = LG.getEdgeListAfter (splitGraph, baseGraphRoot)
          edgesToRejoin = if neighborhood == NNI then take 2 $ LG.getEdgeListAfter (splitGraph, (snd3 newEdge))
                          -- else if neighborhood == SPR then filter ((/=rootIndex) . fst3) edgeListBaseGraph
                          else edgeListBaseGraph L.\\ [outgroupEdge, newEdge]
                          -- else errorWithoutStackTrace ("TBR neighborhood not yet implemented")

      in
      --this in case some pruning has nothting to rejoin--in NNI I believe--this could cause a loop
      if null edgesToRejoin then mutateGraphFGL newGen mutationCounter maxMutations rootIndex outgroupEdge (maxTriesLocal + 1) neighborhood inGraph 
      else
          let (newGen2, additionPointEdge@(e,v,_)) = chooseRandomEdge newGen edgesToRejoin

              (newGen3, newGraph) = if neighborhood `elem` [NNI, SPR] then 
                                       let newEdgeList = [(e, originalConnectionRoot, 0.0),(originalConnectionRoot, v, 0.0)]
                                       in 
                                       (newGen2, LG.insEdges newEdgeList $ LG.delLEdge additionPointEdge splitGraph)
                                     
                                    else -- TBR-- need additional randomized choice and reroot functions
                                       -- get edge list of pruned comnponent and filter out original root edges of pruned component
                                       let punedEdgeList = filter ((/=prunedGraphRoot) . fst3) $ LG.getEdgeListAfter (splitGraph, prunedGraphRoot)
                                           (newGen3, edgeToReroot) = chooseRandomEdge newGen2 punedEdgeList
                                           newTBRGraph = makeTBRNewGraph splitGraph prunedGraphRoot originalConnectionRoot (additionPointEdge, edgeToReroot)

                                       in
                                       -- first case for splits that have no rerooting options (basically SPR only)
                                       if null punedEdgeList then
                                          let newEdgeList = [(e, originalConnectionRoot, 0.0),(originalConnectionRoot, v, 0.0)]
                                          in 
                                          (newGen2, LG.insEdges newEdgeList $ LG.delLEdge additionPointEdge splitGraph)
                                       else 
                                          (newGen3, newTBRGraph)


          in
          -- recurse for next mutation
          mutateGraphFGL newGen3 (mutationCounter + 1) maxMutations rootIndex outgroupEdge 0 neighborhood newGraph

{-TBR stuff from PhyG -}

{- | makeTBRNewGraph takes split graph, rerooted edge and readded edge making new complete graph for rediagnosis etc
-}
makeTBRNewGraph :: Eq a => LG.Gr a Double -> LG.Node -> LG.Node -> (LG.LEdge Double, LG.LEdge Double) -> LG.Gr a Double
makeTBRNewGraph splitGraph prunedGraphRootIndex originalConnectionOfPruned (targetEdge@(u, v, _), rerootEdge) =

    -- pruned graph rerooted edges
    let (prunedEdgesToAdd, prunedEdgesToDelete) = getTBREdgeEditsDec splitGraph prunedGraphRootIndex rerootEdge

    -- create new edges readdition edges
        newEdgeList =
                    [ (u, originalConnectionOfPruned, 0.0)
                    , (originalConnectionOfPruned, v, 0.0)
                    --, (originalConnectionOfPruned, prunedGraphRootIndex, 0.0)
                    ]
        tbrNewGraph =
            LG.insEdges (newEdgeList <> prunedEdgesToAdd) $
                LG.delEdges ((u, v) : prunedEdgesToDelete) splitGraph
    in 
    tbrNewGraph

{- | getTBREdgeEditsDec takes an edge and returns the list of edit to pruned subgraph
as a pair of edges to add and those to delete
since reroot edge is directed (e,v), edges away from v will have correct
orientation. Edges between 'e' and the root will have to be flipped
original root edges and reroort edge are deleted and new root and edge spanning orginal root created
delete original connection edge and creates a new one--like SPR
returns ([add], [delete])
-}
getTBREdgeEditsDec ∷ Eq a => LG.Gr a Double → LG.Node → LG.LEdge Double → ([LG.LEdge Double], [LG.Edge])
getTBREdgeEditsDec inGraph prunedGraphRootIndex rerootEdge =
    -- trace ("Getting TBR Edits for " <> (show rerootEdge)) (
    let -- originalRootEdgeNodes = LG.descendants inGraph prunedGraphRootIndex
        originalRootEdges = LG.out inGraph prunedGraphRootIndex

        -- get path from new root edge fst vertex to orginal root and flip those edges
        -- since (u,v) is u -> v u "closer" to root
        closerToPrunedRootEdgeNode = (fst3 rerootEdge, fromJust $ LG.lab inGraph $ fst3 rerootEdge)
        (nodesInPath, edgesinPath) =
            LG.postOrderPathToNode inGraph closerToPrunedRootEdgeNode (prunedGraphRootIndex, fromJust $ LG.lab inGraph prunedGraphRootIndex)

        -- don't want original root edges to be flipped since later deleted
        edgesToFlip = edgesinPath L.\\ originalRootEdges
        flippedEdges = fmap LG.flipLEdge edgesToFlip

        -- new edges on new root position and spanning old root
        -- add in closer vertex to root to make sure direction of edge is correct
        newEdgeOnOldRoot =
            if (snd3 $ head originalRootEdges) `elem` ((fst3 rerootEdge) : (fmap fst nodesInPath))
                then (snd3 $ head originalRootEdges, snd3 $ last originalRootEdges, 0.0)
                else (snd3 $ last originalRootEdges, snd3 $ head originalRootEdges, 0.0)
        newRootEdges = [(prunedGraphRootIndex, fst3 rerootEdge, 0.0), (prunedGraphRootIndex, snd3 rerootEdge, 0.0)]
    in  -- assumes we are not checking original root
        -- rerooted
        -- delete orignal root edges and rerootEdge
        -- add new root edges
        -- and new edge on old root--but need orientation
        -- flip edges from new root to old (delete and add list)
       
        (newEdgeOnOldRoot : (flippedEdges <> newRootEdges), LG.toEdge rerootEdge : (fmap LG.toEdge (edgesToFlip <> originalRootEdges)))


-- | Main function
main :: IO ()
main = 
  do 
     --get input command filename, ouputs to stdout
    args <- getArgs
    if (length args /= 4) 
      then errorWithoutStackTrace "Require four arguments: graph file name (String), number mutations (Integer), mutation neighborhood (NNI/SPR/TBR), and output format (GraphViz/Newick)"
      else hPutStrLn stderr "Inputs: "
    mapM_ (hPutStrLn stderr) $ fmap ('\t' :) args
    hPutStrLn stderr ""
    
    let (infileName, otherArgs) = fromJust $ uncons args

    let numberMutationsMaybe = readMaybe (args !! 1) :: Maybe Int

    let neighborhoodText = T.toLower $ T.pack $ args !! 2

    let outputFormatText = T.toLower $ T.pack $ (args !! 3)

    let numberMutations = if isJust numberMutationsMaybe then fromJust numberMutationsMaybe
                          else errorWithoutStackTrace ("Second argument needs to be an integer (e.g. 10): " <> (args !! 1))

    let mutationNeighborhood = if T.head neighborhoodText == 'n' then NNI
                               else if T.head neighborhoodText == 's' then SPR
                               else if T.head neighborhoodText == 't' then TBR 
                               else errorWithoutStackTrace ("Third argument needs to be 'NNI', 'SPR', or 'TBR': " <> (args !! 2))

    let outputFormat = if T.head outputFormatText == 'g' then GraphViz
                       else if T.head outputFormatText == 'n' then Newick
                       else errorWithoutStackTrace ("Fourth argument needs to be 'Graphviz' or 'Newick': " <> (args !! 3))



    hPutStrLn stderr $ "Mutating gaph with " <> (show numberMutations) <> " mutations in " <> (show mutationNeighborhood) <> " edit neighborhood"
    hPutStrLn stderr "assumes graph has a single outgroup leaf, ie that one of the two children of the root vertex is a leaf"


    -- read input graph
    graphFileHandle <- openFile infileName ReadMode
    graphContents' <- hGetContents' graphFileHandle
    let graphContents = trim graphContents'

    if null graphContents then errorWithoutStackTrace "\tEmpty graph input"
    else hPutStrLn stderr "Successfully read input graph"

    let firstChar = head graphContents

    -- read iput graph trying enewick and dot returning dot/graphviz format
    -- a bit stupid, but has to do with reusing some graphviz functions
    inputGraph <- if firstChar == '(' then 
                     -- enewick
                     pure $ head $ GFU.forestEnhancedNewickStringList2FGLList (T.pack graphContents)  
                      
                 else if (toLower firstChar == '/') || (toLower firstChar == 'd') || (toLower firstChar == 'g') then do
                     -- gaphviz/dot
                     newGraphFileHandle <- openFile infileName ReadMode
                     dotGraph <- LG.hGetDotLocal newGraphFileHandle
                     hClose newGraphFileHandle
                     pure $ GFU.relabelFGL $ LG.dotToGraph dotGraph
                      
                 else errorWithoutStackTrace ("Input graph file does not appear to be enewick or dot/graphviz")

    -- random initialization
    randomGen <- initStdGen
    
    -- find outgroup edge
    let rootsList = LG.getRoots inputGraph

    if (length rootsList) /= 1 then errorWithoutStackTrace ("Graph must have a single root: " <> (show rootsList))
    else hPutStr stderr "" -- Input graph has a single root"

    let rootEdges = LG.out inputGraph (fst $ head rootsList)
    if (length rootEdges) /= 2 then errorWithoutStackTrace ("Graph root must have two childern: " <> (show rootEdges))
    else hPutStr stderr "" -- Input graph root has two children"

    let rootIndex = fst $ head rootsList
    
    -- get outgroup edge so no rejoining og graphs on thet edge to mainting root position
    let outgroupIndex = if LG.isLeaf inputGraph (snd3 $ head rootEdges) then (snd3 $ head rootEdges)
                        else if LG.isLeaf inputGraph (snd3 $ last rootEdges) then (snd3 $ last rootEdges)
                        else errorWithoutStackTrace ("Graph root must have only one child that is a leaf: " <> (show $ fmap snd3 rootEdges))

    let outgroupEdge = findEdge inputGraph rootIndex outgroupIndex


    --hPutStrLn stderr ("Outgroup edge is " <> (show outgroupEdge))
   
    --hPutStrLn stderr ("Root index: " <> (show rootIndex))
    
    
    -- generatge mutated tree in fgl
    let mutantGraphFGL = snd $ mutateGraphFGL randomGen 0 numberMutations rootIndex outgroupEdge 0 mutationNeighborhood (GFU.textGraph2StringGraph inputGraph)

    -- output trees in formats (newick, dot)
    -- dot format
    let outGraphStringDot = removeLabel00 $ GFU.fgl2DotString mutantGraphFGL
    let outGraphStringDot' = changeDotPreamble "digraph {" "digraph G {\n\trankdir = LR;\tedge [colorscheme=spectral11];\tnode [shape = none];\n" outGraphStringDot

    -- newick format
    --let graphTD = GFU.stringGraph2TextGraphDouble mutantGraphFGL
    let outGraphStringNewick = T.unpack $ GFU.fgl2FEN False False (GFU.stringGraph2TextGraph mutantGraphFGL)

    if outputFormat == GraphViz then hPutStrLn stdout outGraphStringDot'
    else hPutStrLn stdout outGraphStringNewick
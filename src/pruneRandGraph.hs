{- |
Module      :  pruneRandGraph
Description :  prunes graph edges and vertices with 'rand' data key (or other)
Copyright   :  (c) 2024 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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

{- 

1) Need to add "HTU" string option

-}
module Main where

import System.IO
import System.Process
import System.Environment
import Data.List
import qualified Data.List.Split as LS
import Data.Char
import Data.Maybe
import Data.Text.Lazy qualified as T
import GeneralUtilities
import GraphFormatUtilities qualified as GFU
import LocalGraph qualified as LG
import System.Directory
import System.Info
import Text.Read (readMaybe)

import Debug.Trace

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

-- printGraph graphviz simple dot file of graph
-- execute with "dot -Teps test.dot -o test.eps"
-- need to add output to argument filename and call
-- graphviz via System.Process.runprocess
-- also, reorder GenForest so smalles (num leaves) is either first or
-- last so can print small to large all the way so easier to read
-- eps on OSX because ps gets cutt off for some reason and no pdf onOSX
-- -O foir multiple graphs I htink
printGraphVizDot ∷ String → String → IO ()
printGraphVizDot graphDotString dotFile =
    if null graphDotString
        then do error "No graph to report"
        else do
            myHandle ← openFile dotFile WriteMode
            if os /= "darwin"
                then do hPutStrLn stderr ("\tOutputting graphviz to " <> dotFile <> ".pdf.\n")
                else do hPutStrLn stderr ("\tOutputting graphviz to " <> dotFile <> ".eps.\n")
            let outputType =
                    if os == "darwin"
                        then "-Teps"
                        else "-Tpdf"
            -- hPutStrLn myHandle "digraph G {"
            -- hPutStrLn myHandle "\trankdir = LR;"
            -- hPutStrLn myHandle "\tnode [ shape = rect];"
            -- hPutStr myHandle $ (unlines . tail . lines) graphDotString
            hPutStr myHandle graphDotString
            -- hPutStrLn myHandle "}"
            hClose myHandle
            pCode ← findExecutable "dot" -- system "dot" --check for Graphviz
            {-
            hPutStrLn stderr
                (if isJust pCode then --pCode /= Nothing then
                    "executed dot " <> outputType <> dotFile <> " -O " else
                    "Graphviz call failed (not installed or found).  Dot file still created. Dot can be obtained from https://graphviz.org/download")
            -}
            if isJust pCode
                then do
                    createProcess (proc "dot" [outputType, dotFile, "-O"])
                    hPutStrLn stderr ("\tExecuted dot " <> outputType <> " " <> dotFile <> " -O \n")
                else
                    hPutStrLn stderr "\tGraphviz call failed (not installed or found).  Dot file still created. Dot can be obtained from https://graphviz.org/download\n"

-- | getSplitLeaves takes a graph and and an edge an returns the leaves on each side of the edge
-- directed u -> v
getSplitLeaves :: (Eq a, Eq b, Show a, Show b) => LG.Gr a b -> [LNode a] -> LEdge b -> ([LNode a], [LNode a])
getSplitLeaves inGraph nodeList inEdge@(u,v,_)=
    if LG.isEmpty inGraph then error "Empty graph in getSplitLeaves"
    else 
        let vNode = (v, fromJust $ LG.lab inGraph v)
            vSide = vNode : (fst $ LG.nodesAndEdgesAfter inGraph [vNode])
            uSide = nodeList \\ vSide
            uSideLeafList = filter (LG.isLeafLab inGraph) uSide
            vSideLeafList = filter (LG.isLeafLab inGraph) vSide
        in 
        --trace ("GSL: " <> (show (length uSideLeafList, length vSideLeafList)) <> "\n" <> (show uSideLeafList) <> "\n" <> (show vSideLeafList) <> "\n") $ 
        (uSideLeafList, vSideLeafList)

-- | oneOrOther takes a keyWord (as Text) asn sees if that key word is in
-- one or other leaf leabels but not neither or both
oneOrOther :: T.Text -> ([LNode T.Text],[LNode T.Text]) -> Bool
oneOrOther keyWord nodeListPair@(uList, vList) = 
    if null uList && (not . null) vList then trace ("empty u") $ False
    else if null vList && (not . null) uList then trace ("empty v") $ False
    else 
        let uNameList = fmap snd uList
            vNameList = fmap snd vList
            uNameWithKeyword = filter (T.isInfixOf keyWord) uNameList
            vNameWithKeyword = filter (T.isInfixOf keyWord) vNameList
            uAllKey = length uNameList == length uNameWithKeyword
            vAllKey = length vNameList == length vNameWithKeyword
        in
        --trace ("OOO: " <> (show (uAllKey, vAllKey))) $
        if uAllKey && not vAllKey then True
        else if vAllKey && not uAllKey then True
        else False


-- | removeKeyEdges removes edges with one side haveing kwey word in terminal name and other not
-- also removes teh "sister edge" to a delted edge--except if there is only a single edge to be deleted--that
-- is the case where the keyword data are a single clade--has to go somewhere
-- relies on some factors--liek HTU in label for internal nodes 
-- returns graph and number of "extra prunings" (not including suister prunings or others) and
-- number "keyWord" contnaining terminals, which could be max number prunings (not really...)
removeKeyEdges :: T.Text -> LG.Gr T.Text Double -> (LG.Gr T.Text Double, Int, Int, Int)
removeKeyEdges keyWord inGraph = 
    if T.null keyWord || LG.isEmpty inGraph then error "Empty keyWord or graph"
    else
        -- for each edge determine of terminal labels with keyWork are on either side of split.
        -- remove thos edges where on onseid OR other, but not both
        let edgeList = LG.labEdges inGraph
            nodeList = LG.labNodes inGraph
            nonKeyNodeList = filter (not . (T.isInfixOf keyWord) .snd) $ filter (LG.isLeafLab inGraph) nodeList
            keyNodeNumber = length $ filter ((T.isInfixOf keyWord) .snd) $ filter (LG.isLeafLab inGraph) nodeList
            splitsList = fmap (getSplitLeaves inGraph nodeList) edgeList
            edgeSplitPairList = zip edgeList splitsList 

            -- edges to prune based on all splits are keyword 
            toBePrunedEdgeList = fmap fst $ filter ((oneOrOther keyWord) . snd) edgeSplitPairList

            -- sister edges to those deleted (u->v)
            prunedVNodeofEdgeList = zip (fmap snd3 toBePrunedEdgeList) (fmap (fromJust . (LG.lab inGraph)) $ fmap snd3 toBePrunedEdgeList)
            sisterToVNodeList = fmap fst $ fmap head $ fmap (LG.sisterLabNodes inGraph) prunedVNodeofEdgeList
            sisterEdgeList = zip (fmap fst3 toBePrunedEdgeList) sisterToVNodeList

            -- is only 1 edge was to be deleted--then keywaor taxa are a clade and can be simpley snipped off
            -- otherwsie delete the both sets of of edges
            allEdgesToDelete = if length toBePrunedEdgeList == ((2 * keyNodeNumber) - 1) then (fmap LG.toEdge toBePrunedEdgeList)
                               else (fmap LG.toEdge toBePrunedEdgeList) <> sisterEdgeList

            edgePrunedGraph = LG.delEdges allEdgesToDelete inGraph


            -- recursively remove terminal HTUS
            secondEdgePrunedGraph = removeTerminalHTUs edgePrunedGraph

            -- get isolated nodes
            isolatedNodeList = (LG.getIsolatedNodes secondEdgePrunedGraph) \\ nonKeyNodeList

            contractedPrunedGraph = contractIn1Out1Edges $ LG.delNodes (fmap fst isolatedNodeList) secondEdgePrunedGraph

            numberComponents = filter (LG.isRoot contractedPrunedGraph) (LG.nodes contractedPrunedGraph)

        in
        --trace ("RKE: " <> (show (length hTULeafNodes, fmap snd hTULeafNodes)))
        (contractedPrunedGraph, length allEdgesToDelete, length edgeList, length numberComponents)


-- | removeTerminalHTUS recursively (since can create new ones) removes trerminal HTUS, their edges, and sister edges
removeTerminalHTUs :: LG.Gr T.Text Double -> LG.Gr T.Text Double
removeTerminalHTUs inGraph =
    if LG.isEmpty inGraph then LG.empty
    else 
         let nonHTULeafNodes = fmap fst $ filter (not . (T.isInfixOf (T.pack "HTU")) .snd) $ filter (LG.isConnectedLeafLablabelled inGraph) (LG.labNodes inGraph)
             
             hTULeafNodes = filter ((T.isInfixOf (T.pack "HTU")) .snd) $ filter (LG.isConnectedLeafLablabelled inGraph) (LG.labNodes inGraph)
             hTULeafEdgeList = filter ((`elem` (fmap fst hTULeafNodes)) .snd3) (LG.labEdges inGraph)
             
             -- need to restrict to sister edges that do not end in leaves? (ie. without HTU in name)
             sisterHolder = filter (not . null) $ fmap (LG.sisterLabNodes inGraph) hTULeafNodes
             sisterHTUNodeList = if null sisterHolder then []
                                 else (fmap fst $ fmap head sisterHolder)
             sisterHTUEdgeList = filter ((`notElem` nonHTULeafNodes) .snd) $ zip (fmap fst3 hTULeafEdgeList) sisterHTUNodeList

             secondEdgePrunedGraph = LG.delEdges ((fmap LG.toEdge hTULeafEdgeList) <> sisterHTUEdgeList) inGraph 
          in
          --trace ("RTH: " <> (show nonHTULeafNodes)) $
          if null hTULeafNodes then secondEdgePrunedGraph
          else removeTerminalHTUs secondEdgePrunedGraph

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

-- | 'main' Main Function to run latex IPA csv parser
main :: IO ()
main = 
    do
        --get input args
        args <- getArgs
        if (length args < 1) 
            then errorWithoutStackTrace ("Require two arguments:\n\tSingle graph input file (graphviz/newick),\n\tand an optional idenitfier key (e.g.'rand')") 
            else hPutStrLn stderr "Input args: "
        
        mapM_ (hPutStrLn stderr) (fmap ('\t':) args)

        let inFilePutArgs = uncons args
        let infileName = if isJust inFilePutArgs then fst $ fromJust inFilePutArgs
                         else error "Input filename not specified"

        
        let keyWord = if (length args < 1) then args !! 1
                      else "rand"
        
        let infileStub = reverse $ drop 1 $ dropWhile (/= '.') $ reverse infileName
        let outFileStub = if not (null infileStub) then infileStub
                          else infileName
        

        hPutStrLn stderr ("Keyword to prune: \"" <> keyWord <> "\"")
        hPutStrLn stderr ("Internal vertex labels must contain \"HTU\"")
        hPutStrLn stderr ("Printing output graph (in dot/gaphviz format) to " <> (outFileStub <> "-pruned.dot") <> " and " <> (outFileStub <> "-pruned.pdf"))

        -- read input graph
        graphFileHandle <- openFile infileName ReadMode
        graphContents' <- hGetContents' graphFileHandle
        let graphContents = trim graphContents'


        if null graphContents then errorWithoutStackTrace "Empty graph input"
        else hPutStrLn stderr "Read input graph"

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

        -- process graph to remove edges that have keyword on one side and do not on the other.
        let (newGraph, numPrunings, totalEdges, numComponents) = removeKeyEdges (T.pack keyWord) inputGraph
                
        -- output new graph-- output filename from input
        let outGraphString = removeLabel00 $ GFU.fgl2DotString newGraph
        let outGraphString' = changeDotPreamble "digraph {" "digraph G {\n\trankdir = LR;\tedge [colorscheme=spectral11];\tnode [shape = none];\n" outGraphString
        let outGraphString'' = outGraphString' <> "//numPrunings " <> (show numPrunings) <> " totalEdges " <> (show totalEdges) <> " numComponents " <> (show numComponents) <> "\n"
        

        hPutStrLn stderr ("\nNumber prunings: " <> (show numPrunings) <> " Total edges: " <> (show totalEdges) <> " Number components: " <> (show numComponents) <>"\n")


        printGraphVizDot outGraphString'' (outFileStub <> "-pruned.dot")


        --writeFile  (outFileStub <> "-pruned.dot") outGraphString'

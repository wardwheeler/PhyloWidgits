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

-- | Node variety
data DistributionType = Uniform | Yule 
    deriving stock (Show, Eq)

-- | output as Graphviz/Dot or newick
data OutputFormat = GraphViz | Newick 
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

-- | chooseRandomEdge selects man edge at random from list
chooseRandomEdge :: StdGen -> [LG.LEdge String] -> (StdGen, LG.LEdge String)
chooseRandomEdge inGen edgesAvailableToSplit =
   if null edgesAvailableToSplit then error "Null edge list to split"
   else 
      let (index, newGen) = randomR  (0, (length edgesAvailableToSplit) - 1) inGen
      in
      (newGen, edgesAvailableToSplit !! index)

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

          (newGen, edgeToSplit@(e,v,_)) = chooseRandomEdge inGen edgesAvailableToSplit
          newNodeIndex = numLeaves + htuCounter
          addedTerminalIndex = read (drop 1 firstTerminal) :: Int
          
          newNodeList = [(newNodeIndex, "HTU" <> (show newNodeIndex)), (addedTerminalIndex, firstTerminal)]
          
          edgessToAdd = [(e, newNodeIndex, "Edge" <> (show newNodeIndex)), (newNodeIndex, v, "Edge" <> (show v)), (newNodeIndex, addedTerminalIndex, "Edge" <> (show addedTerminalIndex))]

          newGraph = LG.insEdges edgessToAdd $ LG.insNodes newNodeList $ LG.delLEdge edgeToSplit inGraph
      in 
      genRandTreeFGL newGen numLeaves (htuCounter + 1) restList distribution newGraph
     


-- | Main function for conversion
main :: IO ()
main = 
  do 
     --get input command filename, ouputs to stdout
    args <- getArgs
    if (length args /= 3) 
      then errorWithoutStackTrace "Require three arguments: number of leaves in tree (Integer), tree distribution (Uniform/Yule), and output format (GraphViz/Newick)"
      else hPutStrLn stderr "Inputs: "
    mapM_ (hPutStrLn stderr) args
    hPutStrLn stderr ""
    
    let (numLeavesString, otherArgs) = fromJust $ uncons args
    let distributionText = T.toLower $ T.pack $ fst $ fromJust $ uncons otherArgs
    let outputFormatText = T.toLower $ T.pack $ (snd $ fromJust $ uncons otherArgs) !! 0

    let numLeavesMaybe = (readMaybe numLeavesString) :: Maybe Int

    let numLeaves = if isJust numLeavesMaybe then fromJust numLeavesMaybe
                    else errorWithoutStackTrace ("First argument needs to be an integer (e.g. 10): " <> numLeavesString)

    let distribution = if T.head distributionText == 'u' then Uniform
                       else if T.head distributionText == 'y' then Yule
                       else errorWithoutStackTrace ("Second argument needs to be 'Uniform' or 'Yule': " <> (args !! 1))

    let outputFormat = if T.head outputFormatText == 'g' then GraphViz
                       else if T.head outputFormatText == 'n' then Newick
                       else errorWithoutStackTrace ("Third argument needs to be 'Graphviz' or 'Newick': " <> last args)



    hPutStrLn stderr $ "Creating random tree with " <> (show numLeaves) <> " leaves via a " <> (show distribution) <> " distribution"


    -- create leaf labels for tree
    let leafLabelList = drop 3 $ fmap ('T' :) $ fmap show $ [0.. numLeaves - 1] 

    -- create initial 3 taxon tree
    let firstThreeNodes = [(0, "T0"), (1, "T1"),(2, "T2"), (numLeaves, "HTU" <> (show numLeaves)), (numLeaves + 1, "HTU" <> (show $ numLeaves + 1))]
    let firstThreeEdges = [(numLeaves, 0, "Edge" <> (show 0)), (numLeaves, numLeaves + 1, "Edge" <> (show $ numLeaves + 1)), (numLeaves + 1, 1, "Edge" <> (show 1)), (numLeaves + 1, 2, "Edge" <> (show 2))]
    let firstThreeGraph = LG.mkGraph firstThreeNodes firstThreeEdges
   
    -- random initialization
    randomGen <- initStdGen
    
    -- generatge ranodm tree in fgl
    let randTreeFGL = snd $ genRandTreeFGL randomGen numLeaves (2 :: Int) leafLabelList distribution firstThreeGraph

    -- output trees in formats (newick, dot)
    -- dot format
    let outGraphStringDot = removeLabel00 $ GFU.fgl2DotString randTreeFGL
    let outGraphStringDot' = changeDotPreamble "digraph {" "digraph G {\n\trankdir = LR;\tedge [colorscheme=spectral11];\tnode [shape = none];\n" outGraphStringDot

    -- newick format
    let graphTD = GFU.stringGraph2TextGraphDouble randTreeFGL
    let outGraphStringNewick = T.unpack $ GFU.fgl2FEN False False graphTD
    

    if outputFormat == GraphViz then hPutStrLn stdout outGraphStringDot'
    else hPutStrLn stdout outGraphStringNewick
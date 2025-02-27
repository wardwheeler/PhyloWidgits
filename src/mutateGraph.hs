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
chooseRandomEdge :: StdGen -> [LG.LEdge String] -> (StdGen, LG.LEdge String)
chooseRandomEdge inGen edgesAvailableToSplit =
   if null edgesAvailableToSplit then error "Null edge list to split"
   else 
      let (index, newGen) = randomR  (0, (length edgesAvailableToSplit) - 1) inGen
      in
      (newGen, edgesAvailableToSplit !! index)

-- | mutateGraphFGL generates a random gfl tree with leaf label list and distribution
mutateGraphFGL :: StdGen -> Int ->  Int -> Neighborhood -> LG.Gr String Double -> (StdGen, LG.Gr String Double)
mutateGraphFGL inGen mutationCounter maxMutations neighborhood inGraph =
   if mutationCounter >= maxMutations then (inGen, inGraph)
   else if LG.isEmpty inGraph then error "Empty graph in mutateGraphFGL"
   else 
      let newGraph = inGraph
          newGen = inGen
      in
      -- recurse for next mutation
      mutateGraphFGL newGen (mutationCounter + 1) maxMutations neighborhood newGraph


-- | Main function
main :: IO ()
main = 
  do 
     --get input command filename, ouputs to stdout
    args <- getArgs
    if (length args /= 3) 
      then errorWithoutStackTrace "Require four arguments: graph file name (String), number mutations (Integer), mutation neighborhood (NNI/SPR/TBR), and output format (GraphViz/Newick)"
      else hPutStrLn stderr "Inputs: "
    mapM_ (hPutStrLn stderr) args
    hPutStrLn stderr ""
    
    let (infileName, otherArgs) = fromJust $ uncons args

    let numberMutationsMaybe = readMaybe (args !! 1) :: Maybe Int

    let neighborhoodText = T.pack $ args !! 2

    let outputFormatText = T.toLower $ T.pack $ (args !! 3)

    let numberMutations = if isJust numberMutationsMaybe then fromJust numberMutationsMaybe
                          else errorWithoutStackTrace ("Second argument needs to be an integer (e.g. 10): " <> (args !! 1))

    let mutationNeighborhood = if T.head neighborhoodText == 'n' then NNI
                               else if T.head neighborhoodText == 's' then SPR
                               else if T.head neighborhoodText == 't' then TBR 
                               else errorWithoutStackTrace ("THird argument needs to be 'NNI', 'SPR', or 'TBR': " <> (args !! 2))

    let outputFormat = if T.head outputFormatText == 'g' then GraphViz
                       else if T.head outputFormatText == 'n' then Newick
                       else errorWithoutStackTrace ("THird argument needs to be 'Graphviz' or 'Newick': " <> (args !! 3))



    hPutStrLn stderr $ "Mutating gaph with " <> (show numberMutations) <> " mutations in " <> (show mutationNeighborhood) <> " edit neighborhood"


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

    -- random initialization
    randomGen <- initStdGen
    
    -- generatge mutated tree in fgl
    let mutantGraphFGL = snd $ mutateGraphFGL randomGen 0 numberMutations mutationNeighborhood (GFU.textGraph2StringGraph inputGraph)

    -- output trees in formats (newick, dot)
    -- dot format
    let outGraphStringDot = removeLabel00 $ GFU.fgl2DotString mutantGraphFGL
    let outGraphStringDot' = changeDotPreamble "digraph {" "digraph G {\n\trankdir = LR;\tedge [colorscheme=spectral11];\tnode [shape = none];\n" outGraphStringDot

    -- newick format
    --let graphTD = GFU.stringGraph2TextGraphDouble mutantGraphFGL
    let outGraphStringNewick = T.unpack $ GFU.fgl2FEN False False (GFU.stringGraph2TextGraph mutantGraphFGL)

    if outputFormat == GraphViz then hPutStrLn stdout outGraphStringDot'
    else hPutStrLn stdout outGraphStringNewick
{- |
Module      :  graph2Characters
Description :  Takes a newick or Graphviz formatted graph and outputs Hennig86/TNT formated data set
               with a charcter for each subgraph 1 if leaf in, 0 if not.
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
import Text.Read


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


-- | Main function
main :: IO ()
main = 
  do 
     --get input command filename, ouputs to stdout
    args <- getArgs
    if (length args /= 1) 
      then errorWithoutStackTrace "Requires a single arguments: graph file name (String)"
      else hPutStrLn stderr "Inputs: "
    mapM_ (hPutStrLn stderr) $ fmap ('\t' :) args
    hPutStrLn stderr ""
    
    let (infileName, otherArgs) = fromJust $ uncons args

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

    
    -- find graph vertices that are not leaves
    let htuList = filter (not . LG.isLeafLab inputGraph) $ LG.labNodes inputGraph

    -- get descendent lists for each node
    let subGraphNodePairL = fmap (LG.nodesAndEdgesAfter inputGraph) $ fmap (:[]) htuList

    -- remove edges from lists
    let subGraphNodeLL = fmap fst subGraphNodePairL

    -- remove non-leaves form each list of nodes 
    let subGraphLeafLL = fmap (filter (LG.isLeafLab inputGraph)) subGraphNodeLL

    -- generate character "columns" for each htu vertex


    -- crearte sting of matrix

    --- output matrix file string
    let outDataString = "bleh2"
    
    hPutStrLn stdout outDataString
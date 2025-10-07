{- |
Module      :  robinsonFoulds
Description :  Inputs 2 graphs and returns Robinson-Foulds distnace normalized by
                    resolution and leaf number
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

-- | makeColumnCharacters tkaes a list of leafNodes and creates a list of Strings of "1" if
--    leaf in list and "0" if not.
makeColumnCharacters :: [LG.LNode T.Text] -> [LG.LNode T.Text] -> String
makeColumnCharacters leafList leafSubGraphList =
    let inLeafListPair = uncons leafList
    in
    if null leafSubGraphList then error "No subgraph list"
    else if isNothing inLeafListPair then []
    else 
        let (firstLeaf, restLeaves) = fromJust inLeafListPair
        in
        if firstLeaf `elem` leafSubGraphList then
            "1" <> (makeColumnCharacters restLeaves leafSubGraphList)
        else 
            "0" <> (makeColumnCharacters restLeaves leafSubGraphList)

-- | joinNameToMatrix joins leaf names to string of characters
joinNameToMatrix :: (String, String) -> String
joinNameToMatrix (fName, fchars)  =
    fName <> "\t" <> (drop 1 fchars)

-- | Main function
main :: IO ()
main = 
  do 
     --get input command filename, ouputs to stdout
    args <- getArgs
    if (length args /= 1) 
      then errorWithoutStackTrace "Requires a two arguments: graph file names (Strings)"
      else hPutStrLn stderr "Inputs: "
    mapM_ (hPutStrLn stderr) $ fmap ('\t' :) args
    hPutStrLn stderr ""
    
    let infileName1 = args !! 0
    let infileName2 = args !! 1

    -- read input graphs
    graphFileHandle1 <- openFile infileName1 ReadMode
    graphContents1' <- hGetContents' graphFileHandle1
    let graphContents1 = trim graphContents1'

    graphFileHandle2 <- openFile infileName2 ReadMode
    graphContents2' <- hGetContents' graphFileHandle2
    let graphContents2 = trim graphContents2'

    if null graphContents1 then errorWithoutStackTrace "\tEmpty first graph input"
    else if null graphContents2 then errorWithoutStackTrace "\tEmpty second graph input"
    else hPutStrLn stderr "Successfully read input graphs"

    let firstChar1 = head graphContents1
    let firstChar2 = head graphContents2

    -- read iput graphs trying enewick and dot returning dot/graphviz format
    -- a bit stupid, but has to do with reusing some graphviz functions
    inputGraph1 <- if firstChar1 == '(' then 
                     -- enewick
                     pure $ head $ GFU.forestEnhancedNewickStringList2FGLList (T.pack graphContents1)  
                      
                 else if (toLower firstChar1 == '/') || (toLower firstChar1 == 'd') || (toLower firstChar1 == 'g') then do
                     -- gaphviz/dot
                     newGraphFileHandle1 <- openFile infileName1 ReadMode
                     dotGraph1 <- LG.hGetDotLocal newGraphFileHandle1
                     hClose newGraphFileHandle1
                     pure $ GFU.relabelFGL $ LG.dotToGraph dotGraph1
                      
                 else errorWithoutStackTrace ("First input graph file does not appear to be enewick or dot/graphviz")


    inputGraph2 <- if firstChar2 == '(' then 
                     -- enewick
                     pure $ head $ GFU.forestEnhancedNewickStringList2FGLList (T.pack graphContents2)  
                      
                 else if (toLower firstChar2 == '/') || (toLower firstChar2 == 'd') || (toLower firstChar2 == 'g') then do
                     -- gaphviz/dot
                     newGraphFileHandle2 <- openFile infileNamew ReadMode
                     dotGraphw <- LG.hGetDotLocal newGraphFileHandlew
                     hClose newGraphFileHandlew
                     pure $ GFU.relabelFGL $ LG.dotToGraph dotGraphw
                      
                 else errorWithoutStackTrace ("Second input graph file does not appear to be enewick or dot/graphviz")

    
    hPutStrLn stderr "All done."
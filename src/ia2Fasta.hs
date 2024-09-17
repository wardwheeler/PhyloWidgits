{- |
Module      :  ia2Fasta 
Description :  takes Implied Alignment output from PhyG and creates single fasta file by concatenating
               sequences with same taxon name, adding (or not) '#' between fragments
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
-}
module Main where

import System.IO
import System.Process
import System.Environment
import Debug.Trace
import Data.List
import Data.List.Split
import Data.Char
import Data.Maybe

-- | 'trim' trim removes leading and trailing white space
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

editorCrap = ['\r']
whiteSpace = [' ', '\t', '\v']

-- | removeCommentsEtc returns empty if not relevant seqeunxces or title
removeCommentsEtc :: String -> String
removeCommentsEtc inString =
    if null inString then []
    else 
        let wordLine = words inString
        in
        if take 4 wordLine == ["Implied", "Alignments", "for", "Graph"] then []
        else if take 2 wordLine == ["Sequence", "character"] then []
        else inString


-- | getSequences take fasta lines and splits into sequences and returns list 
-- of name, sequence pairs
getSequences :: [String] -> [(String, String)]
getSequences inLineList =
    if null inLineList then []
    else 
        let firstLine = head inLineList
        in
        if null firstLine then getSequences (tail inLineList)
        else if head firstLine == '>' then 
            let (sequenceBody, remainder) = getFirstSequence "" (tail inLineList) (tail inLineList)
            in
            (firstLine, sequenceBody) : getSequences remainder
        else errorWithoutStackTrace ("Format error in input file at line :" <> firstLine)

-- | getFirstSequence removes the first sequnce bnody (before '>') and retuns constenated suquence
-- and reminader of list
getFirstSequence :: String -> [String] -> [String] -> (String, [String])
getFirstSequence curSeqBody remainderStringList inStringList =
    if null inStringList then (curSeqBody, remainderStringList)
    else 
        let firstLine = head inStringList
        in
        if null firstLine then getFirstSequence curSeqBody (tail remainderStringList) (tail inStringList)
        else if head firstLine == '>' then (curSeqBody, remainderStringList)
        else 
            getFirstSequence (curSeqBody <> firstLine) (tail remainderStringList) (tail inStringList) 


-- | mergeSequences merges sequences based on name 
mergeSequences :: Bool -> [(String, String)] -> [(String, String)]
mergeSequences pound inPairList =
    if null inPairList then []
    else 
        let (firstName, firstSeq) = head inPairList
            allFirst = filter ((== firstName) . fst) inPairList
            notFirst = filter ((/= firstName) . fst) inPairList
            firstSeqList = fmap snd allFirst
            firstJoined = if pound then intercalate "#" firstSeqList
                          else concat firstSeqList
        in
        (firstName, firstJoined) : mergeSequences pound notFirst

-- | createFastaString takes name sequecne pairs and reformats for fasta output
createFastaString :: (String, String) -> String
createFastaString (seqName, seqBody) = seqName <> "\n" <> seqBody <> "\n"


-- | 'main' Main Function to run latex IPA csv parser
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        if (length args /= 2) 
            then errorWithoutStackTrace "Require two arguments:\n\tSingle input PhyG implied alignment file (fasta/c),\n\t'pound' or 'nopound' to add (or not) '#' between fragments" 
            else hPutStrLn stderr "Input args: "
        mapM_ (hPutStrLn stderr) (fmap ('\t':) args)

        let toPound = "pound" == fmap toLower (args !! 1)
        --hPutStrLn stderr "\n"

        soundFileHandle <- openFile (head args) ReadMode
        soundContents <- hGetContents soundFileHandle

        -- remve non-sequence stuff (PhyG specific)
        let sequenceLinesOnly = fmap removeCommentsEtc $ lines soundContents

        let sequenceList = getSequences sequenceLinesOnly

        let joinSequenceList = mergeSequences toPound sequenceList

        let newFasta = concatMap createFastaString joinSequenceList

        
        hPutStrLn stderr ("There were " <> (show (length joinSequenceList)) <> " merged sequences")
        hPutStr stdout newFasta
        
        hClose soundFileHandle

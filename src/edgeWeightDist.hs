{- |
Module      :  edgeWeighDist takes a dot file and parses out edge weights and tests versus exponential
               uniform distribiutions via chi-squared values 
Description :  takes Implied Alignment output from PhyG and creates single fasta file by concatenating
               sequences with same taxon name, adding (or not) '#' between fragments
Copyright   :  (c) 2026 Ward C. Wheeler, Division of Invertebrate Zoology, AMNH. All rights reserved.
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
import Text.Read

-- | 'trim' trim removes leading and trailing white space
trim :: String -> String
trim = f . f
    where f = reverse . dropWhile isSpace

editorCrap = ['\r']
whiteSpace = [' ', '\t', '\v']

getEdgeWeights :: String -> [Double]
getEdgeWeights inLine =
    if null inLine then []
    else 
        let fields = words inLine
        in
        if "->" `notElem` fields then []
        else 
            let labelField = last fields
                weightString = takeWhile (/= ']') $ tail $ dropWhile (/= '=') labelField
                weightVal = readMaybe weightString :: Maybe Double
            in
            if isNothing weightVal then errorWithoutStackTrace ("Edge label not a float: " <> weightString)
            else [fromJust weightVal]

getBinRange :: Double -> Double -> Int -> Double -> (Double, Double)
getBinRange minVal width numBins multiplier =

    -- to ensure lowest value in lowest range
    let lowerVal = if multiplier == 1.0 
            then 0.0
            else minVal + ((multiplier - 1.0) * width)

        -- to ensure highest value in highest range
        upperVal = if multiplier /= (fromIntegral numBins)
            then minVal + (multiplier * width)
            else minVal + ((multiplier + 1.0) * width)
    in
    (lowerVal, upperVal)



getRangeList :: [Double] -> Double -> Double -> Int -> [(Double, Double)]
getRangeList valList minVal maxVal numBins = 
    if null valList then error ("Null valList in getBins")
    else 
        let range = maxVal - minVal
            width = range / (fromIntegral numBins)
            multipliers = fmap fromIntegral $ take numBins [1..]
        in
        fmap (getBinRange minVal width numBins) multipliers
        

getBinNumbers :: [Double] -> Int -> (Double, Double)  -> Int
getBinNumbers edgeWeightList curCount (lowVal, highVal) =
    if null edgeWeightList then curCount
    else 
        let firstVal = head edgeWeightList
        in
        if firstVal >= lowVal && firstVal < highVal then    
            getBinNumbers (tail edgeWeightList) (curCount + 1) (lowVal, highVal)
        else 
            getBinNumbers (tail edgeWeightList) curCount (lowVal, highVal)

getExponentialNumber :: Double -> Double -> (Double, Double) -> Double
getExponentialNumber lambda numEdges (a,b) =
    numEdges * ((exp $ -1.0 * a * lambda) - (exp $ -1.0 * b * lambda))

chiSquared :: (Int, Double) -> Double
chiSquared (observed, expected) =
    (((fromIntegral observed) - expected)^2) / expected


-- | 'main' Main Function 
main :: IO ()
main = 
    do
        --get input command filename
        args <- getArgs
        if (length args /= 2) 
            then errorWithoutStackTrace ("Require a two arguments:\n\tInput graphviz (dot/gv) file and number of bins for test."
            <> "\nWarning--if multiple graphs in file all edges will be included" )
            else hPutStrLn stderr "Input args: "
        mapM_ (hPutStrLn stderr) (fmap ('\t':) args)

        graphFileHandle <- openFile (head args) ReadMode
        graphContents <- hGetContents graphFileHandle

        let binNumberMaybe = readMaybe (args !! 1) :: Maybe Int 

        let binNumber = if isJust binNumberMaybe then fromJust binNumberMaybe
                        else errorWithoutStackTrace ("Second argument must be an integer: " <> (args !! 1) )

        let edgeWeights' = concat $ fmap getEdgeWeights (lines graphContents)

        -- This for PhyG root lengths somehow waaay long
        let edgeWeights = drop 2 edgeWeights'

        let meanVal = (sum edgeWeights) / (fromIntegral $ length edgeWeights)

        let maxVal = maximum edgeWeights
        let minVal = minimum edgeWeights

        let rangeList = getRangeList edgeWeights minVal maxVal binNumber
        --hPutStrLn stderr (show rangeList)

        let binList = fmap (getBinNumbers edgeWeights 0) rangeList
        let minBin = minimum binList
        hPutStrLn stderr ("Bin numbers: " <> (show binList))

        

        


        hPutStrLn stderr ("Number of edges = " <> (show $ length edgeWeights) <> " Median value = " <> (show meanVal))
        
        -- Create distribution lists
        -- uniform total number / bins
        let uniformBinList = replicate binNumber ((fromIntegral $ length edgeWeights) / (fromIntegral binNumber))
        --hPutStrLn stderr (show uniformBinList)

        -- exponential interval [a,b] = e^(lambda * a) - e^(lambda * b) * totl number events (edges)
        let lambda = 1.0 / meanVal
        let exponentialBinList = fmap (getExponentialNumber lambda (fromIntegral $ length edgeWeights)) rangeList
        --hPutStrLn stderr (show $ exponentialBinList)

        let uniChiSq = sum $ fmap chiSquared (zip binList uniformBinList)
        let expChiSq = sum $ fmap chiSquared (zip binList exponentialBinList)

        hPutStrLn stderr ("Chi-squared values for Exponential: " <> (show expChiSq) <> " Uniform: " <> (show uniChiSq))

        let result = if expChiSq < uniChiSq then "Exponential"
                     else if uniChiSq < expChiSq then "Uniform"
                     else "Equal"

        hPutStrLn stdout result


        hClose graphFileHandle

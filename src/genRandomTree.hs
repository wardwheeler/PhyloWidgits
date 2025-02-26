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
import GraphFormatUtilities qualified as GFU
import LocalGraph qualified as LG
import Text.Read

-- | Node variety
data DistributionType = Uniform | Yule 
    deriving stock (Show, Eq)

-- | genRandTreeFGL generates a random gfl tree with leaf label list and distribution
genRandTreeFGL :: Int -> Int -> [String] -> DistributionType -> LG.Gr String String -> LG.Gr String String
genRandTreeFGL numLeaves htuCounter leafList distribution inGraph =
   inGraph

-- | Main function for conversion
main :: IO ()
main = 
  do 
     --get input command filename, ouputs to stdout
    args <- getArgs
    if (length args /= 2) 
      then errorWithoutStackTrace "Require two arguments: number of leaves in tree (Integer) and tree distribution (Uniform/Yule)"
      else hPutStrLn stderr "Inputs: "
    mapM_ (hPutStrLn stderr) args
    hPutStrLn stderr ""
    
    let numLeavesString = head args 
    let distributionText = T.toLower $ T.pack $ last args

    let numLeavesMaybe = (readMaybe numLeavesString) :: Maybe Int

    let numLeaves = if isJust numLeavesMaybe then fromJust numLeavesMaybe
                    else errorWithoutStackTrace ("Firat argument needs to be an integer (e.g. 10): " <> numLeavesString)

    let distribution = if T.head distributionText == 'u' then Uniform
                       else if T.head distributionText == 'y' then Yule
                       else errorWithoutStackTrace ("Second argument needs to be 'Uniform' or 'Yule': " <> last args)

    hPutStrLn stderr $ "Creating random tree with " <> (show numLeaves) <> " leaves via a " <> (show distribution) <> " distribution"

    let leafLabelList = drop 3 $ fmap ('T' :) $ fmap show $ [0.. numLeaves - 1] 

    let firstThreeNodes = [(0, "T0"), (1, "T1"),(2, "T2"), (numLeaves, "HTU" <> (show numLeaves)), (numLeaves + 1, "HTU" <> (show $ numLeaves + 1))]
    let firstThreeEdges = [(numLeaves, 0, "Edge" <> (show 0)), (numLeaves, numLeaves + 1, "Edge" <> (show $ numLeaves + 1)), (numLeaves + 1, 1, "Edge" <> (show 1)), (numLeaves + 1, 2, "Edge" <> (show 2))]
    let firstThreeGraph = LG.mkGraph firstThreeNodes firstThreeEdges

    let randTreeFGL = genRandTreeFGL numLeaves (2 :: Int) leafLabelList distribution firstThreeGraph
    
    hPutStrLn stdout $ LG.prettify randTreeFGL
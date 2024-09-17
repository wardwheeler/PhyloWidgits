# PhyloWidgits
Haskell code to perform small, useful tasks.


csv2Fasta 
	converts csv files containig IPA languistic data to fasta/c files for analysis with PhyG (https://github.com/AMNH/PhyG).

csv2TCM 
	Converts csv sound file to tcm for fastc language file

meanWords
	Calculates mean words of languages from fastc files (latex IPA representations) 

ipaConvert
	Inter-converts ipa alphabet symbols to be parsable by phyloAlgoInfo (https://github.com/wardwheeler/PhyloAlgInfo)

getElements
	Returns file of alphabet symbols from fasta/c file

ia2Fasta
	Converts Implied Alignment output from PhyG and creates single fasta file by concatenating
               sequences with same taxon name, adding (or not) '#' between fragments

pruneRandGraph
	Prunes graph edges and vertices with 'rand' data key (or other)

randomSequences
	Naive random sequences generator based on to alphabet, average and variation length, and missing fraction
               	
shuffleSequences
	Creates randomized sequences by shuffling elements of existing sequences, 
	preserving distributional biases present in source data



Compilation

All are verified to compile with ghc-9.10.1 and cabal v 3.12.1

For each binary compilation :
	cabal update
	cabal build binaryName --with-compiler ghc-9.10.1 



References
Wheeler, W. C. and P. M. Whiteley. 2015.  Historical Linguistics as a Sequence Optimization Problem: Uto-Aztecan Language Evolution and Biogeography. Cladistics 31:113-125.

Wheeler, W. C., and A. Washburn. 2019.  FASTC: A file format for multi-character sequence data.  Cladistics, 35:573-575. 

Wheeler, W. C., A. Washburn, and L. M. Crowley.  2024. PhylogeneticGraph (PhyG): A new phylogenetic search and optimization program.  Cladistics, 40:97-105. https://github.com/AMNH/PhyG

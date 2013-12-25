module Main where

import Rep
import Latex
import System.Environment
import System.Process
import Data.List.Split

main :: IO()
main = do
	args <- getArgs
	let filePath = head args
	let outputName = head $ tail args
	rawData <- readFile filePath
	let blankCrossword = constructCrossword (splitOn "\n" rawData)
	filledCrossword <- fillAll blankCrossword
	let puzzleFile = (outputName ++ ".tex")
	let solutionFile = (outputName ++ "-solution.tex")

	writeFile puzzleFile (latexUnsolved filledCrossword)
	writeFile solutionFile (latexSolution filledCrossword)

	_ <- system puzzleFile
	_ <- system solutionFile

	return ()
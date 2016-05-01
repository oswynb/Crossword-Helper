module Main where

import           Control.Monad
import           Data.List.Split
import           Data.Monoid
import           System.Environment
import           System.Process

import           Latex
import           Rep

main :: IO ()
main = do
  args <- getArgs
  if length args < 2 then
    putStrLn "Usage: crossword-helper $CROSS_FILE $OUTPUT_PREFIX"
  else do
    let [filePath, outputName] = args
    rawData <- readFile filePath
    let blankCrossword = constructCrossword (splitOn "\n" rawData)
    filledCrossword <- fillAll blankCrossword
    let puzzleFile = (outputName ++ ".tex")
    let solutionFile = (outputName ++ "-solution.tex")

    writeFile puzzleFile (latexUnsolved filledCrossword)
    writeFile solutionFile (latexSolution filledCrossword)

    void $ system $ "open " <> puzzleFile
    void $ system $ "open " <> solutionFile

module Latex where

import Data.Array.IArray
import Rep

data Mode = Solution | Unsolved

intToWidth :: Int -> Int
intToWidth 0 = 0
intToWidth x = 1 + intToWidth (x `div` 10)

modeToCommand :: Mode -> String
modeToCommand Solution = "\\PuzzleSolution\n"
modeToCommand Unsolved = "\\PuzzleUnsolved\n"

latexHeader :: Mode -> String
latexHeader mode = concat ["\\documentclass[twocolumn]{article}\n",
				           "\\usepackage{cwpuzzle}\n",
				           "\n",
					  	   "\\setlength{\\oddsidemargin}{-.5in}\n",
					  	   "\\setlength{\\evensidemargin}{-.5in}\n",
						   "\\setlength{\\textwidth}{7.5in}\n",
						   "\\setlength{\\topmargin}{-.5in}\n",
						   "\\setlength{\\textheight}{10in}\n",
						   modeToCommand mode,
						   "\\begin{document}\n",
						   "\\pagestyle{empty}\\raggedright\n"]

latexFooter :: String
latexFooter = "\\end{document}\n"						

puzzleLatex :: Crossword -> String
puzzleLatex (Crossword w h t aClues dClues) = concat
	["\\begin{Puzzle}{" ++ (show w) ++ "}{" ++ (show h) ++ "}\n", 
     concat $ map (rowLatex (length (aClues) + length(dClues))) (tileArrayToNestedList t),
	 "\\end{Puzzle}\n",
	 "\n"]

acrossLatex :: [Clue] -> String
acrossLatex = cluesLatex "Across"

downLatex :: [Clue] -> String
downLatex = cluesLatex "Down"

clueLatex :: Clue -> String
clueLatex (Clue _ n c a _) = "  \\Clue{" ++ (show n) ++ "}{" ++ a ++ "}{" ++ c ++ "}\n"

cluesLatex :: String -> [Clue] -> String
cluesLatex name clues = "\\begin{PuzzleClues}{\\textbf{" ++ name ++ "}}\n" ++
	(concat $ (map clueLatex clues)) ++ "\\end{PuzzleClues}\n\n\n"


tileLatex :: Int -> Tile -> String
tileLatex width Void = "|" ++ (replicate (2 + width) ' ') ++ "*" 
tileLatex width (Tile x Nothing) = "|" ++ (replicate (2 + width) ' ') ++ [x]
tileLatex width (Tile x (Just n)) = "|" ++ "[" ++ (show n) ++ "]" ++ (replicate (width - intToWidth n) ' ') ++ [x]

rowLatex :: Int -> [Tile] -> String
rowLatex clueCount tiles =  "  " ++ concat (map (tileLatex $ intToWidth clueCount) tiles) ++ "|.\n"

latexSolution :: Crossword -> String
latexSolution cw = concat [latexHeader Solution,
			               puzzleLatex cw,
			               latexFooter]

latexUnsolved :: Crossword -> String
latexUnsolved c = concat   [latexHeader Unsolved,
                            puzzleLatex c,
                           	acrossLatex (acrossClues c),
                           	downLatex   (downClues   c),
                           	latexFooter]                          
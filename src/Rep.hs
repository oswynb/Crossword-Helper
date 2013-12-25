module Rep where

import Data.Array.IArray
import System.IO
import Data.Maybe
import Data.List.Split

type Position = (Int, Int)

--TileArrays store tiles using (x, y) cartesian co-ordinates
--Use tileArrayToNestedList to convert into a row-major nested list
type TileArray = Array Position Tile 

data Direction = Across | Down

instance Show Direction where
	show Across = "Across"
	show Down   = "Down"			   

data Tile =
	Tile {
		tileChar       :: Char,
		tileClueNumber :: (Maybe Int)
	}
	| Void

instance Show Tile where
	show Void 	    = "#"
	show (Tile c n) = [c]		 

data Clue =
	Clue {
		cluePosition 	:: Position,
		clueNumber		:: Int,
		clueText		:: String,
		clueAnswer		:: String,
		clueDirection	:: Direction		
	}

instance Show Clue where
	show c@(Clue {clueText = ""})    = show (c{clueText = "?"})
	show (Clue pos num clue ans dir) = show num ++ " " ++ show dir ++ ": " ++ clue ++ " - " ++ ans

data Crossword =
	Crossword {	
		width 		:: Int,
		height		:: Int,
		tiles 		:: TileArray,
		acrossClues	:: [Clue],
		downClues	:: [Clue]
	}

isFilled :: Clue -> Bool
isFilled c = (clueText c /= "")

isVoid :: Tile -> Bool
isVoid Void = True
isVoid _	= False

charToTile :: Char -> Tile
charToTile '#' = Void
charToTile c   = Tile c Nothing

coords :: Int -> Int -> [Position]
coords w h = [(i,j) | j <- [1..h], i <- [1..w]]

checkForWords :: Crossword -> [(Int, Position)]
checkForWords c = zip [1..] (filter (\p -> hasEither p c) (coords (width c) (height c)))

addEmptyClues :: Crossword -> Crossword
addEmptyClues c = c{acrossClues = acrossClues, downClues = downClues}
	where
		pairs = checkForWords c
		acrossPositions = filter (\(n, p) -> hasAcross p c) pairs
		downPositions   = filter (\(n, p) -> hasDown   p c) pairs
		acrossClues = map (\(n, p) -> createBlankClue c (n, p, Across)) acrossPositions
		downClues 	= map (\(n, p) -> createBlankClue c (n, p, Down  )) downPositions

createBlankClue :: Crossword -> (Int, Position, Direction) -> Clue
createBlankClue c (n, p, d) = Clue p n "" (recoverWord p c d) d	

recoverWord :: Position -> Crossword -> Direction -> String
recoverWord (x,y) c Down	
	| y > (height c) = ""
	| isVoid t = ""
	| otherwise = (show t) ++ recoverWord (x,y+1) c Down
		where
			t = ((tiles c) ! (x,y))

recoverWord (x,y) c Across
	| x > (width c) = ""
	| isVoid t = ""
	| otherwise = (show t) ++ recoverWord (x+1,y) c Across
		where
			t = ((tiles c) ! (x,y))			

tileArrayToNestedList :: TileArray -> [[Tile]]
tileArrayToNestedList tiles = chunksOf (snd (snd (bounds tiles))) (elems $ ixmap (bounds tiles) (\(i,j) -> (j,i)) tiles)

hasEither :: Position -> Crossword -> Bool
hasEither p c = hasDown p c || hasAcross p c

hasBoth :: Position -> Crossword -> Bool
hasBoth p c = hasDown p c && hasAcross p c

hasDown :: Position -> Crossword -> Bool
hasDown (x,y) (Crossword _w h tiles _ _)
	| isVoid (tiles ! (x,y))	= False
	| y == 1					=  not $ isVoid (tiles ! (x, y+1))
	| y == h 					= False
	| otherwise         		= (not $ isVoid (tiles ! (x, y+1))) && isVoid (tiles ! (x, y-1))

hasAcross :: Position -> Crossword -> Bool
hasAcross (x,y) (Crossword w _h tiles _ _)
	| isVoid (tiles ! (x,y))	= False
	| x == 1 					=  not $ isVoid (tiles ! (x+1, y))
	| x == w 					= False
	| otherwise					= (not $ isVoid (tiles ! (x+1, y))) && isVoid (tiles ! (x-1, y))

constructTileArray :: Int -> Int -> [String] -> TileArray
constructTileArray w h strings = array ((1,1),(w,h)) associations
	where		
		associations = zip (coords w h) (map charToTile (concat strings))

constructCrossword :: [String] -> Crossword
constructCrossword strings = addEmptyClues (Crossword w h tiles [] [])
	where
		h = length strings
		w = length $ head strings
		tiles = constructTileArray w h strings

fillClueUser :: Clue -> IO Clue
fillClueUser c = do
	putStrLn $ show c
	text <- getLine
	return c{clueText = text}

numberTile :: Int -> Tile -> Tile
numberTile i (Tile char _) = (Tile char (Just i))

fillAll :: Crossword -> IO Crossword
fillAll c = do
	filledAcross <- mapM fillClueUser (acrossClues c)
	filledDown <- mapM fillClueUser (downClues c)
	let allFilled = filledDown ++ filledAcross
	let tileUpdates = zip (map cluePosition allFilled)
	                        (map (\clue -> numberTile (clueNumber clue) ((tiles c) ! (cluePosition clue)) ) allFilled)
	return c{acrossClues = filledAcross, downClues = filledDown, tiles = (tiles c // tileUpdates)}
{-

Brian Chau 30006118 b8z7
Daniel Lu 75592063 a7e7

Assignment 3 CPSC 312

-}

rush_hour :: [String] -> [[String]]
rush_hour start
	| valid start	= reverse (statesearch [start] [])
	| otherwise	= error "This is not a Rush Hour board with 6 rows and 6 columns"

valid :: [String] -> Bool
valid (a:b:c:d:e:f:[]) = (valid_row a) && (valid_row b) && (valid_row c) && (valid_row d) && (valid_row e) && (valid_row f)
valid _ = False

valid_row :: String -> Bool
valid_row (_:_:_:_:_:_:[]) = True
valid_row _ = False

statesearch :: [[String]] -> [[String]] -> [[String]]
statesearch unexplored path
	| null unexplored		= []
	| isgoal (head unexplored)	= (head unexplored):path
	| (not (null newstates))	= newstates
	| otherwise			= statesearch (tail unexplored) path
	where newstates = statesearch (generateNewStates (head unexplored) path) ((head unexplored):path)

isgoal :: [String] -> Bool
isgoal (_:_:x:_:_:_:[]) = exiting x
	where exiting (_:_:_:_:e:f:[]) = (e == 'X') && (f == 'X')

generateNewStates :: [String] -> [[String]] -> [[String]]
generateNewStates x explored = removeDupStates (generateAllMoves x) explored

removeDupStates :: [[String]] -> [[String]] -> [[String]]
removeDupStates newstates explored
	| null newstates			= []
	| elem (head newstates) explored	= removeDupStates (tail newstates) explored
	| otherwise				= (head newstates):(removeDupStates (tail newstates) explored)

generateAllMoves :: [String] -> [[String]]
generateAllMoves posn = concat [generateLeftMoves posn,
				generateRightMoves posn,
				generateUpMoves posn,
				generateDownMoves posn]

generateLeftMoves :: [String] -> [[String]]
generateLeftMoves posn = []

generateRightMoves :: [String] -> [[String]]
generateRightMoves posn = []

generateUpMoves :: [String] -> [[String]]
generateUpMoves posn = []

generateDownMoves :: [String] -> [[String]]
generateDownMoves posn = []

{-

Brian Chau 30006118 b8z7
Daniel Lu 75592063 a7e7

Assignment 3

-}

rush_hour :: [String] -> [[String]]
rush_hour start = reverse (statesearch [start] [])

statesearch :: [[String]] -> [[String]] -> [[String]]
statesearch unexplored path
	| null unexplored		= []
	| isgoal (head unexplored)	= (head unexplored):path
	| (not (null newstates))	= newstates
	| otherwise			= statesearch (tail unexplored) path
	where newstates = statesearch (generateNewStates (head unexplored) path) ((head unexplored):path)

isgoal :: [String] -> Bool
isgoal (a:b:c:d:e:f:[]) = exiting c
	where exiting (a:b:c:d:e:f:[]) = ((e == 'X') && (f == 'X'))

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

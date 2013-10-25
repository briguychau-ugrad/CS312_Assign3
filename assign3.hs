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
	where exiting (_:_:_:_:_:f:[]) = f == 'X'

generateNewStates :: [String] -> [[String]] -> [[String]]
generateNewStates x explored = removeDupStates (generateAllMoves x) explored

removeDupStates :: [[String]] -> [[String]] -> [[String]]
removeDupStates newstates explored
	| null newstates			= []
	| elem (head newstates) explored	= removeDupStates (tail newstates) explored
	| otherwise				= (head newstates):(removeDupStates (tail newstates) explored)

replaceString :: [String] -> String -> Int -> [String]
replaceString (b:bs) str index
	| index == 0	= str:bs
	| otherwise	= b:(replaceString bs str (index - 1))

stringSame :: String -> Int -> Int -> Bool
stringSame str from to
	| (from + 1) == to			= (str !! from) == (str !! to)
	| (str !! from) /= (str !! (from + 1))	= False
	| otherwise				= stringSame str (from + 1) to

generateAllMoves :: [String] -> [[String]]
generateAllMoves posn = concat [generateLeftMoves posn,
				generateRightMoves posn,
				generateUpMoves posn,
				generateDownMoves posn]

generateLeftMoves :: [String] -> [[String]]
generateLeftMoves posn = concat [moveleft 0 posn,
				 moveleft 1 posn,
				 moveleft 2 posn,
				 moveleft 3 posn,
				 moveleft 4 posn,
				 moveleft 5 posn]

moveleft :: Int -> [String] -> [[String]]
moveleft index board = map inserter (canshiftL (board !! index) 5)
	where inserter x = replaceString board x index

canshiftL :: String -> Int -> [String]
canshiftL str p
	| p <= 1		= []
	| (str !! p) == '-'	= canshiftL str (p - 1)
	| p == 5		= if ((str !! 4) == '-')
					then (canshiftL str 2)
				  else if ((stringSame str 4 5) && ((str !! 3) == '-'))
				  	then ((str !! 0):(str !! 1):(str !! 2):(str !! 4):(str !! 5):(str !! 3):[]):(canshiftL str 2)
				  else if ((stringSame str 3 5) && ((str !! 2) == '-'))
				  	then ((str !! 0):(str !! 1):(str !! 3):(str !! 4):(str !! 5):(str !! 2):[]):[]
				  else if ((stringSame str 2 5) && ((str !! 1) == '-'))
				  	then ((str !! 0):(str !! 2):(str !! 3):(str !! 4):(str !! 5):(str !! 1):[]):[]
				  else if ((stringSame str 1 5) && ((str !! 0) == '-'))
				  	then ((str !! 1):(str !! 2):(str !! 3):(str !! 4):(str !! 5):(str !! 0):[]):[]
				  else
				  	canshiftL str 4
	| p == 4		= if ((str !! 3) == '-')
					then (canshiftL str 3)
				  else if ((stringSame str 3 4) && ((str !! 2) == '-'))
				  	then ((str !! 0):(str !! 1):(str !! 3):(str !! 4):(str !! 2):(str !! 5):[]):[]
				  else if ((stringSame str 2 4) && ((str !! 1) == '-'))
				  	then ((str !! 0):(str !! 2):(str !! 3):(str !! 4):(str !! 1):(str !! 5):[]):[]
				  else if ((stringSame str 1 4) && ((str !! 0) == '-'))
				  	then ((str !! 1):(str !! 2):(str !! 3):(str !! 4):(str !! 0):(str !! 5):[]):[]
				  else
				  	canshiftL str 3
	| p == 3		= if ((str !! 2) == '-')
					then []
				  else if ((stringSame str 2 3) && ((str !! 1) == '-'))
				  	then ((str !! 0):(str !! 2):(str !! 3):(str !! 1):(str !! 4):(str !! 5):[]):[]
				  else if ((stringSame str 1 3) && ((str !! 0) == '-'))
				  	then ((str !! 1):(str !! 2):(str !! 3):(str !! 0):(str !! 4):(str !! 5):[]):[]
				  else
				  	canshiftL str 2
	| p == 2		= if ((str !! 1) == '-')
					then []
				  else if ((stringSame str 1 2) && ((str !! 0) == '-'))
				  	then ((str !! 1):(str !! 2):(str !! 0):(str !! 3):(str !! 4):(str !! 5):[]):[]
				  else
				  	[]
	| otherwise		= []

generateRightMoves :: [String] -> [[String]]
generateRightMoves posn = concat [moveright 0 posn,
				  moveright 1 posn,
				  moveright 2 posn,
				  moveright 3 posn,
				  moveright 4 posn,
				  moveright 5 posn]

moveright :: Int -> [String] -> [[String]]
moveright index board = map inserter (canshiftR (board !! index) 0)
	where inserter x = replaceString board x index

canshiftR :: String -> Int -> [String]
canshiftR str p
	| p >= 4		= []
	| (str !! p) == '-'	= canshiftR str (p + 1)
	| p == 0		= if ((str !! 1) == '-')
					then (canshiftR str 2)
				  else if ((stringSame str 0 1) && ((str !! 2) == '-'))
				  	then ((str !! 2):(str !! 0):(str !! 1):(str !! 3):(str !! 4):(str !! 5):[]):(canshiftR str 3)
				  else if ((stringSame str 0 2) && ((str !! 3) == '-'))
				  	then ((str !! 3):(str !! 0):(str !! 1):(str !! 2):(str !! 4):(str !! 5):[]):[]
				  else if ((stringSame str 0 3) && ((str !! 4) == '-'))
				  	then ((str !! 4):(str !! 0):(str !! 1):(str !! 2):(str !! 3):(str !! 5):[]):[]
				  else if ((stringSame str 0 4) && ((str !! 5) == '-'))
				  	then ((str !! 5):(str !! 0):(str !! 1):(str !! 2):(str !! 3):(str !! 4):[]):[]
				  else
				  	canshiftR str 1
	| p == 1		= if ((str !! 2) == '-')
					then (canshiftR str 3)
				  else if ((stringSame str 1 2) && ((str !! 3) == '-'))
				  	then ((str !! 0):(str !! 3):(str !! 1):(str !! 2):(str !! 4):(str !! 5):[]):[]
				  else if ((stringSame str 1 3) && ((str !! 4) == '-'))
				  	then ((str !! 0):(str !! 4):(str !! 1):(str !! 2):(str !! 3):(str !! 5):[]):[]
				  else if ((stringSame str 1 4) && ((str !! 5) == '-'))
				  	then ((str !! 0):(str !! 5):(str !! 1):(str !! 2):(str !! 3):(str !! 4):[]):[]
				  else
				  	canshiftR str 2
	| p == 2		= if ((str !! 3) == '-')
					then []
				  else if ((stringSame str 2 3) && ((str !! 4) == '-'))
				  	then ((str !! 0):(str !! 1):(str !! 4):(str !! 2):(str !! 3):(str !! 5):[]):[]
				  else if ((stringSame str 2 4) && ((str !! 5) == '-'))
				  	then ((str !! 0):(str !! 1):(str !! 5):(str !! 2):(str !! 3):(str !! 4):[]):[]
				  else
				  	canshiftR str 3
	| p == 3		= if ((str !! 4) == '-')
					then []
				  else if ((stringSame str 3 4) && ((str !! 5) == '-'))
				  	then ((str !! 0):(str !! 1):(str !! 2):(str !! 5):(str !! 3):(str !! 4):[]):[]
				  else
				  	[]
	| otherwise		= []

generateUpMoves :: [String] -> [[String]]
generateUpMoves posn = []

generateDownMoves :: [String] -> [[String]]
generateDownMoves posn = []

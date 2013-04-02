-- computes number of trailing spaces in the begining of the string
spaces :: Num a => [Char] -> a
spaces (' ':t) = 1 + (spaces t)
spaces (_:t) = 0
spaces [] = 0

-- computes list of trailing spaces of non empty lines
spaceTokens :: String -> [Integer]
spaceTokens = map spaces . (filter (/= "")). lines 

-- given a list (in fact ordered stack) of opened indentations in current scope
-- test if current indentation is correct
correct :: (Num a, Ord a) => Bool -> [a] -> [a] -> Bool
correct _ [] (0:y)  = correct False [0] y
correct _ [] (_:_) = False
correct _ _ []  = True
correct v (x:t) (y:u)
	| (x < y) = (not v) && (correct False (y:x:t) u)
	| x == y = correct False (x:t) u 
	| x > y = correct True t (y:u) 

-- apply tokenization and correctness test to an input text
indents :: String -> Bool
indents = (correct False []) . spaceTokens

-- Unit tests stuff (only very basic)
tests = 
	[("\na b c\n   d e f\n\n hello\n", False),
	("\na b c\n   d e f\n\nhello\n", True),	
	("\na b c\n   d e f\n\n   hello\n", True),
	("\na b c\n   d e f\n\n    hello\n", True),
	("\na b c\n   d e f\n\n    hello\n   a", True),
	("\na b c\n   d e f\n\n    hello\n  a", False)]

unitTest (s, v) = (indents s) == v
unitTests = foldr (&&) True (map unitTest tests)
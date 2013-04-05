-- computes number of trailing spaces in the begining of the string
spaces :: Num a => [Char] -> a
spaces (' ':t) = 1 + (spaces t)
spaces (_:t) = 0
spaces [] = 0

-- computes list of trailing spaces of non empty lines
spaceTokens :: String -> [Integer]
spaceTokens = map spaces . (filter ((/= ""). (dropWhile (== ' ')))). lines 

-- given a list (in fact ordered stack) of opened indentations in current scope
-- test if current indentation is correct
correctIndent :: (Num a, Ord a) => [a] -> [a] -> Bool
correctIndent [] (0:y)  = correctIndent [0] y
correctIndent [] (_:_) = False
correctIndent _ []  = True
correctIndent (x:t) (y:u)
	| x < y  = correctIndent (y:x:t) u  -- indentation increasing case
	| x == y = correctIndent (x:t) u    -- indentation remains the same
	| x > y  = correctDedent t (y:u)    -- dedent case

correctDedent :: (Num a, Ord a) => [a] -> [a] -> Bool
correctDedent [] _ = False
correctDedent _ []  = False
correctDedent (x:t) (y:u)
	| x < y  = False                    -- did not find the indentation level
	| x == y = correctIndent (x:t) u 	-- found, continue with indentation testing
	| x > y  = correctDedent t (y:u)    -- not yet, go on dedenting


-- apply tokenization and correctness test to an input text
indents :: String -> Bool
indents = (correctIndent []) . spaceTokens

-- Unit tests stuff (only very basic)
tests = 
	[("\na b c\n   d e f\n\n hello\n", False),
	("\na b c\n   d e f\n\nhello\n", True),	
	("\na b c\n   d e f\n\n   hello\n", True),
	("\na b c\n   d e f\n\n    hello\n", True),
	("\na b c\n   d e f\n  \n    hello\n   a", True),
	("\na b c\n   d e f\n\n    hello\n  a", False)]

unitTest (s, v) = (indents s) == v
unitTests = foldr (&&) True (map unitTest tests)
import Data.List
import Data.Char
import Debug.Trace

data Block a = Empty | Command a [(Block a)] deriving Show

data Command a = Guardcommand (Guard a) [(Command a)] | Assignment String (Expression a) | Returnstatement (Expression a) deriving Show

data Guard a = G (Expression a) GuardOp (Expression a) (Maybe (Guard a)) deriving Show

data GuardOp = Equals | NotEquals deriving Show

data Expression a = StringLiteral String [String] (Maybe (Expression a)) | Block a [String] (Maybe (Expression a)) | Name String [String] (Maybe (Expression a)) | Expression [String] (Maybe (Expression a)) deriving Show

{-
 TODO:
    refit every function to output a Block instead of a Bool
    every deeper function might need a counter variable
-}
 
checkSyntax :: String -> Bool
checkSyntax text
    | length text < 2 = False
    | length text >= 2 = (checkBlock (removeComments text))

removeComments :: String -> String
removeComments text = (removeCommentsLineByLine (lines text))

removeCommentsLineByLine :: [String] -> String
removeCommentsLineByLine list
    | (length list) == 0 = ""
    | (length (head list)) == 0 = (removeCommentsLineByLine (tail list))
    | otherwise = (removeLineComments (head list) 0) ++ (removeCommentsLineByLine (tail list)) --TODO whole thing now has no '\n'

removeLineComments :: String -> Integer -> String
removeLineComments (x:xs) inQuotes
    | (length xs) == 0 = [x]
    | (x == '%') && (inQuotes == 0) = "" -- trace (show ("comment: " ++ [x] ++ xs)) ""
    | (x == '%') && (inQuotes == 1) = [x] ++ (removeLineComments xs inQuotes) -- trace (show ("rest: " ++ [x] ++ xs)) ([x] ++ (removeLineComments xs inQuotes))
    | (x == '\"') && (inQuotes == 0) = [x] ++ (removeLineComments xs 1)
    | (x == '\"') && (inQuotes == 1) = [x] ++ (removeLineComments xs 0)
    | otherwise = [x] ++ (removeLineComments xs inQuotes)
    
checkBlock :: String -> Bool
checkBlock text = checkBlockStart text
    
checkBlockStart :: String -> Bool
checkBlockStart (x:xs)
    | (x == ' ') = checkBlockStart xs
    | (x == '{') = checkBlockEnd xs
    | otherwise = False
    
checkBlockEnd :: String -> Bool
checkBlockEnd end
    | ((last end) == ' ') = checkBlockEnd (init end)
    | ((last end) == '}') = True
    | otherwise = False

-- TODO will check first command's start and put it into the right check, this recursively puts the rest back here
checkCommandSequence :: String -> Bool
checkCommandSequence text
    | (head text) == ' ' || (head text) == '\t' || (head text) == '\n' = checkCommandSequence (tail text)
    | (head text) == '^' = checkReturnStatement text 0 0
    | (head text) == '[' = checkGuardCommand text 0 0
    | (head text) == '*' || (isAlpha (head text)) = checkAssignment text 0 0
    | otherwise = trace (show ("other char (cs): " ++ [head text])) False

-- TODO isolate the guard command then send back everything after
checkGuardCommand :: String -> Integer -> Integer -> Bool
checkGuardCommand text openBrackets openQuotes
    | (head text) == '[' && openBrackets == 0 && openQuotes == 0 = checkGuardCommand (tail text) 1 0 -- should happen only once at start
    | ( (head text) == '[' || (head text) == '(' || (head text) == '{' ) && openBrackets >= 1 && openQuotes == 0 = checkGuardCommand (tail text) (openBrackets + 1) openQuotes
    | (head text) == ']' && openBrackets == 1 && openQuotes == 0 = checkCommandSequence (tail text) -- should happen only one at end
    | ( (head text) == ']' || (head text) == ')' || (head text) == '}' ) && openBrackets >= 1 && openQuotes == 0 = checkGuardCommand (tail text) (openBrackets - 1) openQuotes
    | (head text) == '\"' && openQuotes == 0 = checkGuardCommand (tail text) openBrackets 1
    | (head text) == '\"' && openQuotes == 1 = checkGuardCommand (tail text) openBrackets 0
    | otherwise = trace (show ("other char (gc): " ++ [head text])) False

-- TODO as above
checkAssignment :: String -> Integer -> Integer -> Bool
checkAssignment text openBrackets openQuotes
    | otherwise = trace (show ("other char (as): " ++ [head text])) False

-- TODO as above
checkReturnStatement :: String -> Integer -> Integer -> Bool
checkReturnStatement text openBrackets openQuotes = True
    | otherwise = trace (show ("other char (rs): " ++ [head text])) False

-- dummy method
checkExpression :: String -> Bool
checkExpression text = True

-- dummy method
checkGuard :: String -> Bool
checkGuard text = True
    
parseFile :: IO ()
parseFile = do
    content <- readFile "..\\non_trivial_example.txt" --later this will load from UI
    -- print (removeComments content) -- TODO no non-exhaustiveness left, but if this is executed, the line below never prints True/False
    print (checkSyntax content)
    
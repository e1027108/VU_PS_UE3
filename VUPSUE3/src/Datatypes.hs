import Data.List
import Data.Char
import Debug.Trace

data Block a = Empty | Command a [(Block a)] deriving Show

data Command a = Guardcommand (Guard a) [(Command a)] | Assignment String (Expression a) | Returnstatement (Expression a) deriving Show

data Guard a = G (Expression a) GuardOp (Expression a) (Maybe (Guard a)) deriving Show

data GuardOp = Equals | NotEquals deriving Show

data Expression a = StringLiteral String [String] (Maybe (Expression a)) | Block a [String] (Maybe (Expression a)) | Name String [String] (Maybe (Expression a)) | Expression [String] (Maybe (Expression a)) deriving Show

{-
 NOTES:
    all checks from Block downward must receive trimped text as input!
 TODO:
    functions need index counter variable so we can send stuff to expression or guard and to later highlight errors (and rebuild code?)
    refit every function to output a Block instead of a Bool
-}

parseFile :: IO ()
parseFile = do
    content <- readFile "..\\non_trivial_example.txt" --later this will load from UI (tbr)
    print (checkSyntax content)

checkSyntax :: String -> Bool
checkSyntax text
    | length text < 2 = False
    | length text >= 2 = (checkBlock (trim (removeComments text)))

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
    | (x == '\"') = [x] ++ (removeLineComments xs (toggleQuotes inQuotes))
    | otherwise = [x] ++ (removeLineComments xs inQuotes)

checkBlock :: String -> Bool
checkBlock text
    | (head text) == '{' && (last text) == '}' = checkCommandSequence (trim (init (tail text))) -- trim all chars except first and last
    | otherwise = trace (show text) False

checkCommandSequence :: String -> Bool
checkCommandSequence text
    | (head text) == '^' = checkReturnStatement text 0 0 0
    | (head text) == '[' = checkGuardCommand text 0 0
    | otherwise = checkAssignment text

checkReturnStatement :: String -> Integer -> Integer -> Integer -> Bool
checkReturnStatement text openBrackets openQuotes gotCircumflex
    | (head text) == '^' && gotCircumflex == 0 = checkReturnStatement (tail text) openBrackets openQuotes 1
    | (head text) == '^' && gotCircumflex == 1 && openQuotes == 0 && openBrackets == 0 = False
    | ( (head text) == '[' || (head text) == '(' || (head text) == '{' ) && openQuotes == 0 = checkReturnStatement (tail text) (openBrackets + 1) 0 1
    | (head text) == '\"' = checkReturnStatement (tail text) openBrackets (toggleQuotes openQuotes) 1
    | ( (head text) == ']' || (head text) == ')' || (head text) == '}' ) && openQuotes == 0 = checkReturnStatement (tail text) (openBrackets - 1) 0 1
    | (head text) == ';' && openQuotes == 0 && openBrackets == 0 = checkCommandSequence (trim (tail text)) --TODO isolate first call with last call --> checkExpression
    | otherwise = checkReturnStatement (tail text) openBrackets openQuotes gotCircumflex
    
checkGuardCommand :: String -> Integer -> Integer -> Bool
checkGuardCommand text openBrackets openQuotes
    | (head text) == '[' && openBrackets == 0 && openQuotes == 0 = checkGuardCommand (tail text) 1 0
    | ( (head text) == '[' || (head text) == '(' || (head text) == '{' ) && openBrackets >= 1 && openQuotes == 0 = checkGuardCommand (tail text) (openBrackets + 1) openQuotes
    | (head text) == ']' && openBrackets == 1 && openQuotes == 0 = checkCommandSequence (trim (tail text)) --TODO isolate first call with last call --> checkGuard
    | ( (head text) == ']' || (head text) == ')' || (head text) == '}' ) && openBrackets >= 1 && openQuotes == 0 = checkGuardCommand (tail text) (openBrackets - 1) openQuotes
    | (head text) == '\"' = checkGuardCommand (tail text) openBrackets (toggleQuotes openQuotes)
    | otherwise = checkGuardCommand (tail text) openBrackets openQuotes

checkAssignment :: String -> Bool
checkAssignment text
    --TODO what about a "*name" expression without '=' before it?
    | (head text) == '*' = checkAssignmentName text 0
    | (isAlpha (head text)) = checkAssignmentName text 1
    | otherwise = checkAssignmentExpression text 0 0
    
checkAssignmentName :: String -> Integer -> Bool
checkAssignmentName text astDone
    | (head text) == '*' && astDone == 0 = checkAssignmentName (tail text) 0
    | (head text) == '*' && astDone == 1 = False
    | isAlpha (head text) && astDone == 0 = checkAssignmentName (tail text) 1
    | isAlphaNum (head text) && astDone == 1 = checkAssignmentName (tail text) 1
    | (head text) == '=' && astDone == 1 = checkAssignmentExpression (trim (tail text)) 0 0
    | not (isSpace (head text)) && astDone == 1 = False -- isAlphaNum and (head text) == '=' are caugth above
    | otherwise = checkAssignmentName (tail text) astDone
    
checkAssignmentExpression :: String -> Integer -> Integer -> Bool
checkAssignmentExpression text openBrackets openQuotes
    | (head text) == '{' && openQuotes == 0 = checkAssignmentExpression (tail text) (openBrackets + 1) 0
    | (head text) == '}' && openQuotes == 0 = checkAssignmentExpression (tail text) (openBrackets - 1) 0
    | (head text) == '\"' = checkAssignmentExpression (tail text) openBrackets (toggleQuotes openQuotes)
    | (head text) == ';' = checkCommandSequence (trim (tail text)) --TODO isolate first call with last call --> checkExpression
    
-- dummy method
checkExpression :: String -> Bool
checkExpression text = True

-- dummy method
checkGuard :: String -> Bool
checkGuard text = True
    
toggleQuotes :: Integer -> Integer
toggleQuotes a
    | a == 0 = 1
    | a == 1 = 0
    | otherwise = -1 -- exception? 

-- otherwise ambiguity problems and overload problems with strip
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
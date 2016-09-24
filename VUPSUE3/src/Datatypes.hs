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
    replace most bracket checks with isBracket
    do not call commandsequence within commands, but rather take command's returned indices to call a new command
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

removeLineComments :: String -> Int -> String
removeLineComments (x:xs) inQuotes
    | (length xs) == 0 = [x]
    | (x == '%') && (inQuotes == 0) = ""
    | (x == '%') && (inQuotes == 1) = [x] ++ (removeLineComments xs inQuotes)
    | (x == '\"') = [x] ++ (removeLineComments xs (toggleQuotes inQuotes))
    | otherwise = [x] ++ (removeLineComments xs inQuotes)

checkBlock :: String -> Bool
checkBlock text
    | (head text) == '{' && (last text) == '}' = checkCommandSequence (trim (init (tail text))) -- trim all chars except first and last
    | otherwise = trace (show text) False
    
checkCommandSequence :: String -> Bool
checkCommandSequence text = do
    if (length text) == 0 then
        True
    else if (head text) == '^' then do
        let rs = (checkReturnStatement text 0 0 0 0)
        if rs == -1 then
            False
        else
            -- check inner expression, then check next command
            checkExpression (trim (tail (take (rs-1) text))) && checkCommandSequence (trim (drop (rs+1) text))
    else if (head text) == '[' then do
        let gc = (checkGuardCommand text 0 0 0)
        if gc == -1 then
            False
        else
            checkGuard (trim (tail (take (gc-1) text))) && checkCommandSequence (trim (drop (gc+1) text))
    else do
        let eqid = findAssignmentEquals text 0 0 0
        if eqid /= 0 then
            if not (checkAssignmentName (take (eqid-1) text) 0) then
                False
            else do
                let asex = (checkAssignmentExpression (drop (eqid+1) text) 0 0 0)
                if asex == -1 then
                    False
                else 
                    checkExpression (trim (take (asex-1) (drop (eqid+1) text))) && checkCommandSequence (trim (drop (eqid+asex+2) text))
        else do
            let asex = checkAssignmentExpression text 0 0 0
            if asex == -1 then
                False
            else
                checkExpression (trim (take (asex-1) text)) && checkCommandSequence (trim (drop (asex+1) text))

-- -1 leads to False, everything else to True
checkReturnStatement :: String -> Int -> Int -> Int -> Int -> Int
checkReturnStatement text openBrackets openQuotes gotCircumflex id
    | (head text) == '^' && gotCircumflex == 0 = checkReturnStatement (tail text) openBrackets openQuotes 1 (id+1)
    | (head text) == '^' && gotCircumflex == 1 && openQuotes == 0 && openBrackets == 0 = -1
    | (isBracket (head text)) == 1 && openQuotes == 0 = checkReturnStatement (tail text) (openBrackets + 1) 0 1 (id+1)
    | (head text) == '\"' = checkReturnStatement (tail text) openBrackets (toggleQuotes openQuotes) 1 (id+1)
    | (isBracket (head text)) == 2 && openQuotes == 0 = checkReturnStatement (tail text) (openBrackets - 1) 0 1 (id+1)
    | (head text) == ';' && openQuotes == 0 && openBrackets == 0 = (id+1) --TODO isolate first call with last call --> checkExpression
    | otherwise = checkReturnStatement (tail text) openBrackets openQuotes gotCircumflex (id+1)
    
checkGuardCommand :: String -> Int -> Int -> Int -> Int
checkGuardCommand text openBrackets openQuotes id
    | (head text) == '[' && openBrackets == 0 && openQuotes == 0 = checkGuardCommand (tail text) 1 0 (id+1)
    | (isBracket (head text)) == 1 && openBrackets >= 1 && openQuotes == 0 = checkGuardCommand (tail text) (openBrackets + 1) openQuotes (id+1)
    | (head text) == ']' && openBrackets == 1 && openQuotes == 0 = (id+1) --TODO isolate first call with last call --> checkGuard
    | (isBracket (head text)) == 2 && openBrackets >= 1 && openQuotes == 0 = checkGuardCommand (tail text) (openBrackets - 1) openQuotes (id+1)
    | (head text) == '\"' = checkGuardCommand (tail text) openBrackets (toggleQuotes openQuotes) (id+1)
    | otherwise = checkGuardCommand (tail text) openBrackets openQuotes (id+1)

checkAssignmentName :: String -> Int -> Bool
checkAssignmentName text astDone
    | (length text) == 0 = True
    | (head text) == '*' && astDone == 0 = checkAssignmentName (tail text) 0
    | (head text) == '*' && astDone == 1 = False
    | isAlpha (head text) && astDone == 0 = checkAssignmentName (tail text) 1
    | isAlphaNum (head text) && astDone == 1 = checkAssignmentName (tail text) 1
    | isSpace (head text) && astDone == 1 = True -- name over
    | otherwise = False -- unexpected character
    
checkAssignmentExpression :: String -> Int -> Int -> Int -> Int
checkAssignmentExpression text openBrackets openQuotes id
    | (length text) == 0 = -1
    | (isBracket (head text)) == 1 && openQuotes == 0 = checkAssignmentExpression (tail text) (openBrackets + 1) 0 (id+1)
    | (isBracket (head text)) == 2 && openQuotes == 0 = checkAssignmentExpression (tail text) (openBrackets - 1) 0 (id+1)
    | (head text) == '\"' = checkAssignmentExpression (tail text) openBrackets (toggleQuotes openQuotes) (id+1)
    | (head text) == ';' && openBrackets == 0 && openQuotes == 0 = (id+1) --TODO isolate first call with last call --> checkExpression
    | otherwise = checkAssignmentExpression (tail text) openBrackets openQuotes (id+1)

findAssignmentEquals :: String -> Int -> Int -> Int -> Int
findAssignmentEquals text id openBrackets openQuotes
    | id >= (length text) = 0 -- no '='
    | (text !! id) == '=' && openBrackets == 0 && openQuotes == 0 = id -- position of '='
    | isBracket (text !! id) == 1 && openQuotes == 0 = findAssignmentEquals text (id + 1) (openBrackets + 1) 0
    | isBracket (text !! id) == 2 && openQuotes == 0 = findAssignmentEquals text (id + 1) (openBrackets - 1) 0
    | (text !! id) == '\"' = findAssignmentEquals text (id + 1) openBrackets (toggleQuotes openQuotes)
    | otherwise = findAssignmentEquals text (id + 1) openBrackets openQuotes

-- dummy method
checkExpression :: String -> Bool
checkExpression text = trace (show ("exp: " ++ text)) True --hopefully only good stuff

-- dummy method
checkGuard :: String -> Bool
checkGuard text = trace (show ("gua: " ++ text)) True
    
toggleQuotes :: Int -> Int
toggleQuotes a
    | a == 0 = 1
    | a == 1 = 0
    | otherwise = -1 -- exception?

isBracket :: Char -> Int
isBracket a
    | a == '{' || a == '(' || a == '[' = 1 -- openBracket
    | a == '}' || a == ')' || a == ']' = 2 -- closeBracket
    | otherwise = 0 -- False

-- otherwise ambiguity problems and overload problems with strip
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
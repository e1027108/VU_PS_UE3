module Datatypes where

import Data.List
import Data.List.Split
import Data.Char
import Debug.Trace
import Data.Maybe

data Block a = B (CommandSequence a) deriving Show

data CommandSequence a = Empty | CS (Command a) (Maybe (CommandSequence a)) deriving Show

data Command a = Guardcommand (Guard a) (CommandSequence a) | Assignment (Maybe String) (Expression a) | Returnstatement (Expression a) deriving Show

data Guard a = G (Expression a) GuardOp (Expression a) (Maybe (Guard a)) deriving Show

data GuardOp = Equals | NotEquals deriving Show

data Expression a = StringLiteral String [String] (Maybe (Expression a)) | Block a [String] (Maybe (Expression a)) | Name String [String] (Maybe (Expression a)) | Expression a [String] (Maybe (Expression a)) deriving Show

parseFile :: IO ()
parseFile = do
    content <- readFile "..\\code.txt"
    print (checkSyntax content)
    content <- readFile "..\\code2.txt"
    print (checkSyntax content)
    content <- readFile "..\\non_trivial_example.txt"
    print (checkSyntax content)

checkSyntax :: String -> Bool
checkSyntax text
    | length text < 2 = False
    | length text >= 2 = (snd (checkBlock (trim (removeComments text))))

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

checkBlock :: String -> (Block String, Bool)
checkBlock text = do
    if (head text) == '{' && (last text) == '}' then do
        let bl = (checkCommandSequence (trim (init (tail text))))
        (B (fst bl), snd bl)
    else
        ( B Empty, False)
    
checkCommandSequence :: String -> (CommandSequence String, Bool)
checkCommandSequence text = do
    if (length text) == 0 then
        (Empty, True)
    else if (head text) == '^' then do
        let rs = (checkReturnStatement text 0 0 0 0)
        if rs == -1 then
            (Empty, False)
        else do
            let chex = checkExpression (trim (tail (take (rs-1) text)))
            let chcs = checkCommandSequence (trim (drop (rs+1) text))
            ( CS (Returnstatement (fst chex)) (Just (fst chcs)), (snd chex && snd chcs))
    else if (head text) == '[' then do
        let gc = (checkGuardCommand text 0 0 0)
        let ci = (findGuardColon (take (gc-1) text) 0 0 0)
        if (gc == -1) || (ci == 0) then
            (Empty, False)
        else do
            let chgu = checkGuard (trim (take (ci-1) (tail text)))
            let chin = checkCommandSequence (trim (drop (ci+1) (take (gc-1) text)))
            let chou = checkCommandSequence (trim (drop (gc) text))
            ( CS (Guardcommand (fst chgu) (fst chin)) (Just(fst chou)), (snd chgu && snd chin && snd chou))
    else do
        let eqid = findAssignmentEquals text 0 0 0
        if eqid /= 0 then
            if not (checkAssignmentName (take (eqid-1) text) 0) then
                (Empty, False)
            else do
                let asex = (checkAssignmentExpression (drop (eqid+1) text) 0 0 0)
                if asex == -1 then
                    (Empty, False)
                else do
                    let chex = checkExpression (trim (take (asex-1) (drop (eqid+1) text)))
                    let chcs = checkCommandSequence (trim (drop (eqid+asex+2) text))
                    ( CS (Assignment (Just(take (eqid-1) text)) (fst chex)) (Just(fst chcs)), (snd chex && snd chcs))
        else do
            let asex = checkAssignmentExpression text 0 0 0
            if asex == -1 then
                (Empty, False)
            else do
                let chex = checkExpression (trim (take (asex-1) text))
                let chcs = checkCommandSequence (trim (drop (asex+1) text))
                (CS (Assignment Nothing (fst chex)) (Just (fst chcs)), (snd chex && snd chcs))

checkReturnStatement :: String -> Int -> Int -> Int -> Int -> Int
checkReturnStatement text openBrackets openQuotes gotCircumflex id
    | (head text) == '^' && gotCircumflex == 0 = checkReturnStatement (tail text) openBrackets openQuotes 1 (id+1)
    | (head text) == '^' && gotCircumflex == 1 && openQuotes == 0 && openBrackets == 0 = -1
    | (isBracket (head text)) == 1 && openQuotes == 0 = checkReturnStatement (tail text) (openBrackets + 1) 0 1 (id+1)
    | (head text) == '\"' = checkReturnStatement (tail text) openBrackets (toggleQuotes openQuotes) 1 (id+1)
    | (isBracket (head text)) == 2 && openQuotes == 0 = checkReturnStatement (tail text) (openBrackets - 1) 0 1 (id+1)
    | (head text) == ';' && openQuotes == 0 && openBrackets == 0 = (id+1)
    | otherwise = checkReturnStatement (tail text) openBrackets openQuotes gotCircumflex (id+1)
    
checkGuardCommand :: String -> Int -> Int -> Int -> Int
checkGuardCommand text openBrackets openQuotes id
    | (head text) == '[' && openBrackets == 0 && openQuotes == 0 = checkGuardCommand (tail text) 1 0 (id+1)
    | (isBracket (head text)) == 1 && openBrackets >= 1 && openQuotes == 0 = checkGuardCommand (tail text) (openBrackets + 1) openQuotes (id+1)
    | (head text) == ']' && openBrackets == 1 && openQuotes == 0 = (id+1)
    | (isBracket (head text)) == 2 && openBrackets >= 1 && openQuotes == 0 = checkGuardCommand (tail text) (openBrackets - 1) openQuotes (id+1)
    | (head text) == '\"' = checkGuardCommand (tail text) openBrackets (toggleQuotes openQuotes) (id+1)
    | otherwise = checkGuardCommand (tail text) openBrackets openQuotes (id+1)
    
findGuardColon :: String -> Int -> Int -> Int -> Int
findGuardColon text id openBrackets openQuotes
    | id >= (length text) = 0 -- no ':' outside of brackets
    | (text !! id) == ':' && openBrackets == 1 && openQuotes == 0 = id -- position of first outside ':'
    | isBracket (text !! id) == 1 && openQuotes == 0 = findGuardColon text (id + 1) (openBrackets + 1) 0
    | isBracket (text !! id) == 2 && openQuotes == 0 = findGuardColon text (id + 1) (openBrackets - 1) 0
    | (text !! id) == '\"' = findGuardColon text (id + 1) openBrackets (toggleQuotes openQuotes)
    | otherwise = findGuardColon text (id + 1) openBrackets openQuotes

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

checkGuard :: String -> (Guard String, Bool)
checkGuard text = do
    let (guardOpParam, exprIndex) = (determineEndIndexForGuard text 0 0 0 0)
    if (exprIndex > -1)
        then do
            let expr1 = fst (splitAt (exprIndex - 1) text)
            let expr2 = snd (splitAt (exprIndex + 2) text)
            let expr2Index = (determineEndIndex expr2 0 0 0 0)
            if (expr2Index > -1)
                then do
                    if ((elemIndex ',' expr2) /= Nothing)
                        then do                                
                            let newexpr2 = fst (splitAt ((fromJust(elemIndex ',' expr2)) - 1) expr2)
                            let guard = snd (splitAt (fromJust(elemIndex ',' expr2) + 2) expr2)
                            let checkExpre1 = checkExpression expr1
                            let checkExpre2 = checkExpression newexpr2
                            let checkG = checkGuard guard                            
                            if ((snd checkExpre1) && (snd checkExpre2) && (snd checkG))
                                then do
                                    ((G (fst checkExpre1) (getGuardOP guardOpParam) (fst checkExpre2) (Just(fst checkG))),True)                                    
                            else
                                ((G (Name "" [] Nothing) Equals (Name "" [] Nothing) Nothing), False)
                    else do
                            let (checkExpr1,b1) = checkExpression expr1
                            let (checkExpr2,b2) = checkExpression expr2                        
                            if (b1 && b2)
                                then do
                                    ((G (checkExpr1) (getGuardOP guardOpParam) (checkExpr2) Nothing), True)                                   
                            else
                                ((G (Name "" [] Nothing) Equals (Name "" [] Nothing) Nothing), False)
            else
                ((G (Name "" [] Nothing) Equals (Name "" [] Nothing) Nothing), False)
    else
        ((G (Name "" [] Nothing) Equals (Name "" [] Nothing) Nothing), False)    
    
checkExpression :: String -> (Expression String, Bool)
checkExpression text = do
    let exprIndex = (determineEndIndex text 0 0 0 0)
    if (exprIndex > -1)
        then do
            let expr1 = fst (splitAt exprIndex text)
            if (checkExprPart1 expr1)
                then do
                    let names = snd (splitAt exprIndex text)
                    if ((names /= "") && (head names) == '.')
                        then do
                            let expr2Index = elemIndex '+' names
                            if (expr2Index /= Nothing)
                                then do
                                    let newnames = splitOn "." (fst (splitAt (fromJust(expr2Index) -1) names))
                                    let optexpr = snd (splitAt (fromJust(expr2Index) +2) names)
                                    if (all checkNames (tail newnames))
                                        then do
                                            if snd(checkExpression optexpr)
                                                then do
                                                    ((fillExprDataStructure expr1 (tail newnames) (Just((fst(checkExpression optexpr))))), True)                                                   
                                                else
                                                    ((Name "" [] Nothing), False)
                                            else
                                                ((Name "" [] Nothing), False)
                            else do
                                let newnames2 = (splitOn "." names)
                                if(all checkNames (tail newnames2))
                                    then do
                                        ((fillExprDataStructure expr1 (tail newnames2) Nothing), True)
                                    else
                                    ((Name "" [] Nothing), False)                               
                    else
                        if ((elemIndex '+' names) /= Nothing)
                            then do
                                let optexpr = snd(splitAt ((fromJust (elemIndex '+' names)) + 2) names)
                                ((fillExprDataStructure expr1 [] (Just((fst(checkExpression optexpr))))), True)
                        else
                            ((fillExprDataStructure expr1 [] Nothing), True)
            else
                ((Name "" [] Nothing), False)
    else
        ((Name "" [] Nothing), False)

fillExprDataStructure :: String -> [String] -> Maybe (Expression String) -> Expression String
fillExprDataStructure expr1 names expr2
    | (head(expr1) == '\"') = ((StringLiteral expr1) names expr2)
    | (head(expr1) == '{') = ((Block expr1) names expr2)
    | (head(expr1) == '(') = ((Expression (init(drop 1 expr1))) names expr2)
    | (head(expr1) == '*' || (isAlpha(head expr1))) = ((Name expr1) names expr2)
    
    -- syntax example of an Expression + Expression : Name "name" ["name1","name2"] (Just (StringLiteral "abc" [] Nothing))
    -- syntax example of an Expression without snd expression: Name "name" ["name1","name2"] Nothing
        
determineEndIndex :: String -> Int -> Int -> Int -> Int -> Int
determineEndIndex text openQuotes openBlock openBrackets index
    | ((text) == []) && ((openQuotes /= 0) || (openBlock /= 0) || (openBrackets /= 0)) = -1
    | ((text) == []) && (openQuotes == 0) && (openBlock == 0) && (openBrackets == 0) = index
    | (((head text) == ' ') || ((head text) == '.')) && (openQuotes == 0) && (openBlock == 0) && (openBrackets == 0) = (index)
    
    | (index >= 0) && ((head text) == '\"' && openQuotes == 0) && (openBlock == 0) && (openBrackets == 0) = determineEndIndex (tail text) (openQuotes + 1) openBlock openBrackets (index + 1)
    | (index >= 0) && ((head text) == '\"' && openQuotes == 1) && (openBlock == 0) && (openBrackets == 0)= (index + 1)
        
    | (index >= 0) && ((head text) == '{' && openBlock == 0) && (openQuotes == 0) && (openBrackets == 0) = determineEndIndex (tail text) openQuotes (openBlock + 1) openBrackets (index + 1)
    | (index >= 0) && ((head text) == '{' && openBlock >= 1) = determineEndIndex (tail text) openQuotes (openBlock + 1) openBrackets (index + 1)
    | (index >= 0) && ((head text) == '}' && openBlock == 1) = (index + 1)
    | (index >= 0) && ((head text) == '}' && openBlock > 1) = determineEndIndex (tail text) openQuotes (openBlock - 1) openBrackets (index + 1)
    
    | (index >= 0) && ((head text) == '(' && openBrackets == 0) && (openQuotes == 0) && (openBlock == 0) = determineEndIndex (tail text) openQuotes openBlock (openBrackets + 1) (index + 1)
    | (index >= 0) && ((head text) == '(' && openBrackets >= 1) = determineEndIndex (tail text) openQuotes openBlock (openBrackets + 1) (index + 1)
    | (index >= 0) && ((head text) == ')' && openBrackets == 1) = (index + 1)
    | (index >= 0) && ((head text) == ')' && openBrackets > 1) = determineEndIndex (tail text) openQuotes openBlock (openBrackets - 1) (index + 1)
    
    | otherwise = determineEndIndex (tail text) openQuotes openBlock openBrackets (index + 1)

determineEndIndexForGuard :: String -> Int -> Int -> Int -> Int -> (Bool, Int)
determineEndIndexForGuard text openQuotes openBlock openBrackets index
    | ((text) == []) && ((openQuotes /= 0) || (openBlock /= 0) || (openBrackets /= 0)) = (False, -1)
    | ((text) == []) && (openQuotes == 0) && (openBlock == 0) && (openBrackets == 0) = (False, -1)
    | ((head text) == '=') && (openQuotes == 0) && (openBlock == 0) && (openBrackets == 0) = (True, index)
    | ((head text) == '#') && (openQuotes == 0) && (openBlock == 0) && (openBrackets == 0) = (False, index)
    | (index >= 0) && ((head text) == '\"' && openQuotes == 0) && (openBlock == 0) && (openBrackets == 0) = determineEndIndexForGuard (tail text) (openQuotes + 1) openBlock openBrackets (index + 1)
    | (index >= 0) && ((head text) == '\"' && openQuotes == 1) && (openBlock == 0) && (openBrackets == 0) = determineEndIndexForGuard (tail text) (openQuotes - 1) openBlock openBrackets (index + 1)
        
    | (index >= 0) && ((head text) == '{' && openBlock == 0) && (openQuotes == 0) && (openBrackets == 0) = determineEndIndexForGuard (tail text) openQuotes (openBlock + 1) openBrackets (index + 1)
    | (index >= 0) && ((head text) == '{' && openBlock >= 1) = determineEndIndexForGuard (tail text) openQuotes (openBlock + 1) openBrackets (index + 1)
    | (index >= 0) && ((head text) == '}' && openBlock >= 1) = determineEndIndexForGuard (tail text) openQuotes (openBlock - 1) openBrackets (index + 1)
    
    | (index >= 0) && ((head text) == '(' && openBrackets == 0) && (openQuotes == 0) && (openBlock == 0) = determineEndIndexForGuard (tail text) openQuotes openBlock (openBrackets + 1) (index + 1)
    | (index >= 0) && ((head text) == '(' && openBrackets >= 1) = determineEndIndexForGuard (tail text) openQuotes openBlock (openBrackets + 1) (index + 1)
    | (index >= 0) && ((head text) == ')' && openBrackets >= 1) = determineEndIndexForGuard (tail text) openQuotes openBlock (openBrackets - 1) (index + 1)
    
    | otherwise = determineEndIndexForGuard (tail text) openQuotes openBlock openBrackets (index + 1)
    
checkExprPart1 :: String -> Bool
checkExprPart1 text
    | ((head text) == '\"') && ((last text) == '\"') = True
    | ((head text) == '{') && ((last text) == '}') = (snd(checkBlock text))
    | ((head text) == '(') && ((last text) == ')') = (snd(checkExpression (init(drop 1 text))))
    | ((head text) == '*') || (isAlpha(head text)) = True
    | otherwise = False

checkNames :: String -> Bool
checkNames text
    | (isAlpha(head text)) = True
    | otherwise = False
    
getGuardOP :: Bool -> GuardOp
getGuardOP bool
    | (bool) = Equals
    | not(bool) = NotEquals
    
toggleQuotes :: Int -> Int
toggleQuotes a
    | a == 0 = 1
    | a == 1 = 0
    | otherwise = -1

isBracket :: Char -> Int
isBracket a
    | a == '{' || a == '(' || a == '[' = 1 -- openBracket
    | a == '}' || a == ')' || a == ']' = 2 -- closeBracket
    | otherwise = 0 -- False

-- otherwise ambiguity problems and overload problems with strip
trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
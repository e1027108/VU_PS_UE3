import Data.List
import Debug.Trace

data Block a = Empty | Command a [(Block a)] deriving Show

data Command a = Guardcommand (Guard a) [(Command a)] | Assignment String (Expression a) | Returnstatement (Expression a) deriving Show

data Guard a = G (Expression a) GuardOp (Expression a) (Maybe (Guard a)) deriving Show

data GuardOp = Equals | NotEquals deriving Show

data Expression a = StringLiteral String [String] (Maybe (Expression a)) | Block a [String] (Maybe (Expression a)) | Name String [String] (Maybe (Expression a)) | Expression [String] (Maybe (Expression a)) deriving Show

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
    | (x == '%') && (inQuotes == 0) = trace (show ("comment: " ++ [x] ++ xs)) ""
    | (x == '%') && (inQuotes == 1) = trace (show ("rest: " ++ [x] ++ xs)) ([x] ++ (removeLineComments xs inQuotes))
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
    
parseFile :: IO ()
parseFile = do
    content <- readFile "..\\non_trivial_example.txt" --later this will load from UI
    -- print (removeComments content) -- TODO no non-exhaustiveness left, but if this is executed, the line below never prints True/False
    print (checkSyntax content)
    
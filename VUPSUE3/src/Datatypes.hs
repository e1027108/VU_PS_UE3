import Data.List

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
removeComments text = (removeCommentsLineByLine (lines text)) -- should now work whole text now

removeCommentsLineByLine :: [String] -> String
removeCommentsLineByLine list = (removeLineComments (head list) 0) ++ (removeCommentsLineByLine (tail list))

--this is non-exhaustive for ghci use, since this would need an additional escape character, but works with files
removeLineComments :: String -> Integer -> String
removeLineComments (x:xs) inComments
    | (x == '%') && (inComments == 0) = ""
    | (x == '\"') && (inComments == 0) = [x] ++ (removeLineComments xs 1)
    | (x == '\"') && (inComments == 1) = [x] ++ (removeLineComments xs 0)
    | otherwise = [x] ++ (removeLineComments xs 0)
    
checkBlock :: String -> Bool
checkBlock text
    | otherwise = True
    
parseFile :: IO ()
parseFile = do
    content <- readFile "..\\non_trivial_example.txt" --later this will load from UI
    --print (removeComments content) -- comment for use/decomment for testing, TODO currently creates non-exhaustive error
    print (checkSyntax content)
    
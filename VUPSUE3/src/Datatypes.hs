data Block = Command deriving Show

data Command = Assignment Guard Colon Command | Returnstatement deriving Show

data Guard = GuardPart1 GuardOp Expression Expression deriving Show

data Expression = StringLiteral | Block | Name | Expression deriving Show

data GuardOp = Equals | NotEquals deriving Show

data Returnstatement = R1 ReturnOperator Expression Semicolon deriving Show


type Colon = Char
type ReturnOperator = Char
type Semicolon = Char
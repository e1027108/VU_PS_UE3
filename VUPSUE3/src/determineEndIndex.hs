determineEndIndex :: String -> Int -> Int -> Int -> Int -> Int
determineEndIndex text openQuotes openBlock openBrackets index
    | (index == 0 && not((head text) `elem` ['\"','{','('])) = (length text)
    | ((text) == []) = -1

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
    
    | otherwise = determineEndIndex (tail text) openQuotes openBlock openBrackets (index+1)
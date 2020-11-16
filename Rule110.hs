initialInput = "   #  ## #####"

newInput = 
    concat [
        " " | x <- [0.. 200]
    ] ++ initialInput ++ 
    concat [
        " " | x <- [0.. 200]
    ]


determineNextGen currentGen 
    | currentGen == "###" = " "
    | currentGen == "## " = "#"
    | currentGen == "# #" = "#"
    | currentGen == "#  " = " "
    | currentGen == " ##" = "#"
    | currentGen == " # " = "#"
    | currentGen == " # " = "#"
    | currentGen == " # " = "#"
    | currentGen == "  #" = "#"
    | otherwise = " "
    
getNewGen input 2 result = result ++ tail input
getNewGen (firstSymbol : secondSymbol : thirdSymbol : restOfInput) length result = 
    let newbornGen = determineNextGen (firstSymbol : secondSymbol : [thirdSymbol])
    in getNewGen (secondSymbol : thirdSymbol : restOfInput) (length - 1) (result ++ newbornGen)

generateNextGen :: (Eq t1, Num t1, Eq t2, Num t2) => [Char] -> t2 -> t1 -> [Char] -> [Char]
generateNextGen _ _ 0 result = result
generateNextGen initialInput lengthOfInput leftGens result = 
    let newGen = getNewGen initialInput (lengthOfInput - 1) [head initialInput]
    in generateNextGen newGen lengthOfInput (leftGens - 1) $ result ++ newGen ++ "\n"
    
main = putStr (generateNextGen newInput (length newInput) 1764 [])
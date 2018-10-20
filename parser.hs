import Data.Char

data AExpr = Var String | AVal Integer | Plus AExpr AExpr | AEncaps AExpr deriving Show
data BExpr = BTrue | BFalse | And BExpr BExpr | Greater AExpr AExpr | BEncaps BExpr deriving Show
data Stmt = Void | While BExpr Stmt | Equal String AExpr | If BExpr Stmt Stmt | Colon Stmt Stmt deriving Show
data Parser a = Parser {apply :: String -> [(String, a)]}

whitespace = [' ', '\n', '\t']

parseChar :: (Char -> Bool) -> Parser Char
parseChar f = Parser (\s -> case s of
                                [] -> []
                                x:xs -> if f x then [(xs, x)] else [])



(.||.) :: (Parser a) -> (Parser a) -> (Parser a)
p1 .||. p2 = Parser (\s -> case apply p1 s of
                        [] -> apply p2 s
                        x -> x)

bind :: (Parser a) -> (a -> Parser b) -> (Parser b)
p1 `bind` f = Parser (\s -> case apply p1 s of
                                    [] -> []
                                    [(r, s)] -> apply (f s) r)

rreturn :: a -> Parser a
rreturn x = Parser (\s -> [(s, x)]) 

plus :: (Parser a) -> (Parser [a])
plus p = p `bind` (\x -> (star p) `bind` (\y -> rreturn (x:y)))

star :: (Parser a) -> (Parser [a])
star p = (plus p) .||. (rreturn [])

parseWs :: Parser String
parseWs = plus (parseChar (\x -> x `elem` whitespace)) 

parseAlpha = parseChar (isAlpha)
parseAlphaNum = parseChar (isAlphaNum)

parseDigit = parseChar (isDigit)

--un char incadrat de white spaces
parseCharWs :: (Char -> Bool) -> (Parser Char)
parseCharWs f = (star parseWs) `bind` (\_ -> (parseChar f) `bind` (\y -> star parseWs `bind` (\_ -> rreturn y))) 

parseVar :: Parser AExpr
parseVar = (plus (parseAlpha) `bind` (\x -> (star parseAlphaNum) `bind` (\y -> rreturn (x ++ y)))) `bind` (\x -> rreturn (Var x))

-- '?' din regex
mostlyOnce :: (Parser a) -> (Parser [a])
mostlyOnce p = (p `bind` (\x -> rreturn [x])) .||. (rreturn [])

parseAVal :: Parser AExpr
parseAVal = ((mostlyOnce (parseChar (== '-'))) `bind` (\x -> (plus parseDigit) `bind` (\y -> rreturn (x++y)))) `bind` (\x -> rreturn (AVal (read x :: Integer)))

--{whitespace}*+{whitespace}*({variable}|{number})
parseSemiSum :: Parser AExpr
parseSemiSum = (parseCharWs (== '+')) `bind` (\_ -> (parseVar .||. parseAVal) `bind` (\y -> rreturn (y)))


multiplePlus = foldr Plus (AVal 0) 

parseSum :: Parser AExpr
parseSum = (parseVar .||. parseAVal) `bind` (\x -> (plus (parseSemiSum) `bind` (\y -> rreturn (multiplePlus y))) `bind` (\y -> rreturn (Plus x y)))

parseAExpr :: Parser AExpr
parseAExpr = (parseSum .||. parseVar) .||. parseAVal

parseEqual :: Parser Stmt
parseEqual = parseVar `bind` (\(Var x) -> parseCharWs (== '=') `bind` (\_ -> parseAExpr `bind` (\y -> rreturn (Equal x y))))

parseColon :: Parser Stmt
parseColon = (parseCharWs (== ';')) `bind` (\_ -> parseEqual)

multipleColons = foldr Colon (Void)

parseInstr :: Parser Stmt
parseInstr = parseEqual `bind` (\x -> (plus (parseColon) `bind` (\y -> rreturn (multipleColons y))) `bind` (\y -> rreturn (Colon x y)))


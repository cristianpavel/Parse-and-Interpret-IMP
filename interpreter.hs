data AExpr = Var String | AVal Integer | Plus AExpr AExpr | AEncaps AExpr deriving Show
data BExpr = BTrue | BFalse | And BExpr BExpr | Greater AExpr AExpr | BEncaps BExpr deriving Show
data Stmt = Void | While BExpr Stmt | Equal String AExpr | If BExpr Stmt Stmt | Colon Stmt Stmt deriving Show
data Prog = Prog [String] Stmt deriving Show

data Tree = Null | Two Token Tree Tree deriving Show
data Token = TIf | TWhile | TEqual | TOpen | TClose | TPlus | TVar String | TAVal Integer | TBVal Bool | TColon | TGreater | TAnd | TBlockO| TBlockE |TElse deriving Show

numbers = ['0','1','2','3','4','5','6','7','8','9']
whitespace = [' ', '\n', '\t']
alfabeta = ['a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','x','y','z']
alfaNumeric = (numbers ++ alfabeta) 

parse [] = []
parse l@(x:xs) = if (x `elem` whitespace) then parse xs else prse l
              where prse ('w':'h':'i':'l':'e':xs) = TWhile:(parse xs)
                    prse ('i':'f':xs) = TIf:(parse xs)
                    prse ('(':xs) = TOpen:(parse xs)
                    prse (')':xs) = TClose:(parse xs)
                    prse ('=':xs) = TEqual:(parse xs)
                    prse ('{':xs) = TBlockO:(parse xs)
                    prse ('}':xs) = TBlockE:(parse xs)
                    prse (';':xs) = TColon:(parse xs)
                    prse ('>':xs) = TGreater:(parse xs)
                    prse ('&':'&':xs) = TAnd:(parse xs)
                    prse ('+':xs) = TPlus:(parse xs)
                    prse ('T':'r':'u':'e':xs) = (TBVal True):(parse xs)
                    prse ('F':'a':'l':'s':'e':xs) = (TBVal False):(parse xs)
                    prse l@(x:xs) = if (x `elem` numbers || x == '-') then let (h, t) = (parseNumber l) in (h:(parse t)) else let (h, t) = (parseVariable l) in (h:(parse t))

parseNumber ('-':l) = let (TAVal x, t) = (parseNumber l) in (TAVal (-x), t)
parseNumber l = let (h, t) = prseNumber (l++[' ']) in (TAVal (read h :: Integer), t) 
            where prseNumber (x:xs)
                    | (x `elem` numbers) = let (h, t) = (prseNumber xs) in (x:h, t)
                    | otherwise = ([], x:xs)

parseVariable l = let (h, t) = (prseVariable (l++[' '])) in (TVar h, t)
                    where prseVariable (x:xs)
                            | (x `elem` alfaNumeric) = let (h, t) = (prseVariable xs) in (x:h, t)
                            | otherwise = ([], x:xs)

addNodeA t Null = Two t Null Null
addNodeA t l@(Two TPlus _ _) = Two t Null l
addNodeA t l@(Two (TAVal _) _ _) = Two t Null l
addNodeA t l@(Two (TVar _) _ _) = Two t Null l
addNodeA t (Two x l r) = Two x (addNodeA t l) r



addNodeB t Null = Two t Null Null
addNodeB t l@(Two TAnd _ _) = Two t Null l
addNodeB t l@(Two TGreater _ _) = Two t Null l
addNodeB t l@(Two (TBVal _) _ _) = Two t Null l
addNodeB t (Two x l r) = Two x (addNodeB t l) r


addNodeLeft t Null = Two t Null Null
addNodeLeft t (Two x l r) = Two x (addNodeLeft t l) r

addTree t Null = t
addTree t (Two x Null r) = Two x t r
addTree t (Two x l r) = Two x (addTree t l) r

getBool (TOpen:l) = let (h,t) = (getBool l)
                        (h2,t2) = (getBool t) 
                  in (addTree h h2, t2)
getBool [] = (Null, [])
getBool (TClose:l) = (Null, l)
getBool ((TVar x):l) = let (h,t) = getBool l in (addNodeLeft (TVar x) h, t)
getBool ((TAVal x):l) = let (h,t) = getBool l in (addNodeLeft (TAVal x) h, t)
getBool ((TBVal x):l) = let (h,t) = getBool l in (addNodeLeft (TBVal x) h, t)
getBool (TAnd:l) = let (h,t) = getBool l in (addNodeB TAnd h, t)
getBool (TGreater:l) = let (h,t) = getBool l in (addNodeA TGreater h, t)
                      

getA (TOpen:l) = let (h,t) = getA l
                     (h2,t2) = getA t 
                  in (addTree h h2, t2)
getA [] = (Null, [])
getA (TClose:l) = (Null, l)
getA ((TVar x):l) = let (h,t) = getA l in (addNodeLeft (TVar x) h, t)
getA ((TAVal x):l) = let (h,t) = getA l in (addNodeLeft (TAVal x) h, t)
getA (TPlus:l) = let (h,t) = getA l in (addNodeA TPlus h, t)

getBlock [] = (Null, [])
getBlock (TBlockO:l) = getBlock l
getBlock (TBlockE:l) = (Null, l)
getBlock ((TVar x):l) = let (h,t) = getBlock l in (addNodeLeft (TVar x) h, t)
getBlock (TEqual:l) = let (h, t) = getBlock l in (addNodeA TEqual h, t)
getBlock ((TAVal x):l) = let (h,t) = getBlock l in (addNodeLeft (TAVal x) h, t)
getBlock (TPlus:l) = let (h,t) = getBlock l in (addNodeA TPlus h, t)
getBlock (TWhile:TOpen:l) = let (h,t) = getBool l
                                (h2,t2) = getBlock t
                                (h3, t3) = getBlock t2
                            in (addTree (Two TWhile h h2) h3, t3)
getBlock (TIf:TOpen:l) =  let (h,t) = getBool l
                              (h2,t2) = getBlock t
                              (h3, t3) = getBlock t2
                              (h4, t4) = getBlock t3
                          in (addTree (Two TIf h (Two TElse h2 h3)) h4, t3)
getBlock (TColon:l) = let (h,t) = getBlock l in (Two TColon Null h, t)
getBlock (TOpen:l) = let (h,t) = (getA l)
                         (h2,t2) = (getBlock t)
                     in (addTree h h2, t2)




buildStmt [] = Null
buildStmt ((TVar x):l) = addNodeLeft (TVar x) (buildStmt l)
buildStmt (TEqual:l) = addNodeA (TEqual) (buildStmt l)
buildStmt ((TAVal x):l) = addNodeLeft (TAVal x) (buildStmt l)
buildStmt (TPlus:l) = addNodeA (TPlus) (buildStmt l)
buildStmt (TWhile:TOpen:l) = let (h,t) = (getBool l)
                                 (h2,t2) = (getBlock t)
                              in  addTree (Two TWhile h h2) (buildStmt t2)
buildStmt (TIf:TOpen:l) = let (h,t) = (getBool l)
                              (h2,t2) = (getBlock t)
                              (h3, t3) = (getBlock t2)
                          in addTree (Two TIf h (Two TElse h2 h3)) (buildStmt t3) 
buildStmt (TColon:l) = Two TColon Null (buildStmt l)
buildStmt (TOpen:l) = let (h,t) = (getA l) in addTree h (buildStmt t)

convertA (Two (TVar x) _ _) = Var x
convertA (Two (TAVal x) _ _) = AVal x
convertA (Two TPlus x y) = Plus (convertA x) (convertA y)

convertB (Two (TBVal x) _ _) = if (x == True) then BTrue else BFalse
convertB (Two TAnd x y) = And (convertB x) (convertB y)
convertB (Two TGreater x y) = Greater (convertA x) (convertA y)


convert Null = Void
convert (Two TColon l r) = Colon (convert l) (convert r)
convert (Two TWhile l r) = While (convertB l) (convert r)
convert (Two TIf b (Two TElse l r)) = If (convertB b) (convert l) (convert r)
convert (Two TEqual l r) = let (Var x) = convertA l
                            in Equal x (convertA r)


getValue :: String -> [(String, Integer)] -> Integer  --aici se poate pune un maybe (dar presupun ca verificarea se face la parser)
getValue key m = let l = filter (\t -> (fst t) == key) m
                    in (snd.head) l
                   
evalAExpr (AVal v) _ = v
evalAExpr (Var x) m = getValue x m
evalAExpr (Plus l r) m = (evalAExpr l m) + (evalAExpr r m)
evalAExpr (AEncaps x) m = evalAExpr x m

evalBExpr BTrue _  = True
evalBExpr BFalse _ = False
evalBExpr (And l r) m =  (evalBExpr l m) &&  (evalBExpr r m)
evalBExpr (Greater l r) m =  (evalAExpr l m) > (evalAExpr r m) 
evalBExpr (BEncaps x) m = evalBExpr x m

eval Void m = m
eval (Colon l r) m = let after_left = (eval l m)
                       in eval r after_left
eval (If cond t f) m = if (evalBExpr cond  m) then eval t m else eval f m 
eval (While cond t) m = if (evalBExpr cond m) then eval (Colon t (While cond t)) m else m
eval (Equal x y) m = let el = filter (\h -> fst h /= x) m
                        in (x, (evalAExpr y m)):el

evalProg (Prog m s) = let t = foldr (\h t -> (h, 0):t) []  m
                        in eval s t


run s = eval (convert (buildStmt (parse s))) []

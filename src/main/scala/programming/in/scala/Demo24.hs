module Demo3 where

import Data.List(permutations,nub)

main = print $ twentyfour [4.0,2.0,3.0,1.0]

data Exp = Val Double 
    | Plus Exp Exp
    | Sub Exp Exp
    | Mult Exp Exp
    | Div Exp Exp deriving (Show,Eq)

eval :: Exp -> Double
eval (Val a) = a
eval (Plus a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mult a b) = eval a * eval b
eval (Div a b) = eval a / eval b

showExp :: Exp -> String
showExp (Val a) = show a
showExp (Plus a b) =   showExp a ++ "+" ++ showExp b 
showExp (Sub a b) =  showExp a ++ "-" ++ showExp b 
showExp (Mult a b) =  
    if level a == 0 && brace a then 
        if level b == 0 && brace b then 
            "(" ++ showExp a  ++ ")*(" ++ showExp b  ++ ")" 
        else "(" ++ showExp a  ++ ")*" ++ showExp b  
    else 
        if level b == 0 && brace b then 
            showExp a  ++ "*(" ++ showExp b  ++ ")" 
        else showExp a  ++ "*" ++ showExp b 
showExp (Div a b) =  
    if level a == 0 && brace a then 
        if level b == 0 && brace b then 
            "(" ++ showExp a  ++ ")/(" ++ showExp b  ++ ")" 
        else "(" ++ showExp a  ++ ")/" ++ showExp b  
    else 
        if level b == 0 && brace b then 
            showExp a  ++ "/(" ++ showExp b  ++ ")" 
        else showExp a  ++ "/" ++ showExp b  

level :: Exp -> Int
level (Val a) = 0
level (Plus a b) = level a + level b
level (Sub a b) = level a + level b
level (Mult a b) = 1 + level a + level b
level (Div a b) = 1 + level a + level b

brace :: Exp -> Bool
brace (Val a) = False
brace (Plus a b) = True
brace (Sub a b) = True
brace (Mult a b) = True
brace (Div a b) = True 


divide :: [a] -> [([a],[a])]
divide xs = [(take n xs,drop n xs) | n <- [1 .. (length xs - 1)]]

buildExpressions :: ([Exp],[Exp]) -> [Exp]
buildExpressions (es1,es2) = 
    [op e1 e2 | e1 <- es1,e2 <- es2,op <- [Plus,Sub,Mult,Div]]

toExpression :: [Double] -> [Exp]
toExpression [] = []
toExpression [x] = [Val x]
toExpression xs = concat
    [buildExpressions (toExpression l,toExpression r) | (l,r) <- divide xs]

generate :: [Double] -> [Exp]
generate ns = concatMap toExpression (permutations ns)

twentyfour :: [Double] -> [String]
twentyfour ns = nub [showExp x | x <- generate ns,eval x == 24.0]






import Data.Char
import Data.List
import Data.Bits


media3 :: (Int,Int,Int) -> Int
media3(x,y,z) = div (x + y + z) 3 

maior2 :: (Int,Int) -> Int
maior2(x,y) = if(x>y) then x else y

segmaior3 :: Int -> Int -> Int -> Int
segmaior3 x y z = if x>y && x<z
                  then x
                  else if x>z && x<y
                  then x
                  else if y>x && y<z
                  then y
                  else if y>z && y<x
                  then y
                  else z

triplo :: Int -> Int
triplo(x) = 3 * x

maiorDeTres :: (Int,Int,Int) -> Int
maiorDeTres(x,y,z) =
    if (x>y && x>z) then x
        else if (y>x && y>z) then y
            else z

menor3 :: Int -> Int -> Int -> Int
menor3 x y z = if x<y && x<z
            then x
            else if y<x && y<z
                    then y
                else z

somatorio:: (Int) -> Int
somatorio 1 = 1
somatorio n = somatorio(n - 1) + n

nEsimoTermoPA:: (Int,Int,Int) -> Int
nEsimoTermoPA(x,y,n) = x + (n-1)*y

nEsimoTermoPG:: (Int,Int,Int) -> Int
nEsimoTermoPG(x,y,n) = x * (y^(n-1))

somaPA:: (Int,Int,Int) -> Int
somaPA(x,y,1) = x
somaPA(x,y,z) = somaPA(x,y,1) + somaPA(x+y,y,z-1)

somaPG:: (Int,Int,Int) -> Int
somaPG(x,y,z) = (x * ((y^z)-1)) `div` (y-1)

fibonacci :: Int -> Int
fibonacci n
    |n == 1 = 1
    |n == 2 = 1
    |n >= 3 = (fibonacci(n-1) + fibonacci(n-2))
    |otherwise = (-1)

bissexto :: Int-> Bool
bissexto x = if ((x `mod` 4) == 0) && ((x `mod` 100) /= 0) then True
             else if ((x `mod` 400) == 0) then True
             else False

is_prime :: Int -> Bool
is_prime 1 = False
is_prime 2 = True
is_prime n 
        | (length [x | x <- [2 .. n-1], n `mod` x == 0]) > 0 = False
        | otherwise = True
             
maiuscula :: Char -> Bool
maiuscula x
    |(toLower x == x) = False
    |otherwise = True

minuscula :: Char -> Bool
minuscula x
    |(toUpper x == x) = False
    |otherwise = True

repete:: String -> Int -> String
repete s x 
    | x == 1 = s
    |otherwise = repete s (x-1) ++ s

espacos :: Int -> String
espacos x
    | x == 1 = " "
    |otherwise = espacos (x-1) ++ " "

parMenorMaior:: (Int,Int,Int) -> (Int,Int)
parMenorMaior (x,y,z) = (menor3 x y z,maiorDeTres(x,y,z))

ehDigito :: Char -> Bool
ehDigito x =
        if ((x >= '0') && (x <= '9')) then True
        else False

tuplaCrescente :: Int -> Int -> Int -> (Int,Int,Int)
tuplaCrescente x y z = ((menor3 x y z),(segmaior3 x y z), (maiorDeTres(x,y,z)))

type Ponto = (Float, Float)
type Reta = (Ponto, Ponto)

retaFormada :: Ponto -> Ponto -> Reta
retaFormada (x,y) (z,a) = ((x,y),(z,a))

retaVertical :: Reta -> Bool
retaVertical ((x,y),(z,k))
                |x == z = True
                |otherwise = False

retaHorizontal :: Reta -> Bool
retaHorizontal ((x,y),(z,k))
                |y==k = True
                |otherwise = False

-- Lista 2:

intervalo :: Int -> Int -> [Int]
intervalo a b
        | a==b = [a]
        |otherwise = [x | x <- [a..b]]

repetidos:: [Int] -> [Int]
repetidos [] = []
repetidos (a:as) = [a,a] ++ repetidos as

ehDivisivel :: Int-> Int -> Bool
ehDivisivel x y
            |x `mod`y == 0 = True
            |otherwise = False
            
divisores :: Int -> [Int]
divisores x = [y | y <- [1..x], ehDivisivel x y == True]

ordena :: [Int] -> [Int]
ordena [] = []
ordena [x] = [x]
ordena y = [minimum y] ++ (ordena (delete(minimum y) y))


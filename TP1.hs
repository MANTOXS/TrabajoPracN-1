{-- Este modulo provee las funcionalidades basicas del TP, se deben implementar todas las funciones
    que estan indefinidas.
-}

module TP1 (factorizar, divide, divisores, esPrimo, divisoresPrimos, comprimir) where

-- | Dados dos enteros X e Y, retorna True si X divide a Y, caso contrario False
divide :: Int -> Int -> Bool
divide x y = mod x y == 0

-- | Dado un número X, retorna la lista de todos los divisores de X.
-- Ej: divisores 10 = [1,2,5,10]
divisores :: Int -> [Int]
divisores x = [n | n <- [1..x], mod x n == 0]

-- | Dado un número, determina si es un número primo.
esPrimo :: Int -> Bool
esPrimo x = [n | n <- [1..x], mod x n == 0] == [1,x]

-- | Dado un número X, retorna la lista de todos los divisores primos de X.
-- Ej: divisoresPrimos 10 = [2,5]
divisoresPrimos :: Int -> [Int]
divisoresPrimos x = [n | n <- [1..x], mod x n == 0, esPrimo n]

calAux :: Int -> [Int] -> Int -> [(Int,Int)]
calAux x [] n = [(x,n)]
calAux x (xs) n 
       | x ==(head xs) = calAux x (tail xs) (n+1)
       | otherwise = (x,n) : calAux (head xs) xs 0 


-- | Dada una lista de números, comprime la lista contando las repeticiones adyacentes.
-- Ej: comprimir [2,2,2,5,2,2] = [(2,3), (5,1), (2,2)]
comprimir :: [Int] -> [(Int, Int)]
comprimir [] = []
comprimir (x:xs) = calAux x xs 1

-- | Dado un número X, retorna la lista de su factorización prima, es decir, una lista de pares
-- donde el primer elemento es el factor primo y el segundo es la cantidad de veces que aparece en la factorización.
-- Ej: factorizar 118800 = [(2,4),(3,3),(5,2),(11,1)]
factorizar :: Int -> [(Int,Int)]
factorizar 1 = []  
factorizar x = comprimir (factorizaAux x (divisoresPrimos x))

-- | Auxiliar que descompone el número en una lista con los factores primos.
factorizaAux :: Int -> [Int] -> [Int]
factorizaAux 1 _ = []
factorizaAux x [] = []
factorizaAux x (y:ys)
  | mod x y == 0 = y : factorizaAux (div x y) (y:ys)
  | otherwise = factorizaAux x ys

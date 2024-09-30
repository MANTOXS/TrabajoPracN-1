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


-- | Dada una lista de números, comprime la lista contando las repeticiones adyacentes.
-- Ej: comprimir [2,2,2,5,2,2] = [(2,3), (5,1), (2,2)]
comprimir :: [Int] -> [(Int, Int)]
comprimir xs = 

-- | Función principal. 
-- Implementar usando las funciones previas, funciones definidas en Haskell y toda otra función auxiliar que sea necesaria.
-- Ej: factorizar 118800 = [(2,4),(3,3),(5,2),(11,1)]
factorizarr :: Int -> [(Int,Int)]
factorizar = undefined
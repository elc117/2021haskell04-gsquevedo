-- Prática 04 de Haskell
-- Nome: Gabriele Soares Quevedo

--1
faixaIdoso :: Int -> String 
faixaIdoso x 
  |x >= 60 && x <= 64 = "IDO64"
  |x >= 65 && x <= 69 = "IDO69"
  |x >= 70 && x <= 74 = "IDO74"
  |x >= 75 && x <= 79 = "IDO79"
  |x >= 80 = "IDO80"
  |otherwise = "ND"
  
--2
classifIdosos :: [(String,Int)] -> [(String,Int,String)] 
classifIdosos x = [(a,b,faixaIdoso b) | (a,b) <- x]

--3
classifIdosos' :: [(String,Int)] -> [(String,Int,String)] 
classifIdosos' x = map(\(a,b) -> (a,b,faixaIdoso b))x

--4
strColor :: (Int,Int,Int) -> String
strColor (a,b,c) = "rgb"++show(a,b,c)

--5
genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n (cx,cy) raio = take n[(x,cy,raio) | x <- (iterate (\x -> (x+2)*raio)cx)]

--6
genReds :: Int -> [(Int,Int,Int)]
genReds x = take x [(x,0,0) | x <- (iterate (\x -> (x+10))x)]

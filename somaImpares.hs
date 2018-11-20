potencia ::  Integer -> Integer -> Integer
potencia numero 0 = 1
potencia numero expoente = numero * potencia numero (expoente - 1)

somaImpares :: [Integer] -> Integer
somaImpares [] = 0
somaImpares (i : numeros)
  | i `mod`2 == 1 = i + somaImpares numeros
  | otherwise = somaImpares numeros
  
substituir :: Integer -> Integer -> [Integer] -> [Integer]


main = do
  print $ potencia 2 4
  print $ somaImpares [6,2,3,2,5]
  

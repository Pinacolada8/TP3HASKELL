potencia :: (Integer,Integer) -> Integer
potencia (numero, 0) = 1
potencia (numero, expoente) = numero * potencia(numero,expoente - 1)

main = do
  print $ potencia (2,4)

-- =>Funcao que realiza a Potencia de um Numero
potencia ::  Integer -> Integer -> Integer
potencia numero 0 = 1
potencia numero expoente = numero * potencia numero (expoente - 1)
--END<=

-- =>Funcao que realiza a Soma dos Impares em uma Lista
somaImpares :: [Integer] -> Integer
somaImpares [] = 0
somaImpares (i : numeros)
  | i `mod`2 == 1 = i + somaImpares numeros
  | otherwise = somaImpares numeros
--END<=

-- =>Substituir Numero  
substituir :: Integer -> Integer -> [Integer] -> [Integer]
substituir alvo valor numeros = map (substituirNumero alvo valor) numeros

--Funcao que complementa a substituir
substituirNumero :: Integer -> Integer -> Integer -> Integer
substituirNumero alvo valor numero
  | numero == alvo = valor
  |otherwise = numero
--END
--END<=

-- =>Verifica se E primo
primo :: Integer -> Bool
primo numero
  | length (divisores numero numero []) == 2 = True
  |otherwise = False
--END<=

-- =>Retorna uma lista com os divisores do numero(FUNCAO AUXILIAR)
divisores :: Integer -> Integer -> [Integer] -> [Integer]
divisores 0  original numeros = numeros
divisores valor orginal numeros 
 | orginal `mod` valor == 0 = [valor] ++ (divisores (valor -1) orginal numeros)
 | otherwise = (divisores (valor -1) orginal numeros)
--END<=

-- => Retorna se o numero e Perfeito
perfeito :: Integer -> Bool
perfeito numero 
  | sum (divisores numero numero []) - numero == numero = True
  |otherwise = False
--END<=

-- => Retorna uma lista que representa o numero em binario
binario :: Integer -> [Integer]
binario 0 = []
binario numero 
  | numero `mod` 2 == 1 = (binario ((numero-1) `div`2)) ++ [1]
  |otherwise =(binario (numero`div`2)) ++ [0]
--END<=

-- => Verifica se os valores sao distintos
distintos :: [Integer] -> Bool
distintos lista
  | all (==True) ([unico x lista | x <- lista]) == True = True
  |otherwise = False

unico :: Integer -> [Integer] -> Bool
unico numero lista
  | length (filter (igual numero) lista) == 1 = True
  | otherwise = False

igual :: Integer -> Integer -> Bool
igual numero alvo
  | alvo == numero = True
  |otherwise = False
--END<=

-- => Verificar se duas listas sao disjuntas
disjuntas :: [Integer] -> [Integer] -> Bool
disjuntas lista1 lista2
  | all (==True) ([unico x lista3 | x <- lista1]) == True = True
  |otherwise = False
  where lista3 = lista1 ++ lista2
--END<=

-- => Verificar se e palindromo
palindromo :: [Integer] -> Bool
palindromo lista
  | all (==True) ([igual (lista !! fromIntegral(x)) (listaReversa !! fromIntegral(x)) | x <- lista]) == True = True
  |otherwise = False
  where listaReversa = reverse lista
--END<=

-- => Somas Parciais
--somasParciais :: [Integer] -> [Integer]

--END<=

-- => Linearizar lista de inteiros
--linearizar :: [listas] -> [b]
--linearizar [] = []
--linearizar (i:lista) = [i] ++ linearizar lista
  
--END<=


-- => Mover a esquerda
shift :: Integer -> [Integer] -> [Integer]
shift pos lista = (let (lista1,lista2) = splitAt (fromIntegral(pos)) lista in lista2 ++ lista1)
--END<=

-- => remove o final da lista
removerFim :: Integer -> [Integer] -> [Integer]
removerFim 0 lista = lista
removerFim remover lista = init (removerFim (remover-1) lista)
--END<=


main = do
  print $ potencia 2 4
  print $ somaImpares [6,2,3,2,5]
  print $ substituir 1 0 [0,1,2,3,1,4]
  print $ primo 17
  print $ perfeito 28
  print $ binario 20
  print $ distintos [1,2,3,4,6]
  print $ disjuntas [1,2,3] [5,4,6,0]
  print $ palindromo [1,2,3,4,3,2,1]
  print $ shift 3 [1,5,6,7,3,4,1]
  print $ removerFim 2 [1,2,3,4,5,6]
  

type Nome = String
type Preco = Int
type Codigo = Int
type Mercadorias = [(Codigo, Nome, Preco)]
type Carrinho = [Codigo]
type Conta = [(Nome,Preco)]

tabelaMercadorias:: Mercadorias
tabelaMercadorias = [ (1234, "Oleo DoBom, 1l" , 195),
                      (4756, "Chocolate Cazzeiro, 250g" , 180),
                      (3216, "Arroz DoBom, 5Kg", 213),
                      (5823, "Balas Pedregulho, 1Kg" , 379),
                      (4719, "Queijo Mineirim, 1Kg" , 449),
                      (6832, "Iogurte Maravilha, 1Kg" , 499),
                      (1112, "Rapadura QuebraDente, 1Kg", 80),
                      (1111, "Sal Donorte, 1Kg", 221),
                      (1113, "Cafe DoBom, 1Kg", 285),
                      (1115, "Biscoito Bibi, 1Kg", 80),
                      (3814, "Sorvete QGelo, 1l", 695)]

formataCentavos :: Preco -> String                
formataCentavos x =  if (mod x 100) < 10 then show (div x 100) ++ "." ++ show (mod x 100) else show(div x 100) ++  "." ++ show(mod x 100)

formataLinha :: (Nome,Preco) -> String
tamanhoLinha :: Int
tamanhoLinha = 30
formataLinha(x,y) = x ++ (replicate (tamanhoLinha - length(x++formataCentavos y)) '.') ++ formataCentavos y

formataLinhas :: [(Nome,Preco)] -> String
formataLinhas [] = ""
formataLinhas x = formataLinha(head x) ++ "\n" ++ formataLinhas (tail x)

formataTotal:: Preco -> String
formataTotal x = formataLinha("\nTotal",x)

formataConta :: Conta -> String
formataConta [] = ""
formataConta x = show("Supermercado QLegal\n\n") ++ formataLinhas x

calculaTotal :: Conta -> Preco
calculaTotal [] = 0
calculaTotal x = snd(head x) + calculaTotal(tail x)

procuraCodigo :: Mercadorias -> Codigo -> (Nome,Preco)
procuraCodigo [] n = ("item desconhecido", 0)
procuraCodigo((a,b,c):xs) n | a==n = (b,c)
                            | otherwise = procuraCodigo xs n

achaItem :: Codigo -> (Nome,Preco)
achaItem x = procuraCodigo tabelaMercadorias x

fazRecibo :: Carrinho -> Conta
fazRecibo [] = []
fazRecibo (x:xs) = [achaItem x] ++ fazRecibo xs

geraRecibo:: Carrinho -> IO()
geraRecibo x = putStr( formataConta (fazRecibo x))
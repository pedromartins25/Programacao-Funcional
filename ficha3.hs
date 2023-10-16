import Ficha1 
-- Exercicio 1

data Hora = H Int Int
         deriving Show

horaValida :: Hora -> Bool
horaValida (H h m) = h >= 0 && h <= 23 && m >= 0 && m <= 59

horaDepoisDe :: Hora -> Hora -> Bool
horaDepoisDe (H h1 m1) (H h2 m2) = h1 > h2 || h1 == h2 && m1 > m2

type Etapa = (Hora, Hora)
type Viagem = [Etapa]

testarEtapa :: Etapa -> Bool
testarEtapa (h1,h2) = horaValida h1 && horaValida h2 && horaDepoisDe h2 h1

testarViagem :: Viagem -> Bool
testarViagem [] = True
testarViagem ((h1,h2):t) = testarEtapa (h1,h2) && testarViagem t &&
                    case t of
                        [] -> True
                        (h3, h4) : t' -> h3 `horaDepoisDe` h2

partidaEchegada :: Viagem -> (Hora,Hora)
partidaEchegada v = (partida,chegada)
       where (partida,_) = head v
             (_,chegada) = last v

tempoViagemEfetiva :: Viagem -> Hora
tempoViagemEfetiva [] = H 0 0


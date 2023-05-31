data Ladron = Ladron {
    nombreLadron :: String,
    habilidades :: [Habilidad],
    armas :: [Arma]
}

data Rehen = Rehen {
    nombreRehen :: String,
    complot :: Int,
    miedo :: Int,
    plan :: Plan
}

type Habilidad = String
type Arma = Rehen -> Rehen
type Plan = Ladron -> Ladron

--------------
-- Punto 01 -- 
--------------

tokio :: Ladron
tokio = Ladron "Tokio" ["trabajo psicologico", "entrar en moto"] [pistola 9, pistola 9, ametralladora 30]

profesor :: Ladron
profesor = Ladron "Profesor" ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelante"] []

pablo :: Rehen
pablo = Rehen "Pablo" 40 30 esconderse 

arturito :: Rehen
arturito = Rehen "Arturo" 70 50 (esconderse . atacarAlLadron pablo)

--------------
-- Punto 02 -- 
--------------

esInteligente :: Ladron -> Bool
esInteligente unLadron = cantidadDeHabilidades unLadron > 2  || es unLadron "Profesor"

cantidadDeHabilidades :: Ladron -> Int
cantidadDeHabilidades = length . habilidades

es :: Ladron -> String -> Bool
es unLadron unNombre = unNombre == nombreLadron unLadron

--------------
-- Punto 03 -- 
--------------

agregarArma :: Arma -> Ladron -> Ladron
agregarArma unArma = mapArmas (unArma :)

mapArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron
mapArmas unaFuncion unLadron = unLadron { armas = unaFuncion (armas unLadron) }

--------------
-- Punto 04 -- 
--------------

disparar :: Rehen -> Ladron -> Rehen
disparar unRehen = ($ unRehen) . armaMasMiedosaPara unRehen 

armaMasMiedosaPara :: Rehen -> Ladron -> Arma
armaMasMiedosaPara unRehen = maximumBy (miedo . ($ unRehen)) . armas

maximumBy :: Ord b => (a -> b) -> [a] -> a
maximumBy f = foldl1 (maxBy f)

maxBy :: Ord b => (a -> b) -> a -> a -> a
maxBy f x y
    | f x > f y = x
    | otherwise = y

hacerseElMalo :: Rehen -> Ladron -> Rehen
hacerseElMalo unRehen unLadron
    | es unLadron "Berlin" = aumenterMiedo (cantidadDeLetras unLadron) unRehen
    | es unLadron "Rio"    = aumentarComplot 20 unRehen
    | otherwise            = aumenterMiedo 10 unRehen

cantidadDeLetras :: Ladron -> Int
cantidadDeLetras = sum . map length . habilidades 

aumenterMiedo :: Int -> Rehen -> Rehen
aumenterMiedo unaCantidad unRehen = unRehen { miedo = miedo unRehen + unaCantidad }

aumentarComplot :: Int -> Rehen -> Rehen
aumentarComplot unaCantidad unRehen = unRehen { complot = complot unRehen + unaCantidad }

--------------
-- Punto 05 -- 
--------------

calmarLasAguas :: Ladron -> [Rehen] -> [Rehen]
calmarLasAguas unLadron = filter ((> 60) . complot) . map (flip disparar unLadron)

--------------
-- Punto 06 -- 
--------------

puedeEscaparse :: Ladron -> Bool
puedeEscaparse = any (empiezaCon "Disfrazarse de") . habilidades

empiezaCon :: Eq a => [a] -> [a] -> Bool
empiezaCon unaPalabra = (== unaPalabra) . take (length unaPalabra)

--------------
-- Punto 07 -- 
--------------

pintaMal :: [Ladron] -> [Rehen] -> Bool
pintaMal ladrones rehenes = complotPromedio rehenes > miedoPromedio rehenes * cantidadTotalArmas ladrones

complotPromedio :: [Rehen] -> Int
complotPromedio = promedio complot 

miedoPromedio :: [Rehen] -> Int
miedoPromedio = promedio miedo 

promedio :: (a -> Int) -> [a] -> Int
promedio f xs = sum (map f xs) `div` length xs

cantidadTotalArmas :: [Ladron] -> Int
cantidadTotalArmas = sum . map cantidadArmas

cantidadArmas :: Ladron -> Int
cantidadArmas = length . armas

--------------
-- Punto 08 -- 
--------------

type Nivel = Int

rebelarseContra :: [Rehen] -> Ladron -> Ladron
rebelarseContra rehenes ladron = foldl rebelarse ladron . map (decNivelComplot 10) $ rehenes

decNivelComplot :: Nivel -> Rehen -> Rehen
decNivelComplot cantidad = mapComplot (subtract cantidad)

mapComplot :: (Nivel -> Nivel) -> Rehen -> Rehen
mapComplot f rehen = rehen { complot = max 0 . f $ complot rehen }

rebelarse :: Ladron -> Rehen -> Ladron
rebelarse unLadron unRehen 
    | esSubversivo unRehen = (plan unRehen) unLadron
    | otherwise            = unLadron

esSubversivo :: Rehen -> Bool
esSubversivo rehen = complot rehen > miedo rehen

atacarAlLadron :: Rehen -> Plan
atacarAlLadron rehenAliado = quitarArmas (cantidadLetrasNombre rehenAliado `div` 10)

cantidadLetrasNombre :: Rehen -> Int
cantidadLetrasNombre = length . nombreRehen

esconderse :: Plan
esconderse ladron = quitarArmas (cantidadDeHabilidades ladron `div` 3) ladron

--------------
-- Punto 09 --
--------------

planValencia :: [Rehen] -> [Ladron] -> Int
planValencia rehenes ladrones = (*1000000) . cantidadTotalArmas . map (rebelarseContra rehenes . agregarArma (ametralladora 45)) $ ladrones

pistola :: Int -> Arma
pistola calibre rehen = incNivelMiedo (3 * length (nombreRehen rehen)) . decNivelComplot (calibre * 5) $ rehen

ametralladora :: Int -> Arma
ametralladora balas = incNivelMiedo balas . mapComplot (`div` 2)

quitarArmas :: Int -> Ladron -> Ladron
quitarArmas cantidad = mapArmas (drop cantidad)

incNivelMiedo :: Nivel -> Rehen -> Rehen
incNivelMiedo cantidad = mapNivelMiedo (+ cantidad)

mapNivelMiedo :: (Nivel -> Nivel) -> Rehen -> Rehen
mapNivelMiedo f rehen = rehen { miedo = max 0 . f $ miedo rehen }

--------------
-- Punto 10 --
--------------

-- No se puede, el plan valencia necesita saber la cantidad de armas del ladron.
-- La consulta nunca terminaría de ejecutarse y se colgaría
-- Loopea

--------------
-- Punto 11 --
--------------

-- Puede ser sí, como no. Depende del plan que tiene el rehen.
-- Si el plan es esconderse necesita saber la cantidad de habilidades del ladron
-- Si pasa eso, ocurre lo mismo que se explicó anteriormente.

--------------
-- Punto 12 --
--------------

funcion ::     b   -> (a -> [c]) -> (b -> a -> Bool) -> Int -> ([a] -> Bool)
--           ^^^^^    ^^^^^^^^^^    ^^^^^^^^^^^^^^^^    ^^^    ^^^^^^^^^^^^^
--Tipo de:   cond        num             lista          str       retorno

funcion cond num lista str =  (> str) . sum . map (length . num) . filter (lista cond)
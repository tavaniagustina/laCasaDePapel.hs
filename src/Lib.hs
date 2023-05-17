data Ladron = Ladron {
    nombreLadron :: String,
    habilidades :: [Habilidad],
    armas :: [Arma]
}

type Arma = Rehen -> Rehen
type Habilidad = String

data Rehen = Rehen {
    nombreRehen :: String,
    nivelComplot :: Nivel,
    nivelMiedo :: Nivel,
    planes :: Plan
}

type Plan = Ladron -> Ladron
type Nivel = Int

--------------
-- Punto 01 -- 
--------------

-- tokio :: Ladron
-- tokio = Ladron "Tokio" ["trabajo psicologico", "entrar en moto"] [pistola 9, pistola 9, ametralladora 30]

-- pistola :: Arma
-- ametralladora :: Arma

-- profesor :: Ladron
-- profesor = Ladron "Profesor" ["disfrazarse de linyera", "disfrazarse de payaso", "estar siempre un paso adelente"] []

-- pablo :: Rehen
-- pablo = Rehen "Pablo" 40 30 esconderse

-- arturito :: Rehen
-- arturito = Rehen "Arturito" 70 50  (atacarAlLadron pablo . esconderse)

-- esconderse :: Plan
-- atacarAlLadron :: Plan

--------------
-- Punto 02 -- 
--------------

esInteligente :: Ladron -> Bool
esInteligente unLadron = es unLadron "Profesor" || cantidadHabilidades unLadron > 2

es :: Ladron -> String -> Bool
es unLadron unNombre = nombreLadron unLadron == unNombre

cantidadHabilidades :: Ladron -> Int
cantidadHabilidades = length . habilidades

--------------
-- Punto 03 -- 
--------------

conseguirArma :: Arma -> Ladron -> Ladron
conseguirArma unArma = mapArmas (unArma :)

mapArmas :: ([Arma] -> [Arma]) -> Ladron -> Ladron
mapArmas unaFuncion unLadron = unLadron { armas = unaFuncion $ armas unLadron}


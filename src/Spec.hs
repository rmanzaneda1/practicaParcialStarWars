module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Test de ejemplo" $ do
    it "El pdepreludat se instaló correctamente" $ do
      doble 1 `shouldBe` 2

--Star Wars Haskell Spacial
data Nave = UnaNave {
  nombre :: String,
  durabilidad :: Number,
  escudo :: Number,
  ataque :: Number,
  poder :: Poder
} deriving (Show, Eq)

tieFighter :: Nave
tieFighter = UnaNave "TIE Fighter" 200 100 50 turbo

xWing :: Nave
xWing = UnaNave "X Winf" 300 150 100 reparacionEmergencia

naveDarthVader :: Nave
naveDarthVader = UnaNave "Nave de Darth Vader" 500 300 200 superTurbo

milleniunFalcon :: Nave
milleniunFalcon = UnaNave "Milleniun Falcon" 1000 500 50 reparacionEmergenciaFalcon

type Poder = Nave -> Nave

poderes = [turbo, reparacionEmergencia, superTurbo, reparacionEmergenciaFalcon]

turbo :: Nave -> Nave
turbo = aumentarAtaque 25

aumentarAtaque :: Number -> Nave -> Nave
aumentarAtaque nro nave = nave { ataque = ataque nave + nro }

reparacionEmergencia :: Nave -> Nave
reparacionEmergencia = aumentarDurabilidad 50 . reducirAtaque 30

aumentarDurabilidad :: Number -> Nave -> Nave
aumentarDurabilidad nro nave = nave { durabilidad = durabilidad nave + nro }
reducirAtaque :: Number -> Nave -> Nave
reducirAtaque nro nave = nave { ataque = ataque nave - nro }

superTurbo :: Nave -> Nave
superTurbo = turbo.turbo.turbo.reducirDurabilidad 45

reducirDurabilidad :: Number -> Nave -> Nave
reducirDurabilidad nro nave = nave { durabilidad = durabilidad nave - nro }

reparacionEmergenciaFalcon :: Nave -> Nave
reparacionEmergenciaFalcon = reparacionEmergencia.aumentarEscudos 100

aumentarEscudos :: Number -> Nave -> Nave
aumentarEscudos nro nave = nave { escudo = escudo nave + nro }

nuevaNave :: Nave
nuevaNave = UnaNave "La Mejor Nave" 100 100 100 superPoder

superPoder :: Nave -> Nave
superPoder = aumentarAtaque 100 . aumentarDurabilidad 100 . aumentarEscudos 100

--2. Calcular la durabilidad total de una flota, formada por un conjunto de naves, 
--   que es la suma de la durabilidad de todas las naves que la integran.

durabilidadTotal :: [Nave] -> Number
durabilidadTotal naves = sum $ map durabilidad naves

--3. Saber como queda una nave luego de ser atacada por otra

resultadoNaveAtacada :: Nave -> Nave -> Nave
resultadoNaveAtacada nave1 nave2 = naveActivada nave1 {
  durabilidad = max 0 (durabilidad nave1) - dañoRecibido nave1 nave2
}

dañoRecibido :: Nave -> Nave -> Number
dañoRecibido nave1 nave2 
 |ataque (naveActivada nave2) - escudo (naveActivada nave1) < 0  = 0
 |otherwise = ataque (naveActivada nave2) - escudo (naveActivada nave1)

naveActivada :: Nave -> Nave
naveActivada nave = (poder nave) nave 

-- 4 Nave fuera de combate, cuando la durabilidad llega a 0

naveFueradeCombate :: Nave -> Bool
naveFueradeCombate nave = durabilidad nave == 0

-- 5 Averiguar cómo queda una flota enemiga luego de realizar una misión sorpresa con una nave 
--siguiendo una estrategia. 
--Una estrategia es una condición por la cual la nave atacante decide atacar o no una cierta nave de 
--la flota. 
--Por lo tanto la misión sorpresa de una nave hacia una flota significa atacar todas aquellas naves 
--de la flota que la estrategia determine que conviene atacar. 

type Flota = Nave
despuesDeMision :: Estrategia -> Nave -> Nave -> Nave
despuesDeMision estrategia nave1 nave2 = undefined

-- Algunas estrategias que existen, y que deben estar reflejadas en la solución, son:
--1. Naves débiles: Son aquellas naves que tienen menos de 200 de escudo.
type Estrategia = Nave -> Bool

navesDebiles :: Nave -> Bool
navesDebiles nave = escudo nave < 200

--2. Naves con cierta peligrosidad: Son aquellas naves que tienen un ataque mayor a un valor dado. 
--Por ejemplo, en alguna misión se podría utilizar una estrategia de peligrosidad mayor a 300, 
--y en otra una estrategia de peligrosidad mayor a 100.

navesPeligrosas :: Number -> Nave -> Bool
navesPeligrosas nro nave = ataque nave > nro

--3. Naves que quedarían fuera de combate: Son aquellas naves de la flota que luego del ataque
--   de la nave atacante quedan fuera de combate. 
naveFueradeCombatedespuesdeAtaque :: Nave -> Nave -> Bool
naveFueradeCombatedespuesdeAtaque nave1 nave2 = naveFueradeCombate $ resultadoNaveAtacada nave1 nave2 

--4. Inventar una nueva estrategia

navesRotas :: Nave -> Bool
navesRotas = naveFueradeCombate 

-- 6 Considerando una nave y una flota enemiga en particular, dadas dos estrategias, 
--determinar cuál de ellas es la que minimiza la durabilidad total de la flota atacada y 
--llevar adelante una misión con ella.

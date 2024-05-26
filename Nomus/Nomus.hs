data Nomu = UnNomu {
    alas :: Bool,
    brazos :: Bool,
    ojos :: Int,
    piel :: String,
    vida :: Int,
    fuerza :: Int
}deriving(Show, Eq)

nomu1 :: Nomu
nomu1 = UnNomu False True 2 "oscuro" 1000 3000

nomu2 :: Nomu
nomu2 = UnNomu {
    alas = True,
    brazos = False,
    ojos = 0,
    piel = "blanco",
    vida = 3000,
    fuerza = 900
}

puedeVer :: Nomu -> Bool
puedeVer nomu = ojos nomu > 0 

categoria :: Nomu -> String
categoria (UnNomu _ _ _ _ _ fuerza)
            | fuerza >= 1000 && fuerza < 3000 = "comun"
            | fuerza >= 3000 && fuerza < 10000 = "fuerte"
            | fuerza >= 10000 = "high-end"
            | otherwise = "pichi"

data Poderes = UnPoder {
    cantCuracion :: Int,
    cantDaño :: Int,
    rango :: Int,
    proba :: Int
}deriving(Show, Eq)

poder1 :: Poderes
poder1 = UnPoder 0 2000 3000 50

poder2 :: Poderes
poder2 = UnPoder 150 5000 50 75

poder3 :: Poderes
poder3 = UnPoder 10000 0 10 100

cuerpoAcuerpo :: Poderes -> Bool
cuerpoAcuerpo ataque = rango ataque < 100

tipoCuracion :: Poderes -> Bool
tipoCuracion cura = cantDaño cura == 0 && cantCuracion cura > 0
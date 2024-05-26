--PARTE 1
data Personajes = UnPersonaje{
        nombre :: String,
        cantDinero :: Int,
        felicidad :: Int
}deriving(Show, Eq)

butters :: Personajes
butters = UnPersonaje "Butters" 100 20
cartman :: Personajes
cartman = UnPersonaje "Cartman" 50 50
stan :: Personajes
stan = UnPersonaje "Stan" 10 10
kenny :: Personajes
kenny = UnPersonaje "Kenny" 6 30

irEscuela :: Personajes -> Personajes
irEscuela personaje
                | nombre personaje == "Butters" = personaje {felicidad = felicidad personaje + 20}
                | otherwise = personaje {felicidad = felicidad personaje - 20}

comerCheesyPoofs :: Int -> Personajes -> Personajes
comerCheesyPoofs cant personaje = personaje {felicidad = felicidad personaje + 10, cantDinero = cantDinero personaje - (10*cant)}

irATrabajar :: String -> Personajes -> Personajes
irATrabajar laburo personaje = personaje {cantDinero = cantDinero personaje + length laburo}

dobleTurno :: String -> Personajes -> Personajes
dobleTurno laburo personaje = personaje {cantDinero = cantDinero (irATrabajar laburo (irATrabajar laburo personaje)), felicidad = felicidad personaje - length laburo}

jugarWoW :: Int -> Int -> Personajes -> Personajes
jugarWoW amigos horas personaje
                    | horas > 5 = personaje {felicidad = felicidad personaje + (amigos * 50), cantDinero = cantDinero personaje - (horas * 10)}
                    | otherwise = personaje {felicidad = felicidad personaje + (amigos * horas * 10), cantDinero = cantDinero personaje - (horas * 10)}

--PARTE 2       
type Accion = Personajes -> Personajes

repetirAccionNVeces :: Personajes -> Accion -> Int -> Personajes
repetirAccionNVeces personaje _ 0 = personaje
repetirAccionNVeces personaje accion n
                                | n > 0 = repetirAccionNVeces (accion personaje) accion (n-1)
                                | otherwise = error "no se permite numeros negativos"

realizarActividades :: [Accion] -> Personajes -> Personajes
realizarActividades listaAct personaje = foldl (\acc funcion -> funcion acc) personaje listaAct

--Logros

type Logro = Personajes -> Bool

serMillo :: Logro
serMillo = (cantDinero cartman <) . cantDinero --cantDinero personaje > cantDinero cartman

estarContento :: Logro --Nivel de felicidad deseado 100
estarContento = (100 <) . felicidad --felicidad personaje >= 100

verPrograma :: Logro
verPrograma = (10 <) . cantDinero --cantDinero personaje >= 10

nombreSimple :: Logro --si el nombre tiene menos de 5 letras
nombreSimple = (5>) . length . nombre

actividadDecisiva :: Logro -> Accion -> Personajes -> Bool
actividadDecisiva logro act personaje = not (logro personaje) && logro (act personaje)

personajesConActDecisiva :: [Personajes] -> Logro -> Accion -> [Personajes]
personajesConActDecisiva lista logro act = filter (actividadDecisiva logro act) lista


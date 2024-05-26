
data Ciudad = UnaCiudad{
    nombre :: String,
    fundacion :: Float,
    atracciones :: [String],
    costoDeVida :: Float
}deriving(Show, Eq)

--PARTE 1
baradero :: Ciudad
baradero = UnaCiudad "Baradero" 1615 ["Parque del Este","Museo Alejandro Barbich"] 150
nullish :: Ciudad
nullish = UnaCiudad "Nullish" 1800 [] 140
caleta :: Ciudad
caleta = UnaCiudad "Caleta Olivia" 1901 ["El Gorosito","Faro Costanera"] 120
maipu :: Ciudad
maipu = UnaCiudad "Maipú" 1878 ["Fortín Kakel"] 115
azul :: Ciudad
azul = UnaCiudad "Azul" 1832 ["Teatro Español", "Parque Municipal Sarmiento", "Costanera Cacique Catriel"] 190

valorCiudad :: Ciudad -> Float
valorCiudad ciudad
                | fundacion ciudad < 1800 = 5 * (1800 - fundacion ciudad)
                | null (atracciones ciudad) = 2 * costoDeVida ciudad
                | otherwise = 3 * costoDeVida ciudad

--PARTE 2
atraccionCopada :: Ciudad -> Bool
--atraccionCopada ciudad = any vocal (atracciones ciudad)
--atraccionCopada ciudad = any (\lugar -> head lugar `elem` "aeiouAEIOU") (atracciones ciudad)
atraccionCopada ciudad = any ((`elem` "aeiouAEIOU").head) (atracciones ciudad)

--vocal :: String -> Bool
--vocal lugar = head lugar `elem` "aeiouAEIOU"

ciudadSobria :: Ciudad -> Int -> Bool
--ciudadSobria ciudad x = not (null (atracciones ciudad)) && all (letras x) (atracciones ciudad)
--ciudadSobria ciudad x = not (null (atracciones ciudad)) && all (\lugar -> length lugar > x) (atracciones ciudad)
ciudadSobria ciudad x = not (null (atracciones ciudad)) && all ((>x).length) (atracciones ciudad)

--letras :: Int -> String -> Bool
--letras x lugar = length lugar > x

ciudadNombreRaro :: Ciudad -> Bool
--ciudadNombreRaro ciudad = length (nombre ciudad) < 5
ciudadNombreRaro = (<5).length.nombre

--PARTE 3
costo :: Ciudad -> Float -> Float
costo ciudad porcentaje = costoDeVida ciudad * porcentaje

sumarAtraccion :: String -> Ciudad -> Ciudad
sumarAtraccion nuevaAtraccion ciudad = ciudad{atracciones = nuevaAtraccion : atracciones ciudad , costoDeVida = costo ciudad 1.2}
--sumarAtraccion ciudad nuevaAtraccion = ciudad{atracciones = atracciones ciudad ++ [nuevaAtraccion], costoDeVida = costo ciudad 1.2}

crisisCiudad :: Ciudad -> Ciudad
crisisCiudad ciudad
                | null (atracciones ciudad) = ciudad{costoDeVida = costoDeVida ciudad - costoDeVida ciudad * 0.1}
                | otherwise = ciudad{atracciones = init (atracciones ciudad), costoDeVida = costoDeVida ciudad - costo ciudad 0.1}

remodelarCiudad :: Float -> Ciudad -> Ciudad
remodelarCiudad porcentaje ciudad = ciudad{costoDeVida = costo ciudad (porcentaje/100 + 1), nombre = "New " ++ nombre ciudad}

reevaluacionCiudad :: Int -> Ciudad -> Ciudad
reevaluacionCiudad n ciudad
                    | ciudadSobria ciudad n = ciudad{costoDeVida = costo ciudad 1.1}
                    | otherwise = ciudad{costoDeVida = costoDeVida ciudad - 3}

--PARTE 4
transformacion :: Float -> Int -> Ciudad -> Ciudad
--transformacion porcentaje n ciudad = reevaluacionCiudad n (crisisCiudad (remodelarCiudad porcentaje ciudad))
transformacion porcentaje n = reevaluacionCiudad n.crisisCiudad.remodelarCiudad porcentaje

--CONTINUACION TP
type Evento = Ciudad -> Ciudad
data Anio = UnAnio{
    num :: Int,
    eventos :: [Evento]
}

anio2022 :: Anio
anio2022 = UnAnio 2022 [crisisCiudad, remodelarCiudad 5, reevaluacionCiudad 7]
anio2015 :: Anio
anio2015 = UnAnio 2015 [crisisCiudad] --debe haber por lo menos un evento
anio2023 :: Anio
anio2023 = UnAnio 2023 [crisisCiudad, sumarAtraccion "parque", remodelarCiudad 10, remodelarCiudad 20]
anioEj :: Anio
anioEj = UnAnio 2020 [remodelarCiudad 10, remodelarCiudad 20]
anio2021 :: Anio
anio2021 = UnAnio 2021 [crisisCiudad, sumarAtraccion "playa"]

anioParaRecordar :: Anio -> Ciudad -> Ciudad
anioParaRecordar anio ciudad = foldl (\acc funcion -> funcion acc) ciudad (eventos anio)

type Comparacion = Ciudad -> Ciudad -> Bool

algoMejor :: Ciudad -> Comparacion -> Evento -> Bool
algoMejor ciudad comparacion evento = comparacion ciudad (evento ciudad)

newNombre :: Comparacion
--newNombre ciudadInicial ciudadNueva = length (nombre ciudadInicial) < length (nombre ciudadNueva)
newNombre ciudadInicial = ((length.nombre) ciudadInicial <).length.nombre

masAtracciones :: Comparacion
--masAtracciones ciudadInicial ciudadNueva = length (atracciones ciudadInicial) < length (atracciones ciudadNueva)
masAtracciones ciudadInicial = ((length.atracciones) ciudadInicial <).length.atracciones

comparacionCostoDeVida :: Comparacion
--comparacionCostoDeVida ciudadInicial ciudadNueva = costoDeVida ciudadInicial < costoDeVida ciudadNueva
comparacionCostoDeVida ciudadInicial = (costoDeVida ciudadInicial <).costoDeVida

modificarEvento :: Ciudad -> Anio -> Comparacion -> Ciudad
modificarEvento ciudad anio comparacion = anioParaRecordar (anio{eventos = filter (\evento -> comparacion ciudad (evento ciudad)) (eventos anio)}) ciudad

costoVidaSube :: Ciudad -> Anio -> Ciudad
costoVidaSube ciudad anio = modificarEvento ciudad anio comparacionCostoDeVida

verificarQueSube :: Ciudad -> Evento -> Bool
verificarQueSube ciudadInicial evento = comparacionCostoDeVida ciudadInicial (evento ciudadInicial)

costoVidaBaje :: Ciudad -> Anio -> Ciudad
costoVidaBaje ciudad anio = modificarEvento ciudad anio verificarQueBaja

verificarQueBaja :: Comparacion
verificarQueBaja ciudadInicial ciudadNueva = not (comparacionCostoDeVida ciudadInicial ciudadNueva) && costoDeVida ciudadInicial /= costoDeVida ciudadNueva

--PARTE 4.5
valorSube :: Ciudad -> Anio -> Ciudad
valorSube ciudad anio = modificarEvento ciudad anio verificarValorSube

verificarValorSube :: Comparacion
verificarValorSube ciudadInicial ciudadNueva = valorCiudad ciudadInicial < valorCiudad ciudadNueva

--PARTE 5
verificarOrdenEventos :: Anio -> Ciudad -> Bool
verificarOrdenEventos anio ciudad = verificarQueEstanOrdenados (eventos anio) ciudad aplicarEventos

verificarOrdenCiudades :: [Ciudad] -> Evento -> Bool
verificarOrdenCiudades ciudades evento = verificarQueEstanOrdenados ciudades evento aplicarACiudades

verificarOrdenAnios :: [Anio] -> Ciudad -> Bool
verificarOrdenAnios anios ciudad = verificarQueEstanOrdenados anios ciudad aplicarAnios

verificarQueEstanOrdenados :: [a] -> b -> (a -> b -> Ciudad) -> Bool
verificarQueEstanOrdenados [cabeza] _ _ = True
verificarQueEstanOrdenados (cabeza1:cabeza2:resto) parametroFijo criterio = comparacionCostoDeVida (criterio cabeza1 parametroFijo) (criterio cabeza2 parametroFijo) && verificarQueEstanOrdenados (cabeza2:resto) parametroFijo criterio

aplicarEventos :: Evento -> Ciudad -> Ciudad
aplicarEventos evento ciudad = evento ciudad

aplicarACiudades :: Ciudad -> Evento -> Ciudad
aplicarACiudades ciudad evento = evento ciudad

aplicarAnios :: Anio -> Ciudad -> Ciudad
aplicarAnios anio ciudad = anioParaRecordar anio ciudad

--PARTE 6
anio2024 :: Anio
anio2024 = UnAnio 2024 eventosInfinitos

eventosInfinitos :: [Evento]
eventosInfinitos = cycle [crisisCiudad, sumarAtraccion "parque", remodelarCiudad 10, remodelarCiudad 20]  --eventos2023->False
--eventosInfinitos = cycle [crisisCiudad, crisisCiudad, crisisCiudad] --> False

--EVENTOS ORDENADOR RESP: Siempre y cuando la lista infinita no este infinitamente ordenada, la funcion ordenarEventos funcionara por
-- la evaluacion perezosa de haskell, ya que si la comparacion de la izquierda del && da False, no se molestara en seguir revisando el resto de la lista
-- Por otro lado, si la lista está infinitamente ordenada, se quedara en bucle analizando la lista y no dará ningún resultado
-- En conclusión, la funcion siempre que funcione devolverá False

--CIUDADES ORDENADAS RESP: la funcion ordenarCiudades funcionará como es debido, ya que, no recibe años

--AÑOS ORDENADOS RESP: Siempre que la funcion ordenarAnios analice el anio2024 se va a romper, porque entra en un bucle infinito
-- La única forma para que no se romp, es que la comparacion con los años anteriores devuelva False
-- Y en conclusión, al igual que en ordenarEventos, lo único que puede devolver esta funcion con el anño 2024 es False
module Analizador where

import Evento

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (toGregorian)

-- FUNCION AUXILIAR: REDONDEAR A 2 DECIMALES
redondear2 :: Double -> Double
redondear2 valor = fromIntegral (round (valor * 100)) / 100.0

-- FUNCION AUXILIAR: QUITAR REPETIDOS
quitarRepetidos :: Eq a => [a] -> [a]
quitarRepetidos [] = []
quitarRepetidos (x:xs) = x : quitarRepetidos (filter (\y -> y /= x) xs)

-- FUNCIONES AUXILIAR: OBTENER MES Y AÑO
obtenerAnioMes :: Int -> (Integer, Int)
obtenerAnioMes ts =
    let utcTime = posixSecondsToUTCTime (fromIntegral ts)
        (anio, mes, _) = toGregorian (utctDay utcTime)
    in (anio, mes)

obtenerAnio :: Int -> Integer
obtenerAnio ts = fst (obtenerAnioMes ts)

-- FUNCION AUXILIAR: ORDENAMIENTO QUICKSORT
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (pivote:resto) = 
    ordenar [menor | menor <- resto, menor <= pivote] 
    ++ [pivote] 
    ++ ordenar [mayor | mayor <- resto, mayor > pivote]



-- 1. CATEGORÍA MÁS FRECUENTE
categoriaMasFrecuente :: [Evento] -> String
categoriaMasFrecuente [] = "No hay eventos cargados"
categoriaMasFrecuente eventos =
    let categorias = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]
        conteos = [ (length (filter (\e -> categoria e == cat) eventos), cat) | cat <- categorias ]
        
        (_, mejorCategoria) = maximum conteos
    in mejorCategoria

-- 2. TOTAL DE VENTAS
totalDeVentas :: [Evento] -> Double
totalDeVentas eventos =
    let compras = filter (\e -> categoria e == "compra") eventos
    in redondear2(sum (map valor compras))


-- 3. TOTAL DE VENTAS MENSUALES Y ANUALES
ventasPorMesYAnio :: [Evento] -> [((Integer, Int), Double)]
ventasPorMesYAnio eventos =
    let compras = filter (\e -> categoria e == "compra") eventos
        fechasUnicas = quitarRepetidos [ obtenerAnioMes (timestamp e) | e <- compras ]
        
        resultados = [ (fecha, redondear2 (sumaVentasFecha fecha compras)) | fecha <- fechasUnicas ]
    in ordenar resultados

sumaVentasFecha :: (Integer, Int) -> [Evento] -> Double
sumaVentasFecha fechaDeseada compras =
    let comprasDeEsaFecha = filter (\e -> obtenerAnioMes (timestamp e) == fechaDeseada) compras
    in sum (map valor comprasDeEsaFecha)

-- 4. PROMEDIO DE VENTAS POR CATEGORÍA POR AÑO
promedioPorCategoriaYAnio :: [Evento] -> [((String, Integer), Double)]
promedioPorCategoriaYAnio eventos =
    let paresUnicos = quitarRepetidos [ (categoria e, obtenerAnio (timestamp e)) | e <- eventos ]
        resultados = [ (par, redondear2 (calcularPromedioPar par eventos)) | par <- paresUnicos ]
    in ordenar resultados

calcularPromedioPar :: (String, Integer) -> [Evento] -> Double
calcularPromedioPar (catDeseada, anioDeseado) eventos =
    let filtrados = filter (\e -> categoria e == catDeseada && obtenerAnio (timestamp e) == anioDeseado) eventos
        cantidad = length filtrados
        suma = sum (map valor filtrados)
    in if cantidad == 0 
       then 0 
       else suma / fromIntegral cantidad
       
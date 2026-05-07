module Analizador where

import Evento

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (toGregorian)

{-
Función: redondear2
Objetivo: Redondear un valor decimal a 2 decimales.
Entrada: Un número Double.
Salida: Un Double redondeado.
Restricciones: El valor debe ser numérico y puede tener pequeñas variaciones por el uso de Double.
-}
redondear2 :: Double -> Double
redondear2 valor = fromIntegral (round (valor * 100)) / 100.0

{-
Función: quitarRepetidos
Objetivo: Eliminar elementos repetidos de una lista.
Entrada: Una lista de elementos comparables.
Salida: La misma lista sin repetidos.
Restricciones: Los elementos deben poder compararse con igualdad.
-}
quitarRepetidos :: Eq a => [a] -> [a]
quitarRepetidos [] = []
quitarRepetidos (x:xs) = x : quitarRepetidos (filter (\y -> y /= x) xs)

{-
Función: obtenerAnioMes
Objetivo: Obtener el año y el mes a partir de un timestamp.
Entrada: Un Int que representa una fecha en formato POSIX.
Salida: Una tupla con año y mes.
Restricciones: El timestamp debe estar en segundos POSIX.
-}
obtenerAnioMes :: Int -> (Integer, Int)
obtenerAnioMes ts =
    let utcTime = posixSecondsToUTCTime (fromIntegral ts)
        (anio, mes, _) = toGregorian (utctDay utcTime)
    in (anio, mes)

{-
Función: obtenerAnio
Objetivo: Obtener solamente el año de un timestamp.
Entrada: Un Int en formato POSIX.
Salida: Un Integer con el año.
Restricciones: Depende de obtenerAnioMes y requiere un timestamp válido.
-}
obtenerAnio :: Int -> Integer
obtenerAnio ts = fst (obtenerAnioMes ts)

{-
Función: ordenar
Objetivo: Ordenar una lista de menor a mayor usando quicksort.
Entrada: Una lista de elementos ordenables.
Salida: La lista ordenada.
Restricciones: Los elementos deben poder compararse con Ord.
-}
ordenar :: Ord a => [a] -> [a]
ordenar [] = []
ordenar (pivote:resto) = 
    ordenar [menor | menor <- resto, menor <= pivote] 
    ++ [pivote] 
    ++ ordenar [mayor | mayor <- resto, mayor > pivote]



{-
Función: categoriaMasFrecuente
Objetivo: Identificar la categoría que más se repite en los eventos.
Entrada: Una lista de eventos.
Salida: Un String con la categoría más frecuente.
Restricciones: Solo toma en cuenta categorías predefinidas. Si no hay eventos, devuelve un mensaje.
-}
categoriaMasFrecuente :: [Evento] -> String
categoriaMasFrecuente [] = "No hay eventos cargados"
categoriaMasFrecuente eventos =
    let categorias = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]
        conteos = [ (length (filter (\e -> categoria e == cat) eventos), cat) | cat <- categorias ]
        
        (_, mejorCategoria) = maximum conteos
    in mejorCategoria

{-
Función: totalDeVentas
Objetivo: Calcular el total vendido considerando solo eventos de compra.
Entrada: Una lista de eventos.
Salida: Un Double con el total de ventas.
Restricciones: Solo suma eventos con categoría "compra" y redondea el resultado a 2 decimales.
-}
totalDeVentas :: [Evento] -> Double
totalDeVentas eventos =
    let compras = filter (\e -> categoria e == "compra") eventos
    in redondear2(sum (map valor compras))


{-
Función: ventasPorMesYAnio
Objetivo: Agrupar las ventas segun mes y año
Entrada: Una lista de eventos
Salida: Una lista con pares ((año, mes), total).
Restricciones: Solo usa eventos de compra y requiere timestamps válidos.
-}
ventasPorMesYAnio :: [Evento] -> [((Integer, Int), Double)]
ventasPorMesYAnio eventos =
    let compras = filter (\e -> categoria e == "compra") eventos
        fechasUnicas = quitarRepetidos [ obtenerAnioMes (timestamp e) | e <- compras ]
        
        resultados = [ (fecha, redondear2 (sumaVentasFecha fecha compras)) | fecha <- fechasUnicas ]
    in ordenar resultados

{-
Función: sumaVentasFecha
Objetivo: Sumar las ventas de un mes y año específicos.
Entrada: Una fecha (año, mes) y una lista de eventos de compra.
Salida: Un Double con la suma de ventas.
Restricciones: La lista recibida debería contener compras, ya que no filtra la categoría nuevamente.
-}
sumaVentasFecha :: (Integer, Int) -> [Evento] -> Double
sumaVentasFecha fechaDeseada compras =
    let comprasDeEsaFecha = filter (\e -> obtenerAnioMes (timestamp e) == fechaDeseada) compras
    in sum (map valor comprasDeEsaFecha)

{-
Función: promedioPorCategoriaYAnio
Objetivo: Calcular el promedio del valor de eventos por categoría y año
Entrada: Una lista de eventos
Salida: Una lista con pares ((categoría, año), promedio)
Restricciones: Usa todas las categorías presentes y requiere timestamps válidos
-}
promedioPorCategoriaYAnio :: [Evento] -> [((String, Integer), Double)]
promedioPorCategoriaYAnio eventos =
    let paresUnicos = quitarRepetidos [ (categoria e, obtenerAnio (timestamp e)) | e <- eventos ]
        resultados = [ (par, redondear2 (calcularPromedioPar par eventos)) | par <- paresUnicos ]
    in ordenar resultados

{-
Función: calcularPromedioPar
Objetivo: Calcular el promedio para una categoría y año específicos
Entrada: Una tupla (categoría, año) y una lista de eventos
Salida: Un double con el promedio
Restricciones: Si no hay eventos que coincidan devuelve 0 para evitar división entre cero.
-}
calcularPromedioPar :: (String, Integer) -> [Evento] -> Double
calcularPromedioPar (catDeseada, anioDeseado) eventos =
    let filtrados = filter (\e -> categoria e == catDeseada && obtenerAnio (timestamp e) == anioDeseado) eventos
        cantidad = length filtrados
        suma = sum (map valor filtrados)
    in if cantidad == 0 
       then 0 
       else suma / fromIntegral cantidad
       
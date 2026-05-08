{-
 - Archivo: Busqueda.hs
 - Descripcion: Implementa la lógica de filtrado para encontrar transacciones específicas dentro de un rango de fechas ingresado por el usuario.
 - Autores: Ginger Rodríguez G.
 -}

module Busqueda where

import Evento
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (toGregorian)

{-
Función: obtenerFechaCompleta
Objetivo: Convertir un timestamp en una fecha completa con año, mes y día.
Entrada: Un Int que representa una fecha en formato POSIX.
Salida: Una tupla (año, mes, día).
Restricciones: El timestamp debe ser valido y estar en segundos POSIX
-}
obtenerFechaCompleta :: Int -> (Integer, Int, Int)
obtenerFechaCompleta ts =
    let utcTime = posixSecondsToUTCTime (fromIntegral ts)
    in toGregorian (utctDay utcTime)

{-
Función: buscarVentasPorRango
Objetivo: Buscar las compras realizadas dentro de un rango de fechas.
Entrada: Una fecha de inicio, una fecha final y una lista de eventos.
Salida: Una lista de eventos de compra que están dentro del rango indicado.
Restricciones: Solo toma en cuenta eventos con categoría "compra". Las fechas deben tener el formato (año, mes, día) y la fecha de inicio debe ser menor o igual a la fecha final.
-}
buscarVentasPorRango :: (Integer, Int, Int) -> (Integer, Int, Int) -> [Evento] -> [Evento]
buscarVentasPorRango fechaInicio fechaFin eventos =
    let compras = filter (\e -> categoria e == "compra") eventos
        
    in filter (\e -> let fechaEvento = obtenerFechaCompleta (timestamp e) 
                     in fechaEvento >= fechaInicio && fechaEvento <= fechaFin) compras
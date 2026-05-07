module Busqueda where

import Evento
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (toGregorian)

obtenerFechaCompleta :: Int -> (Integer, Int, Int)
obtenerFechaCompleta ts =
    let utcTime = posixSecondsToUTCTime (fromIntegral ts)
    in toGregorian (utctDay utcTime)

buscarVentasPorRango :: (Integer, Int, Int) -> (Integer, Int, Int) -> [Evento] -> [Evento]
buscarVentasPorRango fechaInicio fechaFin eventos =
    let compras = filter (\e -> categoria e == "compra") eventos
        
    in filter (\e -> let fechaEvento = obtenerFechaCompleta (timestamp e) 
                     in fechaEvento >= fechaInicio && fechaEvento <= fechaFin) compras
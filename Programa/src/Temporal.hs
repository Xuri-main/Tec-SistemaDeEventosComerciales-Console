module Temporal where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (dayOfWeek)

import Evento
import Analizador


-- 1. MES CON MAYOR VENTA Y DÍA MÁS ACTIVO

-- Mes con mayor venta
mesMayorVenta :: [Evento] -> ((Integer, Int), Double)
mesMayorVenta eventos =
    let ventas = ventasPorMesYAnio eventos
    in buscarMayor ventas

-- Función auxiliar: Buscar la tupla con el monto más grande
buscarMayor :: [((Integer, Int), Double)] -> ((Integer, Int), Double)
buscarMayor [] = ((0,0), 0.0)
buscarMayor [(fecha, monto)] = (fecha, monto)
buscarMayor ((fecha, monto):resto) =
    let (fechaMax, montoMax) = buscarMayor resto
    in if monto > montoMax then (fecha, monto) else (fechaMax, montoMax)

-- Día más activo
traducirDia :: Int -> String
traducirDia ts =
    let utc = posixSecondsToUTCTime (fromIntegral ts)
        dia = fromEnum (dayOfWeek (utctDay utc)) 
    in case dia of
        1 -> "Lunes"
        2 -> "Martes"
        3 -> "Miercoles"
        4 -> "Jueves"
        5 -> "Viernes"
        6 -> "Sabado"
        7 -> "Domingo"
        _ -> "Desconocido"

diaMasActivo :: [Evento] -> String
diaMasActivo eventos =
    let dias = ["Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"]
        conteos = [ (length (filter (\e -> traducirDia (timestamp e) == dia) eventos), dia) | dia <- dias ]
        (_, mejorDia) = maximum conteos
    in mejorDia


-- 2. EVENTO MÁS ANTIGUO Y RECIENTE

-- Devuelve una tupla con (EventoAntiguo, EventoReciente)
extremosTemporales :: [Evento] -> (Evento, Evento)
extremosTemporales eventos =
    let minTs = minimum [ timestamp e | e <- eventos ]
        maxTs = maximum [ timestamp e | e <- eventos ]

        antiguo = head (filter (\e -> timestamp e == minTs) eventos)
        reciente = head (filter (\e -> timestamp e == maxTs) eventos)
    in (antiguo, reciente)


-- 3. RESUMEN DE VENTAS POR INTERVALO
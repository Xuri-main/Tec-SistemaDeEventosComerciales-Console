module Temporal where

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Data.Time.Clock (utctDay)
import Data.Time.Calendar (dayOfWeek)

import Evento
import Analizador


-- 1. MES CON MAYOR VENTA Y DÍA MÁS ACTIVO

{-
Función: mesMayorVenta
Objetivo: Identificar el mes y año donde hubo mayor monto de ventas.
Entrada: Una lista de eventos.
Salida: Una tupla ((año, mes), total) con el periodo de mayor venta.
Restricciones: Depende de ventasPorMesYAnio. Si no hay ventas, buscarMayor devuelve ((0,0),0.0).
-}
mesMayorVenta :: [Evento] -> ((Integer, Int), Double)
mesMayorVenta eventos =
    let ventas = ventasPorMesYAnio eventos
    in buscarMayor ventas

{-
Función: buscarMayor
Objetivo: Buscar dentro de una lista el registro con el monto más alto.
Entrada: Una lista de tuplas con fecha y monto.
Salida: La tupla con el mayor monto.
Restricciones: Si la lista está vacía, devuelve ((0,0),0.0). Compara únicamente el monto.
-}
buscarMayor :: [((Integer, Int), Double)] -> ((Integer, Int), Double)
buscarMayor [] = ((0,0), 0.0)
buscarMayor [(fecha, monto)] = (fecha, monto)
buscarMayor ((fecha, monto):resto) =
    let (fechaMax, montoMax) = buscarMayor resto
    in if monto > montoMax then (fecha, monto) else (fechaMax, montoMax)

{-
Función: traducirDia
Objetivo: Convertir un timestamp en el nombre del día de la semana.
Entrada: Un Int que representa una fecha
Salida: Un String con el día correspondiente.
Restricciones: El timestamp debe ser válido. Si no coincide con ningún día, devuelve "Desconocido".
-}
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

{-
Función: diaMasActivo
Objetivo: Determinar qué día de la semana tiene más eventos registrados.
Entrada: Una lista de eventos.
Salida: Un String con el día más activo.
Restricciones: Cuenta todos los eventos sin importar categoría. En caso de empate, maximum decide según la comparación interna.
-}
diaMasActivo :: [Evento] -> String
diaMasActivo eventos =
    let dias = ["Lunes", "Martes", "Miercoles", "Jueves", "Viernes", "Sabado", "Domingo"]
        conteos = [ (length (filter (\e -> traducirDia (timestamp e) == dia) eventos), dia) | dia <- dias ]
        (_, mejorDia) = maximum conteos
    in mejorDia


-- 2. EVENTO MÁS ANTIGUO Y RECIENTE

{-
Función: extremosTemporales
Objetivo: Obtener el evento más antiguo y el evento más reciente según su timestamp.
Entrada: Una lista de eventos.
Salida: Una tupla con (eventoAntiguo, eventoReciente).
Restricciones: La lista no debe estar vacía, porque usa minimum, maximum y head.
-}
extremosTemporales :: [Evento] -> (Evento, Evento)
extremosTemporales eventos =
    let minTs = minimum [ timestamp e | e <- eventos ]
        maxTs = maximum [ timestamp e | e <- eventos ]

        antiguo = head (filter (\e -> timestamp e == minTs) eventos)
        reciente = head (filter (\e -> timestamp e == maxTs) eventos)
    in (antiguo, reciente)


-- 3. RESUMEN DE VENTAS POR INTERVALO
module Generador where

import Evento
import System.Random (randomRIO)
import Data.Time.Clock.POSIX (getPOSIXTime)

{-
Global: categorias
Objetivo: Guardar las categorías válidas que puede tener un evento.
Entrada: No recibe entrada.
Salida: Una lista de String con las categorías disponibles.
Restricciones: Los eventos generados solo usarán categorías que estén dentro de esta lista.
-}
categorias :: [String]
categorias = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]

{-
Función: generarEventos
Objetivo: Generar automáticamente entre 10 y 25 eventos aleatorios.
Entrada: Una lista de eventos ya existentes.
Salida: Una acción que devuelve una lista de nuevos eventos.
Restricciones: Los nuevos eventos no deben repetir ids existentes y sus fechas se generan desde el día actual hasta dos años después.
-}
generarEventos :: [Evento] -> IO [Evento]
generarEventos eventosExistentes = do
    cantidadGenerar <- randomRIO (10, 25)

    -- Tiempo desde día actual hasta 2 años siguientes 
    tiempoActualPOSIX <- getPOSIXTime
    let tiempoActual = round tiempoActualPOSIX :: Int
    let segundosEnDosAnios = 2 * 365 * 24 * 60 * 60
    let dosAniosDespues = tiempoActual + segundosEnDosAnios

    -- Extraer los ids ya usados
    let idsYaUsados = extraerIds eventosExistentes

    generarNEventos cantidadGenerar idsYaUsados tiempoActual dosAniosDespues

{-
Función: extraerIds
Objetivo: Obtener todos los ids de una lista de eventos.
Entrada: Una lista de eventos.
Salida: Una lista de enteros con los idEvento.
Restricciones: Cada evento debe tener correctamente definido su campo idEvento.
-}
extraerIds :: [Evento] -> [Int]
extraerIds = map idEvento

{-
Función: generarNEventos
Objetivo: Generar una cantidad específica de eventos aleatorios de forma recursiva.
Entrada: Cantidad de eventos faltantes, lista de ids usados, tiempo mínimo y tiempo máximo.
Salida: Una acción IO que devuelve una lista de eventos generados.
Restricciones: Si el id generado ya existe, se intenta generar otro. La categoría, valor y timestamp se asignan aleatoriamente dentro de los rangos definidos.
-}
generarNEventos :: Int -> [Int] -> Int -> Int -> IO [Evento]
generarNEventos 0 _ _ _ = return [] -- Caso base
generarNEventos cantidadFaltante idsYaUsados tiempoMin tiempoMax = do

    idCandidato <- randomRIO (0, 9000000)

    if idCandidato `elem` idsYaUsados
        then do
            generarNEventos cantidadFaltante idsYaUsados tiempoMin tiempoMax
        else do
            indiceCategoria <- randomRIO (0, length categorias - 1)
            let categoriaSeleccionada = categorias !! indiceCategoria

            valorAleatorio <- randomRIO (500.0, 75000.0)
            timestampAleatorio <- randomRIO (tiempoMin, tiempoMax)

            let eventoNuevo = Evento idCandidato categoriaSeleccionada valorAleatorio timestampAleatorio ""

            let nuevosIdsYaUsados = idCandidato : idsYaUsados

            restoEventos <- generarNEventos (cantidadFaltante - 1) nuevosIdsYaUsados tiempoMin tiempoMax

            return (eventoNuevo : restoEventos)
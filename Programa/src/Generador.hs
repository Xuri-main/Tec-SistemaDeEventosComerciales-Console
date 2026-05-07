module Generador where

import Evento
import System.Random (randomRIO)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- Global
categorias :: [String]
categorias = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]

-- Funcion Principal
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

-- Función auxiliar
extraerIds :: [Evento] -> [Int]
extraerIds = map idEvento

-- Funcion Recursiva
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
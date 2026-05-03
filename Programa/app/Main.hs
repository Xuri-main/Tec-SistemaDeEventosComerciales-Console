module Main where

import System.IO (hFlush, stdout)
import Evento 

main :: IO ()
main = do
    putStrLn "[INFO] Iniciando Sistema de Eventos Comerciales..."
    menuPrincipal []

menuPrincipal :: [Evento] -> IO ()
menuPrincipal eventos = do
    putStrLn "\n========================================"
    putStrLn "            MENU PRINCIPAL"
    putStrLn "========================================"
    putStrLn "1. Gestionar Eventos"
    putStrLn "2. Transformacion de eventos"
    putStrLn "3. Analisis de datos"
    putStrLn "4. Analisis temporal"
    putStrLn "5. Busqueda"
    putStrLn "6. Estadisticas"
    putStrLn "7. Salir"
    putStrLn "========================================"
    putStr "Seleccione una opcion: "
    
    hFlush stdout 

    opcion <- getLine

    case opcion of
        "1" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "2" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "3" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "4" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "5" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "6" -> do
            putStrLn "\n[INFO] Modulo en mantenimiento..."
            menuPrincipal eventos

        "7" -> do
            putStrLn "\n[INFO] Saliendo del sistema..."

        _ -> do
            putStrLn "\n[ERROR] Opcion no valida. Intente de nuevo."
            menuPrincipal eventos

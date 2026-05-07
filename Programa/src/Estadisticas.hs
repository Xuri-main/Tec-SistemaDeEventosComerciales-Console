module Estadisticas where

import Evento
import Analizador (quitarRepetidos)

-- Lista de categorías base para iterar
categoriasBase :: [String]
categoriasBase = ["visualizacion", "apartado", "compra", "devolucion", "seguimiento"]

-- CATEGORÍA MÁS VENDIDA 
categoriaMasVendida :: [Evento] -> (String, Double)
categoriaMasVendida eventos =
    let montos = [ (cat, sum [valor e | e <- eventos, categoria e == cat]) | cat <- categoriasBase ]
    in buscarMayorMonto montos

buscarMayorMonto :: [(String, Double)] -> (String, Double)
buscarMayorMonto [] = ("Ninguna", 0.0)
buscarMayorMonto [x] = x
buscarMayorMonto ((cat, monto):xs) =
    let (catMax, montoMax) = buscarMayorMonto xs
    in if monto > montoMax then (cat, monto) else (catMax, montoMax)

-- Prepara el texto para el CSV
reporteA :: [Evento] -> [String]
reporteA eventos =
    let (cat, monto) = categoriaMasVendida eventos
    in [ "REPORTE A: Categoria mas vendida por monto",
         "Categoria,Monto Total",
         cat ++ "," ++ show monto ]

-- CATEGORÍA CON MENOR PARTICIPACIÓN
categoriaMenorParticipacion :: [Evento] -> (String, Int)
categoriaMenorParticipacion eventos =
    let conteos = [ (cat, length [e | e <- eventos, categoria e == cat]) | cat <- categoriasBase ]
    in buscarMenorConteo conteos

buscarMenorConteo :: [(String, Int)] -> (String, Int)
buscarMenorConteo [] = ("Ninguna", 0)
buscarMenorConteo [x] = x
buscarMenorConteo ((cat, count):xs) =
    let (catMin, countMin) = buscarMenorConteo xs
    in if count < countMin then (cat, count) else (catMin, countMin)

reporteB :: [Evento] -> [String]
reporteB eventos =
    let (cat, cantidad) = categoriaMenorParticipacion eventos
    in [ "REPORTE B: Categoria con menor participacion",
         "Categoria,Cantidad de Eventos",
         cat ++ "," ++ show cantidad ]

-- RESUMEN GENERAL
cantidadPorCategoria :: [Evento] -> [(String, Int)]
cantidadPorCategoria eventos = [ (cat, length [e | e <- eventos, categoria e == cat]) | cat <- categoriasBase ]

-- Venta más alta y más baja
ventaMasAltaYBaja :: [Evento] -> (Evento, Evento)
ventaMasAltaYBaja eventos =
    let compras = filter (\e -> categoria e == "compra") eventos
    in if null compras 
       then (Evento 0 "N/A" 0.0 0 "N/A", Evento 0 "N/A" 0.0 0 "N/A")
       else let maxV = maximum [ valor c | c <- compras ]
                minV = minimum [ valor c | c <- compras ]
                alta = head (filter (\c -> valor c == maxV) compras)
                baja = head (filter (\c -> valor c == minV) compras)
            in (alta, baja)

-- Variedad de productos
categoriaMayorVariedad :: [Evento] -> (String, Int)
categoriaMayorVariedad eventos =
    let conteosVariedad = [ (cat, length (quitarRepetidos [valor e | e <- eventos, categoria e == cat])) | cat <- categoriasBase ]
    in buscarMayorVariedad conteosVariedad

buscarMayorVariedad :: [(String, Int)] -> (String, Int)
buscarMayorVariedad [x] = x
buscarMayorVariedad ((cat, count):xs) =
    let (catMax, countMax) = buscarMayorVariedad xs
    in if count > countMax then (cat, count) else (catMax, countMax)

-- Prepara el texto para el CSV
reporteC :: [Evento] -> [String]
reporteC eventos =
    let cantidades = cantidadPorCategoria eventos
        (alta, baja) = ventaMasAltaYBaja eventos
        (catVar, cantVar) = categoriaMayorVariedad eventos
        
        lineasCantidades = [ cat ++ "," ++ show cant | (cat, cant) <- cantidades ]
    in [ "REPORTE C: Resumen General" ] ++
       [ "--- Cantidad de ventas por categoria ---", "Categoria,Cantidad" ] ++ lineasCantidades ++
       [ "", "--- Venta mas alta y mas baja ---", "Tipo,ID Evento,Monto" ] ++
       [ "Venta mas alta," ++ show (idEvento alta) ++ "," ++ show (valor alta) ] ++
       [ "Venta mas baja," ++ show (idEvento baja) ++ "," ++ show (valor baja) ] ++
       [ "", "--- Categoria con mayor variedad de precios/productos ---", "Categoria,Precios Diferentes" ] ++
       [ catVar ++ "," ++ show cantVar ]
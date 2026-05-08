{-
 - Archivo: Estadisticas.hs
 - Descripcion: Calcula resúmenes estadísticos avanzados y formatea la información para su posterior exportación a archivos de reporte.
 - Autores: Jareck Levell C.
 -}

module Estadisticas where

import Evento
import Analizador (quitarRepetidos)

{-
Función: categoriaMasVendida
Objetivo: Encontrar la categoría que acumula mayor monto total.
Entrada: Una lista de eventos.
Salida: Una tupla (categoría, monto total).
Restricciones: Usa las categorías definidas en categoriasGlobales y suma el valor de los eventos de cada categoría.
-}
categoriaMasVendida :: [Evento] -> (String, Double)
categoriaMasVendida eventos =
    let montos = [ (cat, sum [valor e | e <- eventos, categoria e == cat]) | cat <- categoriasGlobales ]
    in buscarMayorMonto montos

{-
Función: buscarMayorMonto
Objetivo: Buscar la categoría con el monto más alto.
Entrada: Una lista de tuplas (categoría, monto).
Salida: La tupla con el mayor monto.
Restricciones: Si la lista está vacía, devuelve ("Ninguna",0.0). Compara solamente el monto.
-}
buscarMayorMonto :: [(String, Double)] -> (String, Double)
buscarMayorMonto [] = ("Ninguna", 0.0)
buscarMayorMonto [x] = x
buscarMayorMonto ((cat, monto):xs) =
    let (catMax, montoMax) = buscarMayorMonto xs
    in if monto > montoMax then (cat, monto) else (catMax, montoMax)

{-
Función: reporteA
Objetivo: Preparar el reporte de la categoría más vendida en formato de texto para CSV.
Entrada: Una lista de eventos.
Salida: Una lista de String con el título, encabezado y resultado del reporte.
Restricciones: Depende de categoriaMasVendida y usa mostrar2Decimales para formatear el monto.
-}
reporteA :: [Evento] -> [String]
reporteA eventos =
    let (cat, monto) = categoriaMasVendida eventos
    in [ "REPORTE A: Categoria mas vendida por monto",
         "Categoria,Monto Total",
         cat ++ "," ++ mostrar2Decimales monto ]

{-
Función: categoriaMenorParticipacion
Objetivo: Encontrar la categoría con menor cantidad de eventos.
Entrada: Una lista de eventos.
Salida: Una tupla (categoría, cantidad).
Restricciones: Cuenta eventos usando categoriasGlobales. Si una categoría no aparece, su cantidad será 0.
-}
categoriaMenorParticipacion :: [Evento] -> (String, Int)
categoriaMenorParticipacion eventos =
    let conteos = [ (cat, length [e | e <- eventos, categoria e == cat]) | cat <- categoriasGlobales ]
    in buscarMenorConteo conteos

{-
Función: buscarMenorConteo
Objetivo: Buscar la categoría con menor cantidad de eventos.
Entrada: Una lista de tuplas (categoría, cantidad).
Salida: La tupla con el menor conteo.
Restricciones: Si la lista está vacía, devuelve ("Ninguna",0). Compara solamente la cantidad.
-}
buscarMenorConteo :: [(String, Int)] -> (String, Int)
buscarMenorConteo [] = ("Ninguna", 0)
buscarMenorConteo [x] = x
buscarMenorConteo ((cat, count):xs) =
    let (catMin, countMin) = buscarMenorConteo xs
    in if count < countMin then (cat, count) else (catMin, countMin)

{-
Función: reporteB
Objetivo: Preparar el reporte de la categoría con menor participación para CSV.
Entrada: Una lista de eventos.
Salida: Una lista de String con el título, encabezado y resultado del reporte.
Restricciones: Depende de categoriaMenorParticipacion.
-}
reporteB :: [Evento] -> [String]
reporteB eventos =
    let (cat, cantidad) = categoriaMenorParticipacion eventos
    in [ "REPORTE B: Categoria con menor participacion",
         "Categoria,Cantidad de Eventos",
         cat ++ "," ++ show cantidad ]

{-
Función: cantidadPorCategoria
Objetivo: Contar cuántos eventos hay por cada categoría.
Entrada: Una lista de eventos.
Salida: Una lista de tuplas (categoría, cantidad).
Restricciones: Solo toma en cuenta las categorías definidas en categoriasGlobales.
-}
cantidadPorCategoria :: [Evento] -> [(String, Int)]
cantidadPorCategoria eventos = [ (cat, length [e | e <- eventos, categoria e == cat]) | cat <- categoriasGlobales ]

{-
Función: ventaMasAltaYBaja
Objetivo: Obtener la compra con mayor valor y la compra con menor valor.
Entrada: Una lista de eventos.
Salida: Una tupla (venta más alta, venta más baja).
Restricciones: Solo analiza eventos con categoría "compra". Si no hay compras, devuelve eventos con datos N/A.
-}
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

{-
Función: categoriaMayorVariedad
Objetivo: Encontrar la categoría con mayor cantidad de valores diferentes.
Entrada: Una lista de eventos.
Salida: Una tupla (categoría, cantidad de valores distintos).
Restricciones: Usa quitarRepetidos para contar valores únicos y trabaja con categoriasGlobales.
-}
categoriaMayorVariedad :: [Evento] -> (String, Int)
categoriaMayorVariedad eventos =
    let conteosVariedad = [ (cat, length (quitarRepetidos [valor e | e <- eventos, categoria e == cat])) | cat <- categoriasGlobales ]
    in buscarMayorVariedad conteosVariedad

{-
Función: buscarMayorVariedad
Objetivo: Buscar la categoría con mayor variedad de precios o valores.
Entrada: Una lista de tuplas (categoría, cantidad).
Salida: La tupla con la mayor cantidad.
Restricciones: La lista no debe estar vacía, porque no tiene caso base para [].
-}
buscarMayorVariedad :: [(String, Int)] -> (String, Int)
buscarMayorVariedad [x] = x
buscarMayorVariedad ((cat, count):xs) =
    let (catMax, countMax) = buscarMayorVariedad xs
    in if count > countMax then (cat, count) else (catMax, countMax)

{-
Función: reporteC
Objetivo: Preparar un resumen general con cantidades por categoría, venta más alta, venta más baja y mayor variedad.
Entrada: Una lista de eventos.
Salida: Una lista de String lista para escribirse en un CSV.
Restricciones: Depende de cantidadPorCategoria, ventaMasAltaYBaja, categoriaMayorVariedad y mostrar2Decimales.
-}
reporteC :: [Evento] -> [String]
reporteC eventos =
    let cantidades = cantidadPorCategoria eventos
        (alta, baja) = ventaMasAltaYBaja eventos
        (catVar, cantVar) = categoriaMayorVariedad eventos
        
        lineasCantidades = [ cat ++ "," ++ show cant | (cat, cant) <- cantidades ]
    in [ "REPORTE C: Resumen General" ] ++
       [ "--- Cantidad de ventas por categoria ---", "Categoria,Cantidad" ] ++ lineasCantidades ++
       [ "", "--- Venta mas alta y mas baja ---", "Tipo,ID Evento,Monto" ] ++
       [ "Venta mas alta," ++ show (idEvento alta) ++ "," ++ mostrar2Decimales (valor alta) ] ++
       [ "Venta mas baja," ++ show (idEvento baja) ++ "," ++ mostrar2Decimales (valor baja) ] ++
       [ "", "--- Categoria con mayor variedad de precios/productos ---", "Categoria,Precios Diferentes" ] ++
       [ catVar ++ "," ++ show cantVar ]
library(shiny)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggparliament)
library(plotly)
library(forcats)
library(sf)

options(digits = 2)

Partido <- read.csv("colores.csv")$Partido
provincias <- c("AVILA", "BURGOS", "LEON", 
                "PALENCIA", "SALAMANCA", "SEGOVIA", 
                "SORIA", "VALLADOLID", "ZAMORA",
                "CYL")
datos <- read.table("partidos_2022_compensacion.txt")

escanos <- read.csv("resultados/escanos.csv")
escanos$Provincia <- gsub('Ó', 'O', as.character(escanos$Provincia))
names(escanos) <- gsub('X', '', as.character(names(escanos)))

calcular_cocientes <-
  function(datos, anio, provincia, method = "D'Hont", escanos_provincia) {
    
    if (method == "D'Hont")
    {
      reparto <-
        data.frame(lapply(datos$Votos, function(x)
          x / (1:escanos_provincia)))
      
    } else if (method == "Sainte-Lagüe") {
      reparto <- data.frame(lapply(datos$Votos, function(x)
        x / (seq(
          1, escanos_provincia * 2, by = 2
        ))))
    }
    
    colnames(reparto) <- datos$Partido
    return(reparto)
  }

asignar_escanos <- function(matriz_reparto) {
  n <- dim(matriz_reparto)[1]
  res <- order(matriz_reparto, decreasing = T)[1:n]
  pos <- arrayInd(res, dim(matriz_reparto), useNames = T)[, 2]
  escanos <-
    data.frame(table(factor(colnames(matriz_reparto)[pos]  ,
                            levels = Partido)))
  
  colnames(escanos) <- c("Partido", "Escanos")
  
  return(escanos)
}

asignar_cuota_hare <-
  function(datos, anio, provincia, escanos_provincia) {
    cuota <- (datos %>% select(Votos) %>% sum()) / escanos_provincia
    datos <- datos %>% filter(Partido != "Votos en blanco") #aquí los votos en blanco afectan la cuota
    escanos <-
      datos %>% mutate(Cociente = Votos %/% cuota, Resto = Votos %% cuota)
    numero_asignados <- escanos %>% select(Cociente) %>% sum()
    asignados_cociente <-
      sort(escanos$Resto, decreasing = T)[escanos_provincia - numero_asignados]
    
    escanos <- escanos %>% mutate(Resto = as.numeric(Resto >= asignados_cociente),
                                  Escanos = Cociente + Resto) %>% select(Partido, Escanos)
    
    
    escanos <- as.data.frame(cbind(Partido)) %>% left_join(escanos, by="Partido")
    
    escanos$Partido <- factor(escanos$Partido, levels = Partido)
    escanos$Escanos[is.na(escanos$Escanos)] <- 0
    
    return(escanos)
    
  }

obtener_reparto <- function(datos, anio, provincia, method = "D'Hont", escanos_provincia = "no"){
  provincia <- toupper(provincia)
  datos <- datos %>% filter(Partido != "Votos en blanco")
  if (escanos_provincia == "no") {
    if (provincia == "CYL") {
      escanos_provincia <-
        as.integer(escanos %>% select(as.character(anio)) %>%
                     sum())
    } else {
      escanos_provincia <-
        as.integer(escanos %>% filter(Provincia == provincia) %>%
                     select(as.character(anio)))
    }
  }
  
  if (method == "Cuota Hare"){
    escanos <- asignar_cuota_hare(datos, anio, provincia, escanos_provincia)
  } else {
    reparto <- calcular_cocientes(datos, anio, provincia, method, escanos_provincia)
    escanos <- asignar_escanos(reparto)
  }
  
  return(escanos)
}


resultados_compensacion <- function(datos, anio, barrera_aut, barrera_prov, method){
  
  #obtención de los votos
  votos_circ <- list()
  votos_totales <- list()
  for (i in 1:9){
    provincia <- provincias[i]
    votos_totales[[i]] <- datos %>% filter(Provincia == provincia) %>% 
      select(Partido, Votos, Porc) %>% distinct()
    votos_circ[[i]] <- votos_totales[[i]] %>% filter(Porc > barrera_prov)
  }
  
  votos_circ[[10]] <- datos %>% group_by(Partido) %>%
    select(Partido, VotosCCAA, PorcCCAA) %>% distinct() %>% ungroup() %>%
    filter(PorcCCAA > barrera_aut) %>% rename(Votos = VotosCCAA, Porc = PorcCCAA)
  names(votos_totales) <- provincias[-10]
  names(votos_circ) <- provincias
  
  # asignación de escaños provincial (2 menos para VLL, BU, LE Y SA y 1 menos
  # para el resto)
  
  escanos_comp <- escanos %>% select(Provincia, as.character(anio)) %>% 
    rename(Escanos = as.character(anio))
  escanos_maspobladas <-
    escanos_comp %>% filter(Provincia %in% c("VALLADOLID",
                                             "LEON",
                                             "BURGOS",
                                             "SALAMANCA")) %>% 
    mutate(Escanos = Escanos - 2)
  
  escanos_menpobladas <-
    escanos_comp %>% filter(!Provincia %in% c("VALLADOLID",
                                             "LEON",
                                             "BURGOS",
                                             "SALAMANCA")) %>% 
    mutate(Escanos = Escanos - 1)
  escanos_fijos <- bind_rows(escanos_maspobladas, escanos_menpobladas)
  
  escanos_circ <- list()
  for (i in 1:9){
    provincia <-  provincias[i]
    escanos_prov <- escanos_fijos %>% filter(Provincia == provincia) %>% 
      pull(Escanos)
    escanos_circ[[i]] <- obtener_reparto(votos_circ[[i]], anio, provincia, method, escanos_prov)
  }
  
  frame_circ <- as.data.frame(escanos_circ)[,-seq(3, 2*length(escanos_circ), by=2)]
  names(frame_circ) <- c("Partido", provincias[-10])
  
  # cáclulo de escaños total 
  # se calcula el reparto con los escaños totales, pero se restan del cálculo 
  # final los diputados conseguidos por partidos que han caído por debajo del 
  # umbral autonómico
  
  # primero se calculan los partidos, en el df anterior, que NO están en los votos totales
  # que ya están filtrados (votos_circ$CYL).
  
  partidos_umbral <- votos_circ$CYL$Partido
  escanos_partidos_noumbral <- frame_circ %>% 
    filter(!Partido %in% partidos_umbral) %>% select(!Partido) %>% sum()
  escanos_repartir_comp <- sum(escanos_comp$Escanos) - escanos_partidos_noumbral
  frame_circ$CYL <- obtener_reparto(votos_circ$CYL, anio, "CYL", method,
                                    escanos_repartir_comp)$Escanos
  
  frame_circ$CYL_fijos <- rowSums(frame_circ[,-c(1, 11)]) # se suman los escaños fijos para comparar
  frame_circ <- frame_circ %>% group_by(Partido) %>% 
    mutate(CYL_final = max(CYL, CYL_fijos), COMP = CYL > CYL_fijos) %>% 
    ungroup()
  
  npartidos_comp <- dim(frame_circ[frame_circ$COMP,])[1]

  for (i in 1:npartidos_comp){
    partido <- frame_circ[frame_circ$COMP,][i,1] %>% pull() %>% as.character()
    cocientes <- c()
    dif_esc <- frame_circ[frame_circ$COMP,][i,]$CYL - frame_circ[frame_circ$COMP,][i,]$CYL_fijos
    
    for (j in 1:9){ #posiciones en las provincias en la lista votos_totales
      # los pull son para transformar a enteros
      temp <- votos_totales[[j]] %>% filter(Partido == partido) %>% pull(Votos)
      cocientes[j] <- if(length(temp) == 0) 0 else temp 
      # UPL, 4, ERROR, HACER QUE NUMERIC(0) SEA UN 0
      esc_fijos <- frame_circ[frame_circ$COMP, ][i,j+1] %>% pull()
      divisor <- if (method=="D'Hont") esc_fijos+1 else 2*esc_fijos+1
      cocientes[j] <- cocientes[j]/divisor
    }

    for (j in 1:dif_esc){
      max_cociente <- which.max(cocientes)
      frame_circ[frame_circ$COMP, ][i, max_cociente+1] <- 
        (frame_circ[frame_circ$COMP, ][i, max_cociente+1] %>% pull()) + 1
      esc_fijos <- frame_circ[frame_circ$COMP, ][i, max_cociente+1] %>% pull()
      divisor <- if (method=="D'Hont") esc_fijos+1 else 2*esc_fijos+1
      cocientes[max_cociente] <- cocientes[max_cociente]/divisor
    }
  }
  frame_circ <- frame_circ %>% select(1:10)
  
  return(frame_circ)
}
resultados_compensacion(datos, 2022, 3, 3, "D'Hont")

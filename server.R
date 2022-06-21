library(shiny)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ggparliament)
library(plotly)
library(forcats)
library(sf)

options(digits = 2)

escanos <- read.csv("./resultados/escanos.csv", header = T)
escanos$Provincia <- gsub('Ó', 'O', as.character(escanos$Provincia))
names(escanos) <- gsub('X', '', as.character(names(escanos)))

colores <- read.csv("colores.csv")
Partido <- colores %>% pull(Partido)
colores$Color[colores$Color==""] <- 1:100
colores_partidos <- setNames(colores$Color, colores$Partido)

datos_geo <- read_sf("./provincias/au.prov_cyl_recintos.shp") %>% rename(Provincia = nombre)
datos_geo$Provincia <- datos_geo$Provincia %>% 
  toupper() %>% gsub('Á', 'A', .) %>% gsub('Ó', 'O', .)
muni_geo <- read_sf("./municipios/au.muni_cyl_recintos.shp") %>% rename(Municipio = nombre)
muni_geo$Municipio <- muni_geo$Municipio %>% toupper() %>% 
  gsub('Á', 'A', .) %>% gsub('É', 'E', .) %>% gsub('Í', 'I', .) %>% 
  gsub('Ó', 'O', .) %>% gsub('Ú', 'U', .)


# Se calculan los blancos y nulos por municipio y se añaden como unos partidos más
# Devuelve un dataframe con la Provincia, Municipio, Partido (con blancos y nulos) y Votos
separacion_bn <- function(datos){
  muni <-
    datos %>% group_by(Provincia, Municipio, Partido) %>% summarise(VotosMuni = sum(Nº.Votos))
  blanconulo <-
    datos %>% select(Provincia,
                     Municipio,
                     Distrito,
                     Sección,
                     Mesa,
                     Votos.Blanco,
                     Votos.Nulos) %>% distinct() %>% group_by(Provincia, Municipio) %>%
    summarise("Votos en blanco" = sum(Votos.Blanco),
              "Votos nulos" = sum(Votos.Nulos)) %>%
    gather(key = "Partido",
           value = "VotosMuni",
           "Votos en blanco",
           "Votos nulos") %>% arrange(Provincia, Municipio) # esto que se aparte, mantenerlo SIEMPRE
  bn <-
    muni %>% bind_rows(blanconulo) %>% arrange(Provincia, Municipio) %>% ungroup() # va con nulos.
  return(bn)
}

# Obtiene los resultados para cada partido
resultados_totales <- function(datos) {
  # se calculan los votos a cada partido en cada provincia
  
  prov <- datos %>% filter(Partido != "Votos nulos") %>% 
    group_by(Provincia, Municipio) %>% mutate(PorcMuni = VotosMuni/sum(VotosMuni)*100) %>% 
    group_by(Provincia, Partido) %>% mutate(Votos = sum(VotosMuni)) %>% 
    ungroup(Partido) %>% mutate(Porc = Votos/sum(VotosMuni) * 100) %>% 
    group_by(Partido) %>% mutate(VotosCCAA = sum(VotosMuni)) %>% 
    ungroup() %>% mutate(PorcCCAA = VotosCCAA/sum(VotosMuni)*100)
  # OJO CON EL UMBRAL AQUÍ
  return(prov)
}

# Obtiene los datos totales de los votos y los transforma en una tabla según
# la comunidad o la provincia, con los votos en blanco y los nulos también,
# en total. El porcentaje se calcula respecto a los votos emitidos, incluido nulos

tabla_informacion <- function(datos, provincia){
  datos_votos <- datos %>% 
    group_by(Provincia, Municipio) %>% mutate(PorcMuni = VotosMuni/sum(VotosMuni)*100) %>% 
    group_by(Provincia, Partido) %>% mutate(Votos = sum(VotosMuni)) %>% 
    group_by(Partido) %>% mutate(VotosCCAA = sum(VotosMuni))
  
  if(provincia == "cyl"){
    votos <- datos_votos %>% group_by(Partido) %>% 
      select(Partido, VotosCCAA) %>% distinct() %>% ungroup() %>%
      rename(Votos = VotosCCAA)
  } else {
    provincia <- provincia %>% gsub("á", "a", ., ignore.case = T) %>% 
      gsub("ó", "o", ., ignore.case = T)
    votos <- datos_votos %>% filter(Provincia == toupper(provincia)) %>% 
      select(Partido, Votos) %>% distinct() %>% ungroup()
  }
  
  nulo <- votos %>% filter(Partido == "Votos nulos")
  votos <- votos %>% filter(Partido != "Votos nulos") %>% 
    arrange(desc(Votos)) %>% mutate(Porcentaje = Votos/sum(Votos)*100) %>% 
    filter(Porcentaje > 0.1)
  blanco <- votos %>% filter(Partido == "Votos en blanco")
  votos <- votos %>% filter(Partido != "Votos en blanco")
  votos <- votos %>% bind_rows(blanco) %>% bind_rows(nulo) 
  
  return(votos)
}

# Obtener los datos de cada provincia
resultados_provincia <- function(datos, provincia, min_threshold = 3) {
  provincia <- toupper(provincia)
  if (provincia == "CYL") {
    reparto <- datos %>% group_by(Partido) %>% 
      select(Partido, VotosCCAA, PorcCCAA) %>% distinct() %>% ungroup() %>% 
      filter(PorcCCAA > min_threshold) %>% rename(Votos = VotosCCAA, Porc = PorcCCAA)
  } else {
    reparto <- datos %>% filter(Provincia == provincia, Porc > min_threshold)
  }
  reparto <- reparto %>% select(Partido, Votos, Porc) %>% distinct() # como sale por municipio,
  # hay que elegir únicamente una vez
  return(reparto)
}

calcular_cocientes <-
  function(datos, anio, provincia, method = "D'Hont", escanos_provincia) {

    if (method == "D'Hont")
    {
      reparto <-
        data.frame(lapply(datos$Votos, function(x)
          x / (1:escanos_provincia)))
      
    } else if (method == "Sainte-Laguë") {
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
  provincias <- c("AVILA", "BURGOS", "LEON", 
                  "PALENCIA", "SALAMANCA", "SEGOVIA", 
                  "SORIA", "VALLADOLID", "ZAMORA",
                  "CYL")
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
  frame_circ <- frame_circ %>% select(1:10, 13) %>% rename(CYL = CYL_final)
  
  return(frame_circ)
}

parlamento <-
  function(datos,
           seats_rows = 1,
           seat_size = 6) {
    parl_data <- parliament_data(
      election_data = datos,
      type = "semicircle",
      parl_rows = seats_rows,
      party_seats = datos$Escanos
    )
    
    # Obtención color y orden de los partidos por escaños conseguidos
    parl_data$Color <-
      colores$Color[match(parl_data$Partido, colores$Partido)]
    partidos_orden <- datos %>% arrange(desc(Escanos)) %>% pull(Partido)
    parl_data$Partido <- factor(parl_data$Partido, levels = partidos_orden)
      
    parlamento <-
      ggplot(parl_data, aes(x, y, colour = Partido, label = Escanos)) +
      geom_parliament_seats(size = seat_size) +
      draw_totalseats(n = sum(datos$Escanos), type = "semicircle") +
      theme_ggparliament() +
      theme(axis.line = element_blank(),
            panel.grid = element_blank())+
      scale_colour_manual(values = parl_data$Color,
                          limits = parl_data$Partido)+
      labs(title = "Reparto de escaños")
    
    parlamento <-
      ggplotly(parlamento, tooltip = c("Partido", "Escanos")) %>% 
      layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
             paper_bgcolor = "rgba(0, 0, 0, 0)") # Para transparencia
    
  }

grafico_votos <- function(datos, provincia = F) {

  if (provincia) {
    datos <- datos %>% rename(Porcentaje = Porc)
    
  } else {
    datos <-
      datos %>% group_by(Partido) %>% select(Partido, PorcCCAA, Partido) %>%
      distinct() %>% filter(PorcCCAA > 0.5) # se quitan partidos pequeños
    datos <- datos %>% rename(Porcentaje = PorcCCAA)
  }
  datos$Color <-
    colores$Color[match(datos$Partido, colores$Partido)]
  # datos$Color[is.na(datos$Color)] <- as.character(1:100)
  datos$Partido <-
    fct_rev(fct_reorder(datos$Partido, datos$Porcentaje))
  
  grafico_barras <- ggplot(datos, aes(Partido, Porcentaje)) +
    geom_col(fill = datos$Color) +
    theme_minimal() +
    labs(x = "Partido", y = "Porcentaje de voto") +
    theme(panel.grid = element_blank())+
    scale_colour_manual(values = datos$Color,
                        limits = datos$Partido)+
    labs(title = "Porcentaje de votos",
         x = "",
         y = "Porcentaje")
    
  grafico_barras <- ggplotly(grafico_barras) %>%
    layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)")
  
  return(grafico_barras)
}

# Cálculo de la tabla que compara el reparto con el sistema actual y un sistema 
# propuesto
tabla_comparacion <- function(reparto, reparto_referencia){
  tabla <- reparto_referencia %>% 
    inner_join(reparto, by = c("Partido" = "Partido")) %>% 
    filter(Escanos.x > 0 | Escanos.y > 0) %>% 
    mutate(Escanos.x = as.integer(Escanos.x),
           Escanos.y = as.integer(Escanos.y)) %>% 
    arrange(desc(Escanos.x)) %>% 
    rename("Sistema actual" = Escanos.x, "Sistema propuesto" = Escanos.y) 

  return(tabla)
}

mapa_masvotado <- function(datos, provincia = "cyl"){
  
  if (provincia == "cyl") {
    masvotado_provincia <- datos %>% group_by(Provincia) %>%
      filter(Votos == max(Votos)) %>% select(Provincia, Partido) %>% distinct()
    
    masvotado_provincia$Colores <-
      colores$Color[match(masvotado_provincia$Partido,
                          colores$Partido)]
    # Cambiar de Colores a Color para poder usar join
    
    datos_mapa <-
      datos_geo %>% left_join(masvotado_provincia, by = "Provincia") %>%
      mutate(Texto = paste("Provincia:", Provincia, "\nPartido más votado:", Partido))
  } else {
    provincia <- provincia %>% gsub("á", "a", ., ignore.case = T) %>% 
      gsub("ó", "o", ., ignore.case = T)
    # Se obtiene el más votado en cada municipio y se unifican los casos en los que se repiten
    # municipios
    masvotado_muni <- datos %>% filter(Provincia==toupper(provincia)) %>% group_by(Municipio) %>% 
      filter(VotosMuni == max(VotosMuni)) %>% select(Provincia, Municipio, Partido) %>% 
      group_by(Municipio) %>% mutate(Veces = n()) %>% ungroup()
    masvotado_muni$Partido[masvotado_muni$Veces>1] = "Empate"
    masvotado_muni <- masvotado_muni %>% distinct() %>% select(-Veces)
    
    masvotado_muni$Colores <- colores$Color[match(masvotado_muni$Partido,
                                                  colores$Partido)]
    # Cambiar de Colores a Color para poder usar join
    
    datos_mapa <- muni_geo %>% inner_join(masvotado_muni, by = "Municipio") %>% 
      mutate(Texto = paste("Municipio:", Municipio, "\nPartido más votado:", Partido))
  }
  # browser()
  mapa <- ggplot(datos_mapa, aes(fill = Partido))+ #, text=Texto
    geom_sf()+
    theme_minimal()+
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank())+
    scale_fill_manual(values=colores_partidos)+
    theme(legend.position = "none")+
    labs(title = "Partido más votado")
  mapa <- ggplotly(mapa)
  mapa <-  mapa %>%  style(hoveron = "fills") %>% # PARA QUE SALGA DENTRO Y NO EN EL BORDE
    layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)") # Para transparencia
  
  return(mapa)
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  anio <- reactive(input$eleccion)
  eleccion <-
    reactive(paste("./resultados_limpios/", anio(), ".csv", sep = ""))
  datos <- reactive({
    read.table(
      eleccion(),
      header = T,
      sep = ";",
      dec = ",",
      stringsAsFactors = T
    )
  })
  eleccion_referencia <- reactive(paste("./repartos/", anio(), ".csv", sep = ""))
  reparto_sist_actual <- reactive(read.csv(eleccion_referencia(), header = T))
  res_totales <- reactive(separacion_bn(datos()))
  res_partidos <- reactive(resultados_totales(res_totales()))
  
  ##################################################################################
  ##################################################################################
  ###############################ANÁLISIS ELECTORAL###############################
  ##################################################################################
  ##################################################################################
  
  resultados_avila <-
    reactive(resultados_provincia(res_partidos(), "avila"))
  resultados_burgos <-
    reactive(resultados_provincia(res_partidos(), "burgos"))
  resultados_leon <-
    reactive(resultados_provincia(res_partidos(), "leon"))
  resultados_palencia <-
    reactive(resultados_provincia(res_partidos(), "palencia"))
  resultados_salamanca <-
    reactive(resultados_provincia(res_partidos(), "salamanca"))
  resultados_segovia <-
    reactive(resultados_provincia(res_partidos(), "segovia"))
  resultados_soria <-
    reactive(resultados_provincia(res_partidos(), "soria"))
  resultados_valladolid <-
    reactive(resultados_provincia(res_partidos(), "valladolid"))
  resultados_zamora <-
    reactive(resultados_provincia(res_partidos(), "zamora"))
  
  reparto_avila <-
    reactive(obtener_reparto(resultados_avila(), as.integer(anio()), "avila"))
  reparto_burgos <-
    reactive(obtener_reparto(resultados_burgos(), as.integer(anio()), "burgos"))
  reparto_leon <-
    reactive(obtener_reparto(resultados_leon(), as.integer(anio()), "leon"))
  reparto_palencia <-
    reactive(obtener_reparto(resultados_palencia(), as.integer(anio()), "palencia"))
  reparto_salamanca <-
    reactive(obtener_reparto(resultados_salamanca(), as.integer(anio()), "salamanca"))
  reparto_segovia <-
    reactive(obtener_reparto(resultados_segovia(), as.integer(anio()), "segovia"))
  reparto_soria <-
    reactive(obtener_reparto(resultados_soria(), as.integer(anio()), "soria"))
  reparto_valladolid <-
    reactive(obtener_reparto(resultados_valladolid(), as.integer(anio()), "valladolid"))
  reparto_zamora <-
    reactive(obtener_reparto(resultados_zamora(), as.integer(anio()), "zamora"))
  
  reparto_cyl_temp <- reactive(
    cbind(
      reparto_avila(),
      reparto_burgos()[-1],
      reparto_leon()[-1],
      reparto_palencia()[-1],
      reparto_salamanca()[-1],
      reparto_segovia()[-1],
      reparto_soria()[-1],
      reparto_valladolid()[-1],
      reparto_zamora()[-1]
    )
  )
  
  reparto_cyl <- reactive(cbind(reparto_cyl_temp()[1],
                                Escanos = as.integer(rowSums(
                                  reparto_cyl_temp()[-1]
                                ))))
  # Esto se hace porque si no, hay recursión infinita
  
  output$cortes_analisis <-
    renderPlotly(parlamento(reparto_cyl(),
                            seats_rows = 5,
                            seat_size = 10))
  output$barras_an_autonomico <-
    renderPlotly(grafico_votos(res_partidos()))
  
  output$evolucion_escanos <- 
    renderPlotly(readRDS("./repartos/evolucion_escanos.rds"))
  
  parlamento_provin_an <- reactive(switch(
    input$provincia,
    "Ávila" = parlamento(reparto_avila()),
    "Burgos" = parlamento(reparto_burgos(), seats_rows = 2),
    "León" = parlamento(reparto_leon(), seats_rows = 2),
    "Palencia" = parlamento(reparto_palencia()),
    "Salamanca" = parlamento(reparto_salamanca(), seats_rows = 2),
    "Segovia" = parlamento(reparto_segovia()),
    "Soria" = parlamento(reparto_soria()),
    "Valladolid" = parlamento(reparto_valladolid(), seats_rows = 2),
    "Zamora" = parlamento(reparto_zamora())
  ))
  
  barras_provin_an <- reactive(switch(
    input$provincia,
    "Ávila" = grafico_votos(resultados_avila(), provincia = T),
    "Burgos" = grafico_votos(resultados_burgos(), provincia = T),
    "León" = grafico_votos(resultados_leon(), provincia = T),
    "Palencia" = grafico_votos(resultados_palencia(), provincia = T),
    "Salamanca" = grafico_votos(resultados_salamanca(), provincia = T),
    "Segovia" = grafico_votos(resultados_segovia(), provincia = T),
    "Soria" = grafico_votos(resultados_soria(), provincia = T),
    "Valladolid" = grafico_votos(resultados_valladolid(), provincia = T),
    "Zamora" = grafico_votos(resultados_zamora(), provincia = T)
  ))
  
  output$texto_provincia <- renderText(paste("Provincia:",input$provincia))
  output$mapa_cyl <- renderPlotly(mapa_masvotado(res_partidos()))
  output$tabla_cyl <- renderTable(tabla_informacion(res_totales(), "cyl"))
  
  output$procuradores_provin <-
    renderPlotly(parlamento_provin_an())
  output$barras_an_provincial <- renderPlotly(barras_provin_an())
  output$mapa_prov <- renderPlotly(mapa_masvotado(res_partidos(), input$provincia))
  output$tabla_prov <- renderTable(tabla_informacion(res_totales(), input$provincia))
  
  
  
  
  
  ##################################################################################
  ##################################################################################
  ###############################MÉTODO PROVINCIAL##################################
  ##################################################################################
  ##################################################################################
  
  metodo_prov <- reactive(input$metodo_prov)
  barrera_prov <- reactive(input$barrera_prov)
  output$texto_metodo_prov_tot <- renderText(paste("Método de reparto:", metodo_prov()))
  output$texto_barrera_prov_tot <- renderText(paste("Barrera electoral: ", barrera_prov(), "%", sep=""))
  output$texto_metodo_prov_prov <- renderText(paste("Método de reparto:", metodo_prov()))
  output$texto_barrera_prov_prov <- renderText(paste("Barrera electoral: ", barrera_prov(), "%", sep=""))
  output$texto_provincia_prov_prov <- renderText(paste("Provincia:", input$provincia_prov))
  
  resultados_avila_prov <-
    reactive(resultados_provincia(res_partidos(), "avila", barrera_prov()))
  resultados_burgos_prov <-
    reactive(resultados_provincia(res_partidos(), "burgos", barrera_prov()))
  resultados_leon_prov <-
    reactive(resultados_provincia(res_partidos(), "leon", barrera_prov()))
  resultados_palencia_prov <-
    reactive(resultados_provincia(res_partidos(), "palencia", barrera_prov()))
  resultados_salamanca_prov <-
    reactive(resultados_provincia(res_partidos(), "salamanca", barrera_prov()))
  resultados_segovia_prov <-
    reactive(resultados_provincia(res_partidos(), "segovia", barrera_prov()))
  resultados_soria_prov <-
    reactive(resultados_provincia(res_partidos(), "soria", barrera_prov()))
  resultados_valladolid_prov <-
    reactive(resultados_provincia(res_partidos(), "valladolid", barrera_prov()))
  resultados_zamora_prov <-
    reactive(resultados_provincia(res_partidos(), "zamora", barrera_prov()))
  
  
  reparto_avila_prov <-
    reactive(obtener_reparto(
      resultados_avila_prov(),
      as.integer(anio()),
      "avila",
      metodo_prov()
    ))
  reparto_burgos_prov <-
    reactive(obtener_reparto(
      resultados_burgos_prov(),
      as.integer(anio()),
      "burgos",
      metodo_prov()
    ))
  reparto_leon_prov <-
    reactive(obtener_reparto(
      resultados_leon_prov(),
      as.integer(anio()),
      "leon",
      metodo_prov()
    ))
  reparto_palencia_prov <-
    reactive(obtener_reparto(
      resultados_palencia_prov(),
      as.integer(anio()),
      "palencia",
      metodo_prov()
    ))
  reparto_salamanca_prov <-
    reactive(obtener_reparto(
      resultados_salamanca_prov(),
      as.integer(anio()),
      "salamanca",
      metodo_prov()
    ))
  reparto_segovia_prov <-
    reactive(obtener_reparto(
      resultados_segovia_prov(),
      as.integer(anio()),
      "segovia",
      metodo_prov()
    ))
  reparto_soria_prov <-
    reactive(obtener_reparto(
      resultados_soria_prov(),
      as.integer(anio()),
      "soria",
      metodo_prov()
    ))
  reparto_valladolid_prov <-
    reactive(
      obtener_reparto(
        resultados_valladolid_prov(),
        as.integer(anio()),
        "valladolid",
        metodo_prov()
      )
    )
  reparto_zamora_prov <-
    reactive(obtener_reparto(
      resultados_zamora_prov(),
      as.integer(anio()),
      "zamora",
      metodo_prov()
    ))
  
  reparto_cyl_temp_prov <- reactive(
    cbind(
      reparto_avila_prov(),
      reparto_burgos_prov()[-1],
      reparto_leon_prov()[-1],
      reparto_palencia_prov()[-1],
      reparto_salamanca_prov()[-1],
      reparto_segovia_prov()[-1],
      reparto_soria_prov()[-1],
      reparto_valladolid_prov()[-1],
      reparto_zamora_prov()[-1]
    )
  )
  
  
  reparto_cyl_prov <- reactive(cbind(reparto_cyl_temp_prov()[1],
                                     Escanos = as.integer(rowSums(
                                       reparto_cyl_temp_prov()[-1]
                                     ))))
  # Esto se hace porque si no, hay recursión infinita
  
  tabla_prov <- reactive(tabla_comparacion(reparto_cyl_prov(), reparto_sist_actual()))
  output$comp_prov <- renderTable(tabla_prov())
  
  output$cortes_prov <-
    renderPlotly(parlamento(
      reparto_cyl_prov(),
      seats_rows = 5,
      seat_size = 10
    ))
  
  parlamento_provin_prov <- reactive(switch(
    input$provincia_prov,
    "Ávila" = parlamento(reparto_avila_prov()),
    "Burgos" = parlamento(reparto_burgos_prov(), seats_rows = 2),
    "León" = parlamento(reparto_leon_prov(), seats_rows = 2),
    "Palencia" = parlamento(reparto_palencia_prov()),
    "Salamanca" = parlamento(reparto_salamanca_prov(), seats_rows = 2),
    "Segovia" = parlamento(reparto_segovia_prov()),
    "Soria" = parlamento(reparto_soria_prov()),
    "Valladolid" = parlamento(reparto_valladolid_prov(), seats_rows = 2),
    "Zamora" = parlamento(reparto_zamora_prov())
  ))
  
  output$procuradores_provin_prov <-
    renderPlotly(parlamento_provin_prov())
  
  
  ##################################################################################
  ##################################################################################
  ###############################MÉTODO AUTONÓMICO##################################
  ##################################################################################
  ##################################################################################
  
  metodo_aut <- reactive(input$metodo_aut)
  barrera_aut <- reactive(input$barrera_aut)
  output$texto_metodo_aut <- renderText(paste("Método de reparto:", metodo_aut()))
  output$texto_barrera_aut <- renderText(paste("Barrera electoral: ", barrera_aut(), "%", sep=""))
  
  resultados_cyl_aut <- reactive(resultados_provincia(res_partidos(), "cyl", 
                                                      min_threshold = barrera_aut()))
  reparto_cyl_aut <- reactive(obtener_reparto(resultados_cyl_aut(), anio(), "cyl", method = metodo_aut()))
  
  output$cortes_aut <- renderPlotly(parlamento(reparto_cyl_aut(), seats_rows = 5, seat_size = 10))

  tabla_aut <- reactive(tabla_comparacion(reparto_cyl_aut(), reparto_sist_actual()))
  output$comp_aut <- renderTable(tabla_aut())
  ##################################################################################
  ##################################################################################
  ##################################COMPENSACIÓN####################################
  ##################################################################################
  ##################################################################################
  
  metodo_comp <- reactive(input$metodo_comp)
  barrera_comp_aut <- reactive(input$barrera_comp_aut)
  barrera_comp_prov <- reactive(input$barrera_comp_prov)
  prov_comp <- reactive(input$provincia_comp_prov %>% toupper() %>% gsub('Á', 'A', .) %>% 
                          gsub('Ó', 'O', .))
  
  output$texto_metodo_comp_tot <- renderText(paste("Método de reparto:", metodo_comp()))
  output$texto_barrera_aut_comp_tot <- renderText(paste("Barrera electoral: ", barrera_comp_aut(), "%", sep=""))
  output$texto_barrera_prov_comp_tot <- renderText(paste("Barrera electoral: ", barrera_comp_prov(), "%", sep=""))
  output$texto_metodo_comp_prov <- renderText(paste("Método de reparto:", metodo_comp()))
  output$texto_barrera_aut_comp_prov <- renderText(paste("Barrera electoral: ", barrera_comp_aut(), "%", sep=""))
  output$texto_barrera_prov_comp_prov <- renderText(paste("Barrera electoral: ", barrera_comp_prov(), "%", sep=""))
  output$texto_provincia_comp_prov <- renderText(paste("Provincia:", input$provincia_comp))
  
  reparto_compensacion <- reactive(resultados_compensacion(res_partidos(), anio(), 
                                                           barrera_comp_aut(), 
                                                           barrera_comp_prov(), 
                                                           metodo_comp()))
  reparto_comp_total <- reactive(reparto_compensacion() %>% select(Partido, CYL) %>% 
    rename(Escanos = CYL))
  reparto_comp_prov <- reactive({
    reparto_comp_prov <- reparto_compensacion() %>% select(Partido, prov_comp())
    colnames(reparto_comp_prov)[2] <- "Escanos"
    reparto_comp_prov
    })
  
  
  seats_prov <- reactive(if(prov_comp() == "VALLADOLID" | prov_comp() == "LEON" |
                   prov_comp() == "BURGOS" | prov_comp() == "SALAMANCA") 2 else 1)
  
  output$cortes_comp <- renderPlotly(parlamento(reparto_comp_total(), 
                                                seats_rows = 5, seat_size = 10))
  output$procuradores_provin_comp <- renderPlotly(parlamento(reparto_comp_prov(), 
                                                             seats_rows = seats_prov()))
  tabla_comp <- reactive(tabla_comparacion(reparto_comp_total(), reparto_sist_actual()))
  output$comp_compens <- renderTable(tabla_comp())
  })

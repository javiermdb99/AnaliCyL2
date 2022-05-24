#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
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

obtener_reparto <- function(datos, anio, provincia, method = "D'Hont"){
  provincia <- toupper(provincia)
  datos <- datos %>% filter(Partido != "Votos en blanco")
  
  if(provincia == "CYL"){
    escanos_provincia <- as.integer(escanos %>% select(as.character(anio)) %>% 
                                      sum())
  } else {
  escanos_provincia <-
    as.integer(escanos %>% filter(Provincia == provincia) %>%
                 select(as.character(anio)))}

  if (method == "Cuota Hare"){
    escanos <- asignar_cuota_hare(datos, anio, provincia, escanos_provincia)
  } else {
    reparto <- calcular_cocientes(datos, anio, provincia, method, escanos_provincia)
    escanos <- asignar_escanos(reparto)
  }
  
  return(escanos)
}

parlamento <-
  function(datos,
           seats_rows = 1,
           seat_size = 10) {
    parl_data <- parliament_data(
      election_data = datos,
      type = "semicircle",
      parl_rows = seats_rows,
      party_seats = datos$Escanos
    )

    parl_data$Color <-
      colores$Color[match(parl_data$Partido, colores$Partido)]
    # parl_data$Color[is.na(parl_data$Color)] <- as.character(1:100)
    parlamento <-
      ggplot(parl_data, aes(x, y, colour = Partido, label = Escanos)) +
      geom_parliament_seats(size = seat_size) +
      draw_totalseats(n = sum(datos$Escanos), type = "semicircle") +
      theme_ggparliament() +
      theme(axis.line = element_blank(),
            panel.grid = element_blank())+
      scale_colour_manual(values = parl_data$Color,
                          limits = parl_data$Partido)
    
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
                        limits = datos$Partido)
  grafico_barras <- ggplotly(grafico_barras) %>%
    layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)")
  
  return(grafico_barras)
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
  
  mapa <- ggplot(datos_mapa, aes(label = Partido))+ #
    geom_sf(fill=datos_mapa$Colores)+
    theme_minimal()+
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank())
  
  mapa <- ggplotly(mapa) %>%  style(hoveron = "fill") %>% # PARA QUE SALGA DENTRO Y NO EN EL BORDE
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
  #res_partidos <- reactive(resultados_totales(datos()))
  
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
  #res_partidos <- reactive(resultados_totales(datos()))
  
  resultados_cyl_aut <- reactive(resultados_provincia(res_partidos(), "cyl", 
                                                      min_threshold = barrera_aut()))
  reparto_cyl_aut <- reactive(obtener_reparto(resultados_cyl_aut(), anio(), "cyl", method = metodo_aut()))
  
  output$cortes_aut <- renderPlotly(parlamento(reparto_cyl_aut(), seats_rows = 5, seat_size = 10))
})

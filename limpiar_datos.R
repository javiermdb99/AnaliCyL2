# Esta función limpia de acentos el dataframe que se ha pasado como argumento,
# así como normaliza los nombres de los partidos a los nombres más conocidos a nivel
# estatal y autonómico
clean <- function(df) {
  df$Partido <- factor(gsub('\\.', "", df$Partido))
  df$Censo <- as.integer(gsub('\\.', "", as.character(df$Censo)))
  
  df$Provincia <- gsub('Á', 'A', as.character(df$Provincia))
  df$Provincia <- gsub('É', 'E', as.character(df$Provincia))
  df$Provincia <- gsub('Í', 'I', as.character(df$Provincia))
  df$Provincia <- gsub('Ó', 'O', as.character(df$Provincia))
  df$Provincia <- gsub('Ú', 'U', as.character(df$Provincia))
  
  df$Municipio <- gsub('Á', 'A', as.character(df$Municipio))
  df$Municipio <- gsub('É', 'E', as.character(df$Municipio))
  df$Municipio <- gsub('Í', 'I', as.character(df$Municipio))
  df$Municipio <- gsub('Ó', 'O', as.character(df$Municipio))
  df$Municipio <- gsub('Ú', 'U', as.character(df$Municipio))
  
  
  df$Partido <-
    as.factor(gsub('ñ', 'n', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('Ñ', 'N', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub(' ', '', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub(
      '.*PODEMOS.*',
      'Podemos',
      as.character(df$Partido),
      ignore.case = T
    ))
  df$Partido <-
    as.factor(gsub('.*IU.*', 'IU', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*PNC.*', 'PNC', as.character(df$Partido), ignore.case = T))
  df$Partido <-
    as.factor(gsub('APB.*', 'APB', as.character(df$Partido))) # cuidado
  df$Partido <-
    as.factor(gsub('AP-.*', 'AP', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('AP', 'AP', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*PSOE.*', 'PSOE', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*PREPAL.*', 'PREPAL', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*PCE.*', 'PCE', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*TC.*', 'PCAS', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*PCAL.*', 'PCAS', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*UPSA.*', 'UPSa', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*PTE.*', 'PTE', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*URCL.*', 'URCL', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('.*CDS.*', 'CDS', as.character(df$Partido)))
  
  return(df)
}

# como cambia según la entrada, esta función sirve para leer los datos y devolver
# los datos limpios
read_and_clean <- function(n_datos) {
  datos <-
    read.table(
      paste("./resultados/", n_datos, ".csv", sep=""),
      header = T,
      sep = ";",
      dec = ",",
      stringsAsFactors = T
    )
  datos <- clean(datos)
  write.table(
    datos,
    file = paste("./resultados_limpios/", n_datos, ".csv", sep=""),
    sep = ";",
    quote = F,
    dec = ",",
    fileEncoding = "UTF-8"
  )
}

anios <- as.character(c(1983, 1987, 1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2022))
for (i in 1:length(anios)){
  read_and_clean(anios[i])
}



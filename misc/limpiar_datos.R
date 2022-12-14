# Esta funci?n limpia de acentos el dataframe que se ha pasado como argumento,
# as? como normaliza los nombres de los partidos a los nombres m?s conocidos a nivel
# estatal y auton?mico
clean <- function(df) {
  df$Partido <- factor(gsub('\\.', "", df$Partido))
  df$Censo <- as.integer(gsub('\\.', "", as.character(df$Censo)))
  
  df$Provincia <- gsub('?', 'A', as.character(df$Provincia))
  df$Provincia <- gsub('?', 'E', as.character(df$Provincia))
  df$Provincia <- gsub('?', 'I', as.character(df$Provincia))
  df$Provincia <- gsub('?', 'O', as.character(df$Provincia))
  df$Provincia <- gsub('?', 'U', as.character(df$Provincia))
  
  df$Municipio <- gsub('?', 'A', as.character(df$Municipio))
  df$Municipio <- gsub('?', 'E', as.character(df$Municipio))
  df$Municipio <- gsub('?', 'I', as.character(df$Municipio))
  df$Municipio <- gsub('?', 'O', as.character(df$Municipio))
  df$Municipio <- gsub('?', 'U', as.character(df$Municipio))
  
  
  df$Partido <-
    as.factor(gsub('?', 'n', as.character(df$Partido)))
  df$Partido <-
    as.factor(gsub('?', 'N', as.character(df$Partido)))
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

# como cambia seg?n la entrada, esta funci?n sirve para leer los datos y devolver
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



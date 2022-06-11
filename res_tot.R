library(dplyr);library(tidyr); library(ggplot2)

ficheros <- list.files("./repartos", full.names = T)
resultados <- lapply(ficheros, function(x) read.csv(x, header=T))
resultados <- do.call(cbind, resultados)
anios <- as.character(c(1983, 1987, 1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2022))
colores <- read.csv("colores.csv")
colores$Color[colores$Color==""] <- 1:100

#eliminar repetidos
resultados <- resultados[,c(1,seq(2, dim(resultados)[2], by=2))]
#años de las elecciones
colnames(resultados) <- c("Partido", anios)
#ordenar el factor partido por ultimos resultados y que aparezca bien en el gráfico
partidos_orden <- resultados %>% arrange(desc(`2022`)) %>% pull(Partido)

#desdoblamiento de resultados
resultados_filas <- resultados %>% gather("Año", "Escaños", 2:12) %>% filter(Escaños > 0)
resultados_filas <- resultados_filas %>% mutate(Partido = factor(Partido, levels = partidos_orden))
resultados_filas$Color <- colores$Color[match(resultados_filas$Partido, colores$Partido)]

# paleta de colores
colores_conescano <- resultados_filas %>% select(Partido, Color) %>% distinct()
colores_conescano <- setNames(colores_conescano$Color, colores_conescano$Partido)

grafico <- resultados_filas %>% 
  ggplot(aes(x = Año, y = Escaños, col = Partido))+
  geom_line(aes(group = Partido))+
  geom_point()+
  scale_colour_manual(values=colores_conescano, 
                     breaks = resultados$Partido[order(resultados$"2022", decreasing = T)]) +
  labs(x = "Convocatoria",
       y = "Escaños obtenidos",
       title = "Evolución de cada partido por convocatoria")+
  theme_minimal()

evolucion_escanos <- ggplotly(grafico) %>% layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                                                  paper_bgcolor = "rgba(0, 0, 0, 0)")
saveRDS(evolucion_escanos, "./repartos/evolucion_escanos.rds")

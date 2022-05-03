library(shiny)
library(plotly)

elecciones <- as.character(c(1983, 1987, 1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2022))
provincias <- c("Ávila", "Burgos", "León", "Palencia", "Salamanca", "Segovia", "Soria", 
                "Valladolid", "Zamora")
metodos <- c("D'Hont","Sainte-Lagüe", "Cuota Hare")
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Estudio sobre las elecciones a las Cortes de Castilla y León"),
    selectInput("eleccion", "¿Qué elecciones quieres analizar?", elecciones, selected = "2019"),
    tabsetPanel(
                tabPanel("Análisis de las elecciones", id="analisis",
                         tabsetPanel(
                           tabPanel("Análisis a nivel autonómico",
                                    fluidRow(
                                      column(5, plotlyOutput("barras_an_autonomico")),
                                      column(7, plotlyOutput("cortes_analisis"))
                                      ),
                                    fluidRow(
                                      column(1),
                                      column(2, tableOutput("tabla_cyl")),
                                      column(1),
                                      column(8, plotlyOutput("mapa_cyl")),
                                    )
                                    ),
                           tabPanel("Análisis a nivel provincial", 
                                    selectInput("provincia", "¿Qué provincia?", provincias, selected="Valladolid"),
                                    #textOutput("prueba"),
                                    fluidRow(
                                      column(5, plotlyOutput("barras_an_provincial")),
                                      column(7, plotlyOutput("procuradores_provin"))),
                                    fluidRow(
                                      column(1),
                                      column(2, tableOutput("tabla_prov")),
                                      column(1),
                                      column(8, plotlyOutput("mapa_prov")),
                                    )
                                    )
                           ),
                         ),
                tabPanel("Sistema de circunscripciones provinciales", id = "prov",
                         fluidRow(
                           #column(4, selectInput("eleccion_prov", "¿Qué elecciones quieres analizar?", elecciones, selected = "2019")),
                           column(4, selectInput("metodo_prov", "¿Qué método quieres utilizar?", metodos, selected = "D'Hont")),
                           column(4, sliderInput("barrera_prov", "¿Qué barrera quieres utilizar?", value = 3, min = 0, max = 10, step= 0.5))
                         ),
                         tabsetPanel(
                           tabPanel("Análisis a nivel autonómico",
                                    fluidRow(
            
                                      column(3),
                                      column(7, plotlyOutput("cortes_prov"), align="center"),
                                      column(3))),
                           
                           tabPanel("Análisis a nivel provincial", 
                                    selectInput("provincia_prov", "¿Qué provincia?", provincias, selected="Valladolid"),
                                    fluidRow(
                                      #column(5, plotlyOutput("barras_prov_provincial")),
                                      column(3),
                                      column(7, plotlyOutput("procuradores_provin_prov"), align="center"),
                                      column(3)))
                         )
                         ),
                tabPanel("Sistema de circunscripción única", id = "auton",
                         fluidRow(
                           #column(4, selectInput("eleccion_aut", "¿Qué elecciones quieres analizar?", elecciones, selected = "2019")),
                           column(4, selectInput("metodo_aut", "¿Qué método quieres utilizar?", metodos, selected = "D'Hont")),
                           column(4, sliderInput("barrera_aut", "¿Qué barrera quieres utilizar?", value = 3, min = 0, max = 10, step= 0.5))
                         ),
                         fluidRow(
                           
                           column(3),
                           column(7, plotlyOutput("cortes_aut"), align="center"),
                           column(3))
                         ),
                tabPanel("Sistema de compensación", id = "comp")
                )

))

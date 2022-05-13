library(shiny)
library(shinydashboard)
library(plotly)

elecciones <- as.character(c(1983, 1987, 1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2022))
provincias <- c("Ávila", "Burgos", "León", "Palencia", "Salamanca", "Segovia", "Soria", 
                "Valladolid", "Zamora")
metodos <- c("D'Hont","Sainte-Lagüe", "Cuota Hare")
# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "AnaliCyL"),
    dashboardSidebar(sidebarMenu(
      selectInput("eleccion", "Convocatoria:", elecciones, selected = "2022"),
      menuItem(
        "Inicio",
        icon = icon("house-user"),
        tabName = "inicio"
      ),
      menuItem(
        "Resultados electorales",
        menuSubItem("Resultados a nivel autonómico", tabName = "resultadosAut"),
        menuSubItem("Resultados a nivel provincial", tabName = "resultadosProv")
      ),
      menuItem(
        "Sistema de circ. provinciales", tabName = "circProv"
        
      ),
      menuItem(
        "Sistema de circ. autonómica", tabName = "circAut"
      ),
      menuItem(
        "Sistema de compensación", tabName = "compensacion"
      )
    )),
    dashboardBody(
      tabItems(
        tabItem(
        tabName = "inicio"
      ),
      tabItem(
        tabName = "resultadosAut",
        tabsetPanel(
          tabPanel(
            "Información sobre los votos",
            column(7, plotlyOutput("barras_an_autonomico")),
            column(2),
            column(2, tableOutput("tabla_cyl")),
            column(1)
          ),
          tabPanel(
            "Reparto de escaños",
            column(7, plotlyOutput("cortes_analisis")),
            column(5, plotlyOutput("mapa_cyl"))
          )
          
        )
      ),
      tabItem(
        "resultadosProv",
        selectInput("provincia", "Provincia:", provincias, selected="Valladolid"),
        tabsetPanel(
          tabPanel(
            "Información sobre los votos",
            column(7, plotlyOutput("barras_an_provincial")),
            column(2),
            column(2, tableOutput("tabla_prov")),
            column(1)
          ),
          tabPanel(
            "Reparto de escaños",
            column(7, plotlyOutput("procuradores_provin")),
            column(5, plotlyOutput("mapa_prov"))
          )
          
        )
      ),
      tabItem(
        "circProv",
        fluidRow(
          column(
            4,
            selectInput(
              "metodo_prov",
              "¿Qué método quieres utilizar?",
              metodos,
              selected = "D'Hont"
            )
          ),
          column(
            4,
            sliderInput(
              "barrera_prov",
              "¿Qué barrera quieres utilizar?",
              value = 3,
              min = 0,
              max = 10,
              step = 0.5
            )
          )),
        tabsetPanel(
          tabPanel(
            "Análisis a nivel autonómico",
            fluidRow(column(2),
                     column(8, plotlyOutput("cortes_prov"), align =
                              "center"),
                     column(2))
          ),
          
          tabPanel(
            "Análisis a nivel provincial",
            selectInput("provincia_prov", "¿Qué provincia?", provincias, selected =
                          "Valladolid"),
            fluidRow(
              column(2),
              column(
                8, plotlyOutput("procuradores_provin_prov"), align = "center"
              ),
              column(2))
          )
        )
      ),
      tabItem(
        "circAut",
        fluidRow(column(
            4,
            selectInput(
              "metodo_aut",
              "¿Qué método quieres utilizar?",
              metodos,
              selected = "D'Hont"
            )
          ),
          column(
            4,
            sliderInput(
              "barrera_aut",
              "¿Qué barrera quieres utilizar?",
              value = 3,
              min = 0,
              max = 10,
              step = 0.5
            )
          )),
        fluidRow(column(2),
                 column(8, plotlyOutput("cortes_aut"), align =
                          "center"),
                 column(2))
        
      )
      )
    )
  )
)

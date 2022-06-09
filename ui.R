library(shiny)
library(shinydashboard)
library(plotly)

elecciones <- as.character(c(1983, 1987, 1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2022))
provincias <- c("Ávila", "Burgos", "León", "Palencia", "Salamanca", "Segovia", "Soria", 
                "Valladolid", "Zamora")
metodos <- c("D'Hont","Sainte-Laguë", "Cuota Hare")

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
        "Sistema de circ. provinciales",
        selectInput(
          "metodo_prov",
          "Método de reparto:",
          metodos,
          selected = "D'Hont"
        ),
        sliderInput(
          "barrera_prov",
          "Barrera de entrada:",
          value = 3,
          min = 0,
          max = 10,
          step = 0.5
        ),
        menuSubItem("Reparto a nivel autonómico", tabName = "circProvAut"),
        menuSubItem("Reparto a nivel provincial", tabName = "circProvProv")
        
      ),
      menuItem(
        "Sistema de circ. autonómica", 
        selectInput(
          "metodo_aut",
          "Método de reparto:",
          metodos,
          selected = "D'Hont"
        ),
        sliderInput(
          "barrera_aut",
          "Barrera de entrada:",
          value = 3,
          min = 0,
          max = 10,
          step = 0.5
        ),
        menuSubItem("Reparto a nivel autonómico", tabName = "circAut")
      ),
      menuItem(
        "Sistema de compensación",
        selectInput("metodo_comp",
                    "Método de reparto:",
                    metodos,
                    selected = "D'Hont"),
        sliderInput(
          "barrera_comp_aut",
          "Barrera de compensación:",
          value = 3,
          min = 0,
          max = 10,
          step = 0.5
        ),
        sliderInput(
          "barrera_comp_prov",
          "Barrera provincial:",
          value = 3,
          min = 0,
          max = 10,
          step = 0.5
        ),
        menuSubItem("Reparto total", tabName = "circCompAut"),
        menuSubItem("Reparto por circunscripciones", tabName = "circCompProv")
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
            column(9, plotlyOutput("barras_an_autonomico")),
            column(2, tableOutput("tabla_cyl")),
            column(1)
          ),
          tabPanel(
            "Reparto de escaños",
            column(5, plotlyOutput("mapa_cyl")),
            column(7, plotlyOutput("cortes_analisis"))
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
            column(5, plotlyOutput("mapa_prov")),
            column(7, plotlyOutput("procuradores_provin"))
          )
          
        )
      ),
      tabItem(
            "circProvAut",
            fluidRow(column(2),
                     column(8, plotlyOutput("cortes_prov"), align =
                              "center"),
                     column(2)
        )
      ),
      tabItem(
        "circProvProv",
        selectInput("provincia_prov", "Provincia:", provincias, selected =
                      "Valladolid"),
        fluidRow(
          column(2),
          column(
            8, plotlyOutput("procuradores_provin_prov"), align = "center"
          ),
          column(2))
      ),
      tabItem(
        "circAut",
        fluidRow(column(2),
                 column(8, plotlyOutput("cortes_aut"), align =
                          "center"),
                 column(2))
        
      ),
      tabItem(
        "circCompAut",
        fluidRow(column(2),
                 column(8), #aquí el gráfico
                 column(2)
        )
      ),
      tabItem(
        "circCompProv",
        selectInput("provincia_comp_prov", "Circunscripción:", provincias, selected =
                      "Valladolid"),
        fluidRow(
          column(2),
          column(8), # aquí el gráfico
          column(2))
      )
      )
    )
  )
)

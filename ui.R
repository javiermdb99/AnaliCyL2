library(shiny)
library(shinydashboard)
library(plotly)

elecciones <- as.character(c(1983, 1987, 1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2022))
provincias <- c("Ávila", "Burgos", "León", "Palencia", "Salamanca", "Segovia", "Soria", 
                "Valladolid", "Zamora")
metodos <- c("D'Hont","Sainte-Laguë", "Cuota Hare")

shinyUI(
  dashboardPage(
    dashboardHeader(title = "ElecCyL"),
    dashboardSidebar(sidebarMenu(
      selectInput("eleccion", "Convocatoria:", elecciones, selected = "2022"),
      menuItem(
        "Inicio",
        icon = icon("house-user"),
        tabName = "inicio"
      ),
      menuItem(
        "Resultados electorales",
        menuSubItem("Resumen convocatorias", tabName = "resumen"),
        menuSubItem("Resultados a nivel autonómico", tabName = "resultadosAut"),
        menuSubItem("Resultados a nivel provincial", tabName = "resultadosProv")
      ),
      menuItem(
        "Circunscripciones provinciales",
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
        "Circunscripción autonómica", 
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
        "Provincias con compensación",
        selectInput("metodo_comp",
                    "Método de reparto:",
                    metodos[-3],
                    selected = "D'Hont"),
        sliderInput(
          "barrera_comp_aut",
          "Barrera de compensación:",
          value = 3,
          min = 2,
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
        menuSubItem("Reparto a nivel autonómico", tabName = "circCompAut"),
        menuSubItem("Reparto a nivel provincial", tabName = "circCompProv")
      )
    )),
    dashboardBody(
      tabItems(
        tabItem(
        tabName = "inicio",
        tags$div(
          style = "width:75%;margin:auto; text-align:center",
          tags$h2(
            "Aplicación Shiny para el estudio de diferentes sistemas de reparto \
                aplicado a las elecciones autonómicas en Castilla y León",
            style = "text-align:center;"
          )),
        tags$h3("En esta aplicación podrás:",),
        tags$div(
          tags$h4(icon("history"), "Convocatorias"),
          tags$br(),
          tags$p("Visualizar todos los resultados de todas las elecciones autonómicas\
          en Castilla y León, así como un resumen de los mismos."),
          tags$h4(icon("divide"), "Sistema de circunscripciones provinciales"),
          tags$br(),
          tags$p("Cambiar la barrera electoral y el método de reparto al sistema\
          actual, el de las circunscripciones provinciales."),
          tags$h4(icon("compress-alt"), "Sistema de circunscripciones autonómica"),
          tags$br(),
          tags$p("Aplicar el sistema de circunscripción única en Castilla y León.\
          Cada voto valdría lo mismo, también pudiendo cambiar el método y la barrera."),
          tags$h4(icon("balance-scale"), "Sistema de circunscripciones provinciales\
          con escaños de compensación"),
          tags$br(),
          tags$p("Utilizar las provincias pero añadir unos escaños de compensación\
          para mayor proporcionalidad. Puedes cambiar las 2 barreras, así como el método."),
          style="background-color:#a5cfc3;\
          max-width:75%;\
          margin:auto;\
          justify-content: center;\
          align-items: center;\
          padding:2%;
          border-radius:15px 40px 80px",
        ),
        tags$footer("Trabajo de Fin de Grado de Javier Martín de Benito",
                    tags$br(),
                    tags$p("javmar1999@gmail.com", style='color:grey'),
                    tags$p("LinkedIn: javiermdb99", style='color:grey'),
                    style="text-align:right;
                    padding-top:4%")
        
        
      ),
      tabItem(
        "resumen",
        fluidRow(column(2),
                 column(8, plotlyOutput("evolucion_escanos"), align =
                          "center"),
                 column(2))
        
      ),
      tabItem(
        tabName = "resultadosAut",
        tags$p("Resultados electorales bajo el sistema actual: "),
        tags$p("Circunscripción: provincia"),
        tags$p("Método de reparto: D'Hont"),
        tags$p("Barrera electoral: 3%"),
        tabsetPanel(
          tabPanel(
            "Resultados",
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
        tags$p("Resultados electorales bajo el sistema actual: "),
        tags$p("Circunscripción: provincia"),
        tags$p("Método de reparto: D'Hont"),
        tags$p("Barrera electoral: 3%"),
        textOutput("texto_provincia"),
        selectInput("provincia", "Provincia:", provincias, selected="Valladolid"),
        tabsetPanel(
          tabPanel(
            "Resultados",
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
            tags$p("Circunscripción: provincia"),
            textOutput("texto_metodo_prov_tot"),
            textOutput("texto_barrera_prov_tot"),
            tags$br(),
            fluidRow(
                     column(8, plotlyOutput("cortes_prov"), align =
                              "center"),
                     column(4, tableOutput("comp_prov"), align =
                              "center"))
      ),
      tabItem(
        "circProvProv",            
        textOutput("texto_metodo_prov_prov"),
        textOutput("texto_barrera_prov_prov"),
        textOutput("texto_provincia_prov_prov"),
        tags$br(),
        selectInput("provincia_prov", "Provincia:", provincias, selected =
                      "Valladolid"),
        fluidRow(
          column(2),
          column(
            8, plotlyOutput("procuradores_provin_prov"), align = "center"
          ),
          column(2))
      ),
      tabItem("circAut",
              tags$p("Circunscripción: autonómica"),
              textOutput("texto_metodo_aut"),
              textOutput("texto_barrera_aut"),
              tags$br(),
              fluidRow(
                column(8, plotlyOutput("cortes_aut"), align =
                         "center"),
                column(4, tableOutput("comp_aut"), align =
                         "center")
              )),
      tabItem("circCompAut",
              tags$p("Circunscripciones provinciales con 13 escaños de compensación"),
              textOutput("texto_metodo_comp_tot"),
              textOutput("texto_barrera_prov_comp_tot"),
              textOutput("texto_barrera_aut_comp_tot"),
              tags$br(),
              fluidRow(
                column(8, plotlyOutput("cortes_comp"), align = "center"),
                column(4, tableOutput("comp_compens"), align = "center")
              )),
      tabItem(
        "circCompProv",
        tags$p("Circunscripciones provinciales con 13 escaños de compensación"),
        textOutput("texto_metodo_comp_prov"),
        textOutput("texto_barrera_prov_comp_prov"),
        textOutput("texto_barrera_aut_comp_prov"),
        textOutput("texto_provincia_comp_prov"),
        tags$br(),
        selectInput("provincia_comp_prov", "Circunscripción:", provincias, selected =
                      "Valladolid"),
        fluidRow(
          column(2),
          column(8, plotlyOutput("procuradores_provin_comp"), align = "center"),
          column(2))
      )
      )
    )
  )
)

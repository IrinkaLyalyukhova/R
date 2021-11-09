
library(shinythemes)
library(shinydashboard)
library(leaflet)

shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),
    
    fluidRow(column(
      12,
      wellPanel(
        h1("Тенденции изменения численности населения в РФ", align = "center"),
        style = "background:#F9E79F"
      )
    )),
    
    fluidRow(column(
      width = 8,  wellPanel(plotOutput("hist_average_number_of_population"), style = "background:#FEF9E7")
    ),
    column(
      width = 4,
      wellPanel(style = "background:#FEF9E7", textOutput("text_for_plot"))
    )),
    
    fluidRow(column(
      12,
      wellPanel(
        h3("Сравнение фактических и предсказанных значений для каждого города", align = "center"),
        style = "background:#F9E79F"
      )
    )),
    
    fluidRow(
      wellPanel(
        style = "background:#FEF9E7",
        sidebarLayout(sidebarPanel(
          style = "background:#F9E79F",
         selectInput("town", "Выберите город:", choices = NULL)
        ),
        mainPanel(plotOutput("comparison_boxplot"))
      ))
  ),
  
  fluidRow(column(
    12,
    wellPanel(
      h3("Карта России с численностью населения по регионам", align = "center"),
      style = "background:#F9E79F"
    )
  )),
  
  fluidRow(
    column(
      width = 8,
      leafletOutput("map", height = 700)
    ),
    column(
      width = 4,
      selectInput("inputYear", "Выберите год:", choices = NULL)
    )
  )
  
)
)

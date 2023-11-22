library(shiny)
library(shinydashboard,  warn.conflicts = FALSE)
library(shinyWidgets,  warn.conflicts = FALSE)
# install.packages("devtools")
#devtools::install_github("rstudio/gridlayout")
library(gridlayout)
library(bslib)
library(DT)
library(dplyr)
library(stringr)
#install.packages("leaflet")
library(leaflet)
#install.packages("tidyverse")
library(lubridate)  # Asegúrate de cargar lubridate
#install.packages("sf")
library(sf)

load("Jp_map.Rdata")
df <- df %>%
  mutate(Periodo = as.Date(df$Periodo, "%d/%m/%y"))
source("graficaydatos.R")
nombresPaises <- leeDatos(df)

# Define UI ----
ui <- fluidPage(
  
  title = "BEDU: Análisis de datos con R",
  collapsible = TRUE,
  titlePanel("Visitantes de Japón"),
  sidebarLayout(
    position = "left",
    sidebarPanel(      
      h3("Filtros"),
      selectInput(inputId = "pais", h4("Selecciona el país:"), 
                 choices = nombresPaises, selected = "Todos"),
      sliderInput(inputId = "anio", h4("Selecciona el período:"),
                  min = 2017, max = 2023, value = c(2017, 2023)),
    ),
    mainPanel(
      img(src = "japan.png", align = "right", height = 75, width = 150),
      tabsetPanel(type = "tabs",
      tabPanel("Gráfico", 
               plotOutput("plotVisitantes")
      ),
      tabPanel("Mapa", leafletOutput("mapa")),
      tabPanel("Predicción", plotOutput("prediccion")),
      tabPanel("Datos", DTOutput(outputId = "dfVisitantes")))
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {
  
  output$plotVisitantes <- renderPlot({
    grafica1(df, input$pais)
  })
  
  output$prediccion <- renderPlot({
    graficaPrediccion()
  })
  
  output$dfVisitantes <- renderDT({
    if(input$pais == "Todos"){
      japan_df <- df
    }else {
      japan_df <- df[df$Pais == input$pais,]
    }
    dfFiltrado <- japan_df
  })
  
  
  # Filtrar datos según la selección del usuario
  datos_filtrados <- reactive({
    
    
    if(input$pais != "Todos"){
      filter(df,Pais==input$pais)%>%distinct(Pais, latitud, longitud)
    }else{
      df%>%distinct(Pais, latitud, longitud)
    }
  })

  # # Crear el mapa
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = datos_filtrados(),
        lat = ~latitud,  # Nombre de la columna de latitudes en df
        lng = ~longitud, # Nombre de la columna de longitudes en df
        label = ~Pais,   # Nombre de la columna de países en df
        #popup = ~paste("País: ", Pais, "<br>Visitantes: ", Visitantes)  # Mostrar información en el popup
      )
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)


install.packages(c("shiny", "leaflet", "dplyr"))

# app.R

# Cargar paquetes
library(shiny)
library(leaflet)
library(dplyr)
library(lubridate)  # Asegúrate de cargar lubridate
library(sf)

# Cargar datos desde el archivo, AQUI DEBES ESPECIFICAR EL NOMBRE QUE LE DISTE A TU ARCHIVO CORREJIDO EN EL
# CODIGO ANTERIOR
load("Jp_map.Rdata")


#----- Lo demas no necesita cambios

# Crear la aplicación Shiny
ui <- fluidPage(
  titlePanel("Mapa Interactivo de Visitantes a Japón"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("mes", "Selecciona el Mes", unique(month(df$Periodo))),
      selectInput("anio", "Selecciona el Año", unique(year(df$Periodo)))
      # Puedes agregar controles adicionales aquí si es necesario
    ),
    mainPanel(
      leafletOutput("mapa")  # Salida del mapa
    )
  )
)

server <- function(input, output) {
  
  # Filtrar datos según la selección del usuario
  datos_filtrados <- reactive({
    filter(df, month(Periodo) == input$mes, year(Periodo) == input$anio)
  })
  
  # Crear el mapa
  output$mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addCircleMarkers(
        data = datos_filtrados(),
        lat = ~latitud,  # Nombre de la columna de latitudes en df
        lng = ~longitud, # Nombre de la columna de longitudes en df
        label = ~Pais,   # Nombre de la columna de países en df
        popup = ~paste("País: ", Pais, "<br>Visitantes: ", Visitantes)  # Mostrar información en el popup
      )
  })
}

shinyApp(ui, server)

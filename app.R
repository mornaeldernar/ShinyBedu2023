library(shiny)
library(gridlayout)
library(bslib)
library(DT)
load("df.RData")
library(dplyr)
library(stringr)


source("graficaydatos.R")

nombresPaises <- leeDatos(df)

ui <- grid_page(
  layout = c(
    "header  header  ",
    "sidebar plot "
  ),
  row_sizes = c(
    "100px",
    "1fr"
  ),
  col_sizes = c(
    "250px",
    "1fr"
  ),
  gap_size = "1rem",
  grid_card(
    area = "sidebar",
    card_header("Filtros"),
    card_body(
      selectInput(
        inputId = "pais",
        label = "Pais",
        choices = nombresPaises,
        selected = "Todos",
        width = "100%"
      ),
      em("Selecciona el pais que quieres filtrar")
    )
  ),
  grid_card_text(
    area = "header",
    content = "Visitantes a Japon",
    alignment = "start",
    is_title = FALSE
  ),
  grid_card(
    area = "plot",
    card_header("Histórico de visitantes"),
    card_body(
      tabsetPanel(
        nav_panel(
          title = "Gráficas",
          plotOutput("plotVisitantes")
        ),
        nav_panel(
          title = "Datos",
          DTOutput(outputId = "dfVisitantes", width = "100%")
        )
      )
    )
  )
)


server <- function(input, output, session) {
  
  output$plotVisitantes <- renderPlot({
    grafica1(df,input$pais)
  })
  
  output$dfVisitantes <- renderDT({
    if(input$pais == "Todos"){
      japan_df <- df
    }else {
      japan_df <- df[df$Pais == input$pais,]
    }
    dfFiltrado <- japan_df
  })
}

shinyApp(ui, server)
  


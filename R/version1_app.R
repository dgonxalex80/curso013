library(shiny)

# Definir la interfaz que verá el estudiante.
ui <- fluidPage(
  titlePanel("Cafetería universitaria | Versión 1"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("n", "Número de clientes:",
                  min = 5, max = 100, value = 20, step = 1)
    ),
    mainPanel(
      h3("Mi primer tablero"),
      textOutput("mensaje")
    )
  )
)

server <- function(input, output, session) {
  output$mensaje <- renderText({
    paste("El modelo representará", input$n, "clientes.")
  })
}

# Iniciar la aplicación conectando interfaz y servidor.
shinyApp(ui, server)

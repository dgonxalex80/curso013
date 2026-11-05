library(shiny)
library(ggplot2)

simular_fila <- function(n, media_llegadas, media_servicio,
                         sd_servicio, semilla) {
  # Fijar la semilla para reproducir los resultados.
  set.seed(semilla)
  # Los mismos U y Z generan escenarios comparables.
  u <- runif(n)
  # Generar tiempos de servicio con distribución normal y evitar negativos.
  z <- rnorm(n)
  b <- -media_llegadas * log(u)
  s <- pmax(0, media_servicio + sd_servicio * z)
  # Acumular los intervalos para obtener instantes de llegada.
  llegada <- cumsum(b)
  inicio <- fin <- numeric(n)
  # Recorrer secuencialmente cada cliente de la fila.
  for (i in seq_len(n)) {
    # Comenzar la atención cuando el cliente llegó y el servidor está libre.
    inicio[i] <- if (i == 1) llegada[i] else max(llegada[i], fin[i-1])
    # Registrar el instante de finalización de la atención.
    fin[i] <- inicio[i] + s[i]
  }
  data.frame(cliente = seq_len(n), llegada, servicio = s,
             espera = inicio - llegada, total = fin - llegada)
}

# Definir la interfaz que verá el estudiante.
ui <- fluidPage(
  titlePanel("Cafetería | Versión 4: experimentación"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Clientes por réplica", 20, min = 2, max = 1000),
      numericInput("a", "Media entre llegadas", 3, min = 0.1),
      numericInput("sA", "Servicio medio A", 2.5, min = 0.1),
      numericInput("sB", "Servicio medio B", 2.0, min = 0.1),
      numericInput("sd", "Desviación del servicio", 0.5, min = 0),
      numericInput("R", "Réplicas", 100, min = 1, max = 2000),
      numericInput("seed", "Semilla base", 123, min = 1),
      actionButton("go", "Comparar escenarios")
    ),
    mainPanel(
      h4("Indicadores promedio entre réplicas"),
      tableOutput("resumen"),
      plotOutput("comparacion"),
      h4("Resultados por réplica"), tableOutput("detalle")
    )
  )
)

server <- function(input, output, session) {
  resultados <- eventReactive(input$go, {
    do.call(rbind, lapply(seq_len(input$R), function(r) {
      semilla <- input$seed + r - 1
      a <- simular_fila(input$n, input$a, input$sA, input$sd, semilla)
      b <- simular_fila(input$n, input$a, input$sB, input$sd, semilla)
      data.frame(replica = r, escenario = c("A", "B"),
                 espera_media = c(mean(a$espera), mean(b$espera)),
                 total_medio = c(mean(a$total), mean(b$total)),
                 prob_esperar = c(mean(a$espera > 0), mean(b$espera > 0)))
    }))
  }, ignoreNULL = FALSE)
  output$resumen <- renderTable({
    x <- resultados()
    resumen <- aggregate(cbind(espera_media, total_medio, prob_esperar) ~ escenario,
                         data = x, FUN = mean)
    resumen[, -1] <- round(resumen[, -1], 3)
    resumen
  })
  output$comparacion <- renderPlot({
    ggplot(resultados(), aes(x = escenario, y = espera_media)) +
      geom_boxplot(fill = "grey85") +
      labs(x = "Escenario", y = "Espera media (min)",
           title = "Variación entre réplicas") + theme_minimal()
  })
  output$detalle <- renderTable(head(resultados(), 12), digits = 3)
}

# Iniciar la aplicación conectando interfaz y servidor.
shinyApp(ui, server)

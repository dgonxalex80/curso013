library(shiny)
library(ggplot2)

simular_fila <- function(n, media_llegadas, media_servicio,
                         sd_servicio, semilla = 123) {
  # Fijar la semilla para reproducir los resultados.
  set.seed(semilla)
  # Generar tiempos entre llegadas con distribución exponencial.
  b <- rexp(n, rate = 1 / media_llegadas)
  # Generar tiempos de servicio con distribución normal y evitar negativos.
  s <- pmax(0.01, rnorm(n, media_servicio, sd_servicio))
  # Acumular los intervalos para obtener instantes de llegada.
  c <- cumsum(b)
  e <- g <- numeric(n)
  # Recorrer secuencialmente cada cliente de la fila.
  for (i in seq_len(n)) {
    e[i] <- if (i == 1) c[i] else max(c[i], g[i - 1])
    g[i] <- e[i] + s[i]
  }
  data.frame(cliente = seq_len(n), entre_llegadas = b,
             llegada = c, servicio = s, inicio = e,
             espera = e - c, fin = g, total = g - c)
}

# Definir la interfaz que verá el estudiante.
ui <- fluidPage(
  titlePanel("Cafetería | Versión 3: indicadores"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Clientes", 20, min = 1, max = 500),
      numericInput("a", "Media entre llegadas", 3, min = 0.1),
      numericInput("s", "Media servicio", 2.5, min = 0.1),
      numericInput("sd", "Desviación servicio", 0.5, min = 0),
      numericInput("seed", "Semilla", 123, min = 1),
      actionButton("go", "Actualizar simulación")
    ),
    mainPanel(
      h4("Indicadores de los clientes simulados"),
      verbatimTextOutput("resumen"),
      plotOutput("histograma", height = "260px"),
      plotOutput("eventos", height = "280px"),
      h4("Tabla de eventos"), tableOutput("tabla")
    )
  )
)

server <- function(input, output, session) {
  d <- eventReactive(input$go, {
    simular_fila(input$n, input$a, input$s, input$sd, input$seed)
  }, ignoreNULL = FALSE)
  output$resumen <- renderPrint({
    x <- d()
    cat("Espera media:", round(mean(x$espera), 2), "min\n")
    cat("Servicio medio:", round(mean(x$servicio), 2), "min\n")
    cat("Tiempo total medio:", round(mean(x$total), 2), "min\n")
    cat("Espera máxima:", round(max(x$espera), 2), "min\n")
    cat("Proporción que espera:", round(mean(x$espera > 0), 3), "\n")
  })
  output$histograma <- renderPlot({
    ggplot(d(), aes(x = espera)) +
      geom_histogram(bins = 12, fill = "grey75", color = "black") +
      labs(title = "Distribución de las esperas", x = "Minutos", y = "Clientes") +
      theme_minimal()
  })
  output$eventos <- renderPlot({
    x <- d()
    plot(x$cliente, x$llegada, type = "b", pch = 16,
         xlab = "Cliente", ylab = "Minuto acumulado",
         main = "Llegada y finalización por cliente")
    lines(x$cliente, x$fin, type = "b", pch = 1, lty = 2)
    legend("topleft", c("Llegada", "Fin"), pch = c(16, 1),
           lty = c(1, 2), bty = "n")
  })
  output$tabla <- renderTable(round(d(), 2))
}

# Iniciar la aplicación conectando interfaz y servidor.
shinyApp(ui, server)

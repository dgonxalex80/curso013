library(shiny)
library(ggplot2)

simular <- function(n, media_a, media_s, sd_s, semilla) {
  # Fijar la semilla para reproducir los resultados.
  set.seed(semilla)
  # Generar tiempos entre llegadas con distribución exponencial.
  b <- rexp(n, rate = 1/media_a)
  # Generar tiempos de servicio con distribución normal y evitar negativos.
  s <- pmax(0.01, rnorm(n, media_s, sd_s))
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
             espera = inicio-llegada, fin, total = fin-llegada)
}

# Definir la interfaz que verá el estudiante.
ui <- fluidPage(
  titlePanel("Cafetería | Versión 5: calibración"),
  sidebarLayout(
    sidebarPanel(
      fileInput("archivo", "CSV de observaciones", accept = ".csv"),
      numericInput("n", "Clientes simulados", 20, min = 1, max = 1000),
      numericInput("seed", "Semilla", 123, min = 1),
      actionButton("go", "Simular con parámetros estimados")
    ),
    mainPanel(
      h4("Parámetros estimados"), tableOutput("parametros"),
      h4("Indicadores simulados"), tableOutput("indicadores"),
      plotOutput("histograma"),
      h4("Eventos simulados"), tableOutput("tabla")
    )
  )
)

server <- function(input, output, session) {
  observaciones <- reactive({
    req(input$archivo)
    x <- read.csv(input$archivo$datapath)
    validate(need(all(c("entre_llegadas", "servicio") %in% names(x)),
                  "El CSV debe contener entre_llegadas y servicio."))
    # Comprobar tipos antes de aplicar comparaciones numéricas.
    validate(need(is.numeric(x$entre_llegadas) && is.numeric(x$servicio),
                  "Las columnas entre_llegadas y servicio deben ser numéricas."))
    # Eliminar registros incompletos de ambas variables.
    x <- x[complete.cases(x[, c("entre_llegadas", "servicio")]), , drop = FALSE]
    # Evitar infinitos, valores no positivos y muestras demasiado pequeñas.
    validate(need(nrow(x) >= 2, "Se necesitan al menos dos observaciones completas."),
             need(all(is.finite(x$entre_llegadas)) && all(is.finite(x$servicio)),
                  "Los tiempos deben ser finitos."),
             need(all(x$entre_llegadas > 0) && all(x$servicio > 0),
                  "Los tiempos observados deben ser positivos."))
    x
  })
  parametros <- reactive({
    x <- observaciones()
    list(a = mean(x$entre_llegadas), s = mean(x$servicio),
         sd = sd(x$servicio))
  })
  output$parametros <- renderTable({
    p <- parametros()
    data.frame(parametro = c("Media entre llegadas", "Media servicio", "DE servicio"),
               minutos = round(c(p$a, p$s, p$sd), 3))
  })
  datos <- eventReactive(input$go, {
    p <- parametros()
    simular(input$n, p$a, p$s, p$sd, input$seed)
  })
  output$indicadores <- renderTable({
    x <- datos()
    data.frame(indicador = c("Espera media", "Total medio", "Proporción que espera"),
               valor = round(c(mean(x$espera), mean(x$total),
                               mean(x$espera > 0)), 3))
  })
  output$histograma <- renderPlot({
    ggplot(datos(), aes(espera)) +
      geom_histogram(bins = 12, fill = "grey80", color = "black") +
      labs(title = "Espera simulada", x = "Minutos", y = "Clientes") +
      theme_minimal()
  })
  output$tabla <- renderTable(head(datos(), 20), digits = 2)
}

# Iniciar la aplicación conectando interfaz y servidor.
shinyApp(ui, server)

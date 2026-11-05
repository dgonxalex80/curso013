library(shiny)

simular_fila <- function(n, media_llegadas, media_servicio,
                         sd_servicio, semilla = 123) {
  # Fijar la semilla para reproducir los resultados.
  set.seed(semilla)
  # Generar tiempos entre llegadas con distribución exponencial.
  entre_llegadas <- rexp(n, rate = 1 / media_llegadas)
  # Generar tiempos de servicio con distribución normal y evitar negativos.
  servicio <- pmax(0.01, rnorm(n, mean = media_servicio,
                            sd = sd_servicio))
  # Acumular los intervalos para obtener instantes de llegada.
  llegada <- cumsum(entre_llegadas)
  inicio <- fin <- numeric(n)
  # Recorrer secuencialmente cada cliente de la fila.
  for (i in seq_len(n)) {
    # Comenzar la atención cuando el cliente llegó y el servidor está libre.
    inicio[i] <- if (i == 1) llegada[i] else max(llegada[i], fin[i - 1])
    # Registrar el instante de finalización de la atención.
    fin[i] <- inicio[i] + servicio[i]
  }
  data.frame(cliente = seq_len(n), entre_llegadas, llegada,
             servicio, inicio, espera = inicio - llegada,
             fin, total = fin - llegada)
}

# Definir la interfaz que verá el estudiante.
ui <- fluidPage(
  titlePanel("Cafetería | Versión 2: fila FIFO"),
  sidebarLayout(
    sidebarPanel(
      numericInput("n", "Clientes:", 20, min = 1, max = 500),
      numericInput("a", "Media entre llegadas (min):", 3, min = 0.1),
      numericInput("s", "Media del servicio (min):", 2.5, min = 0.1),
      numericInput("sd", "Desviación del servicio (min):", 0.5, min = 0),
      numericInput("seed", "Semilla:", 123, min = 1),
      actionButton("simular", "Simular")
    ),
    mainPanel(h4("Eventos por cliente (minutos)"),
              tableOutput("tabla"))
  )
)

server <- function(input, output, session) {
  datos <- eventReactive(input$simular, {
    simular_fila(input$n, input$a, input$s, input$sd, input$seed)
  }, ignoreNULL = FALSE)
  output$tabla <- renderTable(round(data.frame(
    cliente = datos()$cliente,
    entre_llegadas = datos()$entre_llegadas,
    llegada = datos()$llegada,
    servicio = datos()$servicio,
    inicio = datos()$inicio,
    espera = datos()$espera,
    fin = datos()$fin,
    total = datos()$total), 2))
}

# Iniciar la aplicación conectando interfaz y servidor.
shinyApp(ui, server)

# Cargar Shiny: interfaz, reactividad y actualización periódica.
library(shiny)

# Crear la función que simula los eventos de una fila con un servidor.
simular_cafeteria <- function(n, media_llegadas, media_servicio,
                              desviacion, semilla) {
  # Fijar la semilla para obtener una simulación reproducible.
  set.seed(semilla)
  # Generar los intervalos entre llegadas con media indicada.
  intervalos <- rexp(n, rate = 1 / media_llegadas)
  # Acumular intervalos para calcular los instantes de llegada.
  llegada <- cumsum(intervalos)
  # Generar servicios normales, acotados inferiormente a 0.01 minuto.
  servicio <- pmax(0.01, rnorm(n, media_servicio, desviacion))
  # Reservar vectores para los instantes de inicio y fin.
  inicio <- numeric(n)
  fin <- numeric(n)
  # Procesar los clientes en orden de llegada (FIFO).
  for (i in seq_len(n)) {
    # El primero inicia al llegar; los demás esperan al servidor.
    inicio[i] <- if (i == 1) llegada[i] else max(llegada[i], fin[i - 1])
    # El fin de atención es el inicio más la duración del servicio.
    fin[i] <- inicio[i] + servicio[i]
  }
  # Entregar una tabla completa para cálculos y animación.
  data.frame(cliente = seq_len(n), intervalo = intervalos,
             llegada = llegada, servicio = servicio, inicio = inicio,
             espera = inicio - llegada, fin = fin,
             total = fin - llegada)
}

# Definir estilos CSS: ambientación de cafetería, tarjetas y estudiantes.
estilos <- "
body {background:#f2f5f8; font-family:Arial,sans-serif;}
.cabecera {background:#476885;color:white;padding:18px;border-radius:12px;margin-bottom:14px;}
.escena {background:#fff;border:2px solid #476885;border-radius:14px;padding:16px;min-height:310px;}
.zona {background:#edf3f7;border:1px solid #c4d2df;border-radius:10px;padding:12px;margin:8px 0;}
.etiqueta {font-size:15px;font-weight:bold;color:#263b4b;margin-bottom:8px;}
.estudiante {display:inline-block;background:white;border:1px solid #476885;border-radius:9px;
  padding:8px 10px;margin:4px;text-align:center;min-width:65px;}
.avatar {font-size:28px;display:block;}
.vacio {color:#657786;font-style:italic;}
.metrica {background:white;border:1px solid #d1dbe5;border-radius:10px;padding:12px;margin-bottom:10px;}
.metrica strong {font-size:24px;color:#263b4b;}
"

# Construir la interfaz visual de la aplicación.
ui <- fluidPage(
  # Inyectar los estilos CSS definidos anteriormente.
  tags$head(tags$style(HTML(estilos))),
  # Crear una cabecera que sitúa la simulación en la universidad.
  div(class = "cabecera",
      h2("☕ Cafetería universitaria | Gemelo digital"),
      p("Llegadas → Fila de estudiantes → Atención → Salida")),
  # Organizar controles a la izquierda y escena a la derecha.
  sidebarLayout(
    # Panel de controles para construir nuevos escenarios.
    sidebarPanel(
      # Elegir el número de estudiantes.
      numericInput("n", "Estudiantes", value = 20, min = 1, max = 100),
      # Definir la media exponencial de los intervalos entre llegadas.
      numericInput("media_a", "Media entre llegadas (min)", 3, min = 0.1),
      # Definir la media normal de los servicios.
      numericInput("media_s", "Media de servicio (min)", 2.5, min = 0.1),
      # Definir la desviación estándar normal de los servicios.
      numericInput("desv", "Desviación del servicio (min)", 0.5, min = 0),
      # Establecer una semilla para reproducibilidad.
      numericInput("semilla", "Semilla aleatoria", 123, min = 1),
      # Elegir cuántos minutos simulados avanzan por segundo real.
      sliderInput("velocidad", "Velocidad (min simulados/segundo)",
                  min = 0.25, max = 5, value = 1, step = 0.25),
      # Generar una nueva realización con los parámetros actuales.
      actionButton("generar", "Generar nueva simulación"),
      # Iniciar o detener la animación.
      actionButton("reproducir", "Reproducir / Pausar"),
      # Volver al instante cero de la realización actual.
      actionButton("reiniciar", "Reiniciar tiempo"),
      # Mostrar el instante simulado actual.
      h4(textOutput("reloj")),
      # Permitir explorar manualmente un instante de la simulación.
      sliderInput("tiempo", "Explorar tiempo simulado", min = 0,
                  max = 100, value = 0, step = 0.1)
    ),
    # Panel principal: escena, indicadores y tabla de resultados.
    mainPanel(
      # Dibujar la escena dinámica mediante HTML generado por Shiny.
      uiOutput("escena"),
      # Mostrar indicadores del estado actual.
      fluidRow(
        column(4, div(class = "metrica", "En fila", tags$br(),
                      strong(textOutput("en_fila", inline = TRUE)))),
        column(4, div(class = "metrica", "En atención", tags$br(),
                      strong(textOutput("en_atencion", inline = TRUE)))),
        column(4, div(class = "metrica", "Atendidos", tags$br(),
                      strong(textOutput("atendidos", inline = TRUE))))
      ),
      # Presentar métricas globales calculadas para toda la realización.
      h4("Indicadores de la simulación"),
      tableOutput("resumen"),
      # Mostrar la tabla de eventos de los primeros veinte clientes.
      h4("Eventos por estudiante (minutos)"),
      tableOutput("eventos")
    )
  )
)

# Crear la lógica de simulación y actualización de la interfaz.
server <- function(input, output, session) {
  # Guardar el instante actual y el estado de reproducción.
  estado <- reactiveValues(reproduciendo = FALSE)
  # Calcular la realización inicial y regenerarla al pulsar el botón.
  datos <- eventReactive(input$generar, {
    # Crear eventos con los parámetros indicados por el usuario.
    simular_cafeteria(input$n, input$media_a, input$media_s,
                      input$desv, input$semilla)
  }, ignoreNULL = FALSE)
  # Al generar, detener la reproducción y reiniciar el reloj.
  observeEvent(input$generar, {
    estado$reproduciendo <- FALSE
    updateSliderInput(session, "tiempo", value = 0)
  })
  # Ajustar el rango temporal al último cliente atendido.
  observeEvent(datos(), {
    # Actualizar el máximo del control de tiempo.
    updateSliderInput(session, "tiempo", max = ceiling(max(datos()$fin)),
                      value = 0)
  })
  # Alternar reproducción y pausa con el mismo botón.
  observeEvent(input$reproducir, {
    # Cambiar el estado lógico de la animación.
    # Iniciar nuevamente desde cero si la animación había terminado.
    if (!estado$reproduciendo && input$tiempo >= max(datos()$fin)) {
      updateSliderInput(session, "tiempo", value = 0)
    }
    estado$reproduciendo <- !estado$reproduciendo
  })
  # Regresar al comienzo cuando se pulsa reiniciar.
  observeEvent(input$reiniciar, {
    estado$reproduciendo <- FALSE
    updateSliderInput(session, "tiempo", value = 0)
    updateSliderInput(session, "tiempo", value = 0)
  })
  # Crear un pulso cada 200 milisegundos mientras se reproduce.
  # Nota: el deslizador se usa como reloj; puede moverse también en pausa.
  observe({
    # Mantener la actualización periódica reactiva.
    invalidateLater(200, session)
    # Avanzar solo cuando la reproducción está activada.
    if (isTRUE(estado$reproduciendo)) {
      # Incrementar 0.2 segundos reales por la velocidad elegida.
      siguiente <- input$tiempo + 0.2 * input$velocidad
      # Detener la animación cuando sale el último estudiante.
      if (siguiente >= max(datos()$fin)) {
        estado$reproduciendo <- FALSE
      } else {
      }
      # Actualizar el control para reflejar el reloj animado.
      updateSliderInput(session, "tiempo", value = min(siguiente, max(datos()$fin)))
    }
  })
  # Calcular los estados de cada estudiante en el tiempo actual.
  estados <- reactive({
    # Obtener la tabla de eventos.
    d <- datos()
    # Leer el reloj de la simulación.
    t <- input$tiempo
    # Etiquetar el estado de cada cliente.
    d$estado <- ifelse(t < d$llegada, "pendiente",
                      ifelse(t < d$inicio, "fila",
                             ifelse(t < d$fin, "atencion", "salio")))
    # Entregar la tabla etiquetada.
    d
  })
  # Construir una ficha HTML para cada estudiante.
  ficha <- function(ids) {
    # Mostrar una indicación cuando no hay estudiantes.
    if (length(ids) == 0) return(tags$span(class = "vacio", "Sin estudiantes"))
    # Crear una ficha por estudiante presente.
    tagList(lapply(ids, function(i) {
      div(class = "estudiante", span(class = "avatar", "🎒"),
          paste0("Est. ", i))
    }))
  }
  # Dibujar la escena según los estados en el instante actual.
  output$escena <- renderUI({
    # Obtener los estudiantes clasificados por estado.
    d <- estados()
    # Crear el escenario con fila, mostrador y salida.
    div(class = "escena",
        h3("🏫 Zona de cafetería"),
        div(class = "zona", div(class = "etiqueta", "🚶 Llegando / en fila"),
            ficha(d$cliente[d$estado == "fila"])),
        div(class = "zona", div(class = "etiqueta", "☕ Mostrador de atención"),
            ficha(d$cliente[d$estado == "atencion"])),
        div(class = "zona", div(class = "etiqueta", "✅ Ya atendidos"),
            ficha(d$cliente[d$estado == "salio"])))
  })
  # Mostrar el reloj en minutos simulados.
  output$reloj <- renderText(sprintf("Tiempo simulado: %.1f min", input$tiempo))
  # Mostrar cuántos estudiantes están esperando.
  output$en_fila <- renderText(sum(estados()$estado == "fila"))
  # Mostrar cuántos estudiantes están siendo atendidos.
  output$en_atencion <- renderText(sum(estados()$estado == "atencion"))
  # Mostrar cuántos estudiantes ya salieron.
  output$atendidos <- renderText(sum(estados()$estado == "salio"))
  # Calcular indicadores para la realización completa.
  output$resumen <- renderTable({
    # Obtener la tabla de la simulación.
    d <- datos()
    # Presentar tiempos y proporciones de espera.
    data.frame(indicador = c("Espera media (min)", "Tiempo total medio (min)",
                               "Probabilidad empírica de esperar"),
               valor = round(c(mean(d$espera), mean(d$total),
                               mean(d$espera > 0)), 3))
  })
  # Mostrar los eventos individuales para validar la animación.
  output$eventos <- renderTable({
    # Redondear únicamente para visualización.
    head(round(datos(), 2), 20)
  })
}

# Iniciar la aplicación Shiny.
shinyApp(ui = ui, server = server)

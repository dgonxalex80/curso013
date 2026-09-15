# ============================================================
# GEMELO DIGITAL SIMPLE: FILA DE ESPERA
# Shiny + simulación dinámica FIFO/FCFS
# ============================================================

library(shiny)

# ------------------------------------------------------------
# INTERFAZ
# ------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    
    tags$style(HTML("

      body {
        background-color: #F7F9FB;
        font-family: Arial, sans-serif;
      }

      .titulo {
        font-size: 28px;
        font-weight: bold;
        color: #1E4E6B;
        margin-bottom: 5px;
      }

      .subtitulo {
        font-size: 16px;
        color: #607080;
        margin-bottom: 20px;
      }

      .panel-control {
        background: white;
        border-radius: 14px;
        padding: 18px;
        border: 1px solid #DDE4EA;
        margin-bottom: 15px;
      }

      .escenario {
        position: relative;
        height: 520px;
        background: white;
        border-radius: 16px;
        border: 1px solid #D9E1E8;
        overflow: hidden;
      }

      /* -----------------------------
         MOSTRADOR
      ----------------------------- */

      .mostrador {
        position: absolute;
        right: 60px;
        bottom: 90px;
        width: 210px;
        height: 90px;
        background: #66574F;
        border-radius: 8px;
      }

      .mostrador-superior {
        position: absolute;
        right: 45px;
        bottom: 170px;
        width: 230px;
        height: 25px;
        background: #403834;
        border-radius: 6px;
      }

      /* -----------------------------
         SERVIDOR
      ----------------------------- */

      .servidor {
        position: absolute;
        right: 120px;
        bottom: 185px;
        text-align: center;
        font-size: 48px;
      }

      .servidor-texto {
        font-size: 13px;
        color: #31586F;
        font-weight: bold;
      }

      /* -----------------------------
         CLIENTES
      ----------------------------- */

      .cliente {
        position: absolute;
        width: 60px;
        height: 90px;
        text-align: center;
        transition:
          left 0.6s ease-in-out,
          top 0.6s ease-in-out;
      }

      .persona {
        font-size: 42px;
      }

      .maleta {
        font-size: 22px;
        margin-top: -10px;
      }

      .idcliente {
        font-size: 11px;
        font-weight: bold;
        color: #496879;
      }

      /* -----------------------------
         CLIENTE EN ATENCION
      ----------------------------- */

      .atencion {
        position: absolute;
        right: 265px;
        bottom: 175px;
        width: 80px;
        text-align: center;
        transition: all 0.5s ease-in-out;
      }

      .atencion-persona {
        font-size: 48px;
      }

      /* -----------------------------
         INDICADORES
      ----------------------------- */

      .indicador {
        background: white;
        border-radius: 12px;
        padding: 12px;
        margin-bottom: 8px;
        border: 1px solid #DCE4EA;
      }

      .indicador-titulo {
        font-size: 13px;
        color: #72808A;
      }

      .indicador-valor {
        font-size: 25px;
        font-weight: bold;
        color: #1E4E6B;
      }

      .estado {
        position: absolute;
        right: 55px;
        top: 30px;
        background: #E7F1F7;
        padding: 10px 20px;
        border-radius: 10px;
        font-weight: bold;
        color: #1E4E6B;
      }

      .reloj {
        position: absolute;
        left: 25px;
        top: 20px;
        font-size: 20px;
        font-weight: bold;
        color: #1E4E6B;
      }

    "))
  ),
  
  div(
    class = "titulo",
    "Simulación dinámica de una fila de espera"
  ),
  
  div(
    class = "subtitulo",
    "Gemelo digital simplificado de un sistema de atención con un servidor"
  ),
  
  fluidRow(
    
    # ---------------------------------------------------------
    # CONTROLES
    # ---------------------------------------------------------
    
    column(
      width = 3,
      
      div(
        class = "panel-control",
        
        h4("Parámetros"),
        
        sliderInput(
          "media_llegada",
          "Tiempo medio entre llegadas:",
          min = 1,
          max = 10,
          value = 4,
          step = 0.5
        ),
        
        sliderInput(
          "media_servicio",
          "Tiempo medio de servicio:",
          min = 1,
          max = 10,
          value = 5,
          step = 0.5
        ),
        
        sliderInput(
          "sd_servicio",
          "Desviación del servicio:",
          min = 0.1,
          max = 3,
          value = 1,
          step = 0.1
        ),
        
        actionButton(
          "iniciar",
          "▶ Iniciar",
          class = "btn-primary"
        ),
        
        actionButton(
          "pausar",
          "⏸ Pausar"
        ),
        
        actionButton(
          "reiniciar",
          "↻ Reiniciar"
        )
      ),
      
      uiOutput("indicadores")
    ),
    
    # ---------------------------------------------------------
    # ESCENARIO
    # ---------------------------------------------------------
    
    column(
      width = 9,
      
      div(
        class = "escenario",
        
        uiOutput("reloj"),
        
        uiOutput("estado_servidor"),
        
        # servidor
        div(
          class = "servidor",
          div("👨‍💼"),
          div(
            class = "servidor-texto",
            "Servidor"
          )
        ),
        
        # mostrador
        div(class = "mostrador"),
        div(class = "mostrador-superior"),
        
        # cliente atendido
        uiOutput("cliente_atendido"),
        
        # fila
        uiOutput("fila")
      )
    )
  )
)


# ============================================================
# SERVIDOR
# ============================================================

server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # ESTADO DEL SISTEMA
  # ----------------------------------------------------------
  
  rv <- reactiveValues(
    
    tiempo = 0,
    
    funcionando = FALSE,
    
    cola = data.frame(
      id = integer(),
      llegada = numeric()
    ),
    
    servidor = NULL,
    
    tiempo_restante = 0,
    
    proxima_llegada = 1,
    
    id = 0,
    
    atendidos = 0,
    
    tiempos_espera = numeric(),
    
    tiempos_servicio = numeric(),
    
    tiempo_ocupado = 0
  )
  
  
  # ----------------------------------------------------------
  # GENERAR TIEMPO DE LLEGADA
  # ----------------------------------------------------------
  
  nueva_llegada <- reactive({
    
    rexp(
      1,
      rate = 1 / input$media_llegada
    )
    
  })
  
  
  # ----------------------------------------------------------
  # GENERAR TIEMPO DE SERVICIO
  # ----------------------------------------------------------
  
  nuevo_servicio <- reactive({
    
    x <- rnorm(
      1,
      mean = input$media_servicio,
      sd = input$sd_servicio
    )
    
    max(x, 0.5)
    
  })
  
  
  # ----------------------------------------------------------
  # INICIAR
  # ----------------------------------------------------------
  
  observeEvent(input$iniciar, {
    
    rv$funcionando <- TRUE
    
  })
  
  
  # ----------------------------------------------------------
  # PAUSAR
  # ----------------------------------------------------------
  
  observeEvent(input$pausar, {
    
    rv$funcionando <- FALSE
    
  })
  
  
  # ----------------------------------------------------------
  # REINICIAR
  # ----------------------------------------------------------
  
  observeEvent(input$reiniciar, {
    
    rv$tiempo <- 0
    
    rv$funcionando <- FALSE
    
    rv$cola <- data.frame(
      id = integer(),
      llegada = numeric()
    )
    
    rv$servidor <- NULL
    
    rv$tiempo_restante <- 0
    
    rv$proxima_llegada <- 1
    
    rv$id <- 0
    
    rv$atendidos <- 0
    
    rv$tiempos_espera <- numeric()
    
    rv$tiempos_servicio <- numeric()
    
    rv$tiempo_ocupado <- 0
    
  })
  
  
  # ==========================================================
  # MOTOR DE SIMULACIÓN
  # ==========================================================
  
  observe({
    
    invalidateLater(
      250,
      session
    )
    
    if (!rv$funcionando)
      return()
    
    # cada ciclo representa 0.25 unidades de tiempo
    
    dt <- 0.25
    
    rv$tiempo <- rv$tiempo + dt
    
    
    # --------------------------------------------------------
    # LLEGADA DE CLIENTES
    # --------------------------------------------------------
    
    if (rv$tiempo >= rv$proxima_llegada) {
      
      rv$id <- rv$id + 1
      
      nuevo <- data.frame(
        id = rv$id,
        llegada = rv$tiempo
      )
      
      rv$cola <- rbind(
        rv$cola,
        nuevo
      )
      
      rv$proxima_llegada <-
        rv$tiempo +
        rexp(
          1,
          rate = 1 / input$media_llegada
        )
    }
    
    
    # --------------------------------------------------------
    # SERVIDOR LIBRE
    # --------------------------------------------------------
    
    if (
      is.null(rv$servidor) &&
      nrow(rv$cola) > 0
    ) {
      
      cliente <- rv$cola[1, ]
      
      espera <-
        rv$tiempo -
        cliente$llegada
      
      rv$tiempos_espera <-
        c(
          rv$tiempos_espera,
          espera
        )
      
      rv$servidor <- cliente
      
      rv$cola <-
        rv$cola[-1, , drop = FALSE]
      
      servicio <-
        max(
          rnorm(
            1,
            input$media_servicio,
            input$sd_servicio
          ),
          0.5
        )
      
      rv$tiempo_restante <- servicio
      
      rv$tiempos_servicio <-
        c(
          rv$tiempos_servicio,
          servicio
        )
    }
    
    
    # --------------------------------------------------------
    # ATENCIÓN
    # --------------------------------------------------------
    
    if (!is.null(rv$servidor)) {
      
      rv$tiempo_restante <-
        rv$tiempo_restante - dt
      
      rv$tiempo_ocupado <-
        rv$tiempo_ocupado + dt
      
      
      # ------------------------------------------------------
      # FIN DEL SERVICIO
      # ------------------------------------------------------
      
      if (rv$tiempo_restante <= 0) {
        
        rv$atendidos <-
          rv$atendidos + 1
        
        rv$servidor <- NULL
        
        rv$tiempo_restante <- 0
      }
    }
    
  })
  
  
  # ==========================================================
  # DIBUJAR FILA
  # ==========================================================
  
  output$fila <- renderUI({
    
    n <- nrow(rv$cola)
    
    if (n == 0)
      return(NULL)
    
    
    elementos <- lapply(
      seq_len(min(n, 7)),
      function(i) {
        
        cliente <- rv$cola[i, ]
        
        
        # posiciones diagonales
        
        posiciones <- data.frame(
          
          left = c(
            680,
            575,
            470,
            365,
            260,
            155,
            60
          ),
          
          top = c(
            320,
            280,
            240,
            200,
            160,
            120,
            80
          )
        )
        
        
        pos <- posiciones[i, ]
        
        
        div(
          
          class = "cliente",
          
          style = paste0(
            "left:",
            pos$left,
            "px;",
            "top:",
            pos$top,
            "px;"
          ),
          
          div(
            class = "persona",
            "🧍"
          ),
          
          div(
            class = "maleta",
            "🧳"
          ),
          
          div(
            class = "idcliente",
            paste(
              "Cliente",
              cliente$id
            )
          )
        )
      }
    )
    
    
    do.call(
      tagList,
      elementos
    )
    
  })
  
  
  # ==========================================================
  # CLIENTE EN ATENCIÓN
  # ==========================================================
  
  output$cliente_atendido <- renderUI({
    
    if (is.null(rv$servidor))
      return(NULL)
    
    
    div(
      
      class = "atencion",
      
      div(
        class = "atencion-persona",
        "🧍"
      ),
      
      div(
        "📄"
      ),
      
      div(
        class = "idcliente",
        paste(
          "Cliente",
          rv$servidor$id
        )
      )
    )
    
  })
  
  
  # ==========================================================
  # RELOJ
  # ==========================================================
  
  output$reloj <- renderUI({
    
    div(
      class = "reloj",
      
      paste0(
        "Tiempo: ",
        round(
          rv$tiempo,
          1
        )
      )
    )
    
  })
  
  
  # ==========================================================
  # ESTADO DEL SERVIDOR
  # ==========================================================
  
  output$estado_servidor <- renderUI({
    
    estado <-
      if (is.null(rv$servidor)) {
        
        "Servidor libre"
        
      } else {
        
        paste0(
          "Atendiendo cliente ",
          rv$servidor$id
        )
      }
    
    
    div(
      class = "estado",
      estado
    )
    
  })
  
  
  # ==========================================================
  # INDICADORES
  # ==========================================================
  
  output$indicadores <- renderUI({
    
    espera_media <-
      if (length(rv$tiempos_espera) == 0) {
        
        0
        
      } else {
        
        mean(rv$tiempos_espera)
        
      }
    
    
    servicio_medio <-
      if (length(rv$tiempos_servicio) == 0) {
        
        0
        
      } else {
        
        mean(rv$tiempos_servicio)
        
      }
    
    
    utilizacion <-
      if (rv$tiempo == 0) {
        
        0
        
      } else {
        
        100 *
          rv$tiempo_ocupado /
          rv$tiempo
        
      }
    
    
    tagList(
      
      div(
        class = "indicador",
        
        div(
          class = "indicador-titulo",
          "Clientes atendidos"
        ),
        
        div(
          class = "indicador-valor",
          rv$atendidos
        )
      ),
      
      
      div(
        class = "indicador",
        
        div(
          class = "indicador-titulo",
          "Clientes en fila"
        ),
        
        div(
          class = "indicador-valor",
          nrow(rv$cola)
        )
      ),
      
      
      div(
        class = "indicador",
        
        div(
          class = "indicador-titulo",
          "Espera promedio"
        ),
        
        div(
          class = "indicador-valor",
          paste0(
            round(
              espera_media,
              2
            ),
            " min"
          )
        )
      ),
      
      
      div(
        class = "indicador",
        
        div(
          class = "indicador-titulo",
          "Servicio promedio"
        ),
        
        div(
          class = "indicador-valor",
          paste0(
            round(
              servicio_medio,
              2
            ),
            " min"
          )
        )
      ),
      
      
      div(
        class = "indicador",
        
        div(
          class = "indicador-titulo",
          "Utilización"
        ),
        
        div(
          class = "indicador-valor",
          paste0(
            round(
              utilizacion,
              1
            ),
            "%"
          )
        )
      )
    )
    
  })
  
}


# ============================================================
# EJECUTAR
# ============================================================

shinyApp(
  ui = ui,
  server = server
)


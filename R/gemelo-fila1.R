# ============================================================
# GEMELO DIGITAL DIDÁCTICO
# FILA DE ESPERA EN UNA SUCURSAL BANCARIA
# ============================================================

# Instalar si es necesario:
# install.packages(c("shiny", "dplyr"))

library(shiny)
library(dplyr)

# ============================================================
# INTERFAZ
# ============================================================

ui <- fluidPage(
  
  tags$head(
    
    tags$style(HTML("

      body {
        background-color: #f4f6f8;
        font-family: Arial, Helvetica, sans-serif;
      }

      .titulo {
        font-size: 30px;
        font-weight: bold;
        margin-bottom: 20px;
      }

      .panel-control {
        background: white;
        border-radius: 12px;
        padding: 18px;
        box-shadow: 0px 2px 8px rgba(0,0,0,0.12);
      }

      .banco {
        background: white;
        border-radius: 14px;
        padding: 20px;
        min-height: 450px;
        box-shadow: 0px 2px 8px rgba(0,0,0,0.12);
        position: relative;
      }

      .zona-titulo {
        font-weight: bold;
        font-size: 18px;
        margin-bottom: 8px;
      }

      .entrada {
        position: absolute;
        left: 25px;
        top: 180px;
        text-align: center;
      }

      .salida {
        position: absolute;
        right: 30px;
        bottom: 25px;
        text-align: center;
      }

      .persona-grande {
        font-size: 42px;
      }

      .fila {
        position: absolute;
        left: 100px;
        top: 180px;
        right: 270px;
        min-height: 100px;
        display: flex;
        align-items: center;
        flex-wrap: wrap;
        gap: 8px;
      }

      .cliente {
        font-size: 38px;
        text-align: center;
        min-width: 48px;
      }

      .cliente-id {
        font-size: 11px;
        margin-top: -4px;
      }

      .cajeros {
        position: absolute;
        right: 25px;
        top: 55px;
        width: 230px;
      }

      .caja {
        border: 2px solid #444;
        border-radius: 10px;
        margin-bottom: 12px;
        padding: 8px;
        background: #fafafa;
      }

      .caja-titulo {
        font-weight: bold;
        margin-bottom: 5px;
      }

      .cajero {
        font-size: 36px;
        display: inline-block;
        margin-right: 12px;
      }

      .cliente-atendido {
        font-size: 34px;
        display: inline-block;
      }

      .libre {
        font-size: 13px;
        font-weight: bold;
      }

      .metric-card {
        background: white;
        border-radius: 10px;
        padding: 12px;
        margin-bottom: 10px;
        box-shadow: 0px 2px 6px rgba(0,0,0,0.10);
        text-align: center;
      }

      .metric-title {
        font-size: 14px;
        font-weight: bold;
      }

      .metric-value {
        font-size: 26px;
        margin-top: 5px;
      }

      .estado {
        background: white;
        border-radius: 12px;
        padding: 14px;
        box-shadow: 0px 2px 8px rgba(0,0,0,0.12);
      }

    "))
  ),
  
  div(
    class = "titulo",
    "Gemelo Digital - Sistema de Fila de Espera"
  ),
  
  fluidRow(
    
    column(
      width = 3,
      
      div(
        class = "panel-control",
        
        h4("Parámetros del sistema"),
        
        sliderInput(
          "lambda",
          "Llegadas promedio por minuto:",
          min = 0.1,
          max = 2,
          value = 0.5,
          step = 0.1
        ),
        
        sliderInput(
          "servicio",
          "Tiempo promedio de atención (min):",
          min = 1,
          max = 10,
          value = 2,
          step = 0.5
        ),
        
        sliderInput(
          "cajeros",
          "Número de cajeros:",
          min = 1,
          max = 5,
          value = 2,
          step = 1
        ),
        
        hr(),
        
        actionButton(
          "reiniciar",
          "Reiniciar simulación",
          class = "btn btn-primary"
        ),
        
        br(),
        br(),
        
        actionButton(
          "pausar",
          "Pausar / continuar",
          class = "btn btn-warning"
        )
      )
    ),
    
    column(
      width = 9,
      
      uiOutput("banco"),
      
      br(),
      
      fluidRow(
        
        column(
          2,
          div(
            class = "metric-card",
            div(class = "metric-title", "Tiempo"),
            div(class = "metric-value", textOutput("tiempo"))
          )
        ),
        
        column(
          2,
          div(
            class = "metric-card",
            div(class = "metric-title", "En fila"),
            div(class = "metric-value", textOutput("ncola"))
          )
        ),
        
        column(
          2,
          div(
            class = "metric-card",
            div(class = "metric-title", "En servicio"),
            div(class = "metric-value", textOutput("servicio_actual"))
          )
        ),
        
        column(
          2,
          div(
            class = "metric-card",
            div(class = "metric-title", "Atendidos"),
            div(class = "metric-value", textOutput("atendidos"))
          )
        ),
        
        column(
          2,
          div(
            class = "metric-card",
            div(class = "metric-title", "Utilización"),
            div(class = "metric-value", textOutput("utilizacion"))
          )
        ),
        
        column(
          2,
          div(
            class = "metric-card",
            div(class = "metric-title", "Estado"),
            div(class = "metric-value", textOutput("estado_sistema"))
          )
        )
      ),
      
      br(),
      
      div(
        class = "estado",
        
        h4("Interpretación del sistema"),
        
        textOutput("interpretacion")
      )
    )
  )
)

# ============================================================
# SERVIDOR
# ============================================================

server <- function(input, output, session) {
  
  estado <- reactiveValues(
    
    tiempo = 0,
    
    cola = data.frame(
      id = integer(),
      llegada = numeric()
    ),
    
    servicio = data.frame(
      id = integer(),
      restante = numeric(),
      cajero = integer(),
      llegada = numeric()
    ),
    
    atendidos = 0,
    
    siguiente_id = 1,
    
    ocupado_total = 0,
    
    pausado = FALSE
  )
  
  # ============================================================
  # TEMPORIZADOR
  # ============================================================
  
  timer <- reactiveTimer(
    800,
    session = session
  )
  
  # ============================================================
  # MOTOR DE SIMULACIÓN
  # ============================================================
  
  observe({
    
    timer()
    
    req(
      input$lambda,
      input$servicio,
      input$cajeros
    )
    
    isolate({
      
      if (estado$pausado) {
        return()
      }
      
      # --------------------------------------------------------
      # Avanzar tiempo
      # --------------------------------------------------------
      
      estado$tiempo <- estado$tiempo + 1
      
      # --------------------------------------------------------
      # Registrar ocupación
      # --------------------------------------------------------
      
      estado$ocupado_total <-
        estado$ocupado_total +
        nrow(estado$servicio)
      
      # --------------------------------------------------------
      # Reducir tiempo restante de clientes en servicio
      # --------------------------------------------------------
      
      if (nrow(estado$servicio) > 0) {
        
        estado$servicio$restante <-
          estado$servicio$restante - 1
        
        finalizados <-
          estado$servicio$restante <= 0
        
        estado$atendidos <-
          estado$atendidos +
          sum(finalizados)
        
        estado$servicio <-
          estado$servicio[
            !finalizados,
            ,
            drop = FALSE
          ]
      }
      
      # --------------------------------------------------------
      # Generar llegadas
      # --------------------------------------------------------
      
      nuevas_llegadas <-
        rpois(
          1,
          lambda = input$lambda
        )
      
      if (nuevas_llegadas > 0) {
        
        ids_nuevos <-
          seq(
            from = estado$siguiente_id,
            length.out = nuevas_llegadas
          )
        
        nuevos_clientes <-
          data.frame(
            id = ids_nuevos,
            llegada = rep(
              estado$tiempo,
              nuevas_llegadas
            )
          )
        
        estado$cola <-
          bind_rows(
            estado$cola,
            nuevos_clientes
          )
        
        estado$siguiente_id <-
          estado$siguiente_id +
          nuevas_llegadas
      }
      
      # --------------------------------------------------------
      # Cajeros disponibles
      # --------------------------------------------------------
      
      todos_cajeros <-
        seq_len(input$cajeros)
      
      cajeros_ocupados <-
        estado$servicio$cajero
      
      cajeros_libres <-
        setdiff(
          todos_cajeros,
          cajeros_ocupados
        )
      
      # --------------------------------------------------------
      # Pasar clientes de fila a servicio
      # --------------------------------------------------------
      
      while (
        length(cajeros_libres) > 0 &&
        nrow(estado$cola) > 0
      ) {
        
        cliente_id <-
          estado$cola$id[1]
        
        llegada_cliente <-
          estado$cola$llegada[1]
        
        estado$cola <-
          estado$cola[
            -1,
            ,
            drop = FALSE
          ]
        
        cajero <-
          cajeros_libres[1]
        
        duracion <-
          max(
            1,
            ceiling(
              rexp(
                1,
                rate = 1 / input$servicio
              )
            )
          )
        
        estado$servicio <-
          bind_rows(
            
            estado$servicio,
            
            data.frame(
              id = cliente_id,
              restante = duracion,
              cajero = cajero,
              llegada = llegada_cliente
            )
          )
        
        cajeros_ocupados <-
          estado$servicio$cajero
        
        cajeros_libres <-
          setdiff(
            todos_cajeros,
            cajeros_ocupados
          )
      }
    })
  })
  
  # ============================================================
  # PAUSAR / CONTINUAR
  # ============================================================
  
  observeEvent(
    input$pausar,
    {
      estado$pausado <-
        !estado$pausado
    }
  )
  
  # ============================================================
  # REINICIAR
  # ============================================================
  
  observeEvent(
    input$reiniciar,
    {
      
      estado$tiempo <- 0
      
      estado$cola <-
        data.frame(
          id = integer(),
          llegada = numeric()
        )
      
      estado$servicio <-
        data.frame(
          id = integer(),
          restante = numeric(),
          cajero = integer(),
          llegada = numeric()
        )
      
      estado$atendidos <- 0
      
      estado$siguiente_id <- 1
      
      estado$ocupado_total <- 0
      
      estado$pausado <- FALSE
    }
  )
  
  # ============================================================
  # VISUALIZACIÓN DEL BANCO
  # ============================================================
  
  output$banco <- renderUI({
    
    # ----------------------------------------------------------
    # Clientes esperando
    # ----------------------------------------------------------
    
    clientes_fila <- NULL
    
    if (nrow(estado$cola) == 0) {
      
      clientes_fila <-
        tags$div(
          style = "
            font-size:16px;
            margin-top:30px;
          ",
          "No hay clientes esperando"
        )
      
    } else {
      
      clientes_fila <-
        lapply(
          seq_len(nrow(estado$cola)),
          function(i) {
            
            tags$div(
              class = "cliente",
              
              tags$div("🧍"),
              
              tags$div(
                class = "cliente-id",
                paste0(
                  "C",
                  estado$cola$id[i]
                )
              )
            )
          }
        )
    }
    
    # ----------------------------------------------------------
    # Construir cajas
    # ----------------------------------------------------------
    
    cajas_ui <-
      lapply(
        seq_len(input$cajeros),
        function(j) {
          
          cliente_actual <-
            estado$servicio[
              estado$servicio$cajero == j,
              ,
              drop = FALSE
            ]
          
          if (nrow(cliente_actual) == 0) {
            
            contenido_cliente <-
              tags$span(
                class = "libre",
                "LIBRE"
              )
            
          } else {
            
            contenido_cliente <-
              tags$span(
                
                tags$span(
                  class = "cliente-atendido",
                  "🧍"
                ),
                
                tags$span(
                  style = "
                    font-size:12px;
                    margin-left:5px;
                  ",
                  paste0(
                    "C",
                    cliente_actual$id,
                    " · ",
                    cliente_actual$restante,
                    " min"
                  )
                )
              )
          }
          
          tags$div(
            class = "caja",
            
            tags$div(
              class = "caja-titulo",
              paste(
                "Caja",
                j
              )
            ),
            
            tags$span(
              class = "cajero",
              "🧑‍💼"
            ),
            
            contenido_cliente
          )
        }
      )
    
    # ----------------------------------------------------------
    # Banco
    # ----------------------------------------------------------
    
    div(
      class = "banco",
      
      tags$div(
        style = "
          text-align:center;
          font-size:22px;
          font-weight:bold;
        ",
        "Sucursal Bancaria"
      ),
      
      tags$div(
        class = "entrada",
        
        tags$div(
          class = "zona-titulo",
          "Entrada"
        ),
        
        tags$div(
          class = "persona-grande",
          "🚶"
        ),
        
        tags$div(
          "↓"
        )
      ),
      
      tags$div(
        class = "fila",
        
        tags$div(
          style = "
            width:100%;
            font-weight:bold;
            margin-bottom:8px;
          ",
          paste0(
            "Fila de espera: ",
            nrow(estado$cola),
            " clientes"
          )
        ),
        
        clientes_fila
      ),
      
      tags$div(
        class = "cajeros",
        
        tags$div(
          class = "zona-titulo",
          "Puestos de atención"
        ),
        
        cajas_ui
      ),
      
      tags$div(
        class = "salida",
        
        tags$div(
          class = "persona-grande",
          "🚶"
        ),
        
        tags$div(
          "Salida"
        )
      )
    )
  })
  
  # ============================================================
  # INDICADORES
  # ============================================================
  
  output$tiempo <- renderText({
    
    paste0(
      estado$tiempo,
      " min"
    )
  })
  
  output$ncola <- renderText({
    
    nrow(
      estado$cola
    )
  })
  
  output$servicio_actual <- renderText({
    
    nrow(
      estado$servicio
    )
  })
  
  output$atendidos <- renderText({
    
    estado$atendidos
  })
  
  # ============================================================
  # UTILIZACIÓN
  # ============================================================
  
  output$utilizacion <- renderText({
    
    if (
      estado$tiempo == 0 ||
      input$cajeros == 0
    ) {
      
      return("0 %")
    }
    
    util <-
      estado$ocupado_total /
      (
        estado$tiempo *
          input$cajeros
      )
    
    util <-
      min(
        1,
        util
      )
    
    paste0(
      round(
        100 * util,
        1
      ),
      " %"
    )
  })
  
  # ============================================================
  # ESTADO DEL SISTEMA
  # ============================================================
  
  output$estado_sistema <- renderText({
    
    lambda <-
      input$lambda
    
    mu <-
      1 / input$servicio
    
    capacidad <-
      input$cajeros * mu
    
    rho <-
      lambda / capacidad
    
    if (rho < 0.7) {
      
      "Estable"
      
    } else if (rho < 1) {
      
      "Alta carga"
      
    } else {
      
      "Saturado"
    }
  })
  
  # ============================================================
  # INTERPRETACIÓN
  # ============================================================
  
  output$interpretacion <- renderText({
    
    lambda <-
      input$lambda
    
    mu <-
      1 / input$servicio
    
    capacidad <-
      input$cajeros * mu
    
    rho <-
      lambda / capacidad
    
    paste0(
      
      "La tasa de llegada es λ = ",
      round(lambda, 2),
      " clientes/min. ",
      
      "Cada cajero tiene una tasa promedio de servicio μ = ",
      round(mu, 2),
      " clientes/min. ",
      
      "Con ",
      input$cajeros,
      " cajero(s), la capacidad total aproximada es ",
      round(capacidad, 2),
      " clientes/min. ",
      
      "La intensidad de tráfico es ρ = ",
      round(rho, 2),
      ". ",
      
      if (rho < 1) {
        
        "Como ρ < 1, el sistema tiene capacidad promedio suficiente para atender la demanda."
        
      } else {
        
        "Como ρ ≥ 1, la demanda iguala o supera la capacidad de atención y la fila tenderá a crecer."
      }
    )
  })
}

# ============================================================
# EJECUTAR APP
# ============================================================

shinyApp(
  ui = ui,
  server = server
)
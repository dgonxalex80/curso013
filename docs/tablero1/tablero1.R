library(shiny)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(DT)

# Se carga una sola vez al iniciar la aplicación.
beer2 <- read_csv("data/beer2.csv", show_col_types = FALSE)

# Colores por tipo, adaptados a la identidad de curso013.
colores_tipo <- c(
  "lager artesanal" = "#F6A623",
  "clara artesanal" = "#3498C9",
  "lager importada" = "#476885",
  "cerveza normal y helada" = "#E85A67",
  "baja en calorías / sin alcohol" = "#67C1A8"
)

tema_curso <- bs_theme(
  version = 5, bootswatch = "flatly",
  primary = "#2C5697", secondary = "#E85A67",
  success = "#67C1A8", warning = "#F6A623",
  info = "#3498C9"
)

ui <- page_sidebar(
  title = "Caso cerveza",
  theme = tema_curso,
  sidebar = sidebar(
    selectInput("origen", "Origen",
                c("Todos", sort(unique(beer2$origen)))),
    selectInput("tipo", "Tipo de cerveza",
                c("Todos", sort(unique(beer2$tipo)))),
    sliderInput(
      "calificacion_minima", "Calificación mínima",
      min = min(beer2$calificacion, na.rm = TRUE),
      max = max(beer2$calificacion, na.rm = TRUE),
      value = min(beer2$calificacion, na.rm = TRUE),
      step = 0.5
    ),
    helpText("El precio es simulado y el análisis no implica causalidad.")
  ),
  
  # Réplica reactiva de las columnas 500, 300 y 200.
  layout_columns(
    card(
      full_screen = TRUE,
      card_header("Reseñas por tipo de cerveza"),
      plotOutput("grafico_tipo", height = 560)
    ),
    card(
      full_screen = TRUE,
      navset_card_tab(
        nav_panel(
          "Precio por tipo",
          plotOutput("grafico_precio", height = 500)
        ),
        nav_panel(
          "Calificación por origen",
          plotOutput("grafico_origen", height = 500)
        )
      )
    ),
    div(
      class = "d-grid gap-3",
      value_box(
        "Precio mínimo", textOutput("precio_minimo"),
        showcase = icon("tag"),
        theme = "primary"
      ),
      value_box(
        "Precio máximo", textOutput("precio_maximo"),
        showcase = icon("arrow-up"),
        theme = "warning"
      ),
      value_box(
        "Precio promedio", textOutput("precio_promedio"),
        showcase = icon("calculator"),
        theme = "success"
      )
    ),
    col_widths = c(6, 4, 2)
  ),
  
  card(
    full_screen = TRUE,
    card_header("Productos destacados"),
    DTOutput("ranking")
  )
)

server <- function(input, output, session) {
  datos_filtrados <- reactive({
    datos <- beer2 |>
      filter(calificacion >= input$calificacion_minima)
    if (input$origen != "Todos") {
      datos <- datos |> filter(origen == input$origen)
    }
    if (input$tipo != "Todos") {
      datos <- datos |> filter(tipo == input$tipo)
    }
    datos
  })
  
  output$precio_minimo <- renderText({
    datos <- datos_filtrados()
    validate(need(nrow(datos) > 0, "Sin datos"))
    paste0("$", round(min(datos$precio, na.rm = TRUE), 2))
  })
  
  output$precio_maximo <- renderText({
    datos <- datos_filtrados()
    validate(need(nrow(datos) > 0, "Sin datos"))
    paste0("$", round(max(datos$precio, na.rm = TRUE), 2))
  })
  
  output$precio_promedio <- renderText({
    datos <- datos_filtrados()
    validate(need(nrow(datos) > 0, "Sin datos"))
    paste0("$", round(mean(datos$precio, na.rm = TRUE), 2))
  })
  
  output$grafico_tipo <- renderPlot({
    datos <- datos_filtrados()
    validate(need(nrow(datos) > 0, "No hay reseñas para estos filtros"))
    frecuencias <- datos |>
      count(tipo, name = "frecuencia") |>
      mutate(tipo = reorder(tipo, frecuencia))
    
    ggplot(frecuencias, aes(tipo, frecuencia, fill = tipo)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = colores_tipo) +
      labs(x = NULL, y = "Número de reseñas") +
      theme_minimal(base_size = 14)
  })
  
  output$grafico_precio <- renderPlot({
    datos <- datos_filtrados()
    validate(need(nrow(datos) > 0, "No hay reseñas para estos filtros"))
    resumen <- datos |>
      group_by(tipo) |>
      summarise(
        precio_mediano = median(precio, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(resumen,
           aes(reorder(tipo, precio_mediano),
               precio_mediano, fill = tipo)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      scale_fill_manual(values = colores_tipo) +
      labs(x = NULL, y = "Precio mediano simulado") +
      theme_minimal(base_size = 13)
  })
  
  output$grafico_origen <- renderPlot({
    datos <- datos_filtrados()
    validate(need(nrow(datos) > 0, "No hay reseñas para estos filtros"))
    resumen <- datos |>
      group_by(origen) |>
      summarise(
        calificacion_mediana = median(calificacion, na.rm = TRUE),
        .groups = "drop"
      )
    
    ggplot(resumen,
           aes(origen, calificacion_mediana, fill = origen)) +
      geom_col(show.legend = FALSE) +
      scale_fill_manual(values = c(
        "importada" = "#3498C9",
        "nacional" = "#E85A67"
      )) +
      labs(x = NULL, y = "Calificación mediana") +
      theme_minimal(base_size = 13)
  })
  
  output$ranking <- renderDT({
    datos <- datos_filtrados()
    validate(need(nrow(datos) > 0, "No hay reseñas para estos filtros"))
    ranking <- datos |>
      group_by(id, nombre_cerveza, marca, tipo, origen) |>
      summarise(
        resenas = n(),
        calificacion_mediana = median(calificacion, na.rm = TRUE),
        calificacion_media = mean(calificacion, na.rm = TRUE),
        precio_mediano = median(precio, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(resenas >= 100) |>
      arrange(desc(calificacion_mediana),
              desc(calificacion_media), desc(resenas)) |>
      slice_head(n = 100)
    datatable(ranking, rownames = FALSE, filter = "top",
              options = list(pageLength = 10, scrollX = TRUE))
  })
}

shinyApp(ui = ui, server = server)
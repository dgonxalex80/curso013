# ============================================================
# Carga y descripción de la base de reseñas de cerveza
# ============================================================

# Ruta del archivo (ejecutar desde la raíz del proyecto curso013)
ruta_datos <- "data/beer_reviews.csv"

if (!file.exists(ruta_datos)) {
  stop("No se encontró el archivo: ", ruta_datos)
}

# Carga de la base. readr conserva mejor los nombres en UTF-8 y muestra
# el tipo detectado para cada variable.
if (requireNamespace("readr", quietly = TRUE)) {
  cerveza <- readr::read_csv(
    ruta_datos,
    na = c("NA", ""),
    show_col_types = FALSE
  )
} else {
  cerveza <- read.csv(
    ruta_datos,
    check.names = FALSE,
    na.strings = c("NA", ""),
    fileEncoding = "UTF-8"
  )
}

# ------------------------------------------------------------
# 1. Descripción general
# ------------------------------------------------------------
cat("\nDESCRIPCIÓN GENERAL\n")
cat("Filas:", nrow(cerveza), "\n")
cat("Columnas:", ncol(cerveza), "\n")
cat("Cervecerías:", length(unique(na.omit(cerveza$id_cervecería))), "\n")
cat("Cervezas:", length(unique(na.omit(cerveza$id_cerveza))), "\n")
cat("Estilos:", length(unique(na.omit(cerveza$estilo_cerveza))), "\n")
cat("Perfiles:", length(unique(na.omit(cerveza$nombre_perfil_reseña))), "\n")

cat("\nESTRUCTURA DE LA BASE\n")
str(cerveza)

# ------------------------------------------------------------
# 2. Periodo cubierto por las reseñas
# ------------------------------------------------------------
cerveza$fecha_reseña <- as.Date(
  as.POSIXct(cerveza$marca_tiempo_reseña, origin = "1970-01-01", tz = "UTC")
)

cat("\nPERIODO DE LAS RESEÑAS\n")
cat("Desde:", format(min(cerveza$fecha_reseña, na.rm = TRUE)), "\n")
cat("Hasta:", format(max(cerveza$fecha_reseña, na.rm = TRUE)), "\n")

# ------------------------------------------------------------
# 3. Datos faltantes
# ------------------------------------------------------------
faltantes <- data.frame(
  variable = names(cerveza),
  cantidad_na = vapply(cerveza, function(x) sum(is.na(x)), integer(1)),
  porcentaje_na = round(
    100 * vapply(cerveza, function(x) mean(is.na(x)), numeric(1)),
    2
  ),
  row.names = NULL
)
faltantes <- faltantes[order(faltantes$cantidad_na, decreasing = TRUE), ]

cat("\nDATOS FALTANTES POR VARIABLE\n")
print(faltantes[faltantes$cantidad_na > 0, ], row.names = FALSE)

# ------------------------------------------------------------
# 4. Resumen de variables numéricas
# ------------------------------------------------------------
es_numerica <- vapply(cerveza, is.numeric, logical(1))
variables_numericas <- names(cerveza)[es_numerica]

resumen_numerico <- do.call(
  rbind,
  lapply(variables_numericas, function(variable) {
    x <- cerveza[[variable]]
    data.frame(
      variable = variable,
      mínimo = min(x, na.rm = TRUE),
      q1 = as.numeric(quantile(x, 0.25, na.rm = TRUE)),
      mediana = median(x, na.rm = TRUE),
      media = mean(x, na.rm = TRUE),
      q3 = as.numeric(quantile(x, 0.75, na.rm = TRUE)),
      máximo = max(x, na.rm = TRUE),
      desviación = sd(x, na.rm = TRUE),
      n_válido = sum(!is.na(x)),
      row.names = NULL
    )
  })
)

columnas_redondeo <- c("mínimo", "q1", "mediana", "media", "q3", "máximo", "desviación")
resumen_numerico[columnas_redondeo] <- round(resumen_numerico[columnas_redondeo], 2)

cat("\nRESUMEN DE VARIABLES NUMÉRICAS\n")
print(resumen_numerico, row.names = FALSE)

# ------------------------------------------------------------
# 5. Distribuciones categóricas principales
# ------------------------------------------------------------
mostrar_frecuencias <- function(variable, máximo_categorías = Inf) {
  conteo <- sort(table(variable, useNA = "ifany"), decreasing = TRUE)
  porcentaje <- round(100 * prop.table(conteo), 2)
  resultado <- data.frame(
    categoría = names(conteo),
    frecuencia = as.integer(conteo),
    porcentaje = as.numeric(porcentaje),
    row.names = NULL
  )
  head(resultado, máximo_categorías)
}

cat("\nTIPO DE CERVEZA\n")
print(mostrar_frecuencias(cerveza$tipo_cerveza), row.names = FALSE)

cat("\nORIGEN\n")
print(mostrar_frecuencias(cerveza$origen), row.names = FALSE)

cat("\nESTILOS MÁS RESEÑADOS\n")
print(mostrar_frecuencias(cerveza$estilo_cerveza, 15), row.names = FALSE)

cat("\nNACIONALIDADES SIMULADAS MÁS FRECUENTES\n")
print(
  mostrar_frecuencias(cerveza$nacionalidad_simulada_cervecería, 15),
  row.names = FALSE
)

# ------------------------------------------------------------
# 6. Indicadores seleccionados por tipo de cerveza
# ------------------------------------------------------------
resumen_por_tipo <- aggregate(
  cbind(
    calificación_general,
    grado_alcohólico_completo,
    calorías_totales_355ml_simuladas
  ) ~ tipo_cerveza,
  data = cerveza,
  FUN = mean,
  na.rm = TRUE
)

columnas_resumen <- setdiff(names(resumen_por_tipo), "tipo_cerveza")
resumen_por_tipo[columnas_resumen] <- round(resumen_por_tipo[columnas_resumen], 2)

cat("\nPROMEDIOS POR TIPO DE CERVEZA\n")
print(resumen_por_tipo, row.names = FALSE)

# Los objetos cerveza, faltantes, resumen_numerico y resumen_por_tipo quedan
# disponibles en el entorno para continuar el análisis.

beer = cerveza[c(
    "id_cervecería",
    "nombre_cervecería",
    "calificación_general",
    "calificación_aroma",
    "calificación_apariencia",
    "calificación_paladar",                
    "calificación_sabor",
    
    "grado_alcohólico_completo",
    "calorías_del_alcohol_355ml",
    "origen_geográfico_estilo", 
    "origen",   
    "nacionalidad_simulada_cervecería",
    "tipo_cerveza",
    "estilo_cerveza")
 ]

beer = beer[, c(1:9,11,13,14)]

names(beer) = c(
"id",
"nombre_cerveceria",
"calificacion_general",
"calificacion_aroma",
"calificacion_apariencia",
"calificacion_paladar",
"calificacion_sabor",
"grado_alcohólico",
"calorías_355ml",
"origen",
"tipo_cerveza",
"estilo_cerveza")     



beer = beer[,1:11]



entrada <- "data/beer_reviews_original.csv"
salida <- "data/beer_reviews.csv"

datos <- read.csv(
  entrada,
  check.names = FALSE,
  stringsAsFactors = FALSE,
  na.strings = c("", "NA")
)

nombres_originales <- c(
  "brewery_id", "brewery_name", "review_time", "review_overall",
  "review_aroma", "review_appearance", "review_profilename", "beer_style",
  "review_palate", "review_taste", "beer_name", "beer_abv", "beer_beerid"
)

if (!identical(names(datos), nombres_originales)) {
  stop("La estructura de beer_reviews.csv no coincide con la esperada")
}

columnas_numericas <- c(
  "brewery_id", "review_time", "review_overall", "review_aroma",
  "review_appearance", "review_palate", "review_taste", "beer_abv", "beer_beerid"
)

na_antes <- vapply(datos[columnas_numericas], function(x) sum(is.na(x)), integer(1))
datos[columnas_numericas] <- lapply(datos[columnas_numericas], as.numeric)
na_despues <- vapply(datos[columnas_numericas], function(x) sum(is.na(x)), integer(1))
if (any(na_despues > na_antes)) stop("La conversión numérica introdujo valores faltantes")

# Traduce los descriptores; conserva nombres técnicos internacionales como IPA,
# Bock, Lambic, Kölsch, Stout, Porter, Witbier y Hefeweizen.
reemplazos_estilo <- c(
  "Bière de Champagne" = "Cerveza de champaña",
  "Bière de Garde" = "Cerveza de guarda",
  "California Common" = "Común de California",
  "Extra Special" = "Extra especial",
  "India Pale Ale" = "IPA",
  "Farmhouse Ale" = "Ale de granja",
  "Ancient Herbed Ale" = "Ale antigua con hierbas",
  "Foreign / Export" = "Extranjera / de exportación",
  "Fruit / Vegetable Beer" = "Cerveza de frutas / vegetales",
  "Herbed / Spiced Beer" = "Cerveza con hierbas / especias",
  "Low Alcohol Beer" = "Cerveza de bajo contenido alcohólico",
  "Smoked Beer" = "Cerveza ahumada",
  "Chile Beer" = "Cerveza con chile",
  "Rye Beer" = "Cerveza de centeno",
  "Pumpkin Ale" = "Ale de calabaza",
  "Steam Beer" = "Cerveza al vapor",
  "Black & Tan" = "Negra y clara",
  "Winter Warmer" = "Cerveza cálida de invierno",
  "American" = "Estadounidense",
  "English" = "Inglesa",
  "Belgian" = "Belga",
  "German" = "Alemana",
  "Irish" = "Irlandesa",
  "Russian" = "Rusa",
  "Scottish" = "Escocesa",
  "Japanese" = "Japonesa",
  "Czech" = "Checa",
  "Baltic" = "Báltica",
  "Vienna" = "Viena",
  "Munich" = "Múnich",
  "Euro" = "Europea",
  "Flanders" = "Flamenca",
  "Double" = "Doble",
  "Strong" = "Fuerte",
  "Pale" = "Pálida",
  "Dark" = "Oscura",
  "Black" = "Negra",
  "Blonde" = "Rubia",
  "Brown" = "Marrón",
  "Red" = "Roja",
  "Amber" = "Ámbar",
  "Light" = "Ligera",
  "Dry" = "Seca",
  "Sweet" = "Dulce",
  "Milk" = "Leche",
  "Oatmeal" = "Avena",
  "Rice" = "Arroz",
  "Wheat" = "Trigo",
  "Malt Liquor" = "Licor de malta",
  "Wild Ale" = "Ale salvaje",
  "Old Ale" = "Ale añeja",
  "Cream Ale" = "Ale crema",
  "Barleywine" = "Vino de cebada",
  "Wheatwine" = "Vino de trigo",
  "Adjunct" = "con adjuntos",
  "Unblended" = "Sin mezclar",
  "Fruit" = "Fruta"
)

traducir_estilo <- function(x) {
  resultado <- x
  for (origen in names(reemplazos_estilo)) {
    resultado <- gsub(origen, reemplazos_estilo[[origen]], resultado, fixed = TRUE)
  }
  resultado
}

estilo_original <- datos$beer_style
datos$beer_style <- traducir_estilo(datos$beer_style)

names(datos) <- c(
  "id_cervecería", "nombre_cervecería", "marca_tiempo_reseña",
  "calificación_general", "calificación_aroma", "calificación_apariencia",
  "nombre_perfil_reseña", "estilo_cerveza", "calificación_paladar",
  "calificación_sabor", "nombre_cerveza", "grado_alcohólico_cerveza",
  "id_cerveza"
)


# Variables derivadas para una porción estándar de 355 ml (12 onzas).
# Densidad del etanol: 0,789 g/ml; energía del etanol: 7 kcal/g.
datos$alcohol_puro_ml_355ml <- round(355 * datos$grado_alcohólico_cerveza / 100, 2)
datos$alcohol_puro_gramos_355ml <- round(datos$alcohol_puro_ml_355ml * 0.789, 2)
datos$calorías_del_alcohol_355ml <- round(datos$alcohol_puro_gramos_355ml * 7, 1)

# Describe el origen asociado al estilo, no el país de la cervecería.
inferir_origen_estilo <- function(estilo) {
  origen <- rep("No especificado", length(estilo))
  asignar <- function(patron, valor) {
    indices <- grepl(patron, estilo, ignore.case = TRUE) & origen == "No especificado"
    origen[indices] <<- valor
  }
  asignar("American|California Common", "Estados Unidos")
  asignar("English", "Inglaterra")
  asignar("Belgian|Flanders|Dubbel|Tripel|Quadrupel|Witbier|Gueuze|Faro", "Bélgica")
  asignar("German|Berliner|Dortmunder|Dunkel|Doppelbock|Eisbock|Hefeweizen|Keller|Kölsch|Kristalweizen|Maibock|Märzen|Rauchbier|Roggenbier|Schwarzbier|Weizenbock|Gose", "Alemania")
  asignar("Czech", "República Checa")
  asignar("Irish", "Irlanda")
  asignar("Scottish|Scotch", "Escocia")
  asignar("Russian", "Rusia")
  asignar("Japanese|Happoshu", "Japón")
  asignar("Vienna", "Austria")
  asignar("Bière de Champagne|Bière de Garde", "Francia")
  asignar("Baltic", "Región báltica")
  asignar("Sahti", "Finlandia")
  asignar("Kvass", "Europa oriental")
  origen[is.na(estilo)] <- NA_character_
  origen
}

datos$origen_geográfico_estilo <- inferir_origen_estilo(estilo_original)


# Simulación reproducible de variables no observadas.
datos$grado_alcohólico_fue_imputado <- ifelse(is.na(datos$grado_alcohólico_cerveza), "Sí", "No")
mediana_abv_estilo <- ave(datos$grado_alcohólico_cerveza, datos$estilo_cerveza, FUN = function(x) median(x, na.rm = TRUE))
datos$grado_alcohólico_completo <- datos$grado_alcohólico_cerveza
faltante_abv <- is.na(datos$grado_alcohólico_completo)
datos$grado_alcohólico_completo[faltante_abv] <- mediana_abv_estilo[faltante_abv]
datos$grado_alcohólico_completo[is.na(datos$grado_alcohólico_completo)] <- median(datos$grado_alcohólico_cerveza, na.rm = TRUE)
datos$alcohol_puro_ml_355ml_simulado <- round(355 * datos$grado_alcohólico_completo / 100, 2)
datos$alcohol_puro_gramos_355ml_simulado <- round(datos$alcohol_puro_ml_355ml_simulado * 0.789, 2)
ajuste_estilo <- rep(0, nrow(datos))
ajuste_estilo[grepl("Ligera|Light|Low Alcohol", datos$estilo_cerveza, ignore.case = TRUE)] <- -3
ajuste_estilo[grepl("Stout|Porter|Avena|Leche|Dulce", datos$estilo_cerveza, ignore.case = TRUE)] <- 3
ajuste_estilo[grepl("Trigo|Wheat|Witbier|Hefeweizen", datos$estilo_cerveza, ignore.case = TRUE)] <- 2
ajuste_estilo[grepl("Vino de cebada|Barleywine|Malt", datos$estilo_cerveza, ignore.case = TRUE)] <- 4
es_ipa <- grepl("IPA", datos$estilo_cerveza, ignore.case = TRUE)
ajuste_estilo[es_ipa] <- ajuste_estilo[es_ipa] + 1
variación_cerveza <- ((datos$id_cerveza * 37) %% 21 - 10) / 10
datos$carbohidratos_gramos_355ml_simulados <- round(pmax(2, 2 * datos$grado_alcohólico_completo + ajuste_estilo + variación_cerveza), 1)
datos$calorías_totales_355ml_simuladas <- round(7 * datos$alcohol_puro_gramos_355ml_simulado + 4 * datos$carbohidratos_gramos_355ml_simulados + 6, 0)
origen_util <- datos$origen_geográfico_estilo
origen_util[origen_util == "No especificado"] <- NA_character_
tabla_origen <- table(datos$id_cervecería, origen_util, useNA = "no")
origen_modal <- apply(tabla_origen, 1, function(x) if (length(x) == 0 || all(x == 0)) NA_character_ else names(which.max(x)))
datos$nacionalidad_simulada_cervecería <- unname(origen_modal[as.character(datos$id_cervecería)])
sin_origen <- is.na(datos$nacionalidad_simulada_cervecería)
países_respaldo <- c("Estados Unidos", "Alemania", "Bélgica", "Inglaterra", "Irlanda", "Canadá", "República Checa", "Francia", "Japón", "Austria")
datos$nacionalidad_simulada_cervecería[sin_origen] <- países_respaldo[(datos$id_cervecería[sin_origen] %% length(países_respaldo)) + 1]


# Origen comercial simulado; Estados Unidos es el mercado nacional de referencia.
datos$origen <- ifelse(
  datos$nacionalidad_simulada_cervecería == "Estados Unidos",
  "nacional",
  "importada"
)
datos$origen <- factor(datos$origen, levels = c("nacional", "importada"))

# Clasificación comercial simulada y mutuamente excluyente.
es_baja <- grepl("bajo contenido alcohólico|Low Alcohol|Light Lager", datos$estilo_cerveza, ignore.case = TRUE) | datos$grado_alcohólico_completo <= 3.5
es_lager <- grepl("Lager|Pils|Helles|Dunkel|Märzen|Oktoberfest|Schwarzbier|Bock|Eisbock|Doppelbock|Happoshu", datos$estilo_cerveza, ignore.case = TRUE)
es_clara <- grepl("Pálida|Pale|Blonde|Rubia|IPA|Witbier|Hefeweizen|Trigo|Wheat|Kölsch|Cream", datos$estilo_cerveza, ignore.case = TRUE)
es_importada <- datos$nacionalidad_simulada_cervecería != "Estados Unidos"

datos$tipo_cerveza <- "cerveza normal y helada"
datos$tipo_cerveza[es_clara] <- "clara artesanal"
datos$tipo_cerveza[es_lager] <- "lager artesanal"
datos$tipo_cerveza[es_lager & es_importada] <- "lager importada"
datos$tipo_cerveza[es_baja] <- "baja en calorías / sin alcohol"
datos$tipo_cerveza <- factor(datos$tipo_cerveza, levels = c("lager artesanal", "clara artesanal", "lager importada", "cerveza normal y helada", "baja en calorías / sin alcohol"))

write.csv(datos, salida, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")

cat("Archivo creado:", salida, "\n")
cat("Filas:", nrow(datos), " Columnas:", ncol(datos), "\n")

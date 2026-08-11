library(data.table)

ruta <- "data/beer2.csv"
beer2 <- fread(ruta)

# La nutrición solo se transforma en la versión original (que tenía grandes
# masas de valores repetidos). El precio, en cambio, se recalcula siempre a
# partir de variables estables, de modo que este script sea idempotente.
masa_precio_maxima <- max(tabulate(match(beer2$precio, unique(beer2$precio)))) / nrow(beer2)
ajustar_nutricion <- masa_precio_maxima >= 0.01

# Las características comerciales y nutricionales corresponden a la cerveza,
# no a cada reseña. Se genera un único valor por id y luego se replica.
cervezas <- beer2[, .(
  n_resenas = .N,
  alcohol_original = median(alcohol),
  tipo = tipo[1L],
  origen = origen[1L],
  calificacion_promedio = mean(calificacion)
), by = id]

# Precio de una caja de seis. Se incorporan relaciones comerciales esperables:
# prima por importación, prima artesanal y asociación positiva con la valoración
# media del producto. Un componente idiosincrático impide que el precio sea una
# función mecánica de esas características.
cervezas[, u_ruido := (((as.double(id) * 130363 + 7411) %% 1000003) + 0.5) / 1000003]
cervezas[, ruido_precio := 0.42 * qnorm(u_ruido)]
cervezas[, ajuste_tipo_precio := fifelse(
  grepl("artesanal", tipo, ignore.case = TRUE), 0.45,
  fifelse(grepl("baja", tipo, ignore.case = TRUE), -0.25, 0.0)
)]
cervezas[, precio_nuevo := round(pmax(
  2.20,
  4.00 +
    1.10 * (origen == "importada") +
    0.80 * (calificacion_promedio - 3.50) +
    ajuste_tipo_precio + ruido_precio
), 2)]

# Comprimir suavemente únicamente la cola extrema del alcohol. A diferencia de
# pmin/pmax, tanh no crea acumulaciones artificiales en un punto de corte.
cervezas[, alcohol_nuevo := if (ajustar_nutricion) {
  round(60 * tanh(alcohol_original / 60), 2)
} else {
  alcohol_original
}]

# Variación determinista por cerveza para evitar una relación perfectamente
# mecánica. Los estilos más densos reciben un pequeño ajuste de carbohidratos.
cervezas[, ruido := sin(as.double(id) * 12.9898) * 1.6]
cervezas[, ajuste_tipo := fifelse(
  grepl("baja", tipo, ignore.case = TRUE), -2.0,
  fifelse(grepl("artesanal", tipo, ignore.case = TRUE), 1.0, 0.0)
)]
cervezas[, carbohidratos_nuevos := round(
  pmax(2.0, 7.0 + 0.36 * alcohol_nuevo + ajuste_tipo + ruido), 1
)]

# Balance energético aproximado: 7 kcal/g de alcohol y 4 kcal/g de
# carbohidratos, más una pequeña contribución fija de otros componentes.
cervezas[, calorias_nuevas := round(
  7 * alcohol_nuevo + 4 * carbohidratos_nuevos + 6
)]

ajustes <- cervezas[, .(
  id,
  precio = precio_nuevo,
  alcohol = alcohol_nuevo,
  carbohidratos = carbohidratos_nuevos,
  calorias = calorias_nuevas
)]

beer2[, c("precio", "alcohol", "carbohidratos", "calorias") := NULL]
beer2 <- ajustes[beer2, on = "id"]
setcolorder(beer2, c(
  "id", "marca", "precio", "alcohol", "carbohidratos", "calorias",
  "nacionalidad", "calificacion", "aroma", "apariencia", "paladar",
  "sabor", "nombre_cerveza", "tipo", "origen"
))

fwrite(beer2, ruta, quote = TRUE)

cat("Archivo actualizado:", ruta, "\n")
cat("Cervezas:", nrow(cervezas), " Reseñas:", nrow(beer2), "\n")

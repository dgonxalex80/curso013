library(data.table)

ruta <- "data/beer2.csv"
beer2 <- fread(ruta)

# Evita aplicar dos veces las transformaciones sobre el mismo archivo.
masa_precio_maxima <- max(tabulate(match(beer2$precio, unique(beer2$precio)))) / nrow(beer2)
if (masa_precio_maxima < 0.01) {
  stop("beer2.csv ya parece estar ajustado: no se realizaron cambios.")
}

# Las características comerciales y nutricionales corresponden a la cerveza,
# no a cada reseña. Se genera un único valor por id y luego se replica.
cervezas <- beer2[, .(
  n_resenas = .N,
  alcohol_original = median(alcohol),
  tipo = tipo[1L],
  origen = origen[1L]
), by = id]

# Número pseudoaleatorio reproducible asociado al id, sin depender del orden
# de las filas. Se usa para ordenar las cervezas y construir cuantiles ponderados.
cervezas[, clave := (as.double(id) * 104729 + 12345) %% 1000003]
setorder(cervezas, clave, id)
cervezas[, u_precio := (cumsum(n_resenas) - n_resenas / 2) / sum(n_resenas)]

# Precio de una caja de seis: beta simétrica escalada. Tiene forma acampanada,
# soporte finito y densidad que cae suavemente cerca de los extremos.
cervezas[, precio_nuevo := round(2.20 + 5.60 * qbeta(u_precio, 4.5, 4.5), 2)]

# Comprimir suavemente únicamente la cola extrema del alcohol. A diferencia de
# pmin/pmax, tanh no crea acumulaciones artificiales en un punto de corte.
cervezas[, alcohol_nuevo := round(60 * tanh(alcohol_original / 60), 2)]

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

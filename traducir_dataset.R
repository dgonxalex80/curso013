entrada <- "data/dataset_original.csv"
salida <- "data/dataset_es.csv"

datos <- read.csv(
  entrada,
  check.names = FALSE,
  na.strings = c("", "NA"),
  stringsAsFactors = FALSE
)

columnas_categoricas <- c(
  "NAME_CONTRACT_TYPE", "CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY",
  "NAME_TYPE_SUITE", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE",
  "NAME_FAMILY_STATUS", "NAME_HOUSING_TYPE", "OCCUPATION_TYPE",
  "WEEKDAY_APPR_PROCESS_START", "ORGANIZATION_TYPE", "FONDKAPREMONT_MODE",
  "HOUSETYPE_MODE", "WALLSMATERIAL_MODE", "EMERGENCYSTATE_MODE"
)

# Las columnas restantes deben quedar explícitamente como numéricas.
columnas_numericas <- setdiff(names(datos), columnas_categoricas)
na_antes <- vapply(datos[columnas_numericas], function(x) sum(is.na(x)), integer(1))
datos[columnas_numericas] <- lapply(datos[columnas_numericas], as.numeric)
na_despues <- vapply(datos[columnas_numericas], function(x) sum(is.na(x)), integer(1))

if (any(na_despues > na_antes)) {
  problematicas <- names(na_despues)[na_despues > na_antes]
  stop("La conversión numérica introdujo NA en: ", paste(problematicas, collapse = ", "))
}

traducir_valores <- function(x, diccionario) {
  resultado <- unname(diccionario[x])
  sin_traduccion <- is.na(resultado) & !is.na(x)
  resultado[sin_traduccion] <- x[sin_traduccion]
  resultado
}

diccionario_general <- c(
  "Cash loans" = "Préstamos de efectivo",
  "Revolving loans" = "Créditos rotativos",
  "F" = "Femenino", "M" = "Masculino", "Y" = "Sí", "N" = "No",
  "Unaccompanied" = "Sin acompañante", "Family" = "Familia",
  "Spouse, partner" = "Cónyuge o pareja", "Group of people" = "Grupo de personas",
  "Other_A" = "Otro A", "Other_B" = "Otro B", "Children" = "Hijos",
  "Working" = "Empleado", "State servant" = "Empleado público",
  "Pensioner" = "Pensionado", "Commercial associate" = "Asociado comercial",
  "Businessman" = "Empresario", "Student" = "Estudiante",
  "Unemployed" = "Desempleado",
  "Higher education" = "Educación superior",
  "Secondary / secondary special" = "Secundaria o secundaria especial",
  "Incomplete higher" = "Educación superior incompleta",
  "Lower secondary" = "Secundaria básica", "Academic degree" = "Título académico",
  "Married" = "Casado", "Single / not married" = "Soltero",
  "Civil marriage" = "Unión civil", "Widow" = "Viudo", "Separated" = "Separado",
  "House / apartment" = "Casa o apartamento", "With parents" = "Con los padres",
  "Rented apartment" = "Apartamento arrendado",
  "Municipal apartment" = "Apartamento municipal",
  "Office apartment" = "Apartamento de oficina",
  "Co-op apartment" = "Apartamento cooperativo",
  "Low-skill Laborers" = "Obreros no calificados", "Drivers" = "Conductores",
  "Sales staff" = "Personal de ventas",
  "High skill tech staff" = "Personal técnico calificado",
  "Core staff" = "Personal principal", "Laborers" = "Obreros",
  "Managers" = "Directivos", "Accountants" = "Contadores",
  "Medicine staff" = "Personal médico", "Security staff" = "Personal de seguridad",
  "Private service staff" = "Personal de servicios privados",
  "Secretaries" = "Secretarios", "Cleaning staff" = "Personal de limpieza",
  "Cooking staff" = "Personal de cocina", "HR staff" = "Personal de recursos humanos",
  "Waiters/barmen staff" = "Meseros y cantineros",
  "Realty agents" = "Agentes inmobiliarios", "IT staff" = "Personal de TI",
  "MONDAY" = "Lunes", "TUESDAY" = "Martes", "WEDNESDAY" = "Miércoles",
  "THURSDAY" = "Jueves", "FRIDAY" = "Viernes", "SATURDAY" = "Sábado",
  "SUNDAY" = "Domingo",
  "Kindergarten" = "Jardín infantil", "Self-employed" = "Trabajador independiente",
  "Government" = "Gobierno", "School" = "Escuela", "XNA" = "No aplica",
  "Services" = "Servicios", "Bank" = "Banco", "Other" = "Otro",
  "Postal" = "Servicio postal", "Medicine" = "Medicina", "Housing" = "Vivienda",
  "Construction" = "Construcción", "Military" = "Fuerzas militares",
  "Legal Services" = "Servicios legales", "Security" = "Seguridad",
  "University" = "Universidad", "Agriculture" = "Agricultura",
  "Security Ministries" = "Ministerios de seguridad", "Telecom" = "Telecomunicaciones",
  "Emergency" = "Emergencias", "Police" = "Policía", "Electricity" = "Electricidad",
  "Hotel" = "Hotel", "Restaurant" = "Restaurante", "Advertising" = "Publicidad",
  "Mobile" = "Telefonía móvil", "Realtor" = "Inmobiliaria", "Cleaning" = "Limpieza",
  "Culture" = "Cultura", "Insurance" = "Seguros", "Religion" = "Religión",
  "reg oper account" = "cuenta operativa regular",
  "not specified" = "no especificado",
  "org spec account" = "cuenta especial de la organización",
  "reg oper spec account" = "cuenta operativa especial regular",
  "block of flats" = "bloque de apartamentos",
  "specific housing" = "vivienda específica", "terraced house" = "casa adosada",
  "Stone, brick" = "Piedra o ladrillo", "Panel" = "Panel", "Block" = "Bloque",
  "Wooden" = "Madera", "Mixed" = "Mixto", "Monolithic" = "Monolítico",
  "Others" = "Otros", "No" = "No", "Yes" = "Sí"
)

for (columna in columnas_categoricas) {
  datos[[columna]] <- traducir_valores(datos[[columna]], diccionario_general)
}

# Familias de categorías numeradas.
datos$ORGANIZATION_TYPE <- sub("^Industry: type ", "Industria: tipo ", datos$ORGANIZATION_TYPE)
datos$ORGANIZATION_TYPE <- sub("^Trade: type ", "Comercio: tipo ", datos$ORGANIZATION_TYPE)
datos$ORGANIZATION_TYPE <- sub("^Transport: type ", "Transporte: tipo ", datos$ORGANIZATION_TYPE)
datos$ORGANIZATION_TYPE <- sub("^Business Entity Type ", "Entidad empresarial tipo ", datos$ORGANIZATION_TYPE)

tokens <- c(
  SK="", ID="id", CURR="solicitud", NAME="nombre", CONTRACT="contrato",
  TYPE="tipo", CODE="código", GENDER="género", FLAG="indicador", OWN="propio",
  CAR="automóvil", REALTY="inmueble", CNT="cantidad", CHILDREN="hijos",
  AMT="monto", INCOME="ingreso", TOTAL="total", CREDIT="crédito",
  ANNUITY="anualidad", GOODS="bienes", PRICE="precio", SUITE="acompañante",
  EDUCATION="educación", FAMILY="familia", STATUS="estado", HOUSING="vivienda",
  REGION="región", POPULATION="población", RELATIVE="relativa", DAYS="días",
  BIRTH="nacimiento", EMPLOYED="empleado", REGISTRATION="registro",
  PUBLISH="expedición", AGE="edad", MOBIL="móvil", EMP="laboral", WORK="trabajo",
  CONT="contacto", MOBILE="móvil", PHONE="teléfono", EMAIL="correo",
  OCCUPATION="ocupación", FAM="familiares", MEMBERS="miembros", RATING="calificación",
  CLIENT="cliente", W="con", CITY="ciudad", WEEKDAY="día_semana", APPR="aprobación",
  PROCESS="proceso", START="inicio", HOUR="hora", REG="registro", NOT="no",
  LIVE="reside", ORGANIZATION="organización", EXT="externa", SOURCE="fuente",
  APARTMENTS="apartamentos", AVG="promedio", BASEMENTAREA="área_sótano",
  YEARS="años", BEGINEXPLUATATION="inicio_explotación", BUILD="construcción",
  COMMONAREA="área_común", ELEVATORS="ascensores", ENTRANCES="entradas",
  FLOORSMAX="pisos_máximo", FLOORSMIN="pisos_mínimo", LANDAREA="área_terreno",
  LIVINGAPARTMENTS="apartamentos_habitables", LIVINGAREA="área_habitable",
  NONLIVINGAPARTMENTS="apartamentos_no_habitables", NONLIVINGAREA="área_no_habitable",
  MODE="moda", MEDI="mediana", FONDKAPREMONT="fondo_reparación",
  HOUSETYPE="tipo_vivienda", TOTALAREA="área_total", WALLSMATERIAL="material_paredes",
  EMERGENCYSTATE="estado_emergencia", OBS="observaciones", DEF="incumplimientos",
  SOCIAL="social", CIRCLE="círculo", LAST="último", CHANGE="cambio",
  DOCUMENT="documento", REQ="solicitudes", BUREAU="buró", DAY="día",
  WEEK="semana", MON="mes", QRT="trimestre", YEAR="año"
)

traducir_nombre <- function(nombre) {
  partes <- strsplit(nombre, "_", fixed = TRUE)[[1]]
  traducidas <- ifelse(partes %in% names(tokens), tokens[partes], tolower(partes))
  traducidas <- traducidas[nzchar(traducidas)]
  paste(traducidas, collapse = "_")
}

nombres_es <- vapply(names(datos), traducir_nombre, character(1))
nombres_es[names(datos) == "SK_ID_CURR"] <- "id_solicitud"
if (anyDuplicated(nombres_es)) stop("La traducción produjo nombres duplicados")
names(datos) <- nombres_es

# Las fechas de la fuente son desplazamientos retrospectivos respecto a la solicitud.
# Se expresan como días transcurridos positivos para facilitar su interpretación.
columnas_dias <- c(
  "días_nacimiento", "días_empleado", "días_registro",
  "días_id_expedición", "días_último_teléfono_cambio"
)

# 365243 es el código de Home Credit para antigüedad laboral no disponible.
datos[["días_empleado"]][datos[["días_empleado"]] == 365243] <- NA_real_
datos[columnas_dias] <- lapply(datos[columnas_dias], abs)

# La calificación regional válida se encuentra entre 1 y 3.
datos[["región_calificación_cliente_con_ciudad"]][
  datos[["región_calificación_cliente_con_ciudad"]] < 1
] <- NA_real_

write.csv(datos, salida, row.names = FALSE, na = "NA", fileEncoding = "UTF-8")

cat("Archivo creado:", salida, "\n")
cat("Filas:", nrow(datos), " Columnas:", ncol(datos), "\n")

# importar data
library(readr)
df <- read_csv("data/beer2.csv")

# visualizar caracteristicas de la data
head(df)

# Tablas de frecuencia variables cualitativas
# tablas de frecuencia absoluta
table(df$nacionalidad)

# tablas de frecuencia
summarytools::freq(df$nacionalidad)
summarytools::freq(df$nacionalidad, cumul = FALSE)
summarytools::freq(df$nacionalidad, cumul = FALSE, totals = TRUE)
summarytools::freq(df$nacionalidad, cumul = FALSE, order = "freq")


# tablas bivariadas
table(df$nacionalidad, df$origen)
summarytools::freq(df$nacionalidad, df$origen, freq)


print(df$nacionalidad, method = "render", file = "fr_smoker_by_gender.md)


# tabla de frecuencias variables cuantitativas

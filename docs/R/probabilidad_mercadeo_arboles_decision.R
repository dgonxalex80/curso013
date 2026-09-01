# ============================================================
# PROBABILIDAD APLICADA AL MERCADEO MEDIANTE ARBOLES DE DECISION
# Base de datos: Carseats - paquete ISLR2
# ============================================================

# 1. INSTALACION DE PAQUETES
# Ejecutar solamente si los paquetes no estan instalados

# install.packages("ISLR2")
# install.packages("rpart")
# install.packages("rattle")
# install.packages("rpart.plot")


# 2. CARGAR PAQUETES

library(ISLR2)
library(rpart)
library(rattle)
library(rpart.plot)


# 3. CARGAR LOS DATOS

data(Carseats)

# Dimension de la base
dim(Carseats)

# Primeras observaciones
head(Carseats)

# Estructura
str(Carseats)

# Resumen
summary(Carseats)


# 4. CREAR LA VARIABLE VENTAS ALTAS

# Se consideran ventas altas cuando Sales > 8
Carseats$VentaAlta <- ifelse(Carseats$Sales > 8, "Si", "No")

Carseats$VentaAlta <- factor(
  Carseats$VentaAlta,
  levels = c("No", "Si")
)

# Frecuencias
table(Carseats$VentaAlta)


# ============================================================
# 5. PROBABILIDAD MARGINAL
# ============================================================

tabla_venta <- table(Carseats$VentaAlta)

# Frecuencias absolutas
tabla_venta

# Probabilidades
prop.table(tabla_venta)

# P(VentaAlta = Si)
P_A <- mean(Carseats$VentaAlta == "Si")

P_A


# ============================================================
# 6. TABLA DE CONTINGENCIA:
#    UBICACION DEL PRODUCTO Y VENTAS ALTAS
# ============================================================

tabla <- table(
  Ubicacion = Carseats$ShelveLoc,
  VentaAlta = Carseats$VentaAlta
)

tabla


# ============================================================
# 7. PROBABILIDADES CONJUNTAS
# ============================================================

# Tabla completa de probabilidades conjuntas
prop.table(tabla)

# Definimos:
# A = VentaAlta = Si
# B = ShelveLoc = Good

# P(A interseccion B)
P_A_inter_B <- mean(
  Carseats$VentaAlta == "Si" &
  Carseats$ShelveLoc == "Good"
)

P_A_inter_B


# ============================================================
# 8. PROBABILIDAD DEL EVENTO B
# ============================================================

# P(ShelveLoc = Good)
P_B <- mean(Carseats$ShelveLoc == "Good")

P_B


# ============================================================
# 9. PROBABILIDADES CONDICIONALES
# ============================================================

# P(VentaAlta | ShelveLoc)
prop.table(tabla, margin = 1)

# P(VentaAlta = Si | ShelveLoc = Good)
P_A_dado_B <- mean(
  Carseats$VentaAlta[Carseats$ShelveLoc == "Good"] == "Si"
)

P_A_dado_B


# ============================================================
# 10. COMPARACION ENTRE PROBABILIDAD MARGINAL Y CONDICIONAL
# ============================================================

P_A
P_A_dado_B

comparacion <- data.frame(
  Probabilidad = c(
    "P(VentaAlta)",
    "P(VentaAlta | ShelveLoc = Good)"
  ),
  Valor = c(
    P_A,
    P_A_dado_B
  )
)

comparacion


# ============================================================
# 11. REGLA DEL PRODUCTO
# ============================================================

# P(A interseccion B) = P(B) * P(A | B)

P_A_inter_B

P_B * P_A_dado_B

# Verificacion numerica
all.equal(
  P_A_inter_B,
  P_B * P_A_dado_B
)


# ============================================================
# 12. ARBOL DE DECISION SIMPLE
# ============================================================

# Se utiliza solamente la ubicacion del producto
arbol1 <- rpart(
  VentaAlta ~ ShelveLoc,
  data = Carseats,
  method = "class"
)

# Informacion del arbol
print(arbol1)

# Grafico
fancyRpartPlot(arbol1)


# ============================================================
# 13. ARBOL CON VARIABLES DE MERCADEO
# ============================================================

arbol2 <- rpart(
  VentaAlta ~ ShelveLoc + Advertising + Price,
  data = Carseats,
  method = "class",
  control = rpart.control(
    minsplit = 30,
    cp = 0.02
  )
)

# Resultados
print(arbol2)

# Tabla de complejidad
printcp(arbol2)

# Importancia de las variables
arbol2$variable.importance

# Grafico
fancyRpartPlot(arbol2)


# ============================================================
# 14. PROBABILIDADES ESTIMADAS POR EL ARBOL
# ============================================================

probabilidades <- predict(
  arbol2,
  newdata = Carseats,
  type = "prob"
)

head(probabilidades)


# Incorporar la probabilidad estimada de ventas altas
Carseats$ProbVentaAlta <- probabilidades[, "Si"]

head(
  Carseats[
    ,
    c(
      "Sales",
      "ShelveLoc",
      "Advertising",
      "Price",
      "VentaAlta",
      "ProbVentaAlta"
    )
  ]
)


# ============================================================
# 15. NODOS TERMINALES
# ============================================================

Carseats$Nodo <- predict(
  arbol2,
  newdata = Carseats,
  type = "where"
)

table(Carseats$Nodo)


# Probabilidad observada de ventas altas por nodo
probabilidad_nodo <- aggregate(
  I(VentaAlta == "Si") ~ Nodo,
  data = Carseats,
  FUN = mean
)

names(probabilidad_nodo)[2] <- "P_VentaAlta"

probabilidad_nodo


# ============================================================
# FIN DEL EJERCICIO
# ============================================================

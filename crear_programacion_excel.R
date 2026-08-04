# openxlsx no disponible; se genera CSV UTF-8 y LibreOffice lo convierte a XLSX.

salida <- "Programacion_Teoria_de_Probabilidades_2026-2.xlsx"

temas <- list(
  c("Metodología estadística; datos, variables y escalas de medición", "Formular un problema, objetivos y variables; clasificar datos y escalas"),
  c("Bases de datos y calidad de la información", "Importar, documentar y verificar una base de datos con R"),
  c("Tablas de frecuencia: datos cualitativos y cuantitativos", "Construir e interpretar tablas de frecuencia"),
  c("Gráficos de frecuencias y diagrama de Pareto", "Seleccionar y construir visualizaciones pertinentes"),
  c("Indicadores de centro: media, mediana y moda", "Calcular e interpretar medidas de centro"),
  c("Indicadores de dispersión y forma", "Comparar variabilidad, asimetría y curtosis"),
  c("Posición, caja y bigotes, tallo y hojas", "Identificar posición, atípicos y forma de una distribución"),
  c("Análisis conjunto: frecuencias conjunta, marginal y condicional", "Analizar relaciones entre dos variables categóricas"),
  c("Experimentos, espacio muestral y eventos", "Modelar fenómenos aleatorios con espacios muestrales y eventos"),
  c("Operaciones con eventos y diagramas de Venn", "Representar unión, intersección y complemento"),
  c("Regla fundamental del conteo", "Resolver conteos por etapas"),
  c("Permutaciones y combinaciones", "Elegir y aplicar la técnica de conteo adecuada"),
  c("Definición, enfoques y axiomas de probabilidad", "Distinguir enfoques y aplicar axiomas"),
  c("Probabilidad de eventos simples y compuestos", "Calcular probabilidades con reglas aditiva y del complemento"),
  c("Probabilidad condicional e independencia", "Interpretar y calcular probabilidades condicionadas"),
  c("Probabilidad total, Bayes, árboles y tablas", "Actualizar probabilidades y comunicar el razonamiento"),
  c("Concepto y clasificación de variable aleatoria", "Traducir resultados aleatorios a variables numéricas"),
  c("Variable aleatoria discreta y función de probabilidad", "Construir y validar distribuciones discretas"),
  c("Variable continua: densidad y distribución acumulada", "Calcular e interpretar probabilidades mediante densidad y FDA"),
  c("Valor esperado y varianza", "Caracterizar una variable aleatoria con media y varianza"),
  c("Distribuciones conjuntas discretas", "Obtener distribuciones conjuntas, marginales y condicionales"),
  c("Distribuciones conjuntas continuas", "Integrar densidades conjuntas y obtener marginales"),
  c("Covarianza, correlación e independencia", "Evaluar dependencia entre variables aleatorias"),
  c("Combinaciones lineales; esperanza y varianza condicional", "Calcular momentos de combinaciones y condicionales"),
  c("Uniforme discreta, Bernoulli y binomial", "Seleccionar y aplicar modelos discretos básicos"),
  c("Hipergeométrica", "Modelar muestreo sin reemplazo"),
  c("Poisson y Pascal", "Modelar conteos, ocurrencias y número de ensayos"),
  c("Uniforme continua y normal", "Calcular probabilidades continuas y estandarizar"),
  c("Gamma y exponencial", "Modelar tiempos de espera y usar la propiedad sin memoria"),
  c("Weibull y probabilidad de fallas", "Analizar confiabilidad y riesgo de falla"),
  c("t de Student, chi-cuadrado y F de Snedecor", "Reconocer distribuciones base para inferencia"),
  c("Kernel, selección de modelos e integración del curso", "Aproximar distribuciones, comparar modelos y comunicar resultados")
)

capitulo <- rep(c("1. Estadística descriptiva", "2. Introducción a la probabilidad",
                  "3. Variables aleatorias", "4. Modelos de probabilidad"), each = 8)
semana <- rep(1:16, each = 2)
sesion_semana <- rep(1:2, 16)
dia <- ifelse(sesion_semana == 1, "Lunes", "Miércoles")

antes <- ifelse(sesion_semana == 1,
  "Revisar guía, microvideo/lectura y responder 3 preguntas de preparación (45 min).",
  "Revisar retroalimentación de la sesión 1 y resolver ejercicios diagnósticos (45 min).")
durante <- ifelse(sesion_semana == 1,
  "Prueba de entrada; aclaración breve; entrega y presentación de la guía; ejemplo modelado.",
  "Dudas y preguntas; taller colaborativo basado en problemas; puesta en común y cierre.")
despues <- ifelse(sesion_semana == 1,
  "Completar notas y preparar dudas para monitoría (45 min).",
  "Consolidar solución, autoevaluar y registrar hallazgos en portafolio/proyecto (45 min).")
monitor <- ifelse(sesion_semana == 2,
  "Taller semanal con monitor: práctica guiada y retroalimentación (2 h).", "—")

evidencia <- ifelse(sesion_semana == 1,
  "Guía previa + prueba de entrada + explicación breve",
  "Taller resuelto + ticket de salida/portafolio")

evaluacion <- rep("Formativa", 32)
evaluacion[c(6, 14, 22)] <- c("Quiz 1 (5%)", "Quiz 2 (5%)", "Quiz 3 (5%)")
evaluacion[c(8, 20, 28)] <- c("Laboratorio 1 (5%)", "Laboratorio 2 (5%)", "Laboratorio 3 (5%)")
evaluacion[c(10, 20, 30)] <- c("Examen 1 (20%)", "Examen 2 (20%) + Laboratorio 2 (5%)", "Examen 3 (20%)")
evaluacion[32] <- "Proyecto: entrega y sustentación (10%)"

prog <- data.frame(
  Semana = semana,
  Sesión = 1:32,
  `Sesión semanal` = sesion_semana,
  Día = dia,
  Fecha = "Por definir",
  Capítulo = capitulo,
  Tema = vapply(temas, `[[`, character(1), 1),
  `Resultado de aprendizaje de la sesión` = vapply(temas, `[[`, character(1), 2),
  `ANTES de clase (trabajo autónomo)` = antes,
  `DURANTE la clase (2 h)` = durante,
  `DESPUÉS de clase (trabajo autónomo)` = despues,
  `Monitoría semanal (2 h)` = monitor,
  `Evidencia / producto` = evidencia,
  `Evaluación propuesta` = evaluacion,
  `Horas clase` = 2,
  `Horas autónomas` = 1.5,
  `Horas con monitor` = ifelse(sesion_semana == 2, 2, 0),
  check.names = FALSE
)

resumen <- data.frame(
  Concepto = c("Asignatura", "Código", "Semestre de referencia", "Modalidad", "Créditos",
               "Semanas", "Sesiones", "Horas de clase", "Trabajo autónomo",
               "Trabajo con monitor", "Total de horas", "Estructura de aula invertida",
               "Nota sobre fechas y evaluación"),
  Valor = c("Teoría de Probabilidades", "300MAE013", "2026-2", "Presencial", "3",
            "16", "32 (2 por semana)", "64", "48", "32", "144",
            "Antes: preparación individual; durante: aplicación y retroalimentación; después: consolidación y transferencia.",
            "Las fechas y la ubicación de las evaluaciones son propuestas editables; el syllabus solo fija cantidades y porcentajes."),
  check.names = FALSE
)

eval <- data.frame(
  Tipo = c("Examen", "Quiz", "Laboratorio", "Proyecto", "TOTAL"),
  Cantidad = c(3, 3, 3, 1, 10),
  `Porcentaje por actividad` = c("20%", "5%", "5%", "10%", ""),
  `Porcentaje total` = c("60%", "15%", "15%", "10%", "100%"),
  `Ubicación propuesta` = c("Semanas 5, 10 y 15", "Semanas 3, 7 y 11",
                            "Semanas 4, 10 y 14", "Semana 16", ""),
  check.names = FALSE
)

fuentes <- data.frame(
  Fuente = c("Syllabus oficial", "Sitio web local del curso", "Imagen de metodología del curso", "Módulos web del curso"),
  Ubicación = c("pdf/300MAE013 Teoria de Probabilidades Syllabus.pdf", "index.Rmd / docs/index.html",
                "img/metodologia.png", "modulo1.Rmd, modulo2.Rmd, modulo3.Rmd, modulo4.Rmd"),
  Uso = c("Contenidos, carga horaria, estrategias pedagógicas y evaluación",
          "Semestre y organización general", "Patrón semanal: sesión 1 con guía; sesión 2 para dudas y actividades",
          "Objetivos y detalle temático por módulo"),
  check.names = FALSE
)

write.csv(prog, "Programacion_Teoria_de_Probabilidades_2026-2.csv", row.names = FALSE, fileEncoding = "UTF-8", na = "")
cat(normalizePath("Programacion_Teoria_de_Probabilidades_2026-2.csv"), "\n")

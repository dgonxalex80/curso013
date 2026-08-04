library(readxl)

archivo_tp <- "Programacion_Teoria_de_Probabilidades_2026-2.xlsx"
archivo_etd <- "Programacion_Estadistica_Toma_Decisiones.xlsx"

tp <- read_excel(archivo_tp, sheet = 1)
etd <- read_excel(archivo_etd, sheet = "Programación sesión a sesión")

clasificacion <- c(
  rep("COMÚN — coordinar ejemplos y datos", 3),
  rep("COMÚN CON DESFASE — no asumir la misma cobertura", 3),
  rep("RELACIONADO — ETD entra antes en probabilidad", 4),
  rep("COMÚN CON DESFASE — TP aún construye fundamentos", 6),
  rep("COMÚN CON DESFASE — TP entra después en variables aleatorias", 8),
  "COMÚN — modelos discretos, distinto énfasis", "DIFERENTE — TP profundiza hipergeométrica",
  "COMÚN — Poisson; TP añade Pascal", "COMÚN — modelos continuos, distinto momento",
  "RELACIONADO — ETD ya está en inferencia", "DIFERENTE — confiabilidad vs regresión",
  "RELACIONADO — distribuciones para inferencia vs aplicación inferencial",
  "DIFERENTE — cierre de modelos vs examen inferencial"
)

enfasis_tp <- c(
  "Lenguaje estadístico y clasificación formal de variables.",
  "Calidad de datos como base del análisis probabilístico.",
  "Construcción e interpretación formal de frecuencias.",
  "Representación gráfica, Pareto y elección técnica del gráfico.",
  "Definiciones, propiedades y cálculo de medidas de centro.",
  "Dispersión y forma con mayor soporte matemático.",
  "Posición, caja y bigotes, tallo y hojas.",
  "Frecuencias conjuntas, marginales y condicionales.",
  "Formalización de experimento, espacio muestral y eventos.",
  "Álgebra de eventos y representación mediante Venn.",
  "Principio multiplicativo antes de fórmulas combinatorias.",
  "Diferenciar permutación y combinación según orden/reemplazo.",
  "Enfoques y axiomas como fundamento de las reglas.",
  "Demostración y uso de reglas para eventos compuestos.",
  "Definición formal de condicional e independencia.",
  "Probabilidad total y Bayes con árboles y tablas.",
  "Definición y clasificación matemática de variable aleatoria.",
  "Función de masa y acumulada discreta.",
  "Densidad, acumulada e integración en variables continuas.",
  "Derivación y propiedades de esperanza y varianza.",
  "Distribuciones conjuntas discretas y marginalización.",
  "Distribuciones conjuntas continuas e integración doble.",
  "Covarianza, correlación e independencia probabilística.",
  "Combinaciones lineales y momentos condicionales.",
  "Supuestos y cálculo de uniforme discreta, Bernoulli y binomial.",
  "Muestreo sin reemplazo mediante hipergeométrica.",
  "Modelos Poisson y Pascal y sus condiciones.",
  "Uniforme continua y normal con cálculo probabilístico.",
  "Gamma y exponencial; tiempos de espera.",
  "Weibull, confiabilidad y probabilidad de fallas.",
  "t, chi-cuadrado y F como distribuciones de probabilidad.",
  "Kernel, selección, ajuste y simulación de modelos."
)

enfasis_etd <- c(
  "Plantear el estudio y la decisión; población, muestra y unidad.",
  "Pregunta de negocio, codificación y utilidad de la base.",
  "Comunicar patrones con tablas y gráficos para decidir.",
  "Interpretación contextual de tendencia central.",
  "Integración del análisis univariado y detección de atípicos.",
  "Relaciones bivariadas y lectura aplicada de correlación.",
  "Uso de probabilidad para representar incertidumbre decisional.",
  "Conteo y reglas aplicados a escenarios de negocio.",
  "Condicional, Bayes y actualización de decisiones.",
  "Evaluar integración de descriptiva y probabilidad.",
  "Modelar resultados discretos para decisiones.",
  "Interpretar valor esperado como criterio de decisión.",
  "Dependencia y covarianza en contextos aplicados.",
  "Simulación como apoyo a la decisión.",
  "Reconocer condiciones y aplicar binomial.",
  "Tasas y conteos con Poisson.",
  "Variables continuas desde su aplicación.",
  "Uniforme y exponencial en tiempos y procesos.",
  "Normal, percentiles e interpretación aplicada.",
  "Software para seleccionar y validar modelos.",
  "Comparar modelos en un caso de mercadeo/negocios.",
  "Síntesis, supuestos e interpretación gerencial.",
  "Laboratorio y avance del proyecto.",
  "Evaluar selección e interpretación de modelos.",
  "Sesgo, representatividad y diseño de muestreo.",
  "Distribuciones muestrales, error estándar y TLC.",
  "Intervalos de confianza para decisiones.",
  "Contrastes, errores y valor p.",
  "Pruebas para medias/proporciones y comunicación.",
  "Regresión lineal como modelo explicativo/predictivo.",
  "Validación, prueba F, predicción y proyecto.",
  "Evaluación y cierre de inferencia/regresión."
)

alerta <- c(
  "Usar una misma base, pero en TP clasificar formalmente; en ETD formular la decisión.",
  "No reutilizar la consigna: TP verifica calidad; ETD exige pregunta de negocio.",
  "Puede compartirse el conjunto de datos; cambiar el producto esperado.",
  "ETD ya estudia centro mientras TP sigue en gráficos.",
  "TP estudia centro; ETD ya integra dispersión y atípicos.",
  "ETD ve análisis bivariado antes que TP; evitar usar covarianza probabilística todavía.",
  "ETD inicia probabilidad; TP aún cierra descriptiva.",
  "ETD usa conteo; TP trabaja frecuencias conjuntas. Separar vocabulario evento/frecuencia.",
  "Ambos ven eventos, pero ETD ya incluye Bayes; no adelantar Bayes en TP.",
  "ETD evalúa; TP apenas formaliza operaciones con eventos.",
  "ETD entra en variable aleatoria; TP sigue en conteo.",
  "ETD usa esperanza; TP está en permutaciones y combinaciones.",
  "ETD trabaja conjuntas; TP apenas axiomatiza probabilidad.",
  "ETD simula; TP desarrolla reglas de eventos.",
  "ETD está en binomial; TP estudia condicional e independencia.",
  "ETD aplica Poisson; TP cierra Bayes.",
  "ETD entra en continuas; TP introduce variable aleatoria.",
  "Ambos hablan de distribuciones, pero discreta en TP y continua en ETD.",
  "Ambos usan FDA/densidad; ETD ya aplica normal. Distinguir teoría de modelo.",
  "TP deriva momentos; ETD usa software y modelos. Cambiar profundidad y evidencia.",
  "TP formaliza conjuntas; ETD compara modelos en un caso.",
  "TP usa integración continua; ETD sintetiza selección de modelos.",
  "La palabra correlación aparece en ambos: en TP es propiedad probabilística; en ETD es herramienta aplicada.",
  "TP estudia momentos condicionales; ETD realiza evaluación. No mezclar rúbricas.",
  "Ambos abordan binomial: TP enfatiza derivación/supuestos; ETD inicia muestreo.",
  "Hipergeométrica en TP no equivale a distribución muestral en ETD.",
  "Poisson en TP no equivale a intervalos de confianza en ETD.",
  "Normal en TP es modelo de probabilidad; en ETD sirve de puente a pruebas.",
  "Exponencial/gamma en TP; pruebas en ETD: mantener ejemplos y notación separados.",
  "Weibull y fallas en TP; regresión en ETD: cursos ya divergen claramente.",
  "En TP t/chi²/F son distribuciones; en ETD F aparece como prueba del modelo.",
  "Cierres distintos: modelación probabilística en TP; inferencia y regresión en ETD."
)

plan <- data.frame(
  Semana = tp$Semana,
  `Sesión semanal` = tp$`Sesión semanal`,
  `Sesión TP` = tp$Sesión,
  `Teoría de Probabilidades — tema` = tp$Tema,
  `Énfasis exclusivo TP` = enfasis_tp,
  `Evaluación TP` = tp$`Evaluación propuesta`,
  `Sesión ETD` = etd$Sesión,
  `Estadística para la Toma de Decisiones — tema` = etd$`Tema central`,
  `Énfasis exclusivo ETD` = enfasis_etd,
  `Evidencia ETD` = etd$`Evidencia / evaluación`,
  `Relación entre cursos` = clasificacion,
  `Alerta para no confundir` = alerta,
  `Acción docente sugerida` = ifelse(grepl("COMÚN", clasificacion),
    "Reutilizar contexto o base de datos, pero cambiar objetivo, profundidad, notación y producto.",
    ifelse(grepl("RELACIONADO", clasificacion),
      "Explicitar el puente conceptual al iniciar; verificar prerrequisitos sin adelantar el otro curso.",
      "Preparar ejemplos, archivos y rúbricas independientes; anunciar explícitamente el cambio de curso.")),
  check.names = FALSE
)

leyenda <- data.frame(
  Categoría = c("COMÚN", "COMÚN CON DESFASE", "RELACIONADO", "DIFERENTE"),
  Interpretación = c(
    "Mismo núcleo conceptual; se puede compartir contexto, nunca el objetivo ni la evidencia sin adaptación.",
    "Tema parecido en momentos distintos; el principal riesgo es asumir conocimientos aún no vistos.",
    "Existe un puente conceptual, pero cambia el nivel, el propósito o la herramienta.",
    "Los cursos divergen; conviene separar materiales, ejemplos y rúbricas."
  ),
  Acción = c(
    "Marcar cada recurso con TP o ETD y declarar el énfasis al inicio.",
    "Consultar esta fila antes de clase y hacer un diagnóstico de 3 minutos.",
    "Enunciar qué se retoma y qué no se trabajará en esa sesión.",
    "Usar carpetas, colores y casos distintos."
  ), check.names = FALSE
)

write.csv(plan, "Programacion_Paralela_TP_ETD_2026-2.csv", row.names = FALSE,
          fileEncoding = "UTF-8", na = "")
write.csv(leyenda, "Leyenda_Programacion_Paralela.csv", row.names = FALSE,
          fileEncoding = "UTF-8", na = "")
cat("Programación paralela creada:", nrow(plan), "sesiones comparadas.\n")

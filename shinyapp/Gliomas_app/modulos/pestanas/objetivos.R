# =========================================================================== #
# Pestaña 4 · Objetivos
# Objetivo general, objetivos específicos y criterios de cumplimiento, que se
# comprueban contra los datos y los resultados del modelado cada vez.
# =========================================================================== #

OBJETIVOS_ESPECIFICOS <- list(
  c("1", "Caracterizar la cohorte",
    paste("Describir el perfil clínico (edad, género, grupo racial) y mutacional de los",
          "839 pacientes, y verificar la calidad del dataset: duplicados, valores",
          "faltantes, valores atípicos y supuestos distribucionales.")),
  c("2", "Reservar un conjunto de prueba antes de mirar nada",
    paste("Separar el 20 % de los pacientes con una partición estratificada antes de",
          "calcular el primer estadístico, para que ninguna decisión tomada durante el",
          "análisis esté contaminada por los datos con los que se evaluará después.")),
  c("3", "Medir la asociación de cada variable con el grado",
    paste("Contrastar cada predictora contra el grado tumoral con la prueba adecuada a",
          "su naturaleza (Mann-Whitney para la edad, chi-cuadrado y V de Cramér para las",
          "binarias y categóricas) y ordenar las variables por capacidad discriminativa.")),
  c("4", "Descartar redundancia entre marcadores",
    paste("Evaluar la co-ocurrencia entre mutaciones y el factor de inflación de la",
          "varianza para confirmar que el conjunto retenido puede entrar completo en un",
          "modelo sin inestabilidad en la estimación de los parámetros.")),
  c("5", "Entrenar, comparar y reducir el panel",
    paste("Ajustar regresión logística, random forest y XGBoost con validación cruzada",
          "anidada, evaluarlos en el conjunto de prueba y medir cuánta capacidad",
          "discriminativa se conserva con un panel de pocas variables.")),
  c("6", "Poner el análisis a disposición de otros",
    paste("Publicar el desarrollo completo como libro en R (bookdown) y una capa",
          "interactiva —esta aplicación de Shiny— que permita explorar el análisis y",
          "probar los modelos sin tocar el código."))
)

objetivos_tabla_criterios <- function() {
  df <- get_df("full")
  asociacion <- asociacion_con_grado("train")
  m <- matriz_asociacion_genes("train", top_n = 12)
  diag(m) <- 0
  significativas <- sum(asociacion$significativa)
  nulos <- sum(is.na(df[, c(TARGET, FEATURES)]))
  rho_maximo <- max(abs(asociacion$rho), na.rm = TRUE)
  prop_train <- proporcion_grado("train")
  prop_test <- proporcion_grado("test")
  gbm_train <- prop_train$porcentaje[prop_train$grado == "GBM"]
  gbm_test <- prop_test$porcentaje[prop_test$grado == "GBM"]
  brecha <- abs(gbm_train - gbm_test)
  v_maxima <- max(m)
  mejor <- mejor_modelo_test()

  filas <- list(
    c("Dataset sin valores faltantes",
      paste0(nulos, " nulos en ", nrow(df), " × ", length(c(TARGET, FEATURES)), " celdas"),
      if (nulos == 0) "Cumple" else "Revisar"),
    c("La estratificación conserva la proporción de clases",
      paste0("GBM: ", pct(gbm_train, 1), " en entrenamiento y ", pct(gbm_test, 1),
             " en prueba (", num(brecha, 1), " puntos de diferencia)"),
      if (brecha < 2) "Cumple" else "Revisar"),
    c("Al menos un marcador con asociación fuerte ($|r| \\geq 0{,}50$)",
      paste0("El máximo observado es $|r| = ", tnum(rho_maximo, 2), "$"),
      if (rho_maximo >= 0.50) "Cumple" else "No cumple"),
    c("Sin multicolinealidad estructural (V de Cramér < 0,70)",
      paste0("El par de genes más asociado llega a V = ", num(v_maxima, 2)),
      if (v_maxima < 0.70) "Cumple" else "Revisar"),
    c("El panel se puede reducir: no todas las variables aportan",
      paste(significativas, "de", nrow(asociacion), "variables muestran señal"),
      if (significativas < nrow(asociacion)) "Cumple" else "No cumple"),
    c("Un clasificador generaliza a pacientes nuevos (AUC de prueba $\\geq 0{,}85$)",
      paste0("El mejor en prueba es ", NOMBRES_MODELO[[mejor$clave]], " con AUC = ",
             num(mejor$auc, 3)),
      if (mejor$auc >= 0.85) "Cumple" else "No cumple")
  )
  data_table(c("Criterio", "Resultado observado", "Estado"), filas)
}

ui_objetivos <- function() {
  div(
    class = "vista-pestana",
    page_header(
      "Objetivos del proyecto",
      paste("Qué se propone construir, con qué pasos y con qué criterio se considera",
            "suficiente el resultado."),
      "track_changes"
    ),
    card(
      paragraph(paste(
        "Construir una solución analítica integral que identifique el subconjunto",
        "óptimo de factores clínicos y mutaciones genéticas para clasificar con",
        "precisión la severidad del glioma (LGG frente a GBM), reduciendo los costos",
        "asociados a pruebas moleculares innecesarias.")),
      paragraph(paste(
        "El énfasis está en la palabra óptimo: no se busca la mayor cantidad de",
        "información posible, sino la mínima que sostenga la decisión clínica. La",
        "primera fase responde cuáles de las 23 variables disponibles se relacionan de",
        "verdad con el grado del tumor, y con qué fuerza.")),
      paragraph(paste(
        "La segunda fase traduce esos hallazgos en tres clasificadores entrenados y",
        "evaluados con pacientes que no vieron al ajustarse. Sus resultados están en las",
        "pestañas Modelos y Predicción, donde además se puede describir un paciente y",
        "ver qué predice cada modelo y por qué.")),
      titulo = "Objetivo general",
      tono = "acento"
    ),
    section_title("Objetivos específicos"),
    fila(
      class = "g-3",
      lapply(OBJETIVOS_ESPECIFICOS, function(o) {
        columna(
          lg = 4, md = 6, xs = 12, class = "mb-3",
          card(
            div(class = "guide-badge", badge_numero(o[1])),
            div(class = "guide-name", o[2]),
            div(class = "guide-text", o[3]),
            class = "guide-card"
          )
        )
      })
    ),
    section_title("Criterios de cumplimiento"),
    card(
      paragraph(paste(
        "Cada criterio se comprueba contra los datos y los resultados del modelado cada",
        "vez que se abre esta pestaña, no contra una cifra escrita a mano. Si mañana se",
        "reemplaza el dataset o se reentrenan los modelos, la tabla se recalcula y delata",
        "cualquier supuesto que haya dejado de cumplirse.")),
      objetivos_tabla_criterios(),
      titulo = "Estado frente a los criterios definidos",
      subtitulo = "Calculado en vivo sobre el dataset y los modelos del proyecto"
    )
  )
}

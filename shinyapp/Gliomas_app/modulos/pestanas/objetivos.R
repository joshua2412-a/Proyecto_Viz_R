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
  # Antes eran dos objetivos (redundancia y panel). Se unieron para dejar sitio
  # al de modelado sin romper la cuadrícula de 3 + 3 tarjetas (como en Dash).
  c("4", "Delimitar un panel reducido y sin redundancia",
    paste("Separar las variables que aportan señal sobre el grado de las que no y",
          "evaluar la co-ocurrencia entre mutaciones, para sostener con evidencia que",
          "un panel más pequeño basta y que puede entrar en un modelo sin",
          "multicolinealidad.")),
  c("5", "Entrenar y comparar modelos de clasificación",
    paste("Ajustar regresión logística, Random Forest y XGBoost con validación cruzada",
          "anidada, comparar técnicas de balanceo y de optimización de",
          "hiperparámetros, evaluar discriminación y calibración en el conjunto de",
          "prueba y medir cuánto se pierde con solo las variables que más aportan.")),
  c("6", "Poner el análisis a disposición de otros",
    paste("Publicar el desarrollo completo en un libro en R (bookdown) y acompañarlo",
          "de una aplicación interactiva que permita explorar los datos, comparar los",
          "modelos y obtener predicciones sin necesidad de ejecutar código."))
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
  # Con las cifras ya redondeadas: restando antes de redondear salía
  # «42,0 % y 41,7 % (0,4 puntos)», que el lector toma por una cuenta mal hecha.
  gbm_train <- round(gbm_train, 1)
  gbm_test <- round(gbm_test, 1)
  brecha <- abs(gbm_train - gbm_test)
  v_maxima <- max(m)
  # Criterios del modelado, con las cifras que exporta el libro de R
  auc_prueba <- vapply(ORDEN_MODELOS, function(m) RESUMEN$modelos[[m]]$auc_test, numeric(1))
  pr <- RESUMEN$panel_reducido
  auc_panel <- vec(RESUMEN$panel$auc)
  auc_4 <- auc_panel[vec(RESUMEN$panel$k) == 4]

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
    c("Los tres modelos discriminan bien en prueba (AUC ≥ 0,90)",
      paste0("AUC de ", num(min(auc_prueba), 3), " a ", num(max(auc_prueba), 3), " en los ",
             RESUMEN$meta$n_test, " pacientes de prueba"),
      if (min(auc_prueba) >= 0.90) "Cumple" else "No cumple"),
    c("Un panel reducido no se distingue del completo (regla de un error estándar)",
      paste0(pr$k_min, " de 23 variables: AUC ", num(pr$auc_k, 3), " frente a ",
             num(pr$auc_23, 3), " en validación cruzada",
             if (length(auc_4) && pr$k_min != 4) paste0(". Con 4, ", num(auc_4, 3)) else ""),
      if (pr$k_min < 23) "Cumple" else "No cumple")
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
        "información posible, sino la mínima que sostenga la decisión clínica.")),
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
        "Cada criterio se evalúa directamente sobre los datos: las cifras de la tabla",
        "salen del mismo cálculo que el resto de la aplicación, de modo que el estado",
        "refleja lo que los datos muestran y no una afirmación previa.")),
      objetivos_tabla_criterios(),
      titulo = "Estado frente a los criterios definidos",
      subtitulo = "Calculado sobre el dataset del proyecto, no escrito a mano"
    )
  )
}

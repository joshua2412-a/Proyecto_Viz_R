# =========================================================================== #
# Pestaña 9 · Modelos
# El capítulo de modelado en versión navegable: cómo se entrenaron los tres
# clasificadores, cómo rinden con pacientes nuevos, qué variables usan y si sus
# diferencias son reales. Todas las cifras salen de data/resumen_modelado.json,
# el mismo archivo que lee el libro: los dos no pueden contar números distintos.
# =========================================================================== #

radio_modelo <- function(id, valor = "logistica") {
  div(class = "ambito-radio",
      radioButtons(id, label = NULL, inline = TRUE, selected = valor,
                   choiceNames = unname(NOMBRES_MODELO[ORDEN_MODELOS]), choiceValues = ORDEN_MODELOS))
}

auc_panel <- function(k) {
  panel <- RESUMEN$panel
  vec(panel$auc)[vec(panel$k) == k]
}

modelos_kpis <- function() {
  meta <- RESUMEN$meta
  aucs <- vapply(ORDEN_MODELOS, function(m) RESUMEN$modelos[[m]]$auc_test, numeric(1))
  mc <- lapply(ORDEN_MODELOS, matriz_confusion)
  vp <- vapply(mc, `[[`, numeric(1), "vp")
  recall <- vapply(mc, `[[`, numeric(1), "recall")
  top4 <- vapply(utils::head(RESUMEN$importancia, 4), function(f) {
    if (f$variable == "Age_at_diagnosis") "edad" else f$variable
  }, character(1))
  kpi_row(list(
    list(valor = as.character(meta$combinaciones), etiqueta = "Combinaciones evaluadas",
         detalle = paste0("3 modelos × 4 balanceos × 4 optimizadores · ", miles(meta$configuraciones),
                          " configuraciones probadas")),
    list(valor = paste0(num(min(aucs), 2), "–", num(max(aucs), 2)), etiqueta = "AUC en prueba",
         detalle = paste0("Los tres modelos, en ", meta$n_test, " pacientes que no se usaron para entrenar")),
    list(valor = paste0(min(vp), "–", max(vp), " de ", meta$gbm_test), etiqueta = "Glioblastomas detectados",
         detalle = paste0("Con el umbral de 0,5: recall de GBM entre el ", pct(min(recall) * 100, 0),
                          " y el ", pct(max(recall) * 100, 0)),
         color = COLOR_GBM),
    list(valor = as.character(K_PANEL), etiqueta = "Variables que bastan",
         detalle = paste0(lista_y(nombres_panel()), ": AUC ", num(auc_panel(K_PANEL), 3), " frente a ",
                          num(auc_panel(23), 3), " con las 23"))
  ))
}

# --------------------------------------------------------------------------- #
# Subpestaña 1 · Diseño y resultados de la validación anidada
# --------------------------------------------------------------------------- #
etapa <- function(numero, titulo, items, nota) {
  columna(lg = 4, class = "mb-3",
          card(tags$ul(class = "etapa-lista", lapply(items, tags$li)),
               div(class = "etapa-nota", nota),
               titulo = paste(numero, "·", titulo), class = "etapa-card h-100"))
}

lectura_combinaciones <- function() {
  combos <- RESUMEN$combinaciones
  auc <- vapply(combos, function(c) c$auc_media, numeric(1))
  de <- vapply(combos, function(c) c$auc_de, numeric(1))
  mejor <- combos[[which.max(auc)]]
  logisticas <- Filter(function(c) c$modelo == "logistica", combos)
  mejor_log <- logisticas[[which.max(vapply(logisticas, function(c) c$auc_media, numeric(1)))]]
  c(
    paste0("Las ", length(combos), " combinaciones caben en una franja de ", num(max(auc) - min(auc), 3),
           " de AUC (de ", num(min(auc), 3), " a ", num(max(auc), 3), "), mientras que cada una ",
           "varía unos ", num(mean(de), 2), " de un pliegue a otro."),
    paste0("La mejor es ", NOMBRES_MODELO[[mejor$modelo]], " ", NOMBRE_BALANCEO[[mejor$balanceo]],
           " con ", NOMBRE_OPTIMIZADOR[[mejor$metodo]], " (", num(mejor$auc_media, 3), "); la mejor ",
           "regresión logística (", NOMBRE_BALANCEO[[mejor_log$balanceo]], ", ",
           NOMBRE_OPTIMIZADOR[[mejor_log$metodo]], ") queda a ", num(mejor$auc_media - mejor_log$auc_media, 3),
           " con ", num(mejor_log$t_total_s, 0), " segundos de cómputo."),
    "El balanceo casi no mueve la AUC; cambia el recall: más GBM detectados a cambio de algo de precisión.",
    texto_peor_optimizador(combos)
  )
}

# Qué optimizador queda más veces último en su fila (modelo · balanceo) del mapa
texto_peor_optimizador <- function(combos) {
  filas <- split(combos, vapply(combos, function(c) paste(c$modelo, c$balanceo), character(1)))
  peores <- vapply(filas, function(f) f[[which.min(vapply(f, function(c) c$auc_media, numeric(1)))]]$metodo,
                   character(1))
  cuenta <- sort(table(peores), decreasing = TRUE)
  paste0(NOMBRE_OPTIMIZADOR[[names(cuenta)[1]]], " es el optimizador que más veces queda último en su ",
         "fila (", cuenta[[1]], " de ", length(filas), "), aunque por diferencias de milésimas: con 24 ",
         "evaluaciones los cuatro llegan casi al mismo sitio.")
}

lectura_optimizadores <- function() {
  o <- RESUMEN$optimizadores
  area <- vapply(names(o), function(k) o[[k]]$area, numeric(1))
  ext <- vapply(names(o), function(k) o[[k]]$auc_externa, numeric(1))
  opt <- vapply(names(o), function(k) o[[k]]$optimismo, numeric(1))
  seg <- vapply(names(o), function(k) o[[k]]$s_por_eval, numeric(1))
  rapido <- names(which.max(area))
  c(
    paste0(NOMBRE_OPTIMIZADOR[[rapido]], " es el que antes llega a buenas soluciones: tras 6 ",
           "evaluaciones tiene una AUC interna de ", num(o[[rapido]]$auc_t6, 4), ", frente a ",
           num(o$random$auc_t6, 4), " de Random Search."),
    paste0("Con las 24 evaluaciones las AUC externas apenas difieren (", num(min(ext), 4), "–",
           num(max(ext), 4), "). El más optimista, con más distancia entre la AUC interna y la ",
           "externa, es ", NOMBRE_OPTIMIZADOR[[names(which.max(opt))]], "."),
    paste0("El bayesiano ajusta un proceso gaussiano antes de cada evaluación: ", num(seg[["bayesiano"]], 2),
           " s por evaluación, frente a ", num(seg[["random"]], 2), " s de Random Search. Con modelos ",
           "tan baratos, ese costo pesa más que el ajuste de los propios modelos.")
  )
}

panel_diseno <- function() {
  meta <- RESUMEN$meta
  prop <- proporcion_grado("train")
  tagList(
    section_title("El experimento: un producto, no una suma"),
    paragraph(paste(
      "No se entrenó un modelo sino todas las combinaciones de tres decisiones. Cada una de",
      "las 48 se optimizó y se evaluó por separado, con el preprocesamiento y el balanceo",
      "siempre dentro del pipeline: ningún paciente de validación influye en el escalado de",
      "la edad ni sirve de base para crear pacientes sintéticos.")),
    fila(
      class = "g-3",
      etapa("1", "Modelo", c("Regresión logística L1/L2 (glmnet)", "Random Forest (ranger)", "XGBoost (xgboost)"),
            "Un modelo lineal y dos de árboles con supuestos opuestos."),
      etapa("2", "Balanceo", c("Sin balanceo", "SMOTE-NC (themis)", "ADASYN (themis)", "Pesos de clase"),
            paste0("Desbalance leve: ", pct(prop$porcentaje[1], 0), " LGG y ",
                   pct(prop$porcentaje[2], 0), " GBM.")),
      etapa("3", "Optimizador", c("Grid Search", "Random Search", "Bayesiano (ParBayesianOptimization)", "Genético (GA)"),
            paste(meta$presupuesto, "evaluaciones cada uno: comparación a presupuesto fijo."))
    ),
    fila(
      class = "mt-2",
      columna(
        lg = 5, class = "mb-4",
        card(
          stat_list(list(
            c("Bucle externo (solo mide)", paste(meta$k_externo, "pliegues estratificados")),
            c("Bucle interno (busca)", paste(meta$k_interno, "pliegues dentro de cada uno")),
            c("Configuraciones probadas", miles(meta$configuraciones)),
            c("Ajustes internos", miles(meta$ajustes_internos)),
            c("Métrica que guía la búsqueda", "AUC"),
            c("Semilla global", as.character(meta$semilla)),
            c("Cómputo total", paste(num(meta$minutos, 0), "min con 2 procesos"))
          )),
          titulo = "Validación cruzada anidada",
          subtitulo = paste("Elegir hiperparámetros y medir con los mismos datos sobreestima el",
                            "desempeño; por eso se separan."),
          tono = "acento"
        )
      ),
      columna(
        lg = 7, class = "mb-4",
        lectura_guiada(c(
          paste("La AUC se eligió como criterio porque no depende del umbral ni se distorsiona",
                "con el desbalance: mide si el modelo ordena bien a los pacientes por riesgo de GBM."),
          paste("El recall de GBM es la métrica clínica: un glioblastoma clasificado como LGG",
                "retrasa un tratamiento urgente, mientras que el error contrario lleva a pruebas",
                "adicionales."),
          paste("Los", meta$n_test, "pacientes de prueba que reservó el EDA solo se usan al final,",
                "una vez elegidos los tres modelos.")
        ), titulo = "Por qué así")
      )
    ),
    section_title(paste("Las", meta$combinaciones, "combinaciones")),
    card(
      div(class = "control-label", "Métrica del mapa"),
      div(class = "ambito-radio",
          radioButtons("mod_metrica", label = NULL, inline = TRUE, selected = "auc",
                       choiceNames = vapply(METRICAS, `[[`, character(1), "etiqueta", USE.NAMES = FALSE),
                       choiceValues = names(METRICAS))),
      class = "controles-card mb-3"
    ),
    fila(
      columna(lg = 8, class = "mb-4",
              graph_card("fig_mod_mapa", "Media del bucle externo (10 pliegues)",
                         paste("Pasa el cursor por una celda para ver la desviación entre pliegues,",
                               "el recall y el tiempo de cómputo."))),
      columna(lg = 4, class = "mb-4", lectura_guiada(lectura_combinaciones()))
    )
  )
}

# --------------------------------------------------------------------------- #
# Subpestaña 2 · Evaluación en el conjunto de prueba
# --------------------------------------------------------------------------- #
tabla_prueba <- function() {
  filas <- lapply(ORDEN_MODELOS, function(m) {
    mc <- matriz_confusion(m)
    d <- RESUMEN$modelos[[m]]
    ic <- vec(d$auc_test_ic)
    c(NOMBRES_MODELO[[m]], paste(NOMBRE_BALANCEO[[d$balanceo]], "·", NOMBRE_OPTIMIZADOR[[d$metodo]]),
      paste0(num(d$auc_test, 3), " [", num(ic[1], 2), "; ", num(ic[2], 2), "]"),
      pct(mc$recall * 100, 1), pct(mc$especificidad * 100, 1), pct(mc$precision * 100, 1),
      num(d$brier_test, 3))
  })
  data_table(c("Modelo", "Balanceo · optimizador", "AUC [IC 95 %]", "Recall GBM", "Especificidad",
               "Precisión", "Brier"), filas)
}

stats_matriz <- function(modelo, umbral) {
  mc <- matriz_confusion(modelo, umbral)
  stat_list(list(
    c("GBM detectados (recall)", paste0(mc$vp, " de ", mc$vp + mc$fn, " · ", pct(mc$recall * 100, 1))),
    c("LGG bien clasificados (especificidad)",
      paste0(mc$vn, " de ", mc$vn + mc$fp, " · ", pct(mc$especificidad * 100, 1))),
    c("Precisión de la alerta GBM", pct(mc$precision * 100, 1)),
    c("Exactitud global", pct(mc$accuracy * 100, 1))
  ))
}

lectura_prueba <- function() {
  aucs <- vapply(ORDEN_MODELOS, function(m) RESUMEN$modelos[[m]]$auc_test, numeric(1))
  semiamplitud <- mean(vapply(ORDEN_MODELOS, function(m) diff(vec(RESUMEN$modelos[[m]]$auc_test_ic)) / 2,
                              numeric(1)))
  cv <- vapply(ORDEN_MODELOS, function(m) RESUMEN$modelos[[m]]$auc_cv_media, numeric(1))
  mc <- lapply(ORDEN_MODELOS, matriz_confusion)
  fp <- sum(vapply(mc, `[[`, numeric(1), "fp"))
  fn <- sum(vapply(mc, `[[`, numeric(1), "fn"))
  # Falsos positivos que cuesta llegar a un recall del 95 %, según cada ROC
  fpr_95 <- vapply(ORDEN_MODELOS, function(m) {
    roc <- RESUMEN$modelos[[m]]$roc
    min(vec(roc$fpr)[vec(roc$tpr) >= 0.95])
  }, numeric(1))
  c(
    paste0("Las tres curvas se superponen: AUC de ", lista_y(num_v(sort(aucs), 3)), ", con intervalos ",
           "de confianza de unos ±", num(semiamplitud, 2), " que se solapan casi por completo."),
    paste0("La AUC en prueba coincide con la estimación de la validación anidada (",
           num(min(cv), 3), "–", num(max(cv), 3), "): el procedimiento no sobreestimó el desempeño."),
    paste0("Con el umbral de 0,5 hay ", if (fp >= 2 * fn) "más del doble de" else "más",
           " falsos positivos que falsos negativos (", fp, " frente a ", fn, " sumando los tres ",
           "modelos), que es el desequilibrio deseable aquí."),
    paste0("Llevar el recall al 95 % exige aceptar entre un ", pct(min(fpr_95) * 100, 0), " y un ",
           pct(max(fpr_95) * 100, 0), " de falsos positivos: pruébalo con el deslizador de abajo.")
  )
}

texto_calibracion <- function() {
  recal <- RESUMEN$recalibracion
  ece <- function(modelo, metodo) {
    for (r in recal) if (r$Modelo == modelo && r$`Método` == metodo) return(r$ece)
    NA_real_
  }
  peor <- ORDEN_MODELOS[which.max(vapply(ORDEN_MODELOS, function(m) RESUMEN$modelos[[m]]$ece_oof, numeric(1)))]
  nombre_libro <- c(logistica = "Regresión Logística", random_forest = "Random Forest", xgboost = "XGBoost")[[peor]]
  antes <- ece(nombre_libro, "Sin recalibrar")
  metodo <- if (isTRUE(ece(nombre_libro, "Platt") < ece(nombre_libro, "Isotónica"))) "Platt" else "Isotónica"
  despues <- ece(nombre_libro, metodo)
  paste0(
    "Una AUC alta solo garantiza que el modelo ordena bien. El peor calibrado fuera de pliegue es ",
    NOMBRES_MODELO[[peor]], " (ECE ", num(RESUMEN$modelos[[peor]]$ece_oof, 3), "), que en los ",
    "tramos intermedios se aparta de la frecuencia observada.",
    if (!is.na(antes) && !is.na(despues)) {
      paste0(" Recalibrado con ", tolower(metodo), ", su error de calibración en prueba pasa de ",
             num(antes, 3), " a ", num(despues, 3), " sin cambiar casi la AUC.")
    } else ""
  )
}

panel_prueba <- function() {
  meta <- RESUMEN$meta
  tagList(
    section_title(paste("Los tres modelos finales en", meta$n_test, "pacientes nuevos")),
    paragraph(paste(
      "Para cada familia se eligió la combinación con mejor AUC en el bucle externo, se volvió",
      "a optimizar con los", meta$n_train, "pacientes de entrenamiento y se aplicó una sola vez",
      "al conjunto de prueba.")),
    card(tabla_prueba(), class = "mb-4"),
    fila(
      columna(lg = 6, class = "mb-4",
              graph_card("fig_mod_roc", "Curvas ROC en prueba", "Cada punto de la curva es un umbral distinto.")),
      columna(lg = 6, class = "mb-4", lectura_guiada(lectura_prueba()))
    ),
    section_title("Elige el umbral de decisión"),
    card(
      fila(
        class = "align-items-end g-3",
        columna(lg = 6, class = "mb-3 mb-lg-0", div(class = "control-label", "Modelo"),
                radio_modelo("mod_modelo")),
        columna(lg = 6, div(class = "control-label", "Umbral de probabilidad para decir GBM"),
                sliderInput("mod_umbral", label = NULL, min = 0.05, max = 0.95, step = 0.05,
                            value = 0.5, ticks = TRUE, width = "100%"))
      ),
      class = "controles-card mb-3"
    ),
    fila(
      columna(lg = 6, class = "mb-4",
              graph_card("fig_mod_matriz", "Matriz de confusión en prueba", "Cada fila suma su propio 100 %.")),
      columna(lg = 6, class = "mb-4",
              card(uiOutput("mod_matriz_stats"), titulo = "Con este umbral", tono = "acento",
                   subtitulo = paste("Bajar el umbral detecta más GBM y genera más falsas alarmas;",
                                     "subirlo hace lo contrario.")))
    ),
    section_title("¿Se puede creer la probabilidad?"),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_mod_calibracion", "Diagrama de confiabilidad",
                         paste("El modelo es el elegido arriba. El tamaño del punto es el número de",
                               "pacientes del tramo."))),
      columna(
        lg = 5, class = "mb-4",
        card(
          stat_list(lapply(ORDEN_MODELOS, function(m) {
            d <- RESUMEN$modelos[[m]]
            c(NOMBRES_MODELO[[m]], paste0("ECE ", num(d$ece_oof, 3), " · Brier ", num(d$brier_test, 3)))
          })),
          callout(texto_calibracion(), titulo = "Calibración"),
          titulo = "Error de calibración esperado (ECE)",
          subtitulo = paste0("Fuera de pliegue, ", meta$n_train, " pacientes. Más bajo es mejor.")
        )
      )
    )
  )
}

# --------------------------------------------------------------------------- #
# Subpestaña 3 · Interpretación
# --------------------------------------------------------------------------- #
lectura_importancia <- function() {
  imp <- RESUMEN$importancia
  idh1 <- imp[[1]]
  top4 <- utils::head(imp, 4)
  suma4 <- min(sum(vapply(top4, function(f) as.numeric(f$xgboost), numeric(1))),
               sum(vapply(top4, function(f) as.numeric(f$logistica), numeric(1))))
  coefs <- MODELOS$modelos$logistica$coeficientes
  anuladas <- MODELOS$predictoras[coefs == 0]
  c(
    paste0(idh1$variable, " explica por sí solo entre el ", pct(min(idh1$xgboost, idh1$logistica), 0),
           " y el ", pct(max(idh1$xgboost, idh1$logistica), 0), " de la importancia en los dos modelos; ",
           "con ", lista_y(vapply(top4[-1], function(f) nombre_variable_modelo(f$variable), character(1))),
           " se supera el ", pct(floor(suma4 / 10) * 10, 0), "."),
    paste("TP53 sube a los primeros puestos aunque su asociación individual con el grado era",
          "moderada: su efecto aparece una vez conocido IDH1."),
    paste0("ATRX casi desaparece: su información ya está en TP53 e IDH1, con los que coocurre.",
           if (length(anuladas)) paste0(" La regularización L1 de la logística deja en cero ",
                                        length(anuladas), " variables (", lista_y(anuladas), ").") else ""),
    paste("EGFR cambia de signo: asociada a GBM por sí sola, empuja levemente hacia LGG una vez",
          "descontadas la edad y la ausencia de IDH1.")
  )
}

panel_interpretacion <- function() {
  top4 <- vapply(utils::head(RESUMEN$importancia, 4), function(f) {
    if (f$variable == "Age_at_diagnosis") "edad" else f$variable
  }, character(1))
  tagList(
    section_title("Qué variables usan los modelos"),
    paragraph(paste(
      "La importancia se mide con valores SHAP: cuánto aporta cada variable, en promedio y en",
      "valor absoluto, a las predicciones de los 839 pacientes. Se compara un modelo lineal con",
      "uno de árboles porque, si dos modelos construidos con supuestos opuestos coinciden, el",
      "hallazgo no es un artefacto de uno de ellos.")),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_mod_importancia", "Importancia global (SHAP)",
                         paste("Porcentaje del total de |SHAP| medio. Las 10 variables más",
                               "importantes según XGBoost."))),
      columna(lg = 5, class = "mb-4", lectura_guiada(lectura_importancia()))
    ),
    section_title("¿Cuántas variables hacen falta?"),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_mod_panel", "AUC según el tamaño del panel",
                         paste("El ranking de variables se recalcula dentro de cada pliegue externo,",
                               "sin ver sus datos de validación."))),
      columna(
        lg = 5, class = "mb-4",
        card(
          paragraph(paste0(
            "Es la respuesta al objetivo general del proyecto. Con ", panel_en_palabras(),
            " el modelo conserva prácticamente toda la capacidad del panel de 20 genes.")),
          stat_list(c(
            lapply(sort(unique(c(1, 2, K_PANEL))), function(k) {
              c(if (k == 1) paste("Solo", nombres_panel()[1]) else paste(nombres_panel()[seq_len(k)], collapse = " + "),
                paste("AUC", num(auc_panel(k), 3)))
            }),
            list(c("Las 23 variables", paste("AUC", num(auc_panel(23), 3))))
          )),
          callout(
            paste("El ranking se recalcula en cada uno de los 10 pliegues externos sin ver sus",
                  "datos de validación, y el panel sale de las variables que más veces lo",
                  "encabezan. La mutación IDH1 R132H puede detectarse además por",
                  "inmunohistoquímica, sin secuenciar."),
            titulo = "Un panel estable"
          ),
          titulo = "Panel reducido", tono = "acento"
        )
      )
    ),
    callout(
      tagList(
        "En la pestaña Predicción puedes ver estas contribuciones paciente a paciente: cambia una ",
        "mutación y mira cómo se mueve la probabilidad. ",
        enlace_pestana(destino = "tab-prediccion", class = "btn btn-link p-0 align-baseline link-externo",
                       "Ir a Predicción →")
      ),
      titulo = "De la importancia global a la explicación local"
    )
  )
}

# --------------------------------------------------------------------------- #
# Subpestaña 4 · Comparación estadística y optimizadores
# --------------------------------------------------------------------------- #
panel_comparacion <- function() {
  fr <- RESUMEN$friedman
  fac <- RESUMEN$factores
  nombres_factor <- c(modelo = "Modelo", balanceo = "Balanceo", metodo = "Optimizador")
  capitalizar <- function(x) paste0(toupper(substr(x, 1, 1)), substring(x, 2))
  nombre_nivel <- c(NOMBRES_MODELO, stats::setNames(capitalizar(NOMBRE_BALANCEO), names(NOMBRE_BALANCEO)),
                    NOMBRE_OPTIMIZADOR)
  filas_factor <- lapply(names(fac), function(f) {
    d <- fac[[f]]
    rangos <- vapply(names(d$rangos), function(k) paste(nombre_nivel[[k]], num(d$rangos[[k]], 2)), character(1))
    c(nombres_factor[[f]], as.character(d$bloques), paste0("$\\chi^2 = ", tnum(d$chi2, 1), "$"), p_valor(d$p),
      paste(rangos, collapse = ", "))
  })
  significativos <- names(fac)[vapply(fac, function(d) d$p < ALFA, logical(1))]
  filas_delong <- lapply(RESUMEN$delong, function(d) {
    ic <- vec(d$ic)
    c(paste(NOMBRES_MODELO[[d$a]], "vs", NOMBRES_MODELO[[d$b]]), num(d$diferencia, 3, signo = TRUE),
      paste0("[", num(ic[1], 3), "; ", num(ic[2], 3), "]"), p_valor(d$p), p_valor(d$p_holm),
      num(d$cliff, 2, signo = TRUE))
  })
  opt <- RESUMEN$optimizadores
  filas_opt <- lapply(OPTIMIZADORES, function(o) {
    c(NOMBRE_OPTIMIZADOR[[o]], num(opt[[o]]$auc_t6, 4), num(opt[[o]]$auc_t24, 4),
      pct(opt[[o]]$al_mejor * 100, 0), num(opt[[o]]$evals_al_mejor, 0), num(opt[[o]]$auc_externa, 4),
      num(opt[[o]]$optimismo, 4, signo = TRUE))
  })
  veces <- round(fr$variacion_dentro / fr$variacion_entre)
  tagList(
    section_title("¿Son distintos de verdad?"),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_mod_pliegues", "AUC de cada pliegue externo",
                         "Modelos finales. La raya es la media de los 10 pliegues.")),
      columna(
        lg = 5, class = "mb-4",
        card(
          stat_list(list(
            c("DE de un procedimiento entre pliegues", num(fr$variacion_dentro, 3)),
            c("DE entre los 48 procedimientos", num(fr$variacion_entre, 3)),
            c("Rango de las medias (mejor − peor)", num(fr$rango_medias, 3)),
            c("Friedman, 48 combinaciones", paste0("$\\chi^2(", fr$gl, ") = ", tnum(fr$chi2, 1), "$, ", p_tex(fr$p)))
          )),
          titulo = "Variación entre pacientes frente a variación entre métodos",
          subtitulo = paste0("La primera es ", veces, " veces mayor que la segunda."),
          tono = "acento"
        )
      )
    ),
    callout(
      paste0("La prueba de Friedman ", if (fr$p < 0.05) "apenas" else "no", " encuentra diferencias ",
             "entre las 48 combinaciones (", p_tex(fr$p), "), DeLong no distingue entre los tres ",
             "modelos finales en prueba y los tamaños de efecto (delta de Cliff) son pequeños. Con estos datos los ",
             "tres modelos son equivalentes en discriminación; la elección entre ellos depende de ",
             "la interpretabilidad, la calibración y el costo."),
      titulo = "Conclusión estadística"
    ),
    section_title("Comparación dirigida entre los modelos finales"),
    card(data_table(c("Comparación", "Δ AUC (prueba)", "IC 95 % bootstrap", "p DeLong",
                      "p ajustado (Holm)", "δ de Cliff"), filas_delong),
         subtitulo = "Con 3 comparaciones, los p-valores se ajustan por Holm-Bonferroni.", class = "mb-4"),
    section_title("Análisis por factor"),
    card(data_table(c("Factor", "Bloques", "Friedman", "p", "Rango promedio (1 = mejor)"), filas_factor),
         subtitulo = paste0(
           "Cada combinación de los otros dos factores con cada pliegue es un bloque. ",
           if (length(significativos) == 0) "Ningún factor sale significativo." else
             paste0("Sale significativo: ", lista_y(nombres_factor[significativos]),
                    ", con un efecto pequeño sobre la AUC.")),
         class = "mb-4"),
    section_title("Los cuatro optimizadores"),
    card(div(class = "control-label", "Modelo"), radio_modelo("mod_anytime_modelo", "xgboost"),
         class = "controles-card mb-3"),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_mod_anytime", "Curvas anytime",
                         paste("Cuánto le falta a cada método, tras t evaluaciones, para la mejor AUC",
                               "interna que encontró cualquiera de los cuatro. Más abajo es mejor;",
                               "media de 40 búsquedas."))),
      columna(lg = 5, class = "mb-4", lectura_guiada(lectura_optimizadores()
      )))
    ),
    card(data_table(c("Optimizador", "AUC interna t = 6", "t = 24", "Bloques al mejor",
                      "Evaluaciones al mejor", "AUC externa", "Optimismo"), filas_opt),
         subtitulo = paste("Promedios de 120 búsquedas (3 modelos × 4 balanceos × 10 pliegues).",
                           "Optimismo = AUC interna − AUC externa."),
         class = "mb-4")
  )
}

ui_modelos <- function() {
  div(
    class = "vista-pestana",
    titular(
      "03 Los resultados · Modelos",
      "Tres clasificadores",
      "puestos a prueba",
      texto = paste("Regresión logística, Random Forest y XGBoost, comparados con validación",
                    "cruzada anidada, cuatro técnicas de balanceo y cuatro métodos de",
                    "optimización, y evaluados en los pacientes que el EDA dejó sin mirar.")
    ),
    modelos_kpis(),
    div(class = "mt-2",
        subpestanas(
          "mod_subtab",
          tabPanel("Diseño y resultados", value = "diseno", panel_diseno()),
          tabPanel("Evaluación en prueba", value = "prueba", panel_prueba()),
          tabPanel("Interpretación", value = "interpretacion", panel_interpretacion()),
          tabPanel("Comparación estadística", value = "comparacion", panel_comparacion())
        )),
    div(class = "mt-4",
        callout(
          tagList("El desarrollo completo —espacios de búsqueda, complejidad computacional, LIME,",
                  " sensibilidad a la semilla y todas las pruebas— está en el ",
                  tags$a("capítulo de modelado del libro", href = URL_LIBRO_MODELADO, target = "_blank",
                         rel = "noopener noreferrer", class = "link-externo"), "."),
          titulo = "Dónde seguir"
        ))
  )
}

server_modelos <- function(input, output, session) {
  modelo <- reactive(if (is.null(input$mod_modelo)) "logistica" else input$mod_modelo)
  umbral <- reactive(if (is.null(input$mod_umbral)) 0.5 else input$mod_umbral)

  output$fig_mod_mapa <- plotly::renderPlotly(fig_combinaciones(if (is.null(input$mod_metrica)) "auc" else input$mod_metrica))
  output$fig_mod_roc <- plotly::renderPlotly(fig_roc())
  output$fig_mod_matriz <- plotly::renderPlotly(fig_matriz_confusion(modelo(), umbral()))
  output$mod_matriz_stats <- renderUI(stats_matriz(modelo(), umbral()))
  output$fig_mod_calibracion <- plotly::renderPlotly(fig_calibracion(modelo()))
  output$fig_mod_importancia <- plotly::renderPlotly(fig_importancia())
  output$fig_mod_panel <- plotly::renderPlotly(fig_panel_reducido())
  output$fig_mod_pliegues <- plotly::renderPlotly(fig_pliegues())
  output$fig_mod_anytime <- plotly::renderPlotly(
    fig_anytime(if (is.null(input$mod_anytime_modelo)) "xgboost" else input$mod_anytime_modelo)
  )
}

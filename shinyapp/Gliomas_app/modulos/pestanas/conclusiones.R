# =========================================================================== #
# Pestaña 12 · Conclusiones
# Qué quedó establecido con el análisis exploratorio y con el modelado, qué
# significa para el objetivo de reducir el panel de secuenciación y qué falta.
# =========================================================================== #

SIGUIENTES_PASOS <- list(
  c("Validar con una cohorte externa",
    paste("Todo lo observado proviene de TCGA. Repetir el análisis y aplicar los modelos a una",
          "cohorte independiente diría si estas asociaciones y este rendimiento se sostienen",
          "fuera de esta muestra o son particularidades suyas.")),
  c("Probar el panel reducido de forma prospectiva",
    paste("El modelado indica que", panel_en_palabras(), "conservan casi toda la capacidad",
          "discriminativa. Confirmarlo exige medir solo ese panel en pacientes nuevos y comparar",
          "con el diagnóstico completo.")),
  c("Fijar un umbral con criterio clínico",
    paste("El umbral de 0,5 es una convención. Un glioblastoma no detectado cuesta más que una",
          "falsa alarma, así que el punto de corte debería acordarse con especialistas a partir",
          "de la curva ROC y de la calibración.")),
  c("Incorporar las variables que faltan",
    paste("El error más claro de los modelos (un GBM con IDH1 mutado) es un astrocitoma de",
          "grado 4 según la OMS de 2021. Distinguirlo exige variables que el dataset no tiene,",
          "como la codeleción 1p/19q o la deleción de CDKN2A/B."))
)

conclusiones_kpis <- function() {
  a <- asociacion_con_grado("train")
  m <- matriz_asociacion_genes("train", top_n = 12)
  diag(m) <- 0
  mejor <- mejor_modelo_test()
  list(
    list(valor = num(a$rho[a$variable == "IDH1"], 2, signo = TRUE), etiqueta = "Spearman de IDH1",
         detalle = "El marcador más discriminante, asociado al bajo grado", color = COLOR_LGG),
    list(valor = paste(sum(a$significativa), "de", nrow(a)), etiqueta = "Variables con señal",
         detalle = paste("Sobre", tamanos_particion()[["train"]], "pacientes de entrenamiento")),
    list(valor = num(mejor$auc, 3), etiqueta = "AUC en prueba",
         detalle = paste(NOMBRES_MODELO[[mejor$clave]], "en", RESUMEN$meta$n_test, "pacientes nuevos")),
    list(valor = num(auc_panel(K_PANEL), 3), etiqueta = paste("AUC con", K_PANEL, "variables"),
         detalle = paste("Frente a", num(auc_panel(23), 3), "con las 23 (validación anidada)"))
  )
}

conclusiones_tabla_hallazgos <- function() {
  a <- asociacion_con_grado("train")
  prev <- prevalencia_genes("train")
  edad <- estadisticas_edad("train")
  idh1 <- prev[prev$gen == "IDH1", ]
  vif <- vif_predictoras("train")
  data_table(
    c("Hallazgo", "Evidencia", "Consecuencia"),
    list(
      c("IDH1 es el marcador dominante",
        paste0("$r = ", tnum(a$rho[a$variable == "IDH1"], 2, signo = TRUE), "$ · mutado en ",
               pct(idh1$LGG, 1), " de LGG frente a ", pct(idh1$GBM, 1), " de GBM"),
        "Su presencia es casi por sí sola una firma de glioma de bajo grado."),
      c("La edad aporta señal clínica independiente",
        paste0("$r = ", tnum(a$rho[a$variable == "Age_at_diagnosis"], 2, signo = TRUE), "$ · ",
               num(edad["GBM", "media"], 1), " años de media en GBM frente a ",
               num(edad["LGG", "media"], 1), " en LGG"),
        paste("Una variable que ya está en la historia clínica y no cuesta nada medir discrimina",
              "casi tanto como una mutación.")),
      c("La mayoría del panel no discrimina el grado",
        paste(sum(!a$significativa), "de", nrow(a),
              "variables sin asociación apreciable ($p \\geq 0{,}05$ o $|r| < 0{,}10$)"),
        "Sostiene la hipótesis central: un panel reducido podría bastar para decidir el grado."),
      c("No hay multicolinealidad estructural",
        paste0("Ningún par con V de Cramér > 0,70, y el factor de inflación de la varianza más ",
               "alto es ", num(vif$vif[1], 2), " (", nombre_corto(vif$variable[1]), "), lejos del umbral de 5"),
        paste("Los marcadores pueden entrar juntos en un modelo sin inestabilizar sus coeficientes.")),
      c("El grupo racial no es interpretable aquí",
        "Más del 90 % de la cohorte pertenece a una sola categoría",
        "La asociación que aparece es un artefacto de composición de la muestra, no un hallazgo biológico.")
    )
  )
}

conclusiones_tabla_modelado <- function() {
  aucs <- vapply(ORDEN_MODELOS, function(m) RESUMEN$modelos[[m]]$auc_test, numeric(1))
  p_delong <- vapply(RESUMEN$delong, function(d) d$p, numeric(1))
  recall <- vapply(ORDEN_MODELOS, function(m) matriz_confusion(m)$recall, numeric(1))
  ece <- vapply(ORDEN_MODELOS, function(m) RESUMEN$modelos[[m]]$ece_test, numeric(1))
  imp <- RESUMEN$importancia[[1]]
  data_table(
    c("Hallazgo", "Evidencia", "Consecuencia"),
    list(
      c("Los tres modelos discriminan muy bien",
        paste0("AUC en prueba entre ", num(min(aucs), 3), " y ", num(max(aucs), 3),
               "; recall de GBM entre el ", pct(min(recall) * 100, 0), " y el ",
               pct(max(recall) * 100, 0), " con umbral 0,5"),
        "Las variables del panel bastan para separar los dos grados en la gran mayoría de los casos."),
      c("Ninguno es mejor que los otros",
        paste0("DeLong: $p$ entre ", num(min(p_delong), 2), " y ", num(max(p_delong), 2),
               "; Friedman sobre las 48 combinaciones: ", p_tex(RESUMEN$friedman$p)),
        "La elección se hace por interpretabilidad y calibración, no por AUC."),
      c("IDH1 domina también en los modelos",
        paste0("Entre el ", pct(min(imp$xgboost, imp$logistica), 0), " y el ",
               pct(max(imp$xgboost, imp$logistica), 0), " de la importancia SHAP total"),
        "Coinciden el EDA, el modelo lineal y el de árboles: el hallazgo no es un artefacto."),
      c(paste(tools::toTitleCase(numero_texto(K_PANEL)), "variables bastan"),
        paste0("AUC ", num(auc_panel(K_PANEL), 3), " con ", panel_en_palabras(), " frente a ",
               num(auc_panel(23), 3), " con las 23"),
        "Es la respuesta al objetivo: un panel mucho más barato conserva casi toda la capacidad."),
      c("Las probabilidades son creíbles",
        paste0("Error de calibración en prueba entre ", num(min(ece), 3), " y ", num(max(ece), 3)),
        "La probabilidad que da el modelo se puede leer como riesgo, con la prudencia de una muestra pequeña.")
    )
  )
}

ui_conclusiones <- function() {
  a <- asociacion_con_grado("train")
  div(
    class = "vista-pestana",
    page_header(
      "Conclusiones",
      paste0("Un puñado de marcadores concentra casi toda la información sobre el grado del ",
             "tumor, y un modelo con solo ", numero_texto(K_PANEL), " variables lo aprovecha casi entero."),
      "check_circle"
    ),
    kpi_row(conclusiones_kpis()),
    card(
      paragraph(paste0(
        "De las ", nrow(a), " variables disponibles, solo ", sum(a$significativa), " muestran una ",
        "asociación con el grado que sea a la vez estadísticamente significativa y de magnitud ",
        "apreciable. El resto, por sí solo, aporta una varianza explicativa insignificante: no distingue a un ",
        "glioma de bajo grado de un glioblastoma.")),
      paragraph(paste0(
        "El modelado confirma y cuantifica ese resultado: los tres clasificadores alcanzan una AUC ",
        "de alrededor de ", num(mejor_modelo_test()$auc, 2), " con pacientes que no vieron al ",
        "entrenar, y con ", panel_en_palabras(), " se conserva prácticamente toda esa ",
        "capacidad. Esa es la respuesta al objetivo del proyecto.")),
      callout(
        paste("La señal que encuentran los contrastes y los modelos coincide con la literatura",
              "clínica: IDH1, IDH2, ATRX, CIC y TP53 hacia LGG, y la edad, PTEN y EGFR hacia GBM.",
              "Que un análisis hecho a ciegas sobre los datos reproduzca el conocimiento médico",
              "establecido es una señal de validez, aunque no sustituye a una cohorte externa."),
        titulo = "Coherencia con el conocimiento clínico"
      ),
      titulo = "Resultado principal"
    ),
    section_title("Hallazgos del análisis exploratorio"),
    card(conclusiones_tabla_hallazgos(), titulo = "Cinco hallazgos con respaldo cuantitativo",
         subtitulo = "Cifras calculadas sobre el conjunto de entrenamiento", class = "mb-4"),
    section_title("Hallazgos del modelado"),
    card(conclusiones_tabla_modelado(), titulo = "Lo que añaden los clasificadores",
         subtitulo = paste("Validación cruzada anidada sobre entrenamiento y evaluación única en los",
                           RESUMEN$meta$n_test, "pacientes de prueba"),
         class = "mb-4"),
    section_title("Qué falta por hacer"),
    fila(
      class = "g-3",
      lapply(SIGUIENTES_PASOS, function(s) columna(lg = 6, class = "mb-3", card(paragraph(s[2]), titulo = s[1])))
    ),
    card(
      paragraph("El proyecto se entrega en tres piezas complementarias:"),
      bullet_list(list(
        span(tags$b("Libro en R (bookdown): "),
             "el análisis exploratorio y el modelado con su desarrollo estadístico. ",
             enlace_externo("Abrirlo", URL_LIBRO)),
        span(tags$b("Aplicación Shiny: "),
             "esta capa interactiva, con el EDA navegable, la evaluación de los modelos y la predicción paciente a paciente."),
        span(tags$b("Código: "), "pipeline reproducible con una semilla fija, del CSV a los modelos. ",
             enlace_externo("Repositorio", URL_REPO))
      )),
      titulo = "Entregables"
    )
  )
}

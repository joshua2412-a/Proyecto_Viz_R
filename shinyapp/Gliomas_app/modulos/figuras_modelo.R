# =========================================================================== #
# Figuras de las pestañas Modelos y Predicción
#
# Mismas reglas de color que el resto del tablero:
#   * azul y coral significan LGG y GBM, y solo eso: aparecen cuando la marca
#     habla de un grado (la probabilidad que asigna un modelo, el sentido en que
#     una variable empuja una predicción, los pacientes reales de cada grado);
#   * los tres modelos no son grados: tres tintas neutras de distinta claridad,
#     un trazo distinto y siempre su nombre junto a la marca.
# =========================================================================== #

SIMBOLO_MODELO <- c(logistica = "circle", random_forest = "square", xgboost = "diamond")

METRICAS <- list(
  auc = list(etiqueta = "AUC", descripcion = "Área bajo la curva ROC", mas_es_mejor = TRUE),
  recall = list(etiqueta = "Recall GBM",
                descripcion = "Proporción de glioblastomas detectados (umbral 0,5)", mas_es_mejor = TRUE),
  f1 = list(etiqueta = "F1", descripcion = "Media armónica de precisión y recall (umbral 0,5)",
            mas_es_mejor = TRUE),
  brier = list(etiqueta = "Brier",
               descripcion = "Error cuadrático de las probabilidades (más bajo es mejor)",
               mas_es_mejor = FALSE)
)

BALANCEOS <- c("ninguno", "smote", "adasyn", "class_weight")
OPTIMIZADORES <- c("grid", "random", "bayesiano", "genetico")

nombre_variable_modelo <- function(variable) {
  switch(variable, Age_at_diagnosis = "Edad", Race_binary = "Grupo racial (no blanco)",
         Gender = "Género (mujer)", variable)
}

linea_vertical <- function(x, color, dash = "solid", width = 1.2) {
  list(type = "line", x0 = x, x1 = x, y0 = 0, y1 = 1, yref = "paper",
       line = list(color = color, dash = dash, width = width))
}

# --------------------------------------------------------------------------- #
# Modelos · diseño y resultados de la validación anidada
# --------------------------------------------------------------------------- #
fig_combinaciones <- function(metrica = "auc") {
  combos <- RESUMEN$combinaciones
  info <- METRICAS[[metrica]]
  filas <- expand.grid(b = BALANCEOS, m = ORDEN_MODELOS, stringsAsFactors = FALSE)[, c("m", "b")]
  buscar <- function(m, b, o) {
    for (c in combos) if (c$modelo == m && c$balanceo == b && c$metodo == o) return(c)
    NULL
  }
  z <- matrix(NA_real_, nrow(filas), length(OPTIMIZADORES))
  texto <- matrix("", nrow(filas), length(OPTIMIZADORES))
  hover <- matrix("", nrow(filas), length(OPTIMIZADORES))
  for (i in seq_len(nrow(filas))) {
    for (j in seq_along(OPTIMIZADORES)) {
      c <- buscar(filas$m[i], filas$b[i], OPTIMIZADORES[j])
      media <- c[[paste0(metrica, "_media")]]
      de <- c[[paste0(metrica, "_de")]]
      z[i, j] <- media
      texto[i, j] <- num(media, 3)
      hover[i, j] <- paste0(
        "<b>", NOMBRES_MODELO[[filas$m[i]]], "</b> · ", NOMBRE_BALANCEO[[filas$b[i]]], " · ",
        NOMBRE_OPTIMIZADOR[[OPTIMIZADORES[j]]],
        "<br>", info$etiqueta, ": ", num(media, 3), " ± ", num(de, 3), " (10 pliegues)",
        "<br>AUC ", num(c$auc_media, 3), " · recall ", num(c$recall_media, 3),
        "<br>Tiempo de la validación anidada: ", num(c$t_total_s, 0), " s"
      )
    }
  }
  escala <- if (info$mas_es_mejor) SEQ_BLUE else rev(SEQ_BLUE)
  p <- plotly::plot_ly(
    z = z, x = unname(NOMBRE_OPTIMIZADOR[OPTIMIZADORES]),
    y = unname(paste(NOMBRES_MODELO[filas$m], "·", NOMBRE_BALANCEO[filas$b])),
    type = "heatmap", text = texto, texttemplate = "%{text}", textfont = list(size = 12),
    customdata = hover, hovertemplate = "%{customdata}<extra></extra>",
    colorscale = escala_seq(escala), xgap = 2, ygap = 2,
    colorbar = list(title = list(text = info$etiqueta, side = "right"), thickness = 12,
                    outlinewidth = 0, tickfont = list(color = INK_MUTED, size = 11))
  )
  p <- plotly::layout(
    p,
    yaxis = eje_y(autorange = "reversed", showgrid = FALSE, ticks = "", title = list(text = "")),
    xaxis = eje_x(side = "top", showline = FALSE, ticks = "", title = list(text = ""))
  )
  aplicar_tema(p, height = 520, margin = list(l = 10, r = 10, t = 40, b = 10))
}

fig_pliegues <- function() {
  p <- plotly::plot_ly()
  for (m in ORDEN_MODELOS) {
    aucs <- vec(RESUMEN$modelos[[m]]$auc_pliegues)
    nombre <- NOMBRES_MODELO[[m]]
    p <- plotly::add_trace(
      p, x = aucs, y = rep(nombre, length(aucs)), type = "scatter", mode = "markers",
      marker = list(size = 11, color = TONOS_MODELO[[m]], symbol = SIMBOLO_MODELO[[m]],
                    line = list(color = BG_CARD, width = 2), opacity = 0.85),
      customdata = seq_along(aucs), showlegend = FALSE,
      hovertemplate = paste0("Pliegue %{customdata}: AUC %{x:.3f}<extra>", nombre, "</extra>")
    )
    media <- mean(aucs)
    p <- plotly::add_trace(
      p, x = c(media, media), y = c(nombre, nombre), type = "scatter", mode = "markers",
      marker = list(symbol = "line-ns", size = 30, line = list(width = 3, color = INK)),
      showlegend = FALSE,
      hovertemplate = paste0("Media: ", num(media, 3), "<extra>", nombre, "</extra>")
    )
  }
  p <- plotly::layout(
    p,
    xaxis = eje_x(title = list(text = "AUC del pliegue externo"), showgrid = TRUE,
                  gridcolor = GRID, tickformat = ".2f"),
    yaxis = eje_y(showgrid = FALSE, autorange = "reversed", title = list(text = ""),
                  categoryorder = "array", categoryarray = as.list(unname(NOMBRES_MODELO[ORDEN_MODELOS])))
  )
  aplicar_tema(p, height = 260, margin = list(l = 10, r = 20, t = 20, b = 50))
}

fig_anytime <- function(modelo = "xgboost") {
  curvas <- RESUMEN$anytime[[modelo]]
  tonos <- c(grid = "#8796AC", random = "#52698A", bayesiano = "#1E4E86", genetico = "#6B7F9C")
  trazos <- c(grid = "dot", random = "dash", bayesiano = "solid", genetico = "dashdot")
  p <- plotly::plot_ly()
  for (o in OPTIMIZADORES) {
    y <- pmax(vec(curvas[[o]]), 1e-4)
    p <- plotly::add_trace(
      p, x = seq_along(y), y = y, type = "scatter", mode = "lines",
      name = NOMBRE_OPTIMIZADOR[[o]],
      line = list(color = tonos[[o]], width = if (o == "bayesiano") 2.5 else 2, dash = trazos[[o]]),
      hovertemplate = paste0("Evaluación %{x}: le faltan %{y:.4f} de AUC<extra>",
                             NOMBRE_OPTIMIZADOR[[o]], "</extra>")
    )
  }
  marcas <- c(0.0001, 0.0002, 0.0005, 0.001, 0.002, 0.005, 0.01, 0.02, 0.05, 0.1, 0.2, 0.5)
  rotulos <- sub(",$", "", sub("0+$", "", num_v(marcas, 4)))
  p <- plotly::layout(
    p,
    yaxis = eje_y(type = "log", title = list(text = "Brecha con el mejor (AUC, escala log)"),
                  tickvals = marcas, ticktext = rotulos),
    xaxis = eje_x(title = list(text = "Evaluaciones de hiperparámetros"), dtick = 4)
  )
  aplicar_tema(p, height = 360, margin = list(l = 10, r = 10, t = 40, b = 50))
}

# --------------------------------------------------------------------------- #
# Modelos · evaluación en el conjunto de prueba
# --------------------------------------------------------------------------- #
fig_roc <- function() {
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
                         showlegend = FALSE, hoverinfo = "skip",
                         line = list(color = AXIS, dash = "dot", width = 1))
  for (m in ORDEN_MODELOS) {
    datos <- RESUMEN$modelos[[m]]
    nombre <- NOMBRES_MODELO[[m]]
    p <- plotly::add_trace(
      p, x = vec(datos$roc$fpr), y = vec(datos$roc$tpr), type = "scatter", mode = "lines",
      line = list(shape = "hv", color = TONOS_MODELO[[m]], width = 2.2, dash = TRAZO_MODELO[[m]]),
      name = paste0(nombre, " · AUC ", num(datos$auc_test, 3)),
      hovertemplate = paste0("Falsos positivos %{x:.0%} · GBM detectados %{y:.0%}<extra>",
                             nombre, "</extra>")
    )
  }
  p <- plotly::layout(
    p,
    xaxis = eje_x(title = list(text = "Tasa de falsos positivos (LGG marcados como GBM)"),
                  range = c(0, 1), tickformat = ".0%", constrain = "domain"),
    yaxis = eje_y(title = list(text = "Recall de GBM"), range = c(0, 1.02), tickformat = ".0%",
                  scaleanchor = "x", scaleratio = 1),
    legend = leyenda(orientation = "v", x = 0.98, xanchor = "right", y = 0.04,
                     yanchor = "bottom", bgcolor = "rgba(255,255,255,0.85)")
  )
  aplicar_tema(p, height = 420, margin = list(l = 10, r = 10, t = 20, b = 50))
}

matriz_confusion <- function(modelo, umbral = UMBRAL) {
  y <- vec(RESUMEN$test$y)
  pred <- as.integer(vec(RESUMEN$test[[modelo]]) >= umbral)
  vp <- sum(pred == 1 & y == 1)
  fn <- sum(pred == 0 & y == 1)
  fp <- sum(pred == 1 & y == 0)
  vn <- sum(pred == 0 & y == 0)
  list(vp = vp, fn = fn, fp = fp, vn = vn,
       recall = vp / max(vp + fn, 1), especificidad = vn / max(vn + fp, 1),
       precision = vp / max(vp + fp, 1), accuracy = (vp + vn) / length(y))
}

fig_matriz_confusion <- function(modelo, umbral = UMBRAL) {
  mc <- matriz_confusion(modelo, umbral)
  cuentas <- matrix(c(mc$vn, mc$fp, mc$fn, mc$vp), 2, byrow = TRUE)
  pct_fila <- cuentas / rowSums(cuentas)
  pct_fila[is.na(pct_fila)] <- 0
  etiquetas <- matrix(c("LGG bien clasificados", "LGG marcados como GBM",
                        "GBM que se escapan", "GBM detectados"), 2, byrow = TRUE)
  texto <- matrix(paste0("<b>", cuentas, "</b><br>", pct_v(pct_fila * 100, 0)), 2)
  p <- plotly::plot_ly(
    z = pct_fila, x = c("Predicho LGG", "Predicho GBM"), y = c("Real LGG", "Real GBM"),
    type = "heatmap", text = texto, texttemplate = "%{text}", textfont = list(size = 15),
    customdata = etiquetas, hovertemplate = "%{customdata}: %{z:.0%} de su fila<extra></extra>",
    colorscale = escala_seq(), zmin = 0, zmax = 1, showscale = FALSE, xgap = 3, ygap = 3
  )
  p <- plotly::layout(
    p,
    yaxis = eje_y(autorange = "reversed", showgrid = FALSE, ticks = "", title = list(text = "")),
    xaxis = eje_x(side = "top", showline = FALSE, ticks = "", title = list(text = ""))
  )
  aplicar_tema(p, height = 300, margin = list(l = 10, r = 10, t = 40, b = 10))
}

fig_calibracion <- function(modelo) {
  datos <- RESUMEN$modelos[[modelo]]
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = c(0, 1), y = c(0, 1), type = "scatter", mode = "lines",
                         name = "Calibración perfecta", hoverinfo = "skip",
                         line = list(color = AXIS, dash = "dot", width = 1))
  fuentes <- list(
    list(nombre = paste0("Fuera de pliegue (n = ", RESUMEN$meta$n_train, ")"),
         clave = "calibracion_oof", trazo = "solid", simbolo = "circle"),
    list(nombre = paste0("Prueba (n = ", RESUMEN$meta$n_test, ")"),
         clave = "calibracion_test", trazo = "dash", simbolo = "square")
  )
  for (f in fuentes) {
    c <- datos[[f$clave]]
    n <- vec(c$n)
    p <- plotly::add_trace(
      p, x = vec(c$predicha), y = vec(c$observada), type = "scatter", mode = "lines+markers",
      name = f$nombre,
      line = list(color = TONOS_MODELO[[modelo]], width = 2, dash = f$trazo),
      marker = list(size = 6 + sqrt(n), symbol = f$simbolo, color = TONOS_MODELO[[modelo]],
                    line = list(color = BG_CARD, width = 1.5)),
      customdata = n,
      hovertemplate = paste0("Predicha %{x:.0%} · observada %{y:.0%}<br>%{customdata} pacientes",
                             "<extra>", f$nombre, "</extra>")
    )
  }
  p <- plotly::layout(
    p,
    xaxis = eje_x(title = list(text = "Probabilidad de GBM predicha"), range = c(0, 1), tickformat = ".0%"),
    yaxis = eje_y(title = list(text = "Frecuencia observada de GBM"), range = c(0, 1), tickformat = ".0%"),
    legend = leyenda(orientation = "v", x = 0.02, xanchor = "left", y = 0.98, yanchor = "top",
                     bgcolor = "rgba(255,255,255,0.85)")
  )
  aplicar_tema(p, height = 360, margin = list(l = 10, r = 10, t = 20, b = 50))
}

# --------------------------------------------------------------------------- #
# Modelos · interpretación
# --------------------------------------------------------------------------- #
fig_importancia <- function(n = 10) {
  filas <- rev(utils::head(RESUMEN$importancia, n))
  nombres <- vapply(filas, function(f) nombre_variable_modelo(f$variable), character(1))
  p <- plotly::plot_ly()
  for (clave in c("logistica", "xgboost")) {
    valores <- vapply(filas, function(f) as.numeric(f[[clave]]), numeric(1))
    p <- plotly::add_trace(
      p, y = nombres, x = valores, type = "bar", orientation = "h", name = NOMBRES_MODELO[[clave]],
      marker = list(color = TONOS_MODELO[[clave]]), text = pct_v(valores, 1),
      textposition = "outside", textfont = list(size = 11, color = INK_SOFT), cliponaxis = FALSE,
      hovertemplate = paste0("%{y}: %{x:.1f} % de la importancia total<extra>",
                             NOMBRES_MODELO[[clave]], "</extra>")
    )
  }
  p <- plotly::layout(
    p, barmode = "group", bargap = 0.28, bargroupgap = 0.08,
    xaxis = eje_x(title = list(text = "% del total de |SHAP| medio"), showgrid = TRUE,
                  gridcolor = GRID, range = c(0, 58)),
    yaxis = orden_categorias(nombres, showgrid = FALSE, title = list(text = ""))
  )
  aplicar_tema(p, height = 440, margin = list(l = 10, r = 30, t = 40, b = 50))
}

fig_panel_reducido <- function() {
  panel <- RESUMEN$panel
  k <- vec(panel$k)
  auc <- vec(panel$auc)
  de <- vec(panel$de)
  completo <- auc[length(auc)]
  p <- plotly::plot_ly()
  p <- plotly::add_trace(
    p, x = c(k, rev(k)), y = c(auc + de, rev(auc - de)), type = "scatter", mode = "lines",
    fill = "toself", fillcolor = "rgba(30,78,134,0.10)", line = list(width = 0),
    hoverinfo = "skip", showlegend = FALSE
  )
  p <- plotly::add_trace(
    p, x = k, y = auc, type = "scatter", mode = "lines+markers",
    line = list(color = TONOS_MODELO[["xgboost"]], width = 2.5),
    marker = list(size = 8, color = TONOS_MODELO[["xgboost"]], line = list(color = BG_CARD, width = 2)),
    customdata = de, showlegend = FALSE,
    hovertemplate = "%{x} variables: AUC %{y:.3f} ± %{customdata:.3f}<extra></extra>"
  )
  notas <- list(list(
    x = 1, xref = "paper", xanchor = "right", y = completo, yanchor = "top",
    text = paste("Las 23 variables:", num(completo, 3)), showarrow = FALSE,
    font = list(size = 11, color = INK_MUTED)
  ))
  if (K_PANEL %in% k) {
    notas[[2]] <- list(x = K_PANEL, y = auc[k == K_PANEL], text = paste(nombres_panel(), collapse = " + "),
                       showarrow = TRUE, arrowhead = 0, arrowcolor = INK_MUTED, ax = 60, ay = 40,
                       font = list(size = 12, color = INK))
  }
  p <- plotly::layout(
    p,
    shapes = list(list(type = "line", xref = "paper", x0 = 0, x1 = 1, y0 = completo, y1 = completo,
                       line = list(color = INK_MUTED, dash = "dash", width = 1))),
    annotations = notas,
    xaxis = eje_x(title = list(text = "Número de variables (las k más importantes)"), tickvals = k),
    yaxis = eje_y(title = list(text = "AUC (media ± DE, 10 pliegues)"), tickformat = ".2f",
                  range = c(0.8, 0.97))
  )
  aplicar_tema(p, height = 360, margin = list(l = 10, r = 10, t = 20, b = 50))
}

# --------------------------------------------------------------------------- #
# Predicción
# --------------------------------------------------------------------------- #
fig_probabilidades <- function(probas) {
  nombres <- unname(NOMBRES_MODELO[ORDEN_MODELOS])
  valores <- vapply(ORDEN_MODELOS, function(m) probas[[m]], numeric(1), USE.NAMES = FALSE)
  colores <- ifelse(valores >= UMBRAL, COLOR_GBM, COLOR_LGG)
  p <- plotly::plot_ly(
    y = nombres, x = valores, type = "bar", orientation = "h", width = 0.55,
    marker = list(color = colores),
    text = paste0(pct_v(valores * 100, 0), " · ", ifelse(valores >= UMBRAL, "GBM", "LGG")),
    textposition = "outside", cliponaxis = FALSE, textfont = list(size = 13, color = INK),
    hovertemplate = "%{y}: P(GBM) = %{x:.1%}<extra></extra>"
  )
  p <- plotly::layout(
    p,
    shapes = list(linea_vertical(UMBRAL, INK_MUTED, "dot")),
    annotations = list(list(x = UMBRAL, y = 1, yref = "paper", yanchor = "bottom",
                            text = "umbral 50 %", showarrow = FALSE,
                            font = list(size = 11, color = INK_MUTED))),
    xaxis = eje_x(range = c(0, 1.18), tickvals = c(0, 0.25, 0.5, 0.75, 1), tickformat = ".0%",
                  showgrid = TRUE, gridcolor = GRID, title = list(text = "Probabilidad de GBM")),
    yaxis = orden_categorias(nombres, autorange = "reversed", showgrid = FALSE, title = list(text = ""))
  )
  aplicar_tema(p, height = 230, margin = list(l = 10, r = 10, t = 30, b = 45))
}

etiqueta_contribucion <- function(v, paciente) {
  if (v == "Age_at_diagnosis") return(paste("Edad =", num(as.numeric(paciente$Age_at_diagnosis), 0), "años"))
  if (v == "Gender") return(paste("Género =", if (as.integer(paciente$Gender %||% 0) == 1) "mujer" else "hombre"))
  if (v == "Race_binary") {
    return(paste("Grupo racial =", if (as.integer(paciente$Race %||% 0) == 0) "blanco" else "no blanco"))
  }
  paste(v, if (as.integer(paciente[[v]] %||% 0) == 1) "mutado" else "no mutado")
}

fig_explicacion <- function(explicacion, paciente, n = 8) {
  contrib <- explicacion$contribuciones
  orden <- names(contrib)[order(-abs(contrib))]
  top <- utils::head(orden, n)
  resto <- setdiff(orden, top)
  y <- vapply(top, etiqueta_contribucion, character(1), paciente = paciente, USE.NAMES = FALSE)
  x <- unname(contrib[top])
  if (length(resto)) {
    y <- c(y, paste("Otras", length(resto), "variables"))
    x <- c(x, sum(contrib[resto]))
  }
  base <- explicacion$base
  final <- base + sum(contrib)
  if (explicacion$escala == "log-odds") {
    textos <- num_v(x, 2, signo = TRUE)
    corte <- 0
    unidades <- "log-odds de GBM"
    rotulo_final <- paste0("este paciente: ", num(final, 2), " (P = ", pct(100 * sigmoide(final), 0), ")")
    rotulo_base <- paste("base", num(base, 2))
    formato_eje <- NULL
  } else {
    textos <- paste(num_v(x * 100, 1, signo = TRUE), "pp")
    corte <- UMBRAL
    unidades <- "probabilidad de GBM"
    rotulo_final <- paste("este paciente:", pct(final * 100, 0))
    rotulo_base <- paste("base", pct(base * 100, 0))
    formato_eje <- ".0%"
  }
  acumulado <- base + c(0, cumsum(x))
  lo <- min(acumulado, corte)
  hi <- max(acumulado, corte)
  margen <- 0.22 * (if (hi > lo) hi - lo else 1)

  p <- plotly::plot_ly(
    type = "waterfall", orientation = "h", base = base, y = y, x = x,
    measure = rep("relative", length(x)), text = textos, textposition = "outside",
    cliponaxis = FALSE, textfont = list(size = 12, color = INK_SOFT),
    increasing = list(marker = list(color = COLOR_GBM)),
    decreasing = list(marker = list(color = COLOR_LGG)),
    connector = list(line = list(color = AXIS, width = 1)),
    hovertemplate = "%{y}: %{text}<extra></extra>"
  )
  notas <- lapply(list(
    list(x = base, texto = rotulo_base, y = 1.13, color = INK_MUTED),
    list(x = final, texto = rotulo_final, y = 1.05, color = INK),
    list(x = corte, texto = "umbral 50 %", y = -0.02, color = INK_MUTED)
  ), function(a) {
    list(x = a$x, y = a$y, xref = "x", yref = "paper", text = a$texto, showarrow = FALSE,
         xanchor = "center", yanchor = if (a$y > 0) "bottom" else "top",
         font = list(size = 11, color = a$color), bgcolor = "rgba(255,255,255,0.85)")
  })
  eje <- eje_x(range = c(lo - margen, hi + margen),
               title = list(text = paste0("Contribución acumulada (", unidades, ")")),
               showgrid = TRUE, gridcolor = GRID, zeroline = FALSE)
  if (!is.null(formato_eje)) eje$tickformat <- formato_eje
  p <- plotly::layout(
    p, showlegend = FALSE,
    shapes = list(linea_vertical(corte, INK_MUTED, "dot"), linea_vertical(base, AXIS, "dash"),
                  linea_vertical(final, INK, "solid", 2)),
    annotations = notas,
    xaxis = eje,
    yaxis = orden_categorias(y, autorange = "reversed", showgrid = FALSE, title = list(text = ""))
  )
  aplicar_tema(p, height = 150 + 34 * length(y), margin = list(l = 10, r = 20, t = 60, b = 60))
}

fig_contexto <- function(modelo, proba) {
  y <- vec(RESUMEN$test$y)
  prob <- vec(RESUMEN$test[[modelo]])
  p <- plotly::plot_ly()
  for (grado in c(0, 1)) {
    nombre <- if (grado == 0) "LGG" else "GBM"
    valores <- prob[y == grado]
    # Dispersión vertical determinista (no toca la semilla de la sesión)
    jitter <- ((seq_along(valores) * 0.6180339887) %% 1 - 0.5) * 0.44
    p <- plotly::add_trace(
      p, x = valores, y = grado + jitter, type = "scatter", mode = "markers",
      name = paste("Pacientes de prueba con", nombre, "real"),
      marker = list(size = 8, color = COLOR_GRADE_MAP[[nombre]], opacity = 0.65,
                    line = list(color = BG_CARD, width = 1)),
      hovertemplate = paste0(nombre, " real · P(GBM) = %{x:.0%}<extra></extra>")
    )
  }
  p <- plotly::layout(
    p,
    shapes = list(linea_vertical(proba, INK, "solid", 2.5), linea_vertical(UMBRAL, INK_MUTED, "dot", 1)),
    annotations = list(list(x = proba, y = 1, yref = "paper", yanchor = "bottom",
                            text = paste("este perfil:", pct(proba * 100, 0)), showarrow = FALSE,
                            font = list(size = 12, color = INK))),
    xaxis = eje_x(range = c(-0.02, 1.02), tickformat = ".0%", title = list(text = "Probabilidad de GBM"),
                  showgrid = TRUE, gridcolor = GRID),
    yaxis = eje_y(tickvals = c(0, 1), ticktext = c("LGG real", "GBM real"), range = c(-0.5, 1.5),
                  showgrid = FALSE, title = list(text = "")),
    legend = leyenda(orientation = "h", y = 1.18, x = 0, xanchor = "left", yanchor = "bottom")
  )
  aplicar_tema(p, height = 300, margin = list(l = 10, r = 10, t = 70, b = 40))
}

# --------------------------------------------------------------------------- #
# Predicción · cómo decide cada modelo (mismas figuras que el Dash)
# --------------------------------------------------------------------------- #
pregunta_nodo <- function(variable, umbral, menor = FALSE) {
  if (variable == "Age_at_diagnosis") {
    return(paste0("¿Edad ", if (menor) "<" else "≤", " ", num(umbral, 1), " años?"))
  }
  if (variable == "Gender") return("¿Es mujer?")
  if (variable == "Race_binary") return("¿Grupo racial no blanco?")
  paste0("¿", variable, " mutado?")
}

# En la edad la rama izquierda es "por debajo del corte" (sí); en las binarias,
# la izquierda es el 0 (no mutado, hombre, blanco), es decir, "no"
respuesta_nodo <- function(variable, izquierda) {
  si <- if (variable == "Age_at_diagnosis") izquierda else !izquierda
  if (si) "Sí" else "No"
}

# Probabilidad de GBM que da cada uno de los árboles del Random Forest
fig_votacion <- function(probas, indice = NULL) {
  media <- mean(probas)
  bins <- list(start = 0, end = 1.0001, size = 0.05)
  p <- plotly::plot_ly()
  for (lado in list(list(sel = probas < UMBRAL, nombre = "Árboles que dicen LGG", color = COLOR_LGG),
                    list(sel = probas >= UMBRAL, nombre = "Árboles que dicen GBM", color = COLOR_GBM))) {
    p <- plotly::add_trace(
      p, x = probas[lado$sel], type = "histogram", xbins = bins,
      name = paste0(lado$nombre, " (", sum(lado$sel), ")"),
      marker = list(color = lado$color, line = list(color = BG_CARD, width = 1)),
      hovertemplate = "%{y} árboles<extra></extra>"
    )
  }
  formas <- list(linea_vertical(UMBRAL, INK_MUTED, "dot"), linea_vertical(media, INK, "solid", 2.5))
  notas <- list(list(x = media, y = 1, yref = "paper", yanchor = "bottom", showarrow = FALSE,
                     text = paste("media del bosque:", pct(media * 100, 0)),
                     font = list(size = 12, color = INK)))
  if (!is.null(indice)) {
    formas <- c(formas, list(linea_vertical(probas[indice], COLOR_MARK, "dash", 1.6)))
    notas <- c(notas, list(list(x = probas[indice], y = 0, yref = "paper", yanchor = "top",
                                showarrow = FALSE, text = paste("árbol", indice),
                                font = list(size = 11, color = COLOR_MARK))))
  }
  p <- plotly::layout(
    p, barmode = "overlay", shapes = formas, annotations = notas,
    xaxis = eje_x(range = c(-0.02, 1.02), tickformat = ".0%",
                  title = list(text = "Probabilidad de GBM que da el árbol")),
    yaxis = eje_y(title = list(text = "Número de árboles")),
    legend = leyenda(orientation = "h", y = 1.16, x = 0, xanchor = "left", yanchor = "bottom")
  )
  aplicar_tema(p, height = 300, margin = list(l = 10, r = 10, t = 70, b = 45))
}

# Un árbol completo con el camino del paciente resaltado. El árbol entero se
# dibuja tenue para que se vea su tamaño; solo el camino lleva tinta y rótulos.
fig_arbol <- function(arbol, camino) {
  logodds <- identical(camino$escala, "log-odds")
  menor <- isTRUE(camino$menor)
  total <- length(arbol$hoja)
  x <- numeric(total)
  prof <- integer(total)
  contador <- 0
  colocar <- function(n, d) {
    prof[n] <<- d
    if (arbol$hoja[n]) {
      x[n] <<- contador
      contador <<- contador + 1
    } else {
      colocar(arbol$izq[n], d + 1L)
      colocar(arbol$der[n], d + 1L)
      x[n] <<- (x[arbol$izq[n]] + x[arbol$der[n]]) / 2
    }
  }
  colocar(1L, 0L)
  en_camino <- seq_len(total) %in% camino$nodos

  hover <- vapply(seq_len(total), function(n) {
    if (arbol$hoja[n]) {
      if (logodds) return(paste("Hoja · suma", num(arbol$valor[n], 3, signo = TRUE), "a los log-odds"))
      return(paste0("Hoja · ", num(arbol$cobertura[n], 0), " casos de entrenamiento · ",
                    pct(arbol$valor[n] * 100, 0), " GBM"))
    }
    variable <- MODELOS$predictoras[arbol$var[n]]
    pregunta <- pregunta_nodo(variable, umbral_en_unidades(variable, arbol$umbral[n]), menor)
    if (logodds) return(pregunta)
    paste0(pregunta, "<br>", num(arbol$cobertura[n], 0), " casos de entrenamiento · ",
           pct(arbol$valor[n] * 100, 0), " GBM")
  }, character(1))

  ex <- c(); ey <- c(); cx <- c(); cy <- c()
  for (n in which(!arbol$hoja)) {
    for (h in c(arbol$izq[n], arbol$der[n])) {
      if (en_camino[n] && en_camino[h]) {
        cx <- c(cx, x[n], x[h], NA); cy <- c(cy, -prof[n], -prof[h], NA)
      } else {
        ex <- c(ex, x[n], x[h], NA); ey <- c(ey, -prof[n], -prof[h], NA)
      }
    }
  }
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = ex, y = ey, type = "scatter", mode = "lines",
                         line = list(color = AXIS, width = 1), hoverinfo = "skip", showlegend = FALSE)
  p <- plotly::add_trace(p, x = cx, y = cy, type = "scatter", mode = "lines",
                         line = list(color = INK, width = 3), hoverinfo = "skip", showlegend = FALSE)
  for (hojas in c(FALSE, TRUE)) {
    nodos <- which(arbol$hoja == hojas)
    activo <- en_camino[nodos]
    if (hojas) {
      hacia_gbm <- if (logodds) arbol$valor[nodos] > 0 else arbol$valor[nodos] >= UMBRAL
      colores <- ifelse(hacia_gbm, COLOR_GBM, COLOR_LGG)
      tamanos <- ifelse(activo, 18, 8)
      opacidad <- ifelse(activo, 1, 0.45)
    } else {
      colores <- ifelse(activo, INK, AXIS)
      tamanos <- ifelse(activo, 11, 6)
      opacidad <- rep(1, length(nodos))
    }
    p <- plotly::add_trace(
      p, x = x[nodos], y = -prof[nodos], type = "scatter", mode = "markers",
      marker = list(color = colores, size = tamanos, opacity = opacidad,
                    line = list(color = BG_CARD, width = 1)),
      text = hover[nodos], hovertemplate = "%{text}<extra></extra>", showlegend = FALSE
    )
  }
  centro <- (max(x) + min(x)) / 2
  notas <- lapply(camino$pasos, function(paso) {
    n <- paso$nodo
    derecha <- x[n] <= centro
    list(x = x[n], y = -prof[n], showarrow = FALSE,
         text = paste0(pregunta_nodo(paso$variable, paso$umbral, menor), " <b>",
                       respuesta_nodo(paso$variable, paso$izquierda), "</b>"),
         xanchor = if (derecha) "left" else "right", xshift = if (derecha) 10 else -10,
         font = list(size = 11, color = INK), bgcolor = "rgba(255,255,255,0.88)")
  })
  hoja <- camino$hoja
  notas[[length(notas) + 1]] <- list(
    x = x[hoja], y = -prof[hoja], showarrow = FALSE, yshift = -20,
    text = if (logodds) paste0("<b>", num(camino$proba, 3, signo = TRUE), " log-odds</b>")
           else paste0("<b>", pct(camino$proba * 100, 0), " GBM</b>"),
    font = list(size = 12, color = INK), bgcolor = "rgba(255,255,255,0.88)")
  profundidad <- max(prof)
  p <- plotly::layout(
    p, showlegend = FALSE, annotations = notas,
    xaxis = list(visible = FALSE, range = c(min(x) - 1, max(x) + 1)),
    yaxis = list(visible = FALSE, range = c(-profundidad - 0.8, 0.5))
  )
  aplicar_tema(p, height = 110 + 52 * (profundidad + 1), margin = list(l = 10, r = 10, t = 10, b = 10))
}

# Cómo se construye la predicción de XGBoost, árbol a árbol
fig_suma_xgb <- function(base, aportes, indice = NULL) {
  acumulado <- base + c(0, cumsum(aportes))
  proba <- sigmoide(acumulado)
  x <- seq_along(acumulado) - 1
  final <- proba[length(proba)]
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = x, y = proba, type = "scatter", mode = "lines",
                         line = list(color = INK, width = 2.5),
                         hovertemplate = "Tras %{x} árboles: P(GBM) = %{y:.1%}<extra></extra>")
  p <- plotly::add_trace(p, x = max(x), y = final, type = "scatter", mode = "markers",
                         marker = list(size = 12, color = if (final >= UMBRAL) COLOR_GBM else COLOR_LGG,
                                       line = list(color = BG_CARD, width = 2)),
                         hoverinfo = "skip")
  formas <- list(list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = UMBRAL, y1 = UMBRAL,
                      line = list(color = INK_MUTED, dash = "dot", width = 1.2)))
  notas <- list(
    list(x = 1, xref = "paper", y = UMBRAL, yanchor = "top", xanchor = "right", showarrow = FALSE,
         text = "umbral 50 %", font = list(size = 11, color = INK_MUTED)),
    list(x = 0, y = proba[1], xanchor = "left", yanchor = "bottom", yshift = 6, xshift = 12,
         showarrow = FALSE, text = paste("base", pct(proba[1] * 100, 0)),
         font = list(size = 11, color = INK_MUTED), bgcolor = "rgba(255,255,255,0.85)"),
    list(x = max(x), y = final, xanchor = "right", yanchor = "bottom", yshift = 10,
         showarrow = FALSE, text = paste0("<b>", pct(final * 100, 0), "</b>"),
         font = list(size = 13, color = INK))
  )
  if (!is.null(indice)) {
    formas <- c(formas, list(linea_vertical(indice, COLOR_MARK, "dash", 1.4)))
    notas <- c(notas, list(list(x = indice, y = 1, yref = "paper", yanchor = "bottom",
                                showarrow = FALSE, text = paste("árbol", indice),
                                font = list(size = 11, color = COLOR_MARK))))
  }
  p <- plotly::layout(
    p, showlegend = FALSE, shapes = formas, annotations = notas,
    xaxis = eje_x(title = list(text = "Árboles sumados"), range = c(-4, max(x) + 4)),
    yaxis = eje_y(title = list(text = "Probabilidad de GBM"), range = c(-0.02, 1.02),
                  tickformat = ".0%")
  )
  aplicar_tema(p, height = 320, margin = list(l = 10, r = 20, t = 30, b = 45))
}

# La curva en S de la regresión logística, con este perfil y la cohorte de prueba
fig_curva_logistica <- function(z) {
  y <- vec(RESUMEN$test$y)
  prob <- pmin(pmax(vec(RESUMEN$test$logistica), 1e-4), 1 - 1e-4)
  zt <- log(prob / (1 - prob))
  lim <- max(8, ceiling(max(abs(c(zt, z))) + 0.5))
  malla <- seq(-lim, lim, length.out = 240)
  p <- plotly::plot_ly()
  p <- plotly::add_trace(p, x = malla, y = sigmoide(malla), type = "scatter", mode = "lines",
                         line = list(color = AXIS, width = 3), hoverinfo = "skip", showlegend = FALSE)
  for (grado in c(0, 1)) {
    nombre <- if (grado == 0) "LGG" else "GBM"
    sel <- zt[y == grado]
    p <- plotly::add_trace(
      p, x = sel, y = sigmoide(sel), type = "scatter", mode = "markers",
      name = paste(nombre, "real (prueba)"),
      marker = list(size = 7, color = COLOR_GRADE_MAP[[nombre]], opacity = 0.55,
                    line = list(color = BG_CARD, width = 1)),
      hovertemplate = paste0(nombre, " real · suma %{x:.2f} · P = %{y:.0%}<extra></extra>")
    )
  }
  pz <- sigmoide(z)
  p <- plotly::add_trace(
    p, x = z, y = pz, type = "scatter", mode = "markers", name = "Este perfil",
    marker = list(size = 16, color = INK, line = list(color = BG_CARD, width = 2.5)),
    hovertemplate = "Este perfil · suma %{x:.2f} · P = %{y:.1%}<extra></extra>"
  )
  p <- plotly::layout(
    p,
    shapes = list(linea_vertical(0, INK_MUTED, "dot", 1),
                  list(type = "line", x0 = 0, x1 = 1, xref = "paper", y0 = UMBRAL, y1 = UMBRAL,
                       line = list(color = INK_MUTED, dash = "dot", width = 1))),
    annotations = list(list(x = z, y = pz, showarrow = FALSE,
                            text = paste0("<b>este perfil: ", pct(pz * 100, 0), "</b>"),
                            xanchor = if (z > 0) "right" else "left", xshift = if (z > 0) -14 else 14,
                            font = list(size = 12, color = INK), bgcolor = "rgba(255,255,255,0.85)")),
    xaxis = eje_x(title = list(text = "Suma de efectos (log-odds de GBM)"), range = c(-lim, lim)),
    yaxis = eje_y(title = list(text = "Probabilidad de GBM"), range = c(-0.03, 1.03), tickformat = ".0%"),
    legend = leyenda(orientation = "h", y = 1.12, x = 0, xanchor = "left", yanchor = "bottom")
  )
  aplicar_tema(p, height = 340, margin = list(l = 10, r = 20, t = 50, b = 45))
}

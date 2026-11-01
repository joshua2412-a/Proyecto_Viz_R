# =========================================================================== #
# Figuras del análisis exploratorio (plotly)
#
# Versión en R de utils/figures.py. Mismos criterios:
#   * magnitud -> barras de un solo tono; identidad -> azul LGG / coral GBM;
#     polaridad -> escala divergente con gris en el cero.
#   * la identidad nunca depende solo del color: leyenda o etiqueta directa.
# =========================================================================== #

TONO_BARRA <- SEQ_BLUE[5]

# Orden explícito de un eje de categorías (plotly respeta el del vector)
orden_categorias <- function(valores, ...) {
  eje_y(categoryorder = "array", categoryarray = as.list(valores), ...)
}

# --------------------------------------------------------------------------- #
# 1. Composición de la cohorte (dona)
# --------------------------------------------------------------------------- #
fig_distribucion_grado <- function(ambito = "train") {
  datos <- proporcion_grado(ambito)
  total <- sum(datos$pacientes)
  p <- plotly::plot_ly(
    labels = datos$grado, values = datos$pacientes, type = "pie", hole = 0.58,
    sort = FALSE, direction = "counterclockwise",
    marker = list(colors = unname(COLOR_GRADE_MAP[datos$grado]),
                  line = list(color = BG_CARD, width = 2)),
    textinfo = "label+percent", textposition = "outside",
    textfont = list(size = 13, color = INK_SOFT),
    hovertemplate = "<b>%{label}</b><br>%{value} pacientes<br>%{percent}<extra></extra>"
  )
  p <- plotly::layout(
    p, showlegend = FALSE,
    annotations = list(list(
      text = paste0("<b>", total, "</b><br><span style='font-size:12px'>pacientes</span>"),
      showarrow = FALSE, font = list(size = 26, color = INK),
      x = 0.5, y = 0.5, xref = "paper", yref = "paper"
    ))
  )
  aplicar_tema(p, height = 360)
}

# --------------------------------------------------------------------------- #
# 2. Edad al diagnóstico
# --------------------------------------------------------------------------- #
fig_edad_por_grado <- function(ambito = "train", nbins = 28) {
  df <- get_df(ambito)
  p <- plotly::plot_ly()
  formas <- list()
  notas <- list()
  for (grado in ORDEN_GRADE) {
    edades <- df$Age_at_diagnosis[df$grade_label == grado]
    color <- COLOR_GRADE_MAP[[grado]]
    p <- plotly::add_trace(
      p, x = edades, type = "histogram", name = grado, histnorm = "percent",
      nbinsx = nbins, bingroup = "edad", opacity = 0.72,
      marker = list(color = color, line = list(color = BG_CARD, width = 1)),
      hovertemplate = "Edad %{x:.0f} años<br>%{y:.1f}% del grupo<extra>%{fullData.name}</extra>"
    )
    mediana <- stats::median(edades)
    formas[[length(formas) + 1]] <- list(
      type = "line", x0 = mediana, x1 = mediana, y0 = 0, y1 = 1, yref = "paper",
      line = list(color = color, width = 2, dash = "dot")
    )
    notas[[length(notas) + 1]] <- list(
      x = mediana, y = 1, yref = "paper", yanchor = "bottom", yshift = 5,
      text = paste0("mediana ", grado, ": ", num(mediana)), showarrow = FALSE,
      font = list(size = 11, color = color)
    )
  }
  p <- plotly::layout(
    p, barmode = "overlay", shapes = formas, annotations = notas,
    xaxis = eje_x(title = list(text = etiqueta("Age_at_diagnosis"))),
    yaxis = eje_y(title = list(text = "% dentro de cada grado")),
    legend = leyenda(y = 1.16, yanchor = "bottom")
  )
  aplicar_tema(p, height = 400, margin = list(l = 56, r = 20, t = 84, b = 48))
}

fig_boxplot_edad <- function(ambito = "train") {
  df <- get_df(ambito)
  resumen <- estadisticas_edad(ambito)
  p <- plotly::plot_ly()
  notas <- list()
  for (grado in ORDEN_GRADE) {
    edades <- df$Age_at_diagnosis[df$grade_label == grado]
    color <- COLOR_GRADE_MAP[[grado]]
    p <- plotly::add_trace(
      p, y = edades, x = rep(grado, length(edades)), type = "box", name = grado,
      boxpoints = "outliers",
      marker = list(color = color, size = 6, line = list(color = BG_CARD, width = 1)),
      line = list(color = color),
      fillcolor = grDevices::adjustcolor(color, alpha.f = 0.5),
      hovertemplate = "%{y:.1f} años<extra>%{x}</extra>"
    )
    notas[[length(notas) + 1]] <- list(
      x = grado, y = 1, yref = "paper", yanchor = "bottom", yshift = 5,
      text = paste0("media ", num(resumen[grado, "media"]), " ± ", num(resumen[grado, "desviacion"])),
      showarrow = FALSE, font = list(size = 11, color = INK_SOFT)
    )
  }
  p <- plotly::layout(
    p, showlegend = FALSE, annotations = notas,
    xaxis = eje_x(title = list(text = ""), categoryorder = "array",
                  categoryarray = as.list(ORDEN_GRADE)),
    yaxis = eje_y(title = list(text = etiqueta("Age_at_diagnosis")))
  )
  aplicar_tema(p, height = 400, margin = list(l = 56, r = 20, t = 46, b = 48))
}

# --------------------------------------------------------------------------- #
# 3. Variables clínicas categóricas
# --------------------------------------------------------------------------- #
fig_clinica_por_grado <- function(variable, ambito = "train") {
  tabla <- distribucion_clinica(variable, ambito)
  categorias <- unique(tabla$categoria)
  p <- plotly::plot_ly()
  for (grado in ORDEN_GRADE) {
    parte <- tabla[tabla$grade_label == grado, ]
    p <- plotly::add_trace(
      p, x = parte$porcentaje, y = parte$categoria, type = "bar", orientation = "h",
      name = grado, text = pct_v(parte$porcentaje), textposition = "outside",
      textfont = list(size = 11, color = INK_SOFT), cliponaxis = FALSE,
      customdata = parte$pacientes,
      marker = list(color = COLOR_GRADE_MAP[[grado]], line = list(color = BG_CARD, width = 1)),
      hovertemplate = paste0("<b>%{y}</b><br>%{x:.1f}% del grado<br>%{customdata} pacientes",
                             "<extra>%{fullData.name}</extra>")
    )
  }
  p <- plotly::layout(
    p, barmode = "group", bargap = 0.3,
    xaxis = eje_x(title = list(text = "% dentro de cada grado"), showgrid = TRUE,
                  gridcolor = GRID, range = c(0, 105)),
    yaxis = orden_categorias(categorias, title = list(text = ""))
  )
  altura <- if (variable == "Gender") 320 else 420
  aplicar_tema(p, height = altura, margin = list(l = 210, r = 60, t = 56, b = 52))
}

# --------------------------------------------------------------------------- #
# 4. Prevalencia de mutaciones
# --------------------------------------------------------------------------- #
fig_prevalencia_genes <- function(ambito = "train", top_n = 12) {
  datos <- prevalencia_genes(ambito)
  # Orden por brecha absoluta, de menor a mayor: la mayor queda arriba
  datos <- datos[order(abs(datos$diferencia)), ]
  datos <- utils::tail(datos, top_n)
  p <- plotly::plot_ly()
  for (grado in ORDEN_GRADE) {
    p <- plotly::add_trace(
      p, y = datos$gen, x = datos[[grado]], type = "bar", orientation = "h", name = grado,
      marker = list(color = COLOR_GRADE_MAP[[grado]], line = list(color = BG_CARD, width = 1)),
      text = pct_v(datos[[grado]]), textposition = "outside",
      textfont = list(size = 11, color = INK_SOFT), cliponaxis = FALSE,
      hovertemplate = paste0("<b>%{y}</b><br>Mutado en %{x:.1f}% de los casos<extra>", grado, "</extra>")
    )
  }
  p <- plotly::layout(
    p, barmode = "group", bargap = 0.28,
    xaxis = eje_x(title = list(text = "% de pacientes con el gen mutado"),
                  showgrid = TRUE, gridcolor = GRID),
    yaxis = orden_categorias(datos$gen, title = list(text = ""))
  )
  aplicar_tema(p, height = 32 * top_n + 140, margin = list(l = 90, r = 70, t = 56, b = 52))
}

# --------------------------------------------------------------------------- #
# 5. Asociación con el grado (polaridad)
# --------------------------------------------------------------------------- #
fig_asociacion_grado <- function(ambito = "train", top_n = 14) {
  datos <- utils::head(asociacion_con_grado(ambito), top_n)
  datos <- datos[!is.na(datos$rho), ]
  datos$nombre <- vapply(datos$variable, function(v) sub("Mutación en ", "", etiqueta(v)),
                         character(1), USE.NAMES = FALSE)
  datos <- datos[order(datos$rho), ]
  holgura <- 0.12
  rango <- c(min(min(datos$rho), 0) - holgura, max(max(datos$rho), 0) + holgura)

  p <- plotly::plot_ly()
  # Mismo orden de trazas que px.bar en Python: según aparecen en los datos
  efectos <- unique(ifelse(datos$rho >= 0, "Hacia GBM", "Hacia LGG"))
  for (efecto in efectos) {
    parte <- datos[ifelse(datos$rho >= 0, "Hacia GBM", "Hacia LGG") == efecto, ]
    color <- if (efecto == "Hacia GBM") COLOR_GBM else COLOR_LGG
    p <- plotly::add_trace(
      p, x = parte$rho, y = parte$nombre, type = "bar", orientation = "h", name = efecto,
      text = num_v(parte$rho, 2, signo = TRUE), textposition = "outside",
      textfont = list(size = 11, color = INK_SOFT), cliponaxis = FALSE,
      customdata = lapply(seq_len(nrow(parte)), function(i) {
        list(parte$p_valor[i], if (is.na(parte$v_cramer[i])) NULL else parte$v_cramer[i],
             parte$tipo[i])
      }),
      marker = list(
        color = color, line = list(color = BG_CARD, width = 1),
        # Patrón para las que no superan los dos filtros: no se leen como señal
        pattern = list(shape = ifelse(parte$significativa, "", "/"))
      ),
      hovertemplate = paste0("<b>%{y}</b> · %{customdata[2]}<br>Spearman r = %{x:.3f}",
                             "<br>V de Cramér = %{customdata[1]:.3f}",
                             "<br>p = %{customdata[0]:.4f}<extra></extra>")
    )
  }
  p <- plotly::layout(
    p, bargap = 0.3,
    xaxis = eje_x(title = list(text = "Correlación de Spearman con el grado (0 = LGG, 1 = GBM)"),
                  showgrid = TRUE, gridcolor = GRID, zeroline = TRUE, zerolinecolor = AXIS,
                  range = rango),
    yaxis = orden_categorias(datos$nombre, title = list(text = ""))
  )
  aplicar_tema(p, height = 32 * top_n + 140, margin = list(l = 170, r = 70, t = 56, b = 52))
}

# --------------------------------------------------------------------------- #
# 6. Multicolinealidad entre mutaciones
# --------------------------------------------------------------------------- #
fig_matriz_genes <- function(ambito = "train", top_n = 12) {
  m <- matriz_asociacion_genes(ambito, top_n = top_n)
  valores <- m
  diag(valores) <- NA
  p <- plotly::plot_ly(
    z = valores, x = colnames(m), y = rownames(m), type = "heatmap",
    zmin = 0, zmax = max(0.6, max(valores, na.rm = TRUE)),
    colorscale = escala_seq(), xgap = 2, ygap = 2,
    texttemplate = "%{z:.2f}", textfont = list(size = 10),
    colorbar = list(title = list(text = "V"), thickness = 12, len = 0.75),
    hovertemplate = "<b>%{y}</b> vs <b>%{x}</b><br>V de Cramér = %{z:.3f}<extra></extra>"
  )
  p <- plotly::layout(
    p,
    xaxis = eje_x(side = "bottom", tickangle = -45, title = list(text = "")),
    yaxis = eje_y(autorange = "reversed", showgrid = FALSE, title = list(text = ""))
  )
  aplicar_tema(p, height = 520, margin = list(l = 90, r = 24, t = 40, b = 90))
}

# --------------------------------------------------------------------------- #
# 7. Exploración interactiva: la forma la decide el tipo de variable
# --------------------------------------------------------------------------- #
fig_univariada <- function(variable, ambito = "train") {
  nombre <- nombre_variable(variable)
  if (tipo_variable(variable) == TIPO_NUMERICA) {
    serie <- get_df(ambito)[[variable]]
    mediana <- stats::median(serie)
    p <- plotly::plot_ly(
      x = serie, type = "histogram", nbinsx = 30,
      marker = list(color = TONO_BARRA, line = list(color = BG_CARD, width = 1)),
      hovertemplate = "%{x:.0f} años<br>%{y} pacientes<extra></extra>"
    )
    p <- plotly::layout(
      p, showlegend = FALSE, bargap = 0.04,
      shapes = list(list(type = "line", x0 = mediana, x1 = mediana, y0 = 0, y1 = 1,
                         yref = "paper", line = list(color = INK_SOFT, width = 1.2, dash = "dash"))),
      annotations = list(list(x = mediana, y = 1, yref = "paper", yanchor = "bottom",
                              yshift = 5, text = paste("mediana", num(mediana, 0)),
                              showarrow = FALSE, font = list(size = 11, color = INK_SOFT))),
      xaxis = eje_x(title = list(text = etiqueta(variable))),
      yaxis = eje_y(title = list(text = "Pacientes"))
    )
    return(aplicar_tema(p, height = 330, margin = list(l = 56, r = 20, t = 40, b = 48)))
  }

  datos <- distribucion_univariada(variable, ambito)
  p <- plotly::plot_ly(
    x = datos$pacientes, y = datos$categoria, type = "bar", orientation = "h",
    marker = list(color = TONO_BARRA), text = pct_v(datos$porcentaje),
    textposition = "outside", textfont = list(size = 11, color = INK_SOFT),
    cliponaxis = FALSE, hovertemplate = "<b>%{y}</b><br>%{x} pacientes<extra></extra>"
  )
  p <- plotly::layout(
    p, showlegend = FALSE,
    xaxis = eje_x(title = list(text = "Pacientes"), showgrid = TRUE, gridcolor = GRID,
                  range = c(0, max(datos$pacientes) * 1.18)),
    # La más frecuente arriba
    yaxis = orden_categorias(datos$categoria, title = list(text = ""), autorange = "reversed")
  )
  aplicar_tema(p, height = max(230, 74 + 46 * nrow(datos)),
               margin = list(l = 8, r = 64, t = 20, b = 44))
}

fig_bivariada <- function(variable, ambito = "train") {
  if (tipo_variable(variable) == TIPO_NUMERICA) {
    df <- get_df(ambito)
    p <- plotly::plot_ly()
    for (grado in ORDEN_GRADE) {
      p <- plotly::add_trace(
        p, x = df[[variable]][df$grade_label == grado], type = "histogram", name = grado,
        nbinsx = 26, histnorm = "percent", bingroup = "biv", opacity = 0.78,
        marker = list(color = COLOR_GRADE_MAP[[grado]], line = list(color = BG_CARD, width = 1)),
        hovertemplate = paste0("<b>", grado, "</b><br>%{x:.0f} años<br>%{y:.1f} % del grupo<extra></extra>")
      )
    }
    p <- plotly::layout(
      p, barmode = "overlay", bargap = 0.04,
      xaxis = eje_x(title = list(text = etiqueta(variable))),
      yaxis = eje_y(title = list(text = "% dentro de cada grado"))
    )
    return(aplicar_tema(p, height = 360))
  }

  tabla <- tabla_bivariada(variable, ambito)
  gbm <- tabla[tabla$grade_label == "GBM", ]
  orden <- gbm$categoria[order(gbm$porcentaje)]
  orden <- c(setdiff(unique(tabla$categoria), orden), orden)
  tamanos <- tapply(tabla$total_categoria, tabla$categoria, function(x) x[1])
  etiquetas <- paste0(orden, "  (n = ", tamanos[orden], ")")

  p <- plotly::plot_ly()
  for (grado in ORDEN_GRADE) {
    parte <- tabla[tabla$grade_label == grado, ]
    porcentaje <- parte$porcentaje[match(orden, parte$categoria)]
    pacientes <- parte$pacientes[match(orden, parte$categoria)]
    porcentaje[is.na(porcentaje)] <- 0
    pacientes[is.na(pacientes)] <- 0
    p <- plotly::add_trace(
      p, x = porcentaje, y = etiquetas, type = "bar", orientation = "h", name = grado,
      marker = list(color = COLOR_GRADE_MAP[[grado]], line = list(color = BG_CARD, width = 1)),
      text = ifelse(porcentaje >= 9, pct_v(porcentaje, 0), ""), textposition = "inside",
      insidetextanchor = "middle", textfont = list(size = 11, color = "#FFFFFF"),
      customdata = pacientes,
      hovertemplate = paste0("<b>", grado, "</b><br>%{x:.1f} % de la categoría",
                             "<br>%{customdata} pacientes<extra></extra>")
    )
  }
  gbm_cohorte <- mean(get_df(ambito)$grade_label == "GBM") * 100
  corte <- 100 - gbm_cohorte
  anclaje <- if (corte < 22) "left" else if (corte > 78) "right" else "center"
  p <- plotly::layout(
    p, barmode = "stack",
    legend = leyenda(y = 1.14, yanchor = "bottom", traceorder = "normal"),
    shapes = list(list(type = "line", x0 = corte, x1 = corte, y0 = 0, y1 = 1, yref = "paper",
                       line = list(color = INK_SOFT, width = 1.2, dash = "dot"))),
    annotations = list(list(x = corte, y = 1, yref = "paper", yanchor = "bottom", yshift = 5,
                            xanchor = anclaje, text = paste("GBM en la cohorte:", pct(gbm_cohorte, 0)),
                            showarrow = FALSE, font = list(size = 11, color = INK_SOFT))),
    xaxis = eje_x(title = list(text = "% dentro de la categoría"), range = c(0, 100),
                  showgrid = TRUE, gridcolor = GRID, ticksuffix = " %"),
    yaxis = orden_categorias(etiquetas, title = list(text = ""))
  )
  aplicar_tema(p, height = max(260, 118 + 52 * length(orden)),
               margin = list(l = 8, r = 20, t = 86, b = 44))
}

fig_matriz_spearman <- function(ambito = "train") {
  m <- matriz_spearman(ambito)
  etiquetas <- colnames(m)
  valores <- m
  valores[upper.tri(valores)] <- NA
  p <- plotly::plot_ly(
    z = valores, x = etiquetas, y = etiquetas, type = "heatmap",
    colorscale = DIV_BLUE_CORAL, zmid = 0, zmin = -1, zmax = 1, xgap = 1, ygap = 1,
    hovertemplate = "<b>%{y}</b> y <b>%{x}</b><br>r = %{z:.2f}<extra></extra>",
    colorbar = list(title = list(text = "r", side = "top", font = list(size = 11, color = INK_SOFT)),
                    thickness = 11, len = 0.8, tickfont = list(size = 10, color = INK_SOFT),
                    outlinewidth = 0)
  )
  p <- plotly::layout(
    p,
    xaxis = eje_x(showgrid = FALSE, tickangle = -55, tickfont = list(size = 10, color = INK_MUTED),
                  ticks = "", title = list(text = "")),
    yaxis = eje_y(showgrid = FALSE, autorange = "reversed",
                  tickfont = list(size = 10, color = INK_MUTED), ticks = "", title = list(text = ""))
  )
  aplicar_tema(p, height = 620, margin = list(l = 4, r = 4, t = 10, b = 4))
}

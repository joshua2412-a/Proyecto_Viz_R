# =========================================================================== #
# Tema visual: paleta sobria y plantilla de plotly
#
# Solo dos colores significan algo en todo el tablero, y siempre lo mismo:
#   COLOR_LGG -> glioma de bajo grado      COLOR_GBM -> glioblastoma
# Son los del libro y los del tablero de Dash. Todo lo demás es neutro.
# =========================================================================== #

BG_PAGE <- "#FFFFFF"
BG_CARD <- "#FFFFFF"
BG_SOFT <- "#F2F3FA"

HEADER_BG <- "#141B4D"

INK <- "#121A3A"
INK_SOFT <- "#3B4266"
INK_MUTED <- "#5D6487"
HAIRLINE <- "#E6E8F2"
GRID <- "#ECEEF6"
AXIS <- "#CDD1E6"

COLOR_LGG <- "#4E8CD9"
COLOR_GBM <- "#E0785A"
COLOR_MARK <- "#141B4D"
ACCENT_NEUTRAL <- "#141B4D"
COLOR_GRADE_MAP <- c(LGG = COLOR_LGG, GBM = COLOR_GBM)
ORDEN_GRADE <- c("LGG", "GBM")

TONOS_MODELO <- c(logistica = "#1E4E86", random_forest = "#52698A", xgboost = "#7A8BA3")
TRAZO_MODELO <- c(logistica = "solid", random_forest = "dash", xgboost = "dot")

SEQ_BLUE <- c("#F2F5FA", "#D8E3F3", "#B0C8E7", "#84A9D6", "#5C8AC4", "#3A6CAB", "#1F4E85")

# Escala de plotly a partir de la rampa: lista de pares (posición, color)
escala_seq <- function(colores = SEQ_BLUE) {
  posiciones <- seq(0, 1, length.out = length(colores))
  lapply(seq_along(colores), function(i) list(posiciones[i], colores[i]))
}

DIV_BLUE_CORAL <- list(
  list(0.0, "#1F4E85"),
  list(0.25, "#B0C8E7"),
  list(0.5, "#EDF1F7"),
  list(0.75, "#F0B49C"),
  list(1.0, "#C2542F")
)

STATUS_GOOD <- "#1F7A43"
STATUS_WARNING <- "#9A6B08"
STATUS_CRITICAL <- "#B3261E"

FONT_FAMILY <- '"Plus Jakarta Sans", system-ui, -apple-system, "Segoe UI", Roboto, sans-serif'
FONT_MONO <- 'ui-monospace, SFMono-Regular, "SF Mono", Menlo, Consolas, monospace'

# Plantilla "sobrio": se pasa a plotly.js como `layout.template`, así afecta a
# todos los ejes (también a xaxis2, yaxis2... de los subgráficos).
PLANTILLA_SOBRIA <- list(
  layout = list(
    colorway = list(COLOR_LGG, COLOR_GBM),
    separators = ",.",
    font = list(family = FONT_FAMILY, size = 13, color = INK_SOFT),
    title = list(font = list(size = 15, color = INK), x = 0, xanchor = "left"),
    paper_bgcolor = BG_CARD,
    plot_bgcolor = BG_CARD,
    hoverlabel = list(
      bgcolor = BG_CARD, bordercolor = HAIRLINE,
      font = list(family = FONT_FAMILY, size = 12, color = INK)
    ),
    legend = list(
      orientation = "h", yanchor = "bottom", y = 1.02, xanchor = "right", x = 1,
      bgcolor = "rgba(0,0,0,0)", font = list(size = 12, color = INK_SOFT)
    ),
    xaxis = list(
      showgrid = FALSE, zeroline = FALSE, linecolor = AXIS, linewidth = 1,
      ticks = "outside", ticklen = 4, tickcolor = AXIS,
      tickfont = list(color = INK_MUTED, size = 12),
      title = list(font = list(color = INK_SOFT, size = 12)),
      automargin = TRUE
    ),
    yaxis = list(
      gridcolor = GRID, gridwidth = 1, zeroline = FALSE,
      linecolor = "rgba(0,0,0,0)", ticks = "",
      tickfont = list(color = INK_MUTED, size = 12),
      title = list(font = list(color = INK_SOFT, size = 12)),
      automargin = TRUE
    )
  )
)

# Último paso de cada figura: plantilla, altura, márgenes y barra de modos.
# `...` son atributos de layout que se aplican encima de la plantilla.
aplicar_tema <- function(p, height = NULL, margin = list(l = 56, r = 20, t = 44, b = 48), ...) {
  # plotly avisa de que `height` en layout() está obsoleto, pero es la forma de
  # que la figura mande sobre su contenedor (plotlyOutput(height = "auto")).
  p <- suppressWarnings(plotly::layout(
    p,
    template = PLANTILLA_SOBRIA,
    separators = ",.",
    font = list(family = FONT_FAMILY, size = 13, color = INK_SOFT),
    paper_bgcolor = BG_CARD,
    plot_bgcolor = BG_CARD,
    margin = margin,
    height = height,
    ...
  ))
  p <- plotly::config(p, displaylogo = FALSE, responsive = TRUE, locale = "es")
  p
}

# Eje con el estilo de la plantilla, para pasarlo explícito cuando se tocan
# otras propiedades del mismo eje (plotly en R fusiona listas, no plantillas).
eje_x <- function(...) {
  utils::modifyList(PLANTILLA_SOBRIA$layout$xaxis, list(...))
}

eje_y <- function(...) {
  utils::modifyList(PLANTILLA_SOBRIA$layout$yaxis, list(...))
}

leyenda <- function(...) {
  utils::modifyList(PLANTILLA_SOBRIA$layout$legend, list(...))
}

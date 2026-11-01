# =========================================================================== #
# Componentes reutilizables de interfaz
#
# Versión en htmltools de utils/components.py: cada función devuelve el mismo
# HTML (mismas clases de Bootstrap 5 y de www/style.css) que su equivalente de
# Dash, así que el tablero se ve igual en las dos versiones.
# =========================================================================== #

# --------------------------------------------------------------------------- #
# Rejilla de Bootstrap
# --------------------------------------------------------------------------- #
fila <- function(..., class = "") {
  div(class = trimws(paste("row", class)), ...)
}

columna <- function(..., md = NULL, lg = NULL, xl = NULL, sm = NULL, xs = 12, class = "") {
  clases <- c(
    if (!is.null(xs)) paste0("col-", xs),
    if (!is.null(sm)) paste0("col-sm-", sm),
    if (!is.null(md)) paste0("col-md-", md),
    if (!is.null(lg)) paste0("col-lg-", lg),
    if (!is.null(xl)) paste0("col-xl-", xl),
    class
  )
  div(class = paste(clases[nzchar(clases)], collapse = " "), ...)
}

# Íconos de Material Symbols (Google Fonts, fonts.google.com/icons): se escribe
# el nombre y la ligadura de la fuente lo convierte en el dibujo.
icono <- function(nombre, class = "") {
  span(class = trimws(paste("material-symbols-rounded", class)), `aria-hidden` = "true", nombre)
}

# --------------------------------------------------------------------------- #
# Cabeceras y textos
# --------------------------------------------------------------------------- #
page_header <- function(titulo, subtitulo, icono_clase = "") {
  div(
    class = "page-header",
    tags$h2(
      class = "page-title",
      if (nzchar(icono_clase)) icono(icono_clase, "page-icon"),
      # "Contexto clínico: dos tumores..." se compone en dos pesos, como los
      # titulares: lo de antes de los dos puntos en fino y el resto en negrita.
      if (grepl(": ", titulo, fixed = TRUE)) {
        partes <- strsplit(titulo, ": ", fixed = TRUE)[[1]]
        span(span(class = "page-title-ligero", paste0(partes[1], ":")), tags$br(),
             paste(partes[-1], collapse = ": "))
      } else {
        span(titulo)
      }
    ),
    p(class = "page-subtitle", subtitulo),
    tags$hr(class = "page-rule")
  )
}

# Titular en dos pesos de la mezcla A + B ("Describe un paciente / y mira qué
# predicen"), con el control principal de la pestaña a la derecha si lo hay.
titular <- function(eyebrow, linea, destacado, derecha = NULL, ayuda = "", texto = "") {
  div(
    class = "titular-fila",
    div(
      class = "titular-izquierda",
      div(class = "titular-eyebrow", eyebrow),
      tags$h2(class = "titular", linea, tags$br(), tags$strong(destacado)),
      if (nzchar(texto)) p(class = "titular-texto", texto)
    ),
    if (!is.null(derecha)) {
      div(class = "titular-selector",
          if (nzchar(ayuda)) div(class = "titular-ayuda", ayuda),
          derecha)
    }
  )
}

section_title <- function(texto, nivel = 4) {
  etiqueta_h <- switch(as.character(nivel), "3" = tags$h3, "5" = tags$h5, tags$h4)
  etiqueta_h(class = "section-title", texto)
}

paragraph <- function(...) {
  p(class = "body-text", ...)
}

bullet_list <- function(items, color = ACCENT_NEUTRAL) {
  tags$ul(
    class = "bullet-list",
    style = paste0("--bullet-color: ", color, ";"),
    lapply(items, function(item) tags$li(class = "body-text", item))
  )
}

enlace_externo <- function(texto, url, icono_nombre = "arrow_outward") {
  tags$a(
    texto, " ", icono(icono_nombre, "icono-enlace"),
    href = url, target = "_blank", rel = "noopener noreferrer",
    class = "link-externo"
  )
}

# Botón que lleva a otra pestaña del tablero (lo recoge www/navegacion.js)
enlace_pestana <- function(..., destino, class = "") {
  tags$button(type = "button", class = class, `data-ir` = destino, ...)
}

# --------------------------------------------------------------------------- #
# Tarjetas
# --------------------------------------------------------------------------- #
card <- function(..., titulo = NULL, subtitulo = NULL, color = NULL,
                 class = "", tono = "", id = NULL) {
  clases <- c("card", "soft-card", if (nzchar(tono)) paste0("tono-", tono), class)
  div(
    id = id,
    class = paste(clases[nzchar(clases)], collapse = " "),
    style = if (!is.null(color)) paste0("border-top: 2px solid ", color, ";"),
    div(
      class = "card-body",
      if (!is.null(titulo)) tags$h5(class = "card-title-custom", titulo),
      if (!is.null(subtitulo)) p(class = "card-subtitle-custom", subtitulo),
      div(...)
    )
  )
}

kpi_card <- function(valor, etiqueta, detalle = "", color = ACCENT_NEUTRAL) {
  div(
    class = "card soft-card kpi-card",
    div(class = "kpi-accent", style = paste0("background: ", color, ";")),
    div(
      class = "card-body",
      div(class = "kpi-label", etiqueta),
      div(class = "kpi-value", valor),
      if (nzchar(detalle)) div(class = "kpi-detail", detalle)
    )
  )
}

# `kpis`: lista de listas con valor, etiqueta, detalle y color
kpi_row <- function(kpis, md = 3) {
  fila(
    class = "g-3",
    lapply(kpis, function(k) {
      columna(do.call(kpi_card, k), md = md, sm = 6, xs = 12, class = "mb-3")
    })
  )
}

callout <- function(texto, titulo = NULL, color = ACCENT_NEUTRAL) {
  div(
    class = "callout",
    style = paste0("border-left: 3px solid ", color, "; background: ", BG_SOFT, ";"),
    if (!is.null(titulo)) div(class = "callout-title", titulo),
    div(class = "callout-text", texto)
  )
}

# --------------------------------------------------------------------------- #
# Piezas de la exploración interactiva
# --------------------------------------------------------------------------- #
banda_oscura <- function(titulo, texto, eyebrow = "") {
  div(
    class = "banda-oscura",
    if (nzchar(eyebrow)) div(class = "banda-eyebrow", eyebrow),
    tags$h3(class = "banda-titulo", titulo),
    p(class = "banda-texto", texto)
  )
}

# `filas`: lista de pares c(etiqueta, valor)
stat_list <- function(filas) {
  div(
    class = "stat-list",
    lapply(filas, function(f) {
      div(
        class = "stat-fila",
        span(class = "stat-etiqueta", f[[1]]),
        span(class = "stat-valor", f[[2]])
      )
    })
  )
}

badge_numero <- function(texto, variante = "") {
  span(class = if (variante == "gbm") "badge-numero badge-gbm" else "badge-numero", texto)
}

chip_tipo <- function(texto) span(class = "chip-tipo", texto)

lectura_guiada <- function(lineas, titulo = "Lectura guiada") {
  card(
    tags$ul(class = "lectura-lista", lapply(lineas, tags$li)),
    titulo = titulo,
    class = "lectura-card"
  )
}

desplegable <- function(titulo, contenido, abierto = FALSE) {
  panel <- bslib::accordion_panel(title = titulo, contenido)
  bslib::accordion(
    panel,
    open = abierto,
    multiple = TRUE,
    class = "desplegable"
  )
}

# --------------------------------------------------------------------------- #
# Gráficos y tablas
# --------------------------------------------------------------------------- #
# La figura de plotly fija su propia altura (ver aplicar_tema), así que el
# contenedor va en "auto" y crece con ella.
grafico <- function(id) {
  div(class = "graph", plotly::plotlyOutput(id, height = "auto"))
}

graph_card <- function(id, titulo, nota = "") {
  card(
    tags$h5(class = "card-title-custom", titulo),
    grafico(id),
    if (nzchar(nota)) div(class = "graph-note", style = paste0("color: ", INK_MUTED, ";"), nota)
  )
}

# `filas`: lista de vectores (o listas) de celdas
data_table <- function(encabezados, filas, resaltar_primera_columna = TRUE) {
  div(
    class = "table-responsive",
    tags$table(
      class = "table table-hover soft-table",
      tags$thead(tags$tr(lapply(encabezados, tags$th))),
      tags$tbody(
        lapply(filas, function(f) {
          celdas <- lapply(seq_along(f), function(i) {
            clase <- if (i == 1 && resaltar_primera_columna) "cell-strong" else NULL
            tags$td(class = clase, f[[i]])
          })
          tags$tr(celdas)
        })
      )
    )
  )
}

legend_chip <- function(texto, color) {
  span(
    class = "legend-chip",
    span(class = "chip-dot", style = paste0("background: ", color, ";")),
    texto
  )
}

series_chips <- function() {
  div(
    class = "legend-chips",
    legend_chip("LGG · glioma de bajo grado", COLOR_LGG),
    legend_chip("GBM · glioblastoma multiforme", COLOR_GBM)
  )
}

# Aviso cuando una pestaña no se puede construir (falta un archivo, etc.)
aviso_error <- function(error) {
  div(
    class = "alert alert-danger mt-4",
    style = paste0("border-left: 3px solid ", STATUS_CRITICAL, ";"),
    tags$h5(class = "mb-2", "No se pudo construir esta pestaña"),
    div(conditionMessage(error)),
    tags$hr(),
    div(class = "small", "Revisa que existan los archivos de la carpeta data/ (ver README.md).")
  )
}

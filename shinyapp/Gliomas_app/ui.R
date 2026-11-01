# =========================================================================== #
# Interfaz: barra superior, navegación, contenido de la pestaña activa y pie
#
# La estructura es fija y el contenido se monta bajo demanda en server.R
# (output$contenido): solo se construye la pestaña que se está viendo. Los
# identificadores y las clases son los del tablero de Dash (#tabs-principal,
# #contenido-pestana, .vista-pestana...), porque www/banda.js y el resto de
# scripts compartidos los leen directamente.
# =========================================================================== #

bslib::page(
  title = "Gliomas LGG vs GBM · Dashboard Analítico",
  lang = "es",
  # Flatly sobre Bootstrap 5, igual que el tablero de Dash (dbc.themes.FLATLY)
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly",
                   base_font = bslib::font_collection("Plus Jakarta Sans", "system-ui",
                                                      "-apple-system", "Segoe UI", "Roboto",
                                                      "sans-serif")),
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    # Modo claro u oscuro antes del primer pintado: si lo pusiera www/tema.js
    # (que carga después), la página se vería un instante en claro y luego
    # saltaría a oscuro. Este script mínimo solo pone el atributo.
    tags$script(HTML(paste(
      "(function () {",
      "  var guardado = null;",
      "  try { guardado = localStorage.getItem('tema'); } catch (e) {}",
      "  var oscuro = guardado ? guardado === 'oscuro'",
      "    : !!(window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);",
      "  document.documentElement.setAttribute('data-tema', oscuro ? 'oscuro' : 'claro');",
      "})();", sep = "\n"))),
    # Plus Jakarta Sans y los íconos de Material Symbols, de Google Fonts
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Plus+Jakarta+Sans:wght@400;500;600;700&display=swap"),
    tags$link(rel = "stylesheet",
              href = paste0("https://fonts.googleapis.com/css2?family=Material+Symbols+Rounded:",
                            "opsz,wght,FILL,GRAD@20..48,300..600,0..1,0&display=block")),
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$link(rel = "stylesheet", href = "shiny.css"),
    # Fórmulas en LaTeX entre $...$ en cualquier texto (como dcc.Markdown con
    # mathjax=True en Dash). MathJax 2 solo reconoce \( \) por defecto.
    tags$script(type = "text/x-mathjax-config", HTML(paste(
      "MathJax.Hub.Config({",
      "  tex2jax: { inlineMath: [['$', '$'], ['\\\\(', '\\\\)']], processEscapes: true },",
      "  'HTML-CSS': { linebreaks: { automatic: true } }, showMathMenu: false",
      "});", sep = "\n"))),
    # Los scripts van con defer: corren cuando el documento ya está construido,
    # como en Dash, que los carga al final del <body>.
    tags$script(src = "tema.js", defer = NA),
    tags$script(src = "navegacion.js", defer = NA),
    tags$script(src = "indice.js", defer = NA),
    tags$script(src = "contador.js", defer = NA),
    tags$script(src = "intro.js", defer = NA),
    # Cifras de los modelos que dibuja banda.js (antes de cargarlo)
    datos_banda_js(),
    tags$script(src = "banda.js", defer = NA),
    tags$script(src = "enjambre.js", defer = NA),
    tags$script(src = "prediccion.js", defer = NA),
    tags$script(src = "formulas.js", defer = NA)
  ),
  withMathJax(),
  div(
    class = "app-root",
    # Portada animada (www/intro.js): tapa el tablero hasta "Explorar el dashboard"
    portada(),
    barra_superior(),
    barra_navegacion(),
    div(class = "container-fluid page-container",
        div(id = "contenido-pestana", uiOutput("contenido"))),
    pie_pagina(),
    datos_navegacion_js()
  )
)

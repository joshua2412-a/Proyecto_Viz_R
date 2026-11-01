# =========================================================================== #
# Portada animada: un cerebro de partículas con el glioma dentro
#
# Esta función solo arma el marcado (el mismo que utils/intro.py del Dash). La
# animación vive en www/intro.js (WebGL puro, sin librerías) y el estilo en
# www/style.css, sección "Portada".
#
# Las cifras salen del dataset: el número de partículas grandes de cada grado es
# el número de pacientes de ese grado en TCGA.
# =========================================================================== #

portada_rasgo <- function(clase, texto) {
  tags$li(span(class = paste("intro-punto", clase)), span(texto))
}

portada <- function() {
  n_lgg <- sum(DATOS$Grade == 0)
  n_gbm <- sum(DATOS$Grade == 1)
  autores <- paste(vapply(AUTORES, `[[`, character(1), "nombre"), collapse = " · ")
  div(
    id = "intro", class = "intro",
    `data-lgg` = n_lgg, `data-gbm` = n_gbm, `data-modo` = "gbm",
    tags$canvas(class = "intro-lienzo", `aria-hidden` = "true"),
    div(
      class = "intro-panel",
      tags$h1(class = "intro-titulo", "Gliomas de bajo grado", tags$br(), "frente a glioblastoma"),
      p(class = "intro-texto",
        paste0(n_lgg + n_gbm, " pacientes, 23 variables clínicas y genéticas y tres modelos ",
               "para distinguir un grado del otro.")),
      div(
        class = "intro-segmentado", role = "group",
        `aria-label` = "Grado que se muestra en el cerebro",
        tags$button(type = "button", class = "intro-modo", `data-modo` = "gbm",
                    `aria-pressed` = "true", "GBM"),
        tags$button(type = "button", class = "intro-modo", `data-modo` = "lgg",
                    `aria-pressed` = "false", "LGG")
      ),
      tags$ul(
        class = "intro-rasgos intro-rasgos-gbm",
        portada_rasgo("rim", "Anillo grueso e irregular que capta contraste"),
        portada_rasgo("nec", "Necrosis en el centro"),
        portada_rasgo("ede", "Edema e infiltración alrededor")
      ),
      tags$ul(
        class = "intro-rasgos intro-rasgos-lgg",
        portada_rasgo("lgg", "Lesión homogénea y difusa, sin realce"),
        portada_rasgo("lgg-borde", "Bordes bastante definidos"),
        portada_rasgo("sin", "Sin necrosis y con poco edema")
      ),
      tags$button(
        type = "button", class = "intro-entrar",
        span("Explorar el dashboard"),
        span(class = "intro-entrar-icono", icono("arrow_forward"))
      ),
      div(class = "intro-autores", autores)
    ),
    div(
      class = "intro-etiqueta",
      div(class = "intro-etiqueta-gbm", tags$strong("Glioblastoma (GBM)"),
          span(paste(n_gbm, "pacientes"))),
      div(class = "intro-etiqueta-lgg", tags$strong("Glioma de bajo grado (LGG)"),
          span(paste(n_lgg, "pacientes")))
    ),
    div(class = "intro-pista", icono("drag_pan"), span("Arrastra para girar el cerebro")),
    div(class = "intro-nota",
        "Ubicación ilustrativa: el dataset no registra dónde estaba cada tumor.")
  )
}

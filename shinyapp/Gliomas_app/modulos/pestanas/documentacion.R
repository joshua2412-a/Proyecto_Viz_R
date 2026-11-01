# =========================================================================== #
# Pestaña 13 · Documentación
# Enlaza el libro en R capítulo a capítulo, explica cómo se relacionan las
# piezas del proyecto y lo embebe para leerlo sin salir de la app.
# =========================================================================== #

CAPITULOS <- list(
  list(icono = "description", titulo = "Análisis exploratorio",
       descripcion = paste("Univariado, bivariado, pruebas de hipótesis y diagnóstico de",
                           "multicolinealidad, con el desarrollo completo de cada prueba."),
       url = URL_LIBRO_EDA),
  list(icono = "account_tree", titulo = "Modelado predictivo",
       descripcion = paste("Validación anidada, comparación de optimizadores, evaluación en prueba,",
                           "interpretación con SHAP y panel reducido."),
       url = URL_LIBRO_MODELADO),
  list(icono = "data_object", titulo = "Repositorio",
       descripcion = "Código fuente del libro, de esta app y del sitio publicado en GitHub Pages.",
       url = URL_REPO)
)

ENLACES_AUTOR <- list(c("github", "GitHub", "code"), c("linkedin", "LinkedIn", "badge"))

iniciales <- function(nombre) {
  partes <- strsplit(nombre, "\\s+")[[1]][1:2]
  toupper(paste(substr(partes, 1, 1), collapse = ""))
}

ficha_autor <- function(autor) {
  enlaces <- lapply(ENLACES_AUTOR, function(e) {
    if (is.null(autor[[e[1]]])) return(NULL)
    tags$a(href = autor[[e[1]]], target = "_blank", rel = "noopener noreferrer", class = "autor-enlace",
           `aria-label` = paste(e[2], "de", autor$nombre), icono(e[3]), span(e[2]))
  })
  card(div(class = "autor-ficha",
           div(class = "autor-monograma", `aria-hidden` = "true", iniciales(autor$nombre)),
           div(class = "autor-cuerpo",
               div(class = "autor-nombre", autor$nombre),
               div(class = "autor-enlaces", enlaces))))
}

ui_documentacion <- function() {
  div(
    class = "vista-pestana",
    page_header(
      "Documentación del proyecto",
      paste("La app permite explorar el análisis y probar los modelos; el libro lo demuestra paso",
            "a paso, con el desarrollo de cada prueba."),
      "book"
    ),
    fila(
      class = "g-3",
      lapply(CAPITULOS, function(c_) {
        columna(lg = 4, md = 6, xs = 12, class = "mb-3",
                card(div(class = "guide-badge", badge_numero(icono(c_$icono))),
                     div(class = "guide-name", c_$titulo),
                     div(class = "guide-text", c_$descripcion),
                     div(class = "mt-2", enlace_externo("Abrir", c_$url)),
                     class = "guide-card"))
      })
    ),
    section_title("Cómo se relacionan las piezas"),
    fila(
      columna(
        lg = 6, class = "mb-4",
        card(
          paragraph(paste("Libro y app no son dos versiones del mismo contenido: son dos capas del",
                          "mismo proyecto, y comparten los mismos datos y los mismos resultados.")),
          bullet_list(list(
            span(tags$b("rbook_gliomas/ "), "es la fuente del libro en bookdown: los capítulos .Rmd, ",
                 "el dataset, el código del modelado (R/) y sus resultados guardados (resultados/)."),
            span(tags$b("docs/ "), "es el libro compilado que publica GitHub Pages."),
            span(tags$b("shinyapp/Gliomas_app/ "), "es esta app: ui.R y server.R, los módulos de ",
                 "modulos/ y, en data/, el dataset, la partición y los modelos que exporta el libro."),
            span(tags$b("data/modelos_finales.json "), "guarda los modelos entrenados como coeficientes ",
                 "y árboles: la app predice con R base, sin cargar glmnet, ranger ni xgboost.")
          )),
          callout(
            paste("La partición 80/20 es la misma en el libro, en el modelado y en la app",
                  "(train_data.csv y test_data.csv), y los modelos de la app son los que entrena",
                  "el capítulo de modelado del libro, así que las cifras coinciden en las tres piezas."),
            titulo = "Una sola fuente de verdad"
          ),
          titulo = "Arquitectura del proyecto"
        )
      ),
      columna(
        lg = 6, class = "mb-4",
        card(
          paragraph("Para ejecutar la app en local, desde la carpeta del repositorio:"),
          tags$pre(class = "code-block", paste(
            'install.packages(c("shiny", "bslib", "plotly", "jsonlite"))',
            'shiny::runApp("shinyapp/Gliomas_app")', sep = "\n")),
          paragraph("Para compilar el libro (sale en docs/, listo para GitHub Pages):"),
          tags$pre(class = "code-block", paste(
            'setwd("rbook_gliomas")',
            'bookdown::render_book("index.Rmd")', sep = "\n")),
          titulo = "Ejecutar y compilar"
        )
      )
    ),
    section_title("El libro, aquí mismo"),
    card(
      tags$iframe(src = URL_LIBRO, class = "libro-iframe", title = "Libro del proyecto",
                  style = paste("width: 100%; height: 78vh; border: 1px solid #E4E9F0;",
                                "border-radius: 10px; background: #FFFFFF;")),
      div(class = "graph-note mt-2",
          "Si el libro no carga en este marco, ábrelo en una pestaña nueva: ",
          enlace_externo("libro del proyecto", URL_LIBRO)),
      titulo = "Vista embebida",
      subtitulo = "Versión publicada en GitHub Pages"
    ),
    section_title("Autores"),
    fila(class = "g-3", lapply(AUTORES, function(a) columna(md = 6, xs = 12, class = "mb-3", ficha_autor(a))))
  )
}

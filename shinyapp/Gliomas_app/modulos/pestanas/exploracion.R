# =========================================================================== #
# Pestaña 7 · Exploración
# El módulo interactivo: el visitante elige cualquiera de las 23 predictoras y
# la ve sola (univariado) o enfrentada al grado (bivariado).
# =========================================================================== #

VARIABLE_INICIAL <- "Age_at_diagnosis"

exploracion_controles <- function() {
  tamanos <- tamanos_particion()
  card(
    fila(
      class = "align-items-end g-3",
      columna(
        lg = 7, class = "mb-3 mb-lg-0",
        div(class = "control-label", "Variable a explorar"),
        div(class = "selector-variable",
            selectInput("exp_variable", label = NULL, choices = opciones_variables(),
                        selected = VARIABLE_INICIAL, selectize = TRUE, width = "100%"))
      ),
      columna(
        lg = 5,
        div(class = "control-label", "Conjunto de datos"),
        div(class = "ambito-radio",
            radioButtons(
              "exp_ambito", label = NULL, inline = TRUE, selected = "train",
              choiceNames = c(paste0("Entrenamiento (", tamanos[["train"]], ")"),
                              paste0("Prueba (", tamanos[["test"]], ")"),
                              paste0("Completo (", tamanos[["full"]], ")")),
              choiceValues = c("train", "test", "full")
            ))
      )
    ),
    class = "controles-card mb-4"
  )
}

# Ficha sin texto de manual: el nombre, su tipo y los tres datos que hacen falta
# para leer el gráfico de al lado
exploracion_ficha <- function(variable) {
  ficha <- ficha_variable(variable)
  papel <- ficha$papel
  prueba <- if (grepl("·", papel, fixed = TRUE)) trimws(sub("^[^·]*·", "", papel)) else papel
  prueba <- sub("^se contrasta con (la |el )?", "", prueba)
  prueba <- paste0(toupper(substr(prueba, 1, 1)), substr(prueba, 2, nchar(prueba)))
  campo <- function(etiqueta, valor, mono = FALSE) {
    div(class = "ficha-campo",
        div(class = "ficha-etiqueta", etiqueta),
        div(class = if (mono) "ficha-valor ficha-mono" else "ficha-valor", valor))
  }
  div(
    class = "card soft-card ficha-limpia",
    div(
      class = "card-body",
      div(class = "ficha-cabecera",
          span(class = "ficha-nombre", ficha$nombre),
          span(class = "ficha-tipo", tolower(ficha$tipo_nombre))),
      div(class = "ficha-campos",
          campo("En el dataset", ficha$variable, mono = TRUE),
          campo("Prueba", prueba),
          campo("Codificación", ficha$codificacion))
    )
  )
}

exploracion_panel_univariado <- function(variable, ambito) {
  ficha <- ficha_variable(variable)
  resumen <- resumen_univariado(variable, ambito)
  tagList(
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_exp_uni", paste("Distribución de", ficha$nombre),
                         paste0("Sobre el ", AMBITO_NOMBRES[[ambito]], ", sin cruzar con el grado."))),
      columna(lg = 5, class = "mb-4",
              card(stat_list(resumen$filas), titulo = "Resumen estadístico",
                   subtitulo = paste(resumen$n, "pacientes en la selección actual"), tono = "acento"))
    ),
    fila(columna(lg = 12, class = "mb-4", exploracion_ficha(variable)))
  )
}

exploracion_panel_pruebas <- function(variable) {
  prueba <- prueba_bivariada(variable, "train")
  p_texto <- p_tex(prueba$p_valor)
  veredicto <- if (prueba$significativa) {
    paste0("Se rechaza la hipótesis nula (", p_texto, ") y la magnitud supera el umbral ",
           "del proyecto (", prueba$magnitud_nombre, " $= ", tnum(prueba$magnitud, 2), " \\geq ",
           tnum(EFECTO_MINIMO, 2), "$): la asociación es distinguible del azar y además tiene ",
           "tamaño suficiente para que merezca la pena mirarla.")
  } else if (prueba$p_valor < ALFA) {
    paste0("Se rechaza la hipótesis nula (", p_texto, "), pero la magnitud queda por debajo ",
           "del umbral del proyecto (", prueba$magnitud_nombre, " $= ", tnum(prueba$magnitud, 2),
           "$, frente a $", tnum(EFECTO_MINIMO, 2), "$). Significancia no es magnitud: con esta ",
           "muestra, una diferencia pequeña basta para salir significativa.")
  } else {
    paste0("No se rechaza la hipótesis nula (", p_texto, "): los datos son compatibles con ",
           "que esta variable y el grado sean independientes.")
  }
  contenido <- tagList(
    paragraph(prueba$motivo),
    stat_list(c(list(c("Prueba aplicada", prueba$prueba)), prueba$detalle)),
    callout(veredicto, titulo = "Veredicto"),
    if (nzchar(prueba$nota_metodo)) callout(prueba$nota_metodo, titulo = "Cómo se calculó, y por qué así")
  )
  desplegable(paste("Prueba estadística aplicada ·", prueba$prueba), contenido)
}

exploracion_panel_bivariado <- function(variable, ambito) {
  ficha <- ficha_variable(variable)
  nota <- if (ficha$tipo == TIPO_NUMERICA) {
    paste("Cada grado se normaliza a su propio 100 %, porque los dos grupos no tienen el",
          "mismo tamaño. Conjunto de entrenamiento.")
  } else {
    paste("El porcentaje se calcula dentro de cada categoría: la pregunta es qué",
          "proporción de ese grupo acaba siendo GBM. Conjunto de entrenamiento.")
  }
  tagList(
    if (ambito != "train") {
      callout(
        paste0("Tienes seleccionado el ", AMBITO_NOMBRES[[ambito]], ", pero esta comparación ",
               "y sus pruebas se describen siempre sobre el de entrenamiento. El conjunto de ",
               "prueba se apartó para evaluar los modelos, y eso solo vale si nadie ha mirado ",
               "antes cómo se relacionan ahí las variables con el grado. La vista univariada ",
               "sí respeta tu selección: ahí no se cruza nada con el grado."),
        titulo = "Esta vista no cambia de conjunto"
      )
    },
    fila(
      columna(lg = 12, class = "mb-4",
              graph_card("fig_exp_biv", paste(ficha$nombre, "frente al grado tumoral"), nota))
    ),
    exploracion_panel_pruebas(variable)
  )
}

# Subpestañas con el estilo de .sub-tabs (ver www/shiny.css)
subpestanas <- function(id, ...) {
  div(class = "sub-tabs-shiny", tabsetPanel(id = id, type = "tabs", ...))
}

ui_exploracion <- function() {
  div(
    class = "vista-pestana",
    titular(
      "02 El método · Exploración",
      "Elige una variable",
      "y mírala por dentro",
      derecha = div(
        class = "ambito-radio segmentado-oscuro",
        radioButtons("exp_vista", label = NULL, inline = TRUE,
                     choiceNames = c("Univariado", "Frente al grado"),
                     choiceValues = c("uni", "biv"), selected = "uni")
      ),
      ayuda = "Vista",
      texto = "Su distribución, sus descriptivos y la prueba que decide si separa los dos grados."
    ),
    exploracion_controles(),
    div(class = "mb-3", series_chips()),
    uiOutput("exp_panel")
  )
}

server_exploracion <- function(input, output, session) {
  variable <- reactive({
    v <- input$exp_variable
    if (is.null(v) || !v %in% FEATURES) VARIABLE_INICIAL else v
  })
  ambito <- reactive({
    a <- input$exp_ambito
    if (is.null(a) || !a %in% AMBITOS) "train" else a
  })

  vista <- reactive(if (identical(input$exp_vista, "biv")) "biv" else "uni")

  output$exp_panel <- renderUI({
    if (vista() == "biv") exploracion_panel_bivariado(variable(), ambito())
    else exploracion_panel_univariado(variable(), ambito())
  })
  output$fig_exp_uni <- plotly::renderPlotly(fig_univariada(variable(), ambito()))
  output$fig_exp_biv <- plotly::renderPlotly(fig_bivariada(variable(), "train"))
}

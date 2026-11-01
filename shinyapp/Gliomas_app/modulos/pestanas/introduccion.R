# =========================================================================== #
# Pestaña 1 · Introducción
# Portada del tablero: el problema, los indicadores de la cohorte y la guía
# que reparte la lectura del resto de pestañas en cuatro bloques.
# =========================================================================== #

intro_kpis <- function() {
  tamanos <- tamanos_particion()
  prop <- proporcion_grado("full")
  asociacion <- asociacion_con_grado("train")
  prev <- prevalencia_genes("train")
  idh1 <- prev[prev$gen == "IDH1", ]
  list(
    list(valor = as.character(tamanos[["full"]]), etiqueta = "Pacientes analizados",
         detalle = paste0(tamanos[["train"]], " entrenamiento · ", tamanos[["test"]], " prueba"),
         color = ACCENT_NEUTRAL),
    list(valor = pct(prop$porcentaje[prop$grado == "GBM"], 1), etiqueta = "Casos de GBM",
         detalle = "El resto son gliomas de bajo grado (LGG)", color = COLOR_GBM),
    list(valor = paste(sum(asociacion$significativa), "de", nrow(asociacion)),
         etiqueta = "Variables con señal",
         detalle = "Asociadas al grado con $p < 0{,}05$ y $|r| \\geq 0{,}10$", color = ACCENT_NEUTRAL),
    list(valor = paste(num(idh1$LGG - idh1$GBM, 0), "pp"), etiqueta = "Brecha de IDH1",
         detalle = paste0("Mutado en ", pct(idh1$LGG, 1), " de LGG frente a ",
                          pct(idh1$GBM, 1), " de GBM"),
         # La franja parte los dos colores: la cifra es la distancia entre ellos
         color = paste0("linear-gradient(180deg, ", COLOR_LGG, " 0 50%, ", COLOR_GBM, " 50% 100%)"))
  )
}

boton_bloque <- function(tab_id, nombre = NULL, descripcion = NULL) {
  enlace_pestana(
    destino = tab_id, class = "bloque-enlace",
    icono(ICONOS[[tab_id]], "bloque-icono"),
    span(
      class = "bloque-enlace-cuerpo",
      span(class = "bloque-enlace-nombre", if (is.null(nombre)) ETIQUETAS[[tab_id]] else nombre),
      span(class = "bloque-enlace-texto",
           if (is.null(descripcion)) DESCRIPCIONES[[tab_id]] else descripcion)
    ),
    icono("arrow_forward", "bloque-flecha")
  )
}

intro_bloque <- function(bloque) {
  card(
    div(class = "bloque-encabezado",
        span(class = "bloque-numero", bloque$numero),
        span(class = "bloque-titulo", bloque$titulo)),
    p(class = "bloque-resumen", bloque$resumen),
    div(class = "bloque-enlaces", lapply(bloque$pestanas, boton_bloque)),
    class = "bloque-card"
  )
}

intro_tira_documentacion <- function() {
  card(
    fila(
      class = "align-items-center g-3",
      columna(
        lg = 8,
        div(class = "bloque-encabezado",
            icono("book", "bloque-icono"),
            span(class = "bloque-titulo", "Documentación")),
        p(class = "bloque-resumen",
          paste("El desarrollo estadístico completo vive en el libro del proyecto,",
                "escrito en R con bookdown, con el detalle de cada prueba y el",
                "código que la produce."))
      ),
      columna(
        lg = 4,
        div(class = "bloque-enlaces",
            boton_bloque("tab-documentacion", nombre = "Ir a Documentación",
                         descripcion = "El libro en R y el repositorio del proyecto."))
      )
    ),
    class = "bloque-card tira-documentacion"
  )
}

intro_ficha_dataset <- function() {
  card(
    bullet_list(list(
      span(tags$b("Nombre: "), "Glioma Grading Clinical and Mutation Features"),
      span(tags$b("Fuente: "), enlace_externo("UCI Machine Learning Repository", URL_DATASET)),
      span(tags$b("Muestra: "), "839 pacientes"),
      span(tags$b("Atributos: "),
           "23 (20 genes con alta frecuencia de mutación y 3 variables clínicas)"),
      span(tags$b("Objetivo: "), "clasificación binaria LGG vs GBM"),
      span(tags$b("Naturaleza: "),
           "tabular, multivariada (numérica y categórica), sin valores faltantes")
    )),
    callout(
      paste("La variable objetivo es binaria: 1 si el paciente presenta glioblastoma",
            "multiforme (GBM), 0 si el glioma es de bajo grado (LGG)."),
      titulo = "Variable a predecir"
    ),
    titulo = "Ficha técnica del dataset",
    tono = "acento"
  )
}

ui_introduccion <- function() {
  div(
    class = "vista-pestana",
    kpi_row(intro_kpis()),
    fila(
      columna(
        lg = 7, class = "mb-4",
        card(
          paragraph(paste(
            "Los gliomas son el tumor cerebral primario más común en adultos y se",
            "clasifican principalmente en gliomas de bajo grado (LGG) y glioblastoma",
            "multiforme (GBM). La diferencia no es de matiz: cambia el pronóstico, la",
            "agresividad del tratamiento y el seguimiento.")),
          paragraph(paste(
            "Los criterios histológicos e imagenológicos han sido la base del",
            "diagnóstico, pero la caracterización biomolecular se ha vuelto",
            "imprescindible para decidir el tratamiento idóneo.")),
          callout(
            paste("La secuenciación genética completa tiene un coste elevado para los",
                  "sistemas de salud y para los pacientes. Ese gasto es lo que vuelve",
                  "valiosa cualquier respuesta a la pregunta de qué merece la pena medir."),
            titulo = "El cuello de botella"
          ),
          paragraph(paste(
            "Este tablero aborda el desafío en dos pasos: primero identifica qué",
            "variables clínicas y qué mutaciones separan realmente a los dos grupos, y",
            "con qué fuerza; después convierte esos hallazgos en tres modelos de",
            "clasificación y mide cómo rinden con pacientes que no vieron al entrenar.")),
          titulo = "El problema en una página"
        )
      ),
      columna(lg = 5, class = "mb-4", intro_ficha_dataset())
    ),
    section_title("Cómo recorrer el tablero"),
    fila(
      class = "g-3",
      lapply(Filter(function(b) b$en_guia, BLOQUES), function(bloque) {
        columna(intro_bloque(bloque), xl = 3, md = 6, xs = 12, class = "mb-3")
      })
    ),
    intro_tira_documentacion()
  )
}

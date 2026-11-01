# =========================================================================== #
# Pestaña 2 · Contexto clínico
# Qué son LGG y GBM, cómo se distribuyen en la cohorte y por qué la edad y el
# perfil molecular son la base del diagnóstico actual.
# =========================================================================== #

AMBITO_EDA <- "train"  # el EDA se describe sobre el conjunto de entrenamiento

contexto_kpis <- function() {
  edad <- estadisticas_edad(AMBITO_EDA)
  prueba <- prueba_edad_por_grado(AMBITO_EDA)
  tamanos <- tamanos_particion()
  list(
    list(valor = paste(num(edad["LGG", "media"], 1), "años"), etiqueta = "Edad media en LGG",
         detalle = paste0("± ", num(edad["LGG", "desviacion"], 1), " · ",
                          edad["LGG", "pacientes"], " pacientes"),
         color = COLOR_LGG),
    list(valor = paste(num(edad["GBM", "media"], 1), "años"), etiqueta = "Edad media en GBM",
         detalle = paste0("± ", num(edad["GBM", "desviacion"], 1), " · ",
                          edad["GBM", "pacientes"], " pacientes"),
         color = COLOR_GBM),
    list(valor = paste(num(edad["GBM", "media"] - edad["LGG", "media"], 1), "años"),
         etiqueta = "Diferencia de edad",
         detalle = paste("Mann-Whitney", p_valor(prueba$p_valor)),
         color = paste0("linear-gradient(180deg, ", COLOR_LGG, " 0 50%, ", COLOR_GBM, " 50% 100%)")),
    list(valor = as.character(tamanos[["train"]]), etiqueta = "Cohorte descrita",
         detalle = paste("Conjunto de entrenamiento de", tamanos[["full"]], "pacientes"))
  )
}

ui_contexto <- function() {
  edad <- estadisticas_edad(AMBITO_EDA)
  div(
    class = "vista-pestana",
    page_header(
      "Contexto clínico: dos tumores con el mismo origen",
      paste("Los gliomas nacen de las células gliales, pero LGG y GBM se comportan de",
            "forma distinta y se diagnostican en edades distintas."),
      "monitor_heart"
    ),
    kpi_row(contexto_kpis()),
    fila(
      columna(
        lg = 7, class = "mb-4",
        card(
          paragraph(paste(
            "El glioma es un tumor que se origina en las células gliales del sistema",
            "nervioso central. La Organización Mundial de la Salud lo clasifica por",
            "grados según su agresividad; en este proyecto la escala se reduce a la",
            "distinción que más pesa en la decisión clínica:")),
          bullet_list(list(
            span(tags$b("LGG · glioma de bajo grado. "),
                 paste("Crecimiento lento, supervivencia larga y un tratamiento que puede",
                       "ser conservador. Aparece con más frecuencia en adultos jóvenes y",
                       "suele portar mutación en IDH1.")),
            span(tags$b("GBM · glioblastoma multiforme. "),
                 paste("El glioma más agresivo: crecimiento rápido, pronóstico corto y",
                       "necesidad de tratamiento intensivo inmediato. Se concentra en",
                       "edades avanzadas y presenta con más frecuencia alteraciones en PTEN y EGFR."))
          )),
          paragraph(paste(
            "Hasta hace poco el grado se establecía por histología e imagen. La",
            "clasificación actual incorpora el perfil molecular porque dos tumores con",
            "el mismo aspecto al microscopio pueden tener pronósticos muy distintos",
            "según qué genes estén mutados.")),
          callout(
            paste("Esa es la premisa del proyecto: si el perfil molecular es lo que define",
                  "el grado, un subconjunto pequeño de marcadores debería bastar para",
                  "clasificarlo."),
            titulo = "De la histología al perfil molecular"
          ),
          titulo = "Qué distingue a LGG de GBM"
        )
      ),
      columna(
        lg = 5, class = "mb-4",
        graph_card("fig_ctx_cohorte", "Composición de la cohorte",
                   paste("Desbalance leve (ratio ≈ 1,4:1), no severo: no hace falta",
                         "reponderar las clases.")),
        div(class = "mt-3", series_chips())
      )
    ),
    section_title("La edad al diagnóstico separa a los dos grupos"),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_ctx_edad", "Distribución de la edad por grado tumoral",
                         paste("Porcentaje dentro de cada grado para que el tamaño distinto",
                               "de los grupos no distorsione la comparación."))),
      columna(lg = 5, class = "mb-4",
              graph_card("fig_ctx_caja", "Rango y dispersión por grado",
                         paste("Los puntos son valores atípicos dentro de cada grupo: casos",
                               "de GBM inusualmente jóvenes y de LGG inusualmente mayores.")))
    ),
    card(
      paragraph(paste0(
        "La separación es clínicamente relevante y estadísticamente significativa: el ",
        "75 % de los pacientes con LGG tiene ", num(edad["LGG", "q3"], 1), " años o menos, ",
        "mientras que el 75 % de los casos de GBM se diagnostica a partir de los ",
        num(edad["GBM", "q1"], 1), " años. La prueba U de Mann-Whitney (elegida porque ",
        "la edad no sigue una distribución normal: es bimodal, con picos cerca de los 35 ",
        "y los 55 años) rechaza la hipótesis de igualdad de distribuciones con ",
        p_valor(prueba_edad_por_grado(AMBITO_EDA)$p_valor), ".")),
      data_table(
        c("Grado", "Pacientes", "Media", "Mediana", "Q1 - Q3", "Rango"),
        lapply(seq_len(nrow(edad)), function(i) {
          f <- edad[i, ]
          c(f$grade_label, as.character(f$pacientes),
            paste(num(f$media, 2), "±", num(f$desviacion, 2)), num(f$mediana, 2),
            paste(num(f$q1, 2), "-", num(f$q3, 2)), paste(num(f$minimo, 2), "-", num(f$maximo, 2)))
        })
      ),
      callout(
        paste("La edad es un correlato fuerte, no una causa: hay pacientes de 22 años con",
              "glioblastoma y de 87 con glioma de bajo grado. Sirve para estimar riesgo,",
              "nunca para descartar un diagnóstico."),
        titulo = "Lectura prudente"
      ),
      titulo = "Edad al diagnóstico por grado tumoral",
      subtitulo = "Conjunto de entrenamiento · valores en años"
    )
  )
}

server_contexto <- function(input, output, session) {
  output$fig_ctx_cohorte <- plotly::renderPlotly(fig_distribucion_grado(AMBITO_EDA))
  output$fig_ctx_edad <- plotly::renderPlotly(fig_edad_por_grado(AMBITO_EDA))
  output$fig_ctx_caja <- plotly::renderPlotly(fig_boxplot_edad(AMBITO_EDA))
}

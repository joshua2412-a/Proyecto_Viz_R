# =========================================================================== #
# Pestaña 3 · Problema
# Qué se quiere decidir, con qué información y por qué secuenciar el panel
# completo no siempre es viable.
# =========================================================================== #

problema_v_de <- function(variable) {
  a <- asociacion_con_grado(AMBITO_EDA)
  num(a$v_cramer[a$variable == variable], 2)
}

problema_rho_de <- function(variable) {
  a <- asociacion_con_grado(AMBITO_EDA)
  tnum(a$rho[a$variable == variable], 2, signo = TRUE)
}

problema_tabla_senal <- function() {
  a <- asociacion_con_grado(AMBITO_EDA)
  con <- a[a$significativa, ]
  sin <- a[!a$significativa, ]
  data_table(
    c("Grupo", "Variables", "Cuáles"),
    list(
      c("Con señal ($p < 0{,}05$ y $|r| \\geq 0{,}10$)", as.character(nrow(con)),
        # Las clínicas en español, como en el resto de la aplicación
        paste(nombre_corto(con$variable), collapse = ", ")),
      c("Sin señal apreciable", as.character(nrow(sin)),
        paste(nombre_corto(sin$variable), collapse = ", "))
    )
  )
}

ui_problema <- function() {
  a <- asociacion_con_grado(AMBITO_EDA)
  sin_senal <- a$variable[!a$significativa]
  div(
    class = "vista-pestana",
    page_header(
      "El problema: decidir el grado con la menor información posible",
      paste("Clasificar LGG frente a GBM es un problema de clasificación binaria, pero",
            "con una restricción práctica: cada gen que se secuencia cuesta."),
      "error"
    ),
    fila(
      columna(
        lg = 6, class = "mb-4",
        card(
          section_title("Situación", nivel = 5),
          paragraph(paste(
            "La caracterización biomolecular es hoy imprescindible para determinar el",
            "pronóstico y el tratamiento de un glioma. Pero las pruebas de secuenciación",
            "genética completa suponen un coste económico elevado para los sistemas de",
            "salud y para los pacientes, y no todos los centros tienen acceso a ellas en",
            "el momento en que hay que decidir un tratamiento.")),
          section_title("Tensión", nivel = 5),
          paragraph(paste(
            "Se secuencia un panel de 20 genes, pero no todos informan por igual sobre el",
            "grado del tumor. Si una parte de esas mutaciones no ayuda a distinguir LGG de",
            "GBM, se está pagando por información que no cambia la decisión clínica.")),
          section_title("Pregunta analítica", nivel = 5),
          paragraph(paste(
            "¿Cuál es el subconjunto mínimo de variables clínicas y mutaciones que permite",
            "clasificar el grado del glioma con una precisión aceptable, y cuánta",
            "precisión se pierde al reducir el panel?")),
          callout(
            paste("El problema no es solo maximizar la precisión: es identificar cuánta",
                  "precisión se consigue con cuánta información, para que la reducción de",
                  "costes sea una decisión informada y no un recorte a ciegas."),
            titulo = "Cómo se plantea aquí"
          ),
          titulo = "Planteamiento"
        )
      ),
      columna(
        lg = 6, class = "mb-4",
        graph_card("fig_prob_prevalencia", "Prevalencia de mutación por gen y grado tumoral",
                   paste("Ordenado por la diferencia entre grados: arriba, los genes que más",
                         "separan LGG de GBM. IDH1 es el caso extremo."))
      )
    ),
    section_title("No todas las variables informan del grado"),
    card(
      paragraph(paste(
        "Cruzando cada predictora con el grado tumoral en el conjunto de entrenamiento,",
        "las variables se reparten en dos grupos muy desiguales. Las que no alcanzan",
        "significancia estadística o una magnitud mínima de asociación aportan una",
        "varianza explicativa insignificante, y son las primeras candidatas a salir del panel.")),
      problema_tabla_senal(),
      bullet_list(list(
        paste0("IDH1 es el marcador con mayor capacidad discriminativa y de signo inverso a ",
               "la malignidad ($r = ", problema_rho_de("IDH1"), "$): su mutación es ",
               "prácticamente una firma de glioma de bajo grado."),
        paste0("La edad al diagnóstico es el principal correlato clínico directo con la ",
               "severidad ($r = ", problema_rho_de("Age_at_diagnosis"), "$)."),
        paste0("PTEN es el marcador de riesgo genómico más sólido (V de Cramér = ",
               problema_v_de("PTEN"), "), seguido de EGFR y RB1."),
        paste0(lista_y(nombre_corto(sin_senal)), " no muestran asociación apreciable con el grado en esta muestra.")
      )),
      titulo = "Reparto de las 23 predictoras",
      subtitulo = "Conjunto de entrenamiento · Spearman y chi-cuadrado"
    ),
    callout(
      paste("Que una variable no discrimine el grado en esta muestra no significa que sea",
            "irrelevante en oncología: puede importar para el pronóstico, la respuesta al",
            "tratamiento o la supervivencia, que no son lo que este análisis mide."),
      titulo = "Alcance de la afirmación"
    )
  )
}

server_problema <- function(input, output, session) {
  output$fig_prob_prevalencia <- plotly::renderPlotly(fig_prevalencia_genes(AMBITO_EDA, top_n = 12))
}

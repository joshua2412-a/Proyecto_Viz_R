# =========================================================================== #
# Pestaña 5 · Marco teórico
# Qué mide cada variable, cómo se cuantifica una asociación y qué representa
# cada gen del panel.
# =========================================================================== #

FORMULA_CRAMER <- r"($$V \;=\; \sqrt{\frac{\chi^{2}}{n \,\cdot\, \min(k-1,\; r-1)}}$$)"
FORMULA_SPEARMAN <- r"($$r \;=\; 1 - \frac{6 \sum d_i^{2}}{n\,(n^{2}-1)}$$)"

ESTADISTICOS <- list(
  c("p-valor",
    paste("Probabilidad de observar una diferencia al menos tan grande como la observada",
          "si en realidad no hubiera ninguna."),
    paste("Responde a si la asociación es distinguible del azar, no a si es importante.",
          "Con 671 pacientes, diferencias minúsculas pueden salir significativas.")),
  c("Spearman ($r$)",
    "Correlación entre los rangos de dos variables: va de -1 a +1.",
    paste("Aporta el signo, que es lo que permite decir si una variable empuja hacia LGG",
          "(negativo) o hacia GBM (positivo). No exige normalidad ni linealidad.")),
  c("V de Cramér",
    "Fuerza de la asociación entre dos variables categóricas: va de 0 a 1.",
    paste("Es el complemento del chi-cuadrado: la prueba dice si hay asociación, la V dice",
          "cuánta. Sirve tanto contra el grado como entre pares de genes.")),
  c("Rango intercuartílico (IQR)",
    "Distancia entre el percentil 25 y el 75 de una variable continua.",
    paste("Describe la dispersión sin que la arrastren los extremos, y define el criterio",
          "con el que se identifican valores atípicos.")),
  c("Prevalencia",
    "Proporción de pacientes que presentan una mutación.",
    paste("Una mutación muy rara puede tener una asociación fuerte y aun así aportar poco",
          "en la práctica, simplemente porque casi nadie la tiene."))
)

formula <- function(tex) div(class = "formula-block", withMathJax(tex))

marco_tabla_variables <- function() {
  data_table(
    c("Variable", "Tipo", "Codificación", "Papel en el análisis"),
    list(
      # Nombre en español y, entre paréntesis, la columna del CSV: es una tabla
      # de operacionalización, así que se ven las dos.
      c("Grado (Grade)", "Binaria", "0 = LGG · 1 = GBM", "Variable objetivo"),
      c("Edad al diagnóstico (Age_at_diagnosis)", "Continua (años)", "Decimales incluidos: recogen los días exactos",
        "Predictora · se contrasta con Mann-Whitney"),
      c("Género (Gender)", "Binaria", paste(paste(names(GENDER_LABELS), "=", GENDER_LABELS), collapse = " · "),
        "Predictora · chi-cuadrado, V de Cramér y Spearman"),
      c("Grupo racial (Race)", "Categórica (4 niveles)",
        paste(paste(names(RACE_LABELS), "=", RACE_LABELS), collapse = " · "),
        paste("Predictora · chi-cuadrado, V de Cramér y Spearman sobre la versión",
              "agrupada en dos niveles")),
      c(paste(length(GENE_FEATURES), "genes"), "Binarias", "0 = no mutado (wildtype) · 1 = mutado",
        "Predictoras · chi-cuadrado, V de Cramér y Spearman")
    )
  )
}

marco_tabla_genes <- function() {
  prev <- prevalencia_genes(AMBITO_EDA)
  a <- asociacion_con_grado(AMBITO_EDA)
  filas <- lapply(prev$gen, function(gen) {
    fila_a <- a[a$variable == gen, ]
    # Tres decimales: con dos, un r de 0,098 se leía "+0,10", justo el umbral
    rho <- round(fila_a$rho, 3)
    hacia <- if (rho < 0) "LGG" else "GBM"
    marca <- if (fila_a$significativa) "" else if (fila_a$p_valor >= ALFA) {
      " (no significativa)"
    } else {
      paste0(" (débil: $|r| < ", tnum(EFECTO_MINIMO, 2), "$)")
    }
    c(gen, unname(GENE_DESCRIPCION[gen]), pct(prev$prevalencia[prev$gen == gen], 1),
      paste0(num(rho, 3, signo = TRUE), " → ", hacia, marca))
  })
  data_table(c("Gen", "Función biológica", "Prevalencia", "Asociación con el grado"), filas)
}

ui_marco <- function() {
  div(
    class = "vista-pestana",
    page_header(
      "Marco teórico",
      paste("Qué mide cada variable, qué prueba estadística le corresponde y cómo se",
            "interpretan sus resultados."),
      "menu_book"
    ),
    section_title("Operacionalización de las variables"),
    card(
      paragraph(paste(
        "El dataset llega ya codificado numéricamente, y esa codificación es la que da",
        "sentido a los signos: como el género vale 0 en los hombres y 1 en las mujeres,",
        "una correlación o un coeficiente positivo en género indicaría más riesgo de GBM",
        "en las mujeres que en los hombres, que son la categoría de referencia.")),
      marco_tabla_variables(),
      titulo = "Las 23 predictoras y la variable objetivo"
    ),
    section_title("Cómo se mide una asociación"),
    fila(
      columna(
        lg = 6, class = "mb-4",
        card(
          paragraph(paste(
            "Decir que una mutación está relacionada con el grado del tumor no significa",
            "nada hasta que se cuantifica. Dos preguntas distintas, dos números distintos:",
            "¿es distinguible del azar?, y ¿de qué tamaño es el efecto?")),
          paragraph(paste(
            "Para variables categóricas, la prueba de chi-cuadrado responde a la primera y",
            "la V de Cramér a la segunda. La V normaliza el estadístico por el tamaño de la",
            "muestra y por las dimensiones de la tabla, de modo que queda acotada entre 0 y",
            "1 y se puede comparar entre pares de variables:")),
          formula(FORMULA_CRAMER),
          callout(
            paste("Esta distinción es la que evita la trampa más común del análisis",
                  "exploratorio: con muestras grandes, casi todo sale significativo. Por",
                  "eso aquí una variable solo se considera con señal si además alcanza una",
                  "magnitud mínima."),
            titulo = "Significancia no es magnitud"
          ),
          titulo = "Significancia y tamaño del efecto"
        )
      ),
      columna(
        lg = 6, class = "mb-4",
        card(
          paragraph(paste(
            "La correlación de Spearman aporta lo que la V de Cramér no puede dar: la",
            "dirección. Se calcula sobre los rangos de los datos, no sobre sus valores, de",
            "modo que no exige normalidad ni una relación lineal.")),
          formula(FORMULA_SPEARMAN),
          # La fórmula clásica supone que no hay empates; con variables 0/1 casi
          # todo son empates.
          paragraph(paste(
            "La fórmula es la de datos sin empates. Con variables 0/1, casi todos los",
            "valores empatan, y el coeficiente se calcula como la correlación de Pearson",
            "entre los rangos, que es su definición general.")),
          bullet_list(list(
            "Signo negativo: la variable acompaña a los gliomas de bajo grado.",
            "Signo positivo: acompaña a los glioblastomas.",
            "Cerca de cero: no distingue entre los dos."
          )),
          paragraph(paste(
            "Codificando el grado como 0 = LGG y 1 = GBM, el signo de $r$ se lee",
            "directamente como la dirección hacia la que empuja cada marcador.")),
          titulo = "Dirección del efecto"
        )
      )
    ),
    section_title("Los estadísticos que verás en Resultados"),
    card(
      data_table(c("Estadístico", "Qué mide", "Cómo leerlo"), ESTADISTICOS),
      titulo = "Cinco números y su interpretación",
      subtitulo = "Aparecen en las tablas y los tooltips de la pestaña de resultados",
      class = "mb-4"
    ),
    section_title("El panel de genes"),
    card(
      paragraph(paste(
        "Los 20 genes del panel son los de mayor frecuencia de mutación en los proyectos",
        "TCGA-LGG y TCGA-GBM. La prevalencia y la asociación se calculan sobre el",
        "conjunto de entrenamiento.")),
      marco_tabla_genes(),
      div(class = "mt-3",
          enlace_externo("Ver el desarrollo completo del EDA en el libro", URL_LIBRO_EDA)),
      titulo = "Función biológica, prevalencia y dirección de cada gen",
      subtitulo = "Ordenado por prevalencia de mutación en la cohorte de entrenamiento"
    )
  )
}

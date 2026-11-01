# =========================================================================== #
# Pestaña 6 · Metodología
# Del CSV a las conclusiones: partición, control de calidad y la prueba que
# recibe cada tipo de variable. Todo reproducible con una semilla.
# =========================================================================== #

ETAPAS <- list(
  c("1", "Verificación del dataset",
    paste("839 registros y 24 variables, sin valores nulos ni faltantes. El único",
          "registro duplicado corresponde a dos pacientes distintos que comparten",
          "fenotipo y perfil genético, así que se conserva. Las variables binarias se",
          "convierten a tipo categórico para el análisis exploratorio.")),
  c("2", "Partición estratificada 80/20",
    paste("Se reserva el 20 % de los pacientes antes de mirar cualquier estadístico, de",
          "modo que el EDA no filtre información del conjunto de prueba. La",
          "estratificación mantiene la proporción LGG/GBM en ambos lados. Ninguna prueba",
          "ni ninguna comparación contra el grado se calcula sobre esa reserva; en",
          "Exploración se puede comparar la distribución de cada variable entre",
          "particiones, que es descripción y no compromete nada.")),
  c("3", "Análisis univariado",
    paste("Distribución de cada variable por separado: medidas de tendencia central y",
          "dispersión para la edad, frecuencias para las categóricas y las 20 mutaciones.",
          "Se evalúan normalidad (Shapiro-Wilk) y valores atípicos (regla del rango",
          "intercuartílico).")),
  c("4", "Análisis bivariado contra el grado",
    paste("Cada predictora se cruza con la variable objetivo usando la prueba que",
          "corresponde a su naturaleza, y se cuantifica la fuerza de la asociación además",
          "de su significancia: un p-valor pequeño con una muestra grande no implica un",
          "efecto relevante.")),
  c("5", "Diagnóstico de multicolinealidad",
    paste("Se mide la co-ocurrencia entre mutaciones con la V de Cramér y el factor de",
          "inflación de la varianza para detectar redundancias que inestabilizarían la",
          "estimación de parámetros en un modelo posterior.")),
  c("6", "Modelado y publicación",
    paste("Con las predictoras caracterizadas se entrenan tres clasificadores (pestaña",
          "Modelos). El desarrollo completo se publica como libro en R con bookdown, y",
          "esta app de Shiny reproduce sus figuras de forma interactiva."))
)

PRUEBAS <- list(
  c("Mann-Whitney (U)", "Edad al diagnóstico frente al grado",
    paste("No paramétrica: se elige porque Shapiro-Wilk rechaza la normalidad de la edad,",
          "que presenta un patrón bimodal. Compara distribuciones completas, no solo medias.")),
  c("Shapiro-Wilk", "Normalidad de la edad",
    paste("Determina qué familia de pruebas es válida después. Con n = 671 es muy",
          "sensible, así que su resultado se lee junto a la asimetría y la forma de la",
          "distribución, no de forma aislada.")),
  c("Chi-cuadrado de independencia", "Género, grupo racial y cada mutación frente al grado",
    paste("Contrasta si la distribución de una categórica cambia entre LGG y GBM. Las",
          "categorías con frecuencias muy bajas se agrupan antes para que las frecuencias",
          "esperadas no invaliden la prueba.")),
  c("V de Cramér", "Fuerza de las asociaciones categóricas",
    paste("Complementa al chi-cuadrado: este dice si hay asociación, la V dice cuánta. Se",
          "usa tanto contra el grado como entre pares de genes.")),
  c("Correlación de Spearman", "Dirección y magnitud frente al grado",
    paste("Da el signo, que es lo que permite decir si una variable empuja hacia LGG o",
          "hacia GBM. Al ser de rangos, no exige linealidad ni normalidad."))
)

metodologia_kpis <- function() {
  tamanos <- tamanos_particion()
  prop <- proporcion_grado("train")
  asociacion <- asociacion_con_grado("train")
  list(
    list(valor = as.character(tamanos[["train"]]), etiqueta = "Pacientes en entrenamiento",
         detalle = paste0("LGG ", pct(prop$porcentaje[1], 1), " · GBM ", pct(prop$porcentaje[2], 1))),
    list(valor = as.character(tamanos[["test"]]), etiqueta = "Pacientes en prueba",
         detalle = paste0("Reservados antes del EDA (", pct(TEST_SIZE * 100, 0), " del total)")),
    list(valor = as.character(nrow(asociacion)), etiqueta = "Variables contrastadas",
         detalle = paste0(length(NUMERIC_FEATURES), " numérica · ", length(CATEGORICAL_FEATURES),
                          " categóricas · ", length(GENE_FEATURES), " mutaciones")),
    list(valor = "seed 42", etiqueta = "Reproducibilidad",
         detalle = "Misma partición en el libro, el modelado y esta app")
  )
}

tarjeta_guia <- function(numero, titulo, descripcion) {
  card(
    div(class = "guide-badge", badge_numero(numero)),
    div(class = "guide-name", titulo),
    div(class = "guide-text", descripcion),
    class = "guide-card"
  )
}

ui_metodologia <- function() {
  div(
    class = "vista-pestana",
    page_header(
      "Metodología",
      paste("Del CSV a las conclusiones del EDA, en seis etapas reproducibles con la misma",
            "semilla que usa el libro."),
      "assignment"
    ),
    kpi_row(metodologia_kpis()),
    section_title("Las seis etapas"),
    fila(
      class = "g-3",
      lapply(ETAPAS, function(e) {
        columna(lg = 4, md = 6, xs = 12, class = "mb-3", tarjeta_guia(e[1], e[2], e[3]))
      })
    ),
    section_title("Qué prueba se aplica a cada variable"),
    card(
      paragraph(paste(
        "La prueba no se elige por costumbre sino por la naturaleza del dato: una",
        "variable continua y asimétrica, una categórica de cuatro niveles y un indicador",
        "binario no admiten el mismo contraste.")),
      data_table(
        c("Bloque", "Variables", "Prueba contra el grado", "Motivo"),
        list(
          c("Clínica numérica", paste(NUMERIC_FEATURES, collapse = ", "), "Mann-Whitney (U)",
            paste("La variable es continua y no normal; se comparan las distribuciones de",
                  "los dos grupos.")),
          c("Clínicas categóricas", paste(CATEGORICAL_FEATURES, collapse = ", "),
            "Chi-cuadrado + V de Cramér + Spearman",
            paste("Gender es binaria. Race tiene cuatro niveles sin orden natural, así que",
                  "para contrastarla se reagrupa en dos (White frente al resto) y así admite",
                  "también dirección.")),
          c("Mutacionales", paste(length(GENE_FEATURES), "genes"),
            "Chi-cuadrado + V de Cramér + Spearman",
            paste("Indicadores 0/1: se contrasta la independencia y se añade Spearman para",
                  "conocer la dirección del efecto."))
        )
      ),
      callout(
        paste("Toda la descripción se hace sobre el conjunto de entrenamiento. El de prueba",
              "permanece intacto para evaluar los modelos que salen de estos hallazgos."),
        titulo = "Separación estricta"
      ),
      titulo = "Correspondencia entre tipo de dato y contraste"
    ),
    section_title("Las pruebas, una a una"),
    fila(
      class = "g-3",
      lapply(PRUEBAS, function(pr) {
        columna(lg = 4, md = 6, xs = 12, class = "mb-3",
                card(div(class = "card-subtitle-custom", pr[2]), paragraph(pr[3]), titulo = pr[1]))
      })
    ),
    card(
      bullet_list(list(
        "Nivel de significancia $\\alpha = 0{,}05$ en todos los contrastes.",
        paste("Una variable se considera con señal cuando es significativa y además alcanza",
              "$|r| \\geq 0{,}10$: la significancia sola no basta con muestras de este tamaño."),
        "El umbral de redundancia entre pares de variables se fija en V de Cramér > 0,70."
      )),
      div(class = "mt-3",
          enlace_externo("Ver el desarrollo completo del EDA en el libro", URL_LIBRO_EDA)),
      titulo = "Criterios de decisión",
      subtitulo = "Los umbrales se fijaron antes de mirar los resultados"
    )
  )
}

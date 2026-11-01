# =========================================================================== #
# Pestaña 11 · Limitaciones
# Qué NO se puede concluir con este trabajo, declarado de forma explícita para
# que ninguna cifra del tablero se lea con más alcance del que tiene.
# =========================================================================== #

limitaciones_lista <- function() {
  mejor <- mejor_modelo_test()
  ic <- RESUMEN$modelos[[mejor$clave]]$auc_test_ic
  # El panel más pequeño que más se acerca al umbral de la regla de un error
  # estándar sin alcanzarlo (con los resultados del libro de R, el de tres)
  k <- vec(RESUMEN$panel$k); auc <- vec(RESUMEN$panel$auc); de <- vec(RESUMEN$panel$de)
  umbral <- auc[k == max(k)] - de[k == max(k)] / sqrt(RESUMEN$meta$k_externo)
  menores <- which(k < K_PANEL)
  cerca <- menores[which.max(auc[menores])]
  list(
    c("Cohorte única y no local",
      paste("Todos los pacientes provienen de los proyectos TCGA-LGG y TCGA-GBM, recogidos",
            "mayoritariamente en centros de Estados Unidos. No hay validación externa con una",
            "cohorte independiente, y menos aún con población latinoamericana, así que el",
            "desempeño observado no es trasladable sin más a otro contexto asistencial.")),
    c("Fuerte desbalance en el grupo racial",
      paste("Más de nueve de cada diez pacientes pertenecen al grupo White. Las categorías",
            "minoritarias tienen tan pocas observaciones que su coeficiente no es estimable",
            "con precisión: la asociación significativa entre grupo racial y grado debe",
            "leerse con máxima cautela y no como un efecto biológico.")),
    c("Asociación, no causalidad",
      paste("Las pruebas de independencia, las correlaciones y la importancia de las",
            "variables en los modelos detectan asociación estadística. Que IDH1 mutado",
            "acompañe a los gliomas de bajo grado no establece un mecanismo causal: la",
            "explicación biológica está fuera del alcance de estos contrastes.")),
    c("El análisis describe el grado, no el pronóstico",
      paste("La variable objetivo es la clasificación histológica LGG/GBM, con el criterio",
            "anterior a la clasificación de la OMS de 2021. Nada de lo que se",
            "observa aquí habla de supervivencia, de respuesta al tratamiento ni de",
            "progresión: son preguntas distintas que requieren otros datos y otro diseño.")),
    c("Panel de genes fijo y binario",
      paste("Las 20 mutaciones entran como indicadores 0/1: no se distingue el tipo de",
            "variante, su carga alélica ni su localización. Dos pacientes con 'IDH1 mutado'",
            "pueden tener alteraciones biológicamente distintas.")),
    c("Un conjunto de prueba pequeño",
      paste0("Los modelos se evalúan sobre ", RESUMEN$meta$n_test, " pacientes (",
             RESUMEN$meta$gbm_test, " con GBM). El intervalo de confianza del AUC del mejor ",
             "modelo va de ", num(ic[[1]], 3), " a ", num(ic[[2]], 3), ", y con esa amplitud las ",
             "diferencias entre los tres clasificadores no se distinguen del azar: la cifra ",
             "puntual no es la capacidad definitiva de ninguno.")),
    c("La reducción de costes es una hipótesis, no un resultado clínico",
      paste0("Que un panel reducido (", numero_texto(K_PANEL), " variables) rinda casi como ",
             "las 23 sugiere que bastaría para clasificar el grado. Validar eso exige un ",
             "estudio prospectivo con el panel reducido, no solo modelos entrenados sobre ",
             "datos históricos. Además, el tamaño del panel depende del criterio: con ",
             numero_texto(k[cerca]), " variables la AUC queda a ",
             num(umbral - auc[cerca], 4), " del umbral de la regla de un error estándar.")),
    c("Contrastes múltiples: corregidos, pero un cribado sigue siendo un cribado",
      paste("Se contrastan 23 variables contra el grado por separado, así que algún p-valor",
            "pequeño podría salir por azar. La tabla de Resultados reporta el q-valor de",
            "Benjamini-Hochberg, que controla la proporción de falsos descubrimientos, y",
            "ninguna asociación del panel se cae al corregir. Lo que la corrección no arregla",
            "es la naturaleza del cribado: la relevancia conjunta la deciden los modelos."))
  )
}

limitaciones_tabla_representatividad <- function() {
  tabla <- distribucion_clinica("Race", "train")
  agregado <- tapply(tabla$pacientes, tabla$categoria, sum)
  agregado <- sort(agregado, decreasing = TRUE)
  total <- sum(agregado)
  data_table(
    c("Grupo racial reportado", "Pacientes", "% de la cohorte de entrenamiento"),
    lapply(names(agregado), function(cat) {
      c(cat, as.character(agregado[[cat]]), pct(agregado[[cat]] / total * 100, 2))
    })
  )
}

ui_limitaciones <- function() {
  tamanos <- tamanos_particion()
  a <- asociacion_con_grado("train")
  div(
    class = "vista-pestana",
    page_header(
      "Limitaciones",
      paste("Ocho fronteras del trabajo, declaradas antes de que alguien las encuentre",
            "leyendo las cifras con demasiada confianza."),
      "warning"
    ),
    callout(
      paste0("Todo lo que se afirma aquí se sostiene sobre ", tamanos[["full"]], " pacientes de ",
             "una sola fuente, y los ", nrow(a), " contrastes se calculan sobre los ",
             tamanos[["train"]], " del conjunto de entrenamiento. Es una muestra suficiente para ",
             "orientar decisiones de análisis y pequeña para una afirmación clínica."),
      titulo = "El tamaño manda"
    ),
    fila(
      class = "g-3",
      lapply(limitaciones_lista(), function(l) {
        columna(lg = 6, class = "mb-3", card(paragraph(l[2]), titulo = l[1]))
      })
    ),
    section_title("La limitación más medible: representatividad"),
    card(
      paragraph(paste(
        "El sesgo de composición no es una sospecha, se puede contar. Esta es la",
        "distribución real por grupo racial en la cohorte de entrenamiento, y explica por",
        "qué cualquier conclusión sobre grupos minoritarios queda fuera del alcance del",
        "trabajo.")),
      limitaciones_tabla_representatividad(),
      titulo = "Composición de la cohorte por grupo racial"
    )
  )
}

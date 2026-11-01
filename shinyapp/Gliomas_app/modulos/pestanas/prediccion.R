# =========================================================================== #
# Pestaña 10 · Predicción
# El visitante describe un paciente —edad, género, grupo racial y estado de los
# 20 genes— y ve qué probabilidad de glioblastoma le asigna cada uno de los
# tres modelos finales y por qué (valores SHAP exactos, ver modulos/modelo.R).
#
# Es una herramienta docente: muestra cómo razona un modelo entrenado con 671
# pacientes de TCGA, no sustituye a ningún diagnóstico.
# =========================================================================== #

# Los genes del panel reducido (conservan casi toda la capacidad del modelo
# completo; ver la pestaña Modelos). Salen del libro: modulos/modelo.R.
GENES_PANEL <- intersect(VARIABLES_PANEL, GENE_FEATURES)

PERFILES <- list(
  lgg = list(nombre = "Perfil típico de LGG (oligodendroglioma)",
             Age_at_diagnosis = 38, Gender = 0, Race = 0, genes = c("IDH1", "CIC", "FUBP1")),
  gbm = list(nombre = "Perfil típico de GBM (IDH sin mutar)",
             Age_at_diagnosis = 64, Gender = 0, Race = 0, genes = c("PTEN", "TP53", "NF1")),
  vacio = list(nombre = "Sin mutaciones, 50 años",
               Age_at_diagnosis = 50, Gender = 0, Race = 0, genes = character(0))
)
# El caso difícil (PACIENTE_DIFICIL) es el peor error de XGBoost en prueba; se
# calcula en modulos/modelo.R a partir del resumen del libro.
PERFIL_INICIAL <- "gbm"

prediccion_genes_ordenados <- function() {
  orden <- vapply(RESUMEN$importancia, `[[`, character(1), "variable")
  resto <- orden[orden %in% GENE_FEATURES & !orden %in% GENES_PANEL]
  c(resto, setdiff(GENE_FEATURES, c(resto, GENES_PANEL)))
}

prediccion_opciones_perfil <- function() {
  opciones <- c(stats::setNames(names(PERFILES), vapply(PERFILES, `[[`, character(1), "nombre")))
  dificil <- caso_dificil()
  opciones[paste0("Caso difícil: paciente ", PACIENTE_DIFICIL, " de prueba (",
                  dificil$grado, if (dificil$idh1) " con IDH1 mutado" else "", ")")] <-
    paste0("test-", PACIENTE_DIFICIL)
  # El caso difícil ya tiene su entrada: no se repite (selectize descarta valores duplicados)
  filas <- setdiff(sort(INDICES$test), PACIENTE_DIFICIL + 1L)
  prueba <- DATOS[filas, ]
  etiquetas <- paste0("Paciente ", filas - 1L, " de prueba · ", prueba$grade_label, " · ",
                      vapply(prueba$Age_at_diagnosis, num, character(1), decimales = 0), " años")
  c(opciones, stats::setNames(paste0("test-", filas - 1L), etiquetas))
}

# Paciente como lista de variables del dataset, y su grado real si lo hay
prediccion_perfil <- function(valor) {
  if (!is.null(valor) && startsWith(valor, "test-")) {
    indice <- as.integer(sub("test-", "", valor, fixed = TRUE))
    fila <- DATOS[indice + 1L, ]
    paciente <- c(list(Age_at_diagnosis = as.numeric(fila$Age_at_diagnosis),
                       Gender = as.integer(fila$Gender), Race = as.integer(fila$Race)),
                  stats::setNames(lapply(GENE_FEATURES, function(g) as.integer(fila[[g]])), GENE_FEATURES))
    return(list(paciente = paciente, grado = as.integer(fila$Grade), indice = indice))
  }
  p <- PERFILES[[valor %||% PERFIL_INICIAL]] %||% PERFILES[[PERFIL_INICIAL]]
  paciente <- c(list(Age_at_diagnosis = p$Age_at_diagnosis, Gender = p$Gender, Race = p$Race),
                stats::setNames(lapply(GENE_FEATURES, function(g) as.integer(g %in% p$genes)), GENE_FEATURES))
  list(paciente = paciente, grado = NULL, indice = NULL)
}

prediccion_paciente_formulario <- function(edad, genero, raza, panel, resto) {
  mutados <- union(panel %||% character(0), resto %||% character(0))
  c(list(Age_at_diagnosis = as.numeric(edad %||% 50),
         Gender = as.integer(genero %||% 0), Race = as.integer(raza %||% 0)),
    stats::setNames(lapply(GENE_FEATURES, function(g) as.integer(g %in% mutados)), GENE_FEATURES))
}

mismo_paciente <- function(a, b) {
  campos <- c("Age_at_diagnosis", "Gender", "Race", GENE_FEATURES)
  all(vapply(campos, function(k) isTRUE(as.numeric(a[[k]]) == as.numeric(b[[k]])), logical(1)))
}

chips_genes <- function(id, genes, marcados) {
  div(
    class = "chips-genes",
    checkboxGroupInput(
      id, label = NULL, inline = TRUE,
      choiceNames = lapply(genes, function(g) span(class = "chip-gen", title = GENE_DESCRIPCION[[g]], g)),
      choiceValues = genes,
      selected = intersect(genes, marcados)
    )
  )
}

pred_campo <- function(etiqueta, ..., extra = NULL, class = "") {
  div(class = trimws(paste("form-campo", class)),
      div(class = "campo-fila", span(class = "campo-label", etiqueta), extra),
      ...)
}

prediccion_formulario <- function() {
  inicial <- prediccion_perfil(PERFIL_INICIAL)$paciente
  marcados <- GENE_FEATURES[vapply(GENE_FEATURES, function(g) inicial[[g]] == 1, logical(1))]
  div(
    class = "card soft-card formulario-prediccion",
    div(
      class = "card-body form-cuerpo",
      tags$h5(class = "form-titulo", "Perfil del paciente"),
      pred_campo("Punto de partida",
                 div(class = "selector-variable selector-app",
                     selectInput("pred_perfil", label = NULL, choices = prediccion_opciones_perfil(),
                                 selected = PERFIL_INICIAL, selectize = TRUE, width = "100%"))),
      pred_campo("Edad al diagnóstico",
                 div(class = "slider-app",
                     sliderInput("pred_edad", label = NULL, min = 10, max = 90, step = 1,
                                 value = inicial$Age_at_diagnosis, ticks = TRUE, width = "100%")),
                 extra = span(class = "campo-valor", textOutput("pred_edad_valor", inline = TRUE))),
      div(
        class = "form-fila",
        pred_campo("Género",
                   div(class = "ambito-radio",
                       radioButtons("pred_genero", label = NULL, inline = TRUE,
                                    choiceNames = c("Hombre", "Mujer"), choiceValues = c("0", "1"),
                                    selected = as.character(inicial$Gender)))),
        pred_campo("Grupo racial",
                   div(class = "selector-variable selector-app",
                       selectInput("pred_raza", label = NULL,
                                   choices = stats::setNames(names(RACE_LABELS), RACE_LABELS),
                                   selected = as.character(inicial$Race), selectize = FALSE,
                                   width = "100%")),
                   class = "form-campo-ancho")
      ),
      pred_campo(
        "Genes mutados",
        div(class = "control-ayuda",
            "Panel reducido: con la edad, concentran casi toda la capacidad del modelo."),
        chips_genes("pred_genes_panel", GENES_PANEL, marcados),
        div(class = "control-ayuda mt-3",
            "Resto del panel, de más a menos peso. Pasa el cursor para ver qué hace cada gen."),
        chips_genes("pred_genes_resto", prediccion_genes_ordenados(), marcados),
        extra = span(class = "form-bloque-cuenta", textOutput("pred_genes_cuenta", inline = TRUE))
      ),
      div(
        class = "pred-acciones",
        div(class = "pred-pendiente", textOutput("pred_pendiente", inline = TRUE)),
        actionButton("pred_predecir", class = "boton-predecir",
                     label = tagList(span(class = "boton-predecir-texto", "Predecir"),
                                     span(class = "boton-predecir-icono", icono("arrow_forward"))))
      )
    )
  )
}

prediccion_veredicto <- function(probas) {
  valores <- unlist(probas[ORDEN_MODELOS])
  gbm <- ORDEN_MODELOS[valores >= UMBRAL]
  if (length(gbm) == 3) {
    titulo <- "Los tres modelos apuntan a glioblastoma (GBM)"
    color <- COLOR_GBM
    fondo <- "#FCEEE9"
  } else if (length(gbm) == 0) {
    titulo <- "Los tres modelos apuntan a glioma de bajo grado (LGG)"
    color <- COLOR_LGG
    fondo <- "#E8F0FB"
  } else {
    titulo <- paste0("Los modelos no coinciden: ", lista_y(NOMBRES_MODELO[gbm]),
                     if (length(gbm) == 1) " dice" else " dicen", " GBM")
    color <- STATUS_WARNING
    fondo <- "#FBF3E0"
  }
  minimo <- min(valores)
  maximo <- max(valores)
  detalle <- if (maximo < 0.35 || minimo > 0.65) {
    "Predicción firme: las tres probabilidades están lejos del umbral."
  } else if (minimo >= 0.35 && maximo <= 0.65) {
    paste("Zona de incertidumbre: las tres probabilidades rondan el 50 %. Un perfil así no",
          "se puede clasificar con confianza solo con estas variables.")
  } else {
    paste0("Las probabilidades van del ", pct(minimo * 100, 0), " al ", pct(maximo * 100, 0),
           ": alguna está cerca del umbral, así que conviene leer la explicación.")
  }
  tagList(
    div(class = "veredicto", style = paste0("background: ", fondo, ";"),
        span(class = "veredicto-punto", style = paste0("background: ", color, ";")),
        span(class = "veredicto-titulo", titulo)),
    div(class = "veredicto-texto", detalle)
  )
}

# Probabilidad de GBM de cada modelo en mosaicos: la barra crece desde cero
# (CSS, .prob-barra) y la cifra cuenta hasta su valor (www/contador.js)
prediccion_mosaicos <- function(probas) {
  div(
    class = "prob-mosaicos",
    lapply(ORDEN_MODELOS, function(m) {
      prob <- probas[[m]]
      es_gbm <- prob >= UMBRAL
      color <- if (es_gbm) COLOR_GBM else COLOR_LGG
      div(
        class = "prob-mosaico",
        div(class = "prob-mosaico-nombre", NOMBRES_MODELO[[m]]),
        div(class = "prob-mosaico-fila",
            span(class = "prob-mosaico-cifra",
                 span(class = "cifra-animada", round(prob * 100)), " %"),
            span(class = "prob-grado", style = paste0("color: ", if (es_gbm) "#B4512F" else "#2F6DB8", ";"),
                 if (es_gbm) "GBM" else "LGG")),
        div(class = "prob-riel prob-riel-fino",
            div(class = "prob-barra",
                style = paste0("width: ", sprintf("%.1f", prob * 100), "%; background: ", color, ";")),
            div(class = "prob-umbral"))
      )
    })
  )
}

prediccion_real <- function(cargado, paciente, probas) {
  if (is.null(cargado) || is.null(cargado$grado) || !mismo_paciente(cargado$paciente, paciente)) {
    return(NULL)
  }
  grado <- cargado$grado
  aciertos <- ORDEN_MODELOS[vapply(ORDEN_MODELOS, function(m) as.integer(probas[[m]] >= UMBRAL) == grado,
                                   logical(1))]
  texto <- if (length(aciertos) == 3) "los tres modelos aciertan" else if (length(aciertos) == 0)
    "ningún modelo acierta" else paste0("acierta", if (length(aciertos) > 1) "n" else "", " ",
                                        lista_y(NOMBRES_MODELO[aciertos]))
  color <- if (grado == 1) COLOR_GBM else COLOR_LGG
  div(class = "real-badge",
      span(class = "chip-dot", style = paste0("background: ", color, ";")),
      span(paste0("Diagnóstico real en TCGA del paciente ", cargado$indice, ": ",
                  GRADE_LABELS[[as.character(grado)]], " · ", texto)))
}

prediccion_nombre <- function(v, paciente) {
  if (v == "Age_at_diagnosis") return(paste0("la edad, ", num(paciente$Age_at_diagnosis, 0), " años"))
  if (v == "Gender") return("el género")
  if (v == "Race_binary") return("el grupo racial")
  paste(v, if (isTRUE(paciente[[v]] == 1)) "mutado" else "sin mutar")
}

prediccion_lectura <- function(explicacion, paciente, modelo, proba) {
  c_ <- explicacion$contribuciones
  log_odds <- explicacion$escala == "log-odds"
  formato <- function(v) if (log_odds) num(v, 2, signo = TRUE) else paste(num(v * 100, 1, signo = TRUE), "puntos")
  # Se ignoran las contribuciones mínimas: nombrarlas solo distrae
  minimo <- if (log_odds) 0.05 else 0.01
  hacia_gbm <- utils::head(names(sort(c_[c_ >= minimo], decreasing = TRUE)), 3)
  hacia_lgg <- utils::head(names(sort(c_[c_ <= -minimo])), 3)
  lineas <- paste0(NOMBRES_MODELO[[modelo]], " asigna a este perfil una probabilidad de GBM del ",
                   pct(proba * 100, 0), ".")
  for (par in list(list(hacia_gbm, "GBM"), list(hacia_lgg, "LGG"))) {
    lista <- par[[1]]
    if (length(lista)) {
      lineas <- c(lineas, paste0("Empuja hacia ", par[[2]], ": ",
                                 lista_y(vapply(lista, function(v) {
                                   paste0(prediccion_nombre(v, paciente), " (", formato(c_[[v]]), ")")
                                 }, character(1))), "."))
    } else {
      lineas <- c(lineas, paste0("Ninguna variable empuja de forma apreciable hacia ", par[[2]], "."))
    }
  }
  unidad <- if (log_odds) "en log-odds: +1 multiplica por 2,7 las probabilidades a favor de GBM" else
    "en puntos porcentuales de probabilidad"
  c(lineas, paste0("Las contribuciones están ", unidad, ". Sumadas al valor base dan exactamente ",
                   "la predicción del modelo."))
}

# El camino del paciente en un árbol, paso a paso
prediccion_lista_camino <- function(camino) {
  menor <- isTRUE(camino$menor)
  pasos <- lapply(camino$pasos, function(p) {
    tags$li(span(pregunta_nodo(p$variable, p$umbral, menor)),
            tags$b(class = "camino-respuesta", respuesta_nodo(p$variable, p$izquierda)))
  })
  if (identical(camino$escala, "log-odds")) {
    hacia_gbm <- camino$proba > 0
    texto <- paste0("Hoja: suma ", num(camino$proba, 3, signo = TRUE), " a los log-odds, un ",
                    "pequeño empujón hacia ", if (hacia_gbm) "GBM" else "LGG", ".")
  } else {
    hacia_gbm <- camino$proba >= UMBRAL
    texto <- paste0("Hoja con ", num(camino$cobertura, 0), " casos de entrenamiento: ",
                    pct(camino$proba * 100, 0), " GBM. Este árbol dice ",
                    if (hacia_gbm) "GBM" else "LGG", ".")
  }
  pasos[[length(pasos) + 1]] <- tags$li(
    class = "camino-hoja",
    span(class = "chip-dot", style = paste0("background: ", if (hacia_gbm) COLOR_GBM else COLOR_LGG, ";")),
    span(texto)
  )
  tags$ol(class = "camino-lista", pasos)
}

prediccion_control_arbol <- function(id_slider, id_boton, n, valor, boton, ayuda) {
  card(
    div(class = "control-label", "Árbol que se dibuja"),
    fila(
      class = "align-items-center",
      columna(md = 9, sliderInput(id_slider, label = NULL, min = 1, max = n, step = 1,
                                  value = valor, width = "100%")),
      columna(md = 3, class = "text-md-end mt-3 mt-md-0",
              actionButton(id_boton, boton, class = "boton-secundario"))
    ),
    div(class = "control-ayuda mt-2", ayuda),
    class = "controles-card my-4"
  )
}

prediccion_seccion_logistica <- function() {
  tagList(
    section_title("Cómo pesa la regresión logística"),
    paragraph(paste("La regresión logística suma un efecto por variable y convierte esa suma en",
                    "probabilidad con una curva en S. Los efectos son los de la cascada de arriba;",
                    "aquí se ve dónde cae el perfil sobre la curva.")),
    fila(
      columna(lg = 8, class = "mb-4",
              graph_card("fig_log_curva", "La curva en S",
                         "Puntos de fondo: los 168 pacientes de prueba en el color de su grado real.")),
      columna(lg = 4, class = "mb-4", uiOutput("pred_log_resumen"))
    )
  )
}

prediccion_seccion_rf <- function(inicial) {
  n <- length(MODELOS$modelos$random_forest$arboles)
  tagList(
    section_title("Cómo vota el Random Forest"),
    paragraph(paste0("El Random Forest reúne ", n, " árboles de decisión. Cada uno se entrenó ",
                     "con una muestra distinta de pacientes y de variables, así que cada uno da ",
                     "su propia probabilidad de GBM. La predicción del bosque es la media de todas.")),
    graph_card("fig_rf_votacion", paste("Lo que dice cada uno de los", n, "árboles"),
               paste("La línea negra es la media, es decir, la predicción del Random Forest.",
                     "La discontinua marca el árbol que se dibuja abajo.")),
    prediccion_control_arbol("pred_rf_arbol", "pred_rf_rep", n, arbol_representativo(inicial),
                             "Árbol más representativo",
                             "Al predecir se elige el árbol cuya probabilidad está más cerca de la media del bosque."),
    fila(
      columna(lg = 8, class = "mb-4",
              graph_card("fig_rf_arbol", "El árbol completo y el camino del paciente",
                         paste("Cada punto es una pregunta; los extremos son hojas, coloreadas por",
                               "el grado que predicen. Pasa el cursor por un nodo para ver cuántos",
                               "casos llegaron a él."))),
      columna(lg = 4, class = "mb-4",
              card(uiOutput("pred_rf_camino"), titulo = "Paso a paso",
                   subtitulo = "Las preguntas que responde el paciente en este árbol, de la raíz a la hoja."))
    )
  )
}

prediccion_seccion_xgb <- function(inicial) {
  n <- length(MODELOS$modelos$xgboost$arboles)
  tagList(
    section_title("Cómo suma XGBoost"),
    paragraph(paste0("XGBoost no promedia árboles: los suma. Parte de un valor base y cada uno de ",
                     "sus ", n, " árboles, pequeños y de dos preguntas, corrige un poco lo que ",
                     "dejaron los anteriores. La línea muestra cómo llega a la predicción.")),
    graph_card("fig_xgb_suma", "La predicción, árbol a árbol",
               paste("Cada árbol suma o resta en log-odds; el eje lo traduce a probabilidad. La",
                     "discontinua marca el árbol que se dibuja abajo.")),
    prediccion_control_arbol("pred_xgb_arbol", "pred_xgb_rep", n, arbol_mas_influyente(inicial),
                             "Árbol que más pesa",
                             "Al predecir se elige el árbol que más mueve la predicción de este paciente."),
    fila(
      columna(lg = 8, class = "mb-4",
              graph_card("fig_xgb_arbol", "Un árbol de XGBoost y el camino del paciente",
                         paste("Las hojas no dan una probabilidad: suman un valor pequeño a los",
                               "log-odds (coral, hacia GBM; azul, hacia LGG)."))),
      columna(lg = 4, class = "mb-4",
              card(uiOutput("pred_xgb_camino"), titulo = "Paso a paso",
                   subtitulo = "Las preguntas de este árbol y lo que suma."))
    )
  )
}

prediccion_resumen_logistica <- function(z) {
  div(
    class = "log-resumen",
    div(class = "prob-mosaico",
        div(class = "prob-mosaico-nombre", "Suma de efectos"),
        div(class = "prob-mosaico-cifra", num(z, 2, signo = TRUE))),
    div(class = "prob-mosaico",
        div(class = "prob-mosaico-nombre", "Probabilidad de GBM"),
        div(class = "prob-mosaico-cifra", pct(sigmoide(z) * 100, 0))),
    div(class = "prob-nota",
        paste("Por encima de 0 la suma da más del 50 %. Cerca de 0 la curva es empinada y un solo",
              "cambio mueve mucho la probabilidad; en los extremos se aplana y casi no la mueve."))
  )
}

ui_prediccion <- function() {
  inicial <- prediccion_perfil(PERFIL_INICIAL)$paciente
  div(
    class = "vista-pestana",
    titular(
      "03 Los resultados · Predicción",
      "Describe un paciente",
      "y mira qué predicen",
      derecha = div(class = "ambito-radio segmentado-oscuro",
                    radioButtons("pred_modelo", label = NULL, inline = TRUE,
                                 choiceNames = unname(NOMBRES_MODELO[ORDEN_MODELOS]),
                                 choiceValues = ORDEN_MODELOS, selected = "logistica")),
      ayuda = "Modelo que se explica"
    ),
    callout(
      paste("Esta herramienta muestra cómo razonan modelos entrenados con 671 pacientes de",
            "TCGA, con un perfil clínico y mutacional muy limitado. No es un instrumento",
            "diagnóstico ni sustituye a la valoración histológica y molecular de un especialista."),
      titulo = "Uso docente, no clínico",
      color = STATUS_WARNING
    ),
    fila(
      class = "g-4",
      columna(lg = 5, class = "mb-4", prediccion_formulario()),
      columna(
        lg = 7, class = "mb-4 columna-apilada",
        div(class = "bloque-resultados mb-4",
            uiOutput("pred_veredicto"),
            uiOutput("pred_real"),
            uiOutput("pred_probas"),
            div(class = "prob-nota",
                paste("Probabilidad de GBM según cada modelo. Por encima del 50 % (la marca de",
                      "la barra) clasifica como GBM."))),
        prediccion_tarjeta_enjambre()
      )
    ),
    section_title("Por qué lo predice"),
    div(class = "mb-3", series_chips()),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_pred_explicacion", "Contribución de cada variable",
                         paste("Valores SHAP exactos. Coral: empuja hacia GBM; azul: hacia LGG. El",
                               "valor base es la predicción media de la cohorte."))),
      columna(lg = 5, class = "mb-4", uiOutput("pred_lectura"))
    ),
    # Solo se ve la sección del modelo elegido arriba; Shiny no calcula las
    # salidas que quedan ocultas hasta que se muestran
    conditionalPanel("input.pred_modelo == 'logistica'", prediccion_seccion_logistica()),
    conditionalPanel("input.pred_modelo == 'random_forest'", prediccion_seccion_rf(inicial)),
    conditionalPanel("input.pred_modelo == 'xgboost'", prediccion_seccion_xgb(inicial)),
    callout(paragraph(texto_caso_dificil()), titulo = "Dónde se equivocan los modelos")
  )
}

# El peor error de XGBoost en prueba, descrito con sus datos
caso_dificil <- function() {
  fila <- DATOS[PACIENTE_DIFICIL + 1L, ]
  mutados <- GENE_FEATURES[vapply(GENE_FEATURES, function(g) fila[[g]] == 1, logical(1))]
  probas <- unlist(predecir(as.list(fila)))
  list(grado = GRADE_LABELS[[as.character(fila$Grade)]], real = fila$Grade,
       edad = fila$Age_at_diagnosis, mutados = mutados, idh1 = "IDH1" %in% mutados,
       fallan = sum(as.integer(probas >= UMBRAL) != fila$Grade))
}

texto_caso_dificil <- function() {
  d <- caso_dificil()
  otro <- if (d$real == 1) "LGG" else "GBM"
  modelos <- if (d$fallan == 3) "Los tres modelos lo clasifican" else
    paste(tools::toTitleCase(numero_texto(d$fallan)), "de los tres modelos lo clasifican")
  base <- paste0(
    "Prueba a cargar el caso difícil (paciente ", PACIENTE_DIFICIL, "): un ",
    if (d$real == 1) "glioblastoma" else "glioma de bajo grado", " de ", round(d$edad),
    " años con ", if (length(d$mutados)) paste(lista_y(d$mutados), if (length(d$mutados) == 1) "mutado" else "mutados")
    else "ningún gen mutado", ". ", modelos, " como ", otro, ".")
  if (d$real == 1 && d$idh1) {
    paste(base, "La mutación de IDH1 pesa más que todo lo demás. Es el perfil de un astrocitoma",
          "IDH-mutado de grado 4, que la clasificación de la OMS de 2021 ya no llama",
          "glioblastoma; distinguirlo exigiría variables que el dataset no tiene.")
  } else {
    paste(base, "Su perfil se parece más al del otro grado que al suyo: distinguirlo exigiría",
          "variables que el dataset no tiene, como la histología o la deleción de CDKN2A/B.")
  }
}

# «Dónde cae este perfil» dibujado con partículas (www/enjambre.js). Cada punto
# es uno de los pacientes de prueba, en la probabilidad de GBM que le da el
# modelo y en la fila de su grado real. Al cambiar de modelo, los puntos se
# desplazan a su nueva probabilidad (se ve quién cruza el umbral); al predecir,
# el perfil viaja a su sitio y se encienden los pacientes con una probabilidad
# parecida. La lectura escrita de debajo dice lo mismo en palabras. Los datos de
# prueba van en el propio HTML (data-prueba); el modelo y la probabilidad del
# perfil llegan con el mensaje "pred-contexto" (ver server_prediccion).
prediccion_tarjeta_enjambre <- function() {
  test <- RESUMEN$test
  prueba <- c(list(y = as.integer(unlist(test$y))),
              lapply(stats::setNames(ORDEN_MODELOS, ORDEN_MODELOS),
                     function(m) round(as.numeric(unlist(test[[m]])), 4)))
  n_lgg <- sum(prueba$y == 0)
  n_gbm <- sum(prueba$y == 1)
  div(
    class = "card soft-card tarjeta-relleno tarjeta-enjambre",
    div(
      class = "card-body",
      tags$h5(class = "card-title-custom", "Dónde cae este perfil"),
      div(
        id = "pred-enjambre", class = "enjambre-lienzo",
        `data-prueba` = as.character(jsonlite::toJSON(prueba, auto_unbox = FALSE)),
        tags$canvas(role = "img",
                    `aria-label` = paste("Los", length(prueba$y), "pacientes de prueba según la",
                                         "probabilidad de GBM que les da el modelo")),
        div(class = "enjambre-punta", `aria-hidden` = "true")
      ),
      div(
        class = "enjambre-leyenda", `aria-hidden` = "true",
        span(tags$i(class = "enjambre-punto lgg"), paste0("LGG real (", n_lgg, ")")),
        span(tags$i(class = "enjambre-punto gbm"), paste0("GBM real (", n_gbm, ")")),
        span(tags$i(class = "enjambre-punto perfil"), "este perfil")
      ),
      div(id = "pred-contexto-lectura", class = "enjambre-lectura", `aria-live` = "polite"),
      div(class = "graph-note",
          paste("Probabilidades que da el modelo elegido arriba a los", length(prueba$y),
                "pacientes de prueba, separados por su grado real. Pasa el ratón por un punto",
                "para ver su valor."))
    )
  )
}

server_prediccion <- function(input, output, session) {
  cargado <- reactiveVal(prediccion_perfil(PERFIL_INICIAL))
  # Último paciente enviado con el botón: los resultados solo se recalculan
  # cuando cambia este, no con cada clic en el formulario
  predicho <- reactiveVal(prediccion_perfil(PERFIL_INICIAL)$paciente)

  # Elegir un perfil rellena el formulario (y los genes se encienden en cascada)
  observeEvent(input$pred_perfil, ignoreInit = TRUE, {
    perfil <- prediccion_perfil(input$pred_perfil)
    p <- perfil$paciente
    # El deslizador trabaja con años enteros; el paciente guardado también, para
    # que el formulario y el paciente cargado sigan siendo "el mismo" al comparar
    p$Age_at_diagnosis <- round(p$Age_at_diagnosis)
    perfil$paciente <- p
    mutados <- GENE_FEATURES[vapply(GENE_FEATURES, function(g) p[[g]] == 1, logical(1))]
    session$sendCustomMessage("cargando_perfil", TRUE)
    updateSliderInput(session, "pred_edad", value = p$Age_at_diagnosis)
    updateRadioButtons(session, "pred_genero", selected = as.character(p$Gender))
    updateSelectInput(session, "pred_raza", selected = as.character(p$Race))
    updateCheckboxGroupInput(session, "pred_genes_panel", selected = intersect(GENES_PANEL, mutados))
    resto <- setdiff(mutados, GENES_PANEL)
    # Un vector vacío no desmarca nada en updateCheckboxGroupInput: hace falta character(0)
    updateCheckboxGroupInput(session, "pred_genes_resto", selected = if (length(resto)) resto else character(0))
    cargado(perfil)
  })

  formulario <- reactive({
    req(input$pred_edad)
    prediccion_paciente_formulario(input$pred_edad, input$pred_genero, input$pred_raza,
                                   input$pred_genes_panel, input$pred_genes_resto)
  })
  observeEvent(input$pred_predecir, predicho(formulario()))

  output$pred_edad_valor <- renderText(paste(input$pred_edad %||% 50, "años"))
  output$pred_genes_cuenta <- renderText({
    n <- length(union(input$pred_genes_panel, input$pred_genes_resto))
    if (n == 1) "1 marcado" else paste(n, "marcados")
  })
  output$pred_pendiente <- renderText({
    if (mismo_paciente(formulario(), predicho())) "" else "Hay cambios sin aplicar"
  })

  modelo <- reactive(if (is.null(input$pred_modelo)) "logistica" else input$pred_modelo)
  probas <- reactive(predecir(predicho()))
  explicacion <- reactive(explicar(predicho(), modelo()))

  output$pred_veredicto <- renderUI(prediccion_veredicto(probas()))
  output$pred_real <- renderUI(prediccion_real(cargado(), predicho(), probas()))
  output$pred_probas <- renderUI(prediccion_mosaicos(probas()))
  # El enjambre se mueve en el navegador: aquí solo se le dice qué modelo
  # mirar y dónde cae el perfil. También al entrar en la pestaña, porque la
  # tarjeta se monta de nuevo en cada visita.
  observe({
    if (!identical(input$pestana %||% TAB_INICIAL, "tab-prediccion")) return()
    session$sendCustomMessage("pred-contexto", list(modelo = modelo(), p = probas()[[modelo()]]))
  })
  output$fig_pred_explicacion <- plotly::renderPlotly(fig_explicacion(explicacion(), predicho()))
  output$pred_lectura <- renderUI({
    lectura_guiada(prediccion_lectura(explicacion(), predicho(), modelo(), probas()[[modelo()]]),
                   titulo = "Lectura de esta predicción")
  })

  # Regresión logística
  output$fig_log_curva <- plotly::renderPlotly(fig_curva_logistica(logodds_logistica(predicho())))
  output$pred_log_resumen <- renderUI(prediccion_resumen_logistica(logodds_logistica(predicho())))

  # Random Forest: con cada predicción nueva, o al pedirlo, vuelve al árbol representativo
  observeEvent(list(predicho(), input$pred_rf_rep), ignoreInit = TRUE, {
    updateSliderInput(session, "pred_rf_arbol", value = arbol_representativo(predicho()))
  })
  arbol_rf <- reactive({
    n <- length(MODELOS$modelos$random_forest$arboles)
    min(max(as.integer(input$pred_rf_arbol %||% 1L), 1L), n)
  })
  camino_rf <- reactive(camino_paciente(predicho(), arbol_rf(), "random_forest"))
  output$fig_rf_votacion <- plotly::renderPlotly(fig_votacion(probas_arboles(predicho()), arbol_rf()))
  output$fig_rf_arbol <- plotly::renderPlotly(
    fig_arbol(MODELOS$modelos$random_forest$arboles[[arbol_rf()]], camino_rf()))
  output$pred_rf_camino <- renderUI(prediccion_lista_camino(camino_rf()))

  # XGBoost: con cada predicción nueva, o al pedirlo, vuelve al árbol que más pesa
  observeEvent(list(predicho(), input$pred_xgb_rep), ignoreInit = TRUE, {
    updateSliderInput(session, "pred_xgb_arbol", value = arbol_mas_influyente(predicho()))
  })
  arbol_xgb <- reactive({
    n <- length(MODELOS$modelos$xgboost$arboles)
    min(max(as.integer(input$pred_xgb_arbol %||% 1L), 1L), n)
  })
  camino_xgb <- reactive(camino_paciente(predicho(), arbol_xgb(), "xgboost"))
  output$fig_xgb_suma <- plotly::renderPlotly({
    suma <- aportes_arboles(predicho())
    fig_suma_xgb(suma$base, suma$aportes, arbol_xgb())
  })
  output$fig_xgb_arbol <- plotly::renderPlotly(
    fig_arbol(MODELOS$modelos$xgboost$arboles[[arbol_xgb()]], camino_xgb()))
  output$pred_xgb_camino <- renderUI(prediccion_lista_camino(camino_xgb()))
}

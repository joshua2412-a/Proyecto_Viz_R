# =========================================================================== #
# Carga del dataset y estadística descriptiva
#
# Versión en R de utils/data_loader.py. Reproduce los mismos contrastes con
# las funciones base de R, que dan los mismos resultados que scipy:
#
#   scipy.stats.mannwhitneyu        -> wilcox.test(exact = FALSE, correct = TRUE)
#   scipy.stats.chi2_contingency    -> chisq.test(correct = TRUE)  (Yates en 2x2)
#   scipy.stats.fisher_exact        -> fisher.test
#   scipy.stats.spearmanr           -> cor(method = "spearman")
#   pandas .skew()                  -> asimetría ajustada de Fisher-Pearson (G1)
#
# La partición train/test es la del libro (80/20 estratificada): se leen
# data/train_data.csv y data/test_data.csv, los mismos archivos que usan el
# libro en R y sus modelos.
# =========================================================================== #

AMBITOS <- c("train", "test", "full")
AMBITO_NOMBRES <- c(
  train = "conjunto de entrenamiento",
  test = "conjunto de prueba",
  full = "dataset completo"
)

ALFA <- 0.05
EFECTO_MINIMO <- 0.10

# --------------------------------------------------------------------------- #
# Dataset y partición
# --------------------------------------------------------------------------- #
cargar_datos <- function() {
  if (!file.exists(DATA_PATH)) {
    stop("No se encontró ", DATA_PATH, ". Coloca TCGA_InfoWithGrade.csv en data/.")
  }
  df <- utils::read.csv(DATA_PATH, check.names = FALSE)
  faltantes <- setdiff(c(TARGET, FEATURES), names(df))
  if (length(faltantes)) {
    stop("El dataset no tiene las columnas esperadas. Faltan: ",
         paste(faltantes, collapse = ", "))
  }
  df$grade_label <- unname(GRADE_LABELS[as.character(df$Grade)])
  df$gender_label <- unname(GENDER_LABELS[as.character(df$Gender)])
  df$race_label <- unname(RACE_LABELS[as.character(df$Race)])
  df
}

# Cada fila de train/test se localiza en el dataset completo por su contenido
# (todas las columnas); si un perfil aparece repetido, cada copia ocupa una
# fila distinta. Así el resto de la app trabaja con índices de DATOS.
cargar_particion <- function(datos) {
  for (ruta in c(TRAIN_PATH, TEST_PATH)) {
    if (!file.exists(ruta)) stop("No se encontró ", ruta, ".")
  }
  columnas <- c(FEATURES, TARGET)
  clave <- function(d) do.call(paste, c(d[, columnas], sep = "|"))
  todas <- clave(datos)
  usadas <- logical(length(todas))
  localizar <- function(ruta) {
    parte <- utils::read.csv(ruta, check.names = FALSE)
    vapply(clave(parte), function(k) {
      i <- which(todas == k & !usadas)[1]
      if (is.na(i)) stop("Una fila de ", ruta, " no está en el dataset completo.")
      usadas[i] <<- TRUE
      i
    }, integer(1), USE.NAMES = FALSE)
  }
  list(train = localizar(TRAIN_PATH), test = localizar(TEST_PATH), full = seq_len(nrow(datos)))
}

DATOS <- cargar_datos()
INDICES <- cargar_particion(DATOS)

get_df <- function(ambito = "train") {
  if (!ambito %in% AMBITOS) stop("Ámbito no válido: ", ambito)
  DATOS[INDICES[[ambito]], , drop = FALSE]
}

tamanos_particion <- function() vapply(INDICES, length, integer(1))

# Caché por ámbito: cada cálculo se hace una vez por proceso, aunque varias
# pestañas y varias sesiones lo pidan.
.cache <- new.env(parent = emptyenv())
cacheado <- function(clave, expr) {
  if (!exists(clave, envir = .cache, inherits = FALSE)) {
    assign(clave, force(expr), envir = .cache)
  }
  get(clave, envir = .cache, inherits = FALSE)
}

# --------------------------------------------------------------------------- #
# Utilidades estadísticas
# --------------------------------------------------------------------------- #
asimetria <- function(x) {
  # La de pandas: coeficiente de Fisher-Pearson ajustado por tamaño (G1)
  n <- length(x)
  m <- mean(x)
  m2 <- mean((x - m)^2)
  m3 <- mean((x - m)^3)
  if (n < 3 || m2 == 0) return(NA_real_)
  g1 <- m3 / m2^1.5
  g1 * sqrt(n * (n - 1)) / (n - 2)
}

spearman <- function(x, y) {
  suppressWarnings(stats::cor(as.numeric(x), as.numeric(y), method = "spearman"))
}

mann_whitney <- function(x, y) {
  prueba <- stats::wilcox.test(x, y, alternative = "two.sided",
                               exact = FALSE, correct = TRUE)
  list(estadistico = unname(prueba$statistic), p_valor = prueba$p.value)
}

# Chi-cuadrado sin Yates y V de Cramér con la corrección de sesgo de Bergsma,
# como el libro: Yates es un ajuste del contraste, no del tamaño del efecto.
chi2_sin_yates <- function(tabla) {
  esperadas <- outer(rowSums(tabla), colSums(tabla)) / sum(tabla)
  sum((tabla - esperadas)^2 / esperadas)
}

cramer_v <- function(tabla) {
  tabla <- as.matrix(tabla)
  if (nrow(tabla) < 2 || ncol(tabla) < 2) return(0)
  n <- sum(tabla)
  if (n <= 1) return(0)
  r <- nrow(tabla)
  k <- ncol(tabla)
  phi2 <- chi2_sin_yates(tabla) / n
  phi2_corr <- max(0, phi2 - ((k - 1) * (r - 1)) / (n - 1))
  r_corr <- r - ((r - 1)^2) / (n - 1)
  k_corr <- k - ((k - 1)^2) / (n - 1)
  denominador <- min(k_corr - 1, r_corr - 1)
  if (denominador <= 0) return(0)
  sqrt(phi2_corr / denominador)
}

# La prueba de independencia del libro: chi-cuadrado, o Fisher exacto si la
# tabla es 2x2 y alguna frecuencia esperada baja de 5.
prueba_independencia <- function(tabla) {
  tabla <- as.matrix(tabla)
  if (nrow(tabla) < 2 || ncol(tabla) < 2) {
    # Una sola categoría presente: no hay nada que contrastar (scipy da p = 1)
    return(list(chi2 = 0, gl = 0L, esperada_min = NA_real_, p_chi2 = 1,
                p_valor = 1, v_cramer = 0, usar_fisher = FALSE,
                prueba = "Chi-cuadrado"))
  }
  esperadas <- outer(rowSums(tabla), colSums(tabla)) / sum(tabla)
  p_chi2 <- suppressWarnings(stats::chisq.test(tabla, correct = TRUE)$p.value)
  usar_fisher <- all(dim(tabla) == c(2, 2)) && any(esperadas < 5)
  p_final <- if (usar_fisher) stats::fisher.test(tabla)$p.value else p_chi2
  list(
    chi2 = chi2_sin_yates(tabla),
    gl = as.integer((nrow(tabla) - 1) * (ncol(tabla) - 1)),
    esperada_min = min(esperadas),
    p_chi2 = p_chi2,
    p_valor = p_final,
    v_cramer = cramer_v(tabla),
    usar_fisher = usar_fisher,
    prueba = if (usar_fisher) "Fisher exacto" else "Chi-cuadrado"
  )
}

race_binaria <- function(df) as.integer(df$Race != 0)

serie_para_prueba <- function(variable, df) {
  if (variable == "Race") unname(RACE_AGRUPADA[as.character(df$Race)]) else df[[variable]]
}

# --------------------------------------------------------------------------- #
# Descriptivos de la variable objetivo y de las clínicas
# --------------------------------------------------------------------------- #
proporcion_grado <- function(ambito = "train") {
  df <- get_df(ambito)
  conteo <- as.numeric(table(factor(df$grade_label, levels = ORDEN_GRADE)))
  data.frame(grado = ORDEN_GRADE, pacientes = conteo,
             porcentaje = conteo / sum(conteo) * 100)
}

estadisticas_edad <- function(ambito = "train") {
  df <- get_df(ambito)
  filas <- lapply(ORDEN_GRADE, function(g) {
    x <- df$Age_at_diagnosis[df$grade_label == g]
    q <- stats::quantile(x, c(0.25, 0.75), names = FALSE)
    data.frame(grade_label = g, pacientes = length(x), media = mean(x),
               desviacion = stats::sd(x), mediana = stats::median(x),
               q1 = q[1], q3 = q[2], minimo = min(x), maximo = max(x),
               iqr = q[2] - q[1])
  })
  out <- do.call(rbind, filas)
  rownames(out) <- out$grade_label
  out
}

prueba_edad_por_grado <- function(ambito = "train") {
  df <- get_df(ambito)
  lgg <- df$Age_at_diagnosis[df$Grade == 0]
  gbm <- df$Age_at_diagnosis[df$Grade == 1]
  mw <- mann_whitney(lgg, gbm)
  list(estadistico = mw$estadistico, p_valor = mw$p_valor,
       n_lgg = length(lgg), n_gbm = length(gbm))
}

distribucion_clinica <- function(variable, ambito = "train") {
  df <- get_df(ambito)
  columna_etiqueta <- c(Gender = "gender_label", Race = "race_label")[[variable]]
  tabla <- as.data.frame(table(grade_label = df$grade_label,
                               categoria = df[[columna_etiqueta]]),
                         stringsAsFactors = FALSE)
  names(tabla)[3] <- "pacientes"
  tabla <- tabla[tabla$pacientes > 0, ]
  totales <- stats::ave(tabla$pacientes, tabla$grade_label, FUN = sum)
  tabla$porcentaje <- tabla$pacientes / totales * 100
  tabla
}

# --------------------------------------------------------------------------- #
# Mutaciones
# --------------------------------------------------------------------------- #
prevalencia_genes <- function(ambito = "train") {
  cacheado(paste0("prevalencia_", ambito), {
    df <- get_df(ambito)
    filas <- lapply(GENE_FEATURES, function(gen) {
      lgg <- mean(df[[gen]][df$Grade == 0]) * 100
      gbm <- mean(df[[gen]][df$Grade == 1]) * 100
      data.frame(gen = gen, prevalencia = mean(df[[gen]]) * 100,
                 mutados = sum(df[[gen]]), LGG = lgg, GBM = gbm,
                 diferencia = gbm - lgg)
    })
    out <- do.call(rbind, filas)
    out <- out[order(-out$prevalencia), ]
    rownames(out) <- NULL
    out
  })
}

asociacion_con_grado <- function(ambito = "train") {
  cacheado(paste0("asociacion_", ambito), {
    df <- get_df(ambito)
    filas <- lapply(c(NUMERIC_FEATURES, CATEGORICAL_FEATURES, GENE_FEATURES), function(variable) {
      serie_orden <- if (variable == "Race") race_binaria(df) else df[[variable]]
      rho <- spearman(serie_orden, df$Grade)

      if (variable %in% NUMERIC_FEATURES) {
        mw <- mann_whitney(df[[variable]][df$Grade == 0], df[[variable]][df$Grade == 1])
        p_variable <- mw$p_valor
        v <- NA_real_
        prueba <- "U de Mann-Whitney"
        tipo <- "Clínica numérica"
      } else {
        resultado <- prueba_independencia(table(serie_para_prueba(variable, df), df$Grade))
        p_variable <- resultado$p_valor
        v <- resultado$v_cramer
        prueba <- resultado$prueba
        tipo <- if (variable %in% CATEGORICAL_FEATURES) "Clínica categórica" else "Mutación"
      }
      magnitud <- abs(rho)
      data.frame(
        variable = variable, tipo = tipo, prueba = prueba, rho = rho,
        v_cramer = v, p_valor = p_variable, magnitud = magnitud,
        significativa = isTRUE(p_variable < ALFA && magnitud >= EFECTO_MINIMO),
        stringsAsFactors = FALSE
      )
    })
    tabla <- do.call(rbind, filas)
    # Benjamini-Hochberg sobre los 23 contrastes, igual que el libro
    tabla$q_valor <- stats::p.adjust(tabla$p_valor, method = "BH")
    tabla$sobrevive_bh <- tabla$q_valor < ALFA
    tabla <- tabla[order(-abs(tabla$rho), na.last = TRUE), ]
    rownames(tabla) <- NULL
    tabla
  })
}

matriz_spearman <- function(ambito = "train") {
  cacheado(paste0("spearman_", ambito), {
    df <- get_df(ambito)
    columnas <- c(TARGET, "Gender", NUMERIC_FEATURES, GENE_FEATURES)
    m <- df[, columnas]
    m[["Race (binaria)"]] <- race_binaria(df)
    suppressWarnings(stats::cor(as.matrix(m), method = "spearman"))
  })
}

vif_predictoras <- function(ambito = "train") {
  cacheado(paste0("vif_", ambito), {
    df <- get_df(ambito)
    m <- data.frame(
      Age_at_diagnosis = as.numeric(df$Age_at_diagnosis),
      Gender = as.numeric(df$Gender),
      Race = as.numeric(df$Race != 0)
    )
    for (gen in GENE_FEATURES) m[[gen]] <- as.numeric(df[[gen]])
    m <- as.matrix(m)

    filas <- lapply(colnames(m), function(nombre) {
      objetivo <- m[, nombre]
      diseno <- cbind(1, m[, colnames(m) != nombre, drop = FALSE])
      ajuste <- stats::lm.fit(diseno, objetivo)
      suma_total <- sum((objetivo - mean(objetivo))^2)
      r2 <- if (suma_total > 0) 1 - sum(ajuste$residuals^2) / suma_total else 0
      r2 <- min(max(r2, 0), 1 - 1e-9)
      data.frame(variable = nombre, r2 = r2, vif = 1 / (1 - r2))
    })
    out <- do.call(rbind, filas)
    out <- out[order(-out$vif), ]
    rownames(out) <- NULL
    out
  })
}

matriz_asociacion_genes <- function(ambito = "train", top_n = 12) {
  cacheado(paste0("genes_", ambito, "_", top_n), {
    df <- get_df(ambito)
    genes <- utils::head(prevalencia_genes(ambito)$gen, top_n)
    m <- diag(length(genes))
    dimnames(m) <- list(genes, genes)
    for (i in seq_along(genes)) {
      for (j in seq_along(genes)) {
        if (j <= i) next
        tabla <- table(df[[genes[i]]], df[[genes[j]]])
        valor <- if (all(dim(tabla) == c(2, 2))) cramer_v(tabla) else 0
        m[i, j] <- valor
        m[j, i] <- valor
      }
    }
    m
  })
}

# --------------------------------------------------------------------------- #
# Exploración interactiva (pestaña Exploración)
# --------------------------------------------------------------------------- #
TIPO_NUMERICA <- "numerica"
TIPO_BINARIA <- "binaria"
TIPO_CATEGORICA <- "categorica"
TIPO_NOMBRES <- c(numerica = "Numérica continua", binaria = "Binaria",
                  categorica = "Categórica nominal")

tipo_variable <- function(variable) {
  if (variable %in% NUMERIC_FEATURES) return(TIPO_NUMERICA)
  if (variable %in% GENE_FEATURES || variable == "Gender") return(TIPO_BINARIA)
  if (variable %in% CATEGORICAL_FEATURES) return(TIPO_CATEGORICA)
  stop(variable, " no es una predictora del dataset.")
}

etiquetas_categorias <- function(variable) {
  if (variable == "Gender") return(GENDER_LABELS)
  if (variable == "Race") return(RACE_LABELS)
  if (variable %in% GENE_FEATURES) return(MUTACION_LABELS)
  NULL
}

nombre_variable <- function(variable) {
  if (variable %in% GENE_FEATURES) variable else etiqueta(variable)
}

opciones_variables <- function() {
  genes <- prevalencia_genes("train")$gen
  c(
    "Edad al diagnóstico" = "Age_at_diagnosis",
    "Género" = "Gender",
    "Grupo racial reportado" = "Race",
    stats::setNames(genes, paste(genes, "· mutación"))
  )
}

ficha_variable <- function(variable) {
  tipo <- tipo_variable(variable)
  etiquetas <- etiquetas_categorias(variable)
  es_gen <- variable %in% GENE_FEATURES
  if (es_gen) {
    descripcion <- unname(GENE_DESCRIPCION[variable])
    papel <- "Predictora molecular · chi-cuadrado, V de Cramér y Spearman"
  } else {
    descripcion <- unname(CLINICA_DESCRIPCION[variable])
    papel <- unname(VARIABLE_PAPEL[variable])
  }
  codificacion <- if (tipo == TIPO_NUMERICA) {
    "Continua, en años (con decimales)"
  } else {
    paste(paste(names(etiquetas), "=", etiquetas), collapse = " · ")
  }
  list(variable = variable, nombre = nombre_variable(variable), tipo = tipo,
       tipo_nombre = unname(TIPO_NOMBRES[tipo]), codificacion = codificacion,
       descripcion = descripcion, papel = papel, es_gen = es_gen)
}

serie_legible <- function(variable, df) {
  etiquetas <- etiquetas_categorias(variable)
  if (is.null(etiquetas)) df[[variable]] else unname(etiquetas[as.character(df[[variable]])])
}

# Conteo de mayor a menor, como value_counts() de pandas
conteo_desc <- function(x) {
  t <- table(x)
  t[order(-as.numeric(t))]
}

resumen_univariado <- function(variable, ambito = "train") {
  df <- get_df(ambito)
  tipo <- tipo_variable(variable)
  serie <- df[[variable]]
  n <- length(serie)

  if (tipo == TIPO_NUMERICA) {
    q <- stats::quantile(serie, c(0.25, 0.75), names = FALSE)
    iqr <- q[2] - q[1]
    atipicos <- sum(serie < q[1] - 1.5 * iqr | serie > q[2] + 1.5 * iqr)
    filas <- list(
      c("Observaciones", as.character(n)),
      c("Media", num(mean(serie), 2)),
      c("Mediana", num(stats::median(serie), 2)),
      c("Desviación estándar", num(stats::sd(serie), 2)),
      c("Rango intercuartílico", paste(num(q[1], 2), "a", num(q[2], 2))),
      c("Mínimo y máximo", paste(num(min(serie), 2), "a", num(max(serie), 2))),
      c("Asimetría", num(asimetria(serie), 2, signo = TRUE)),
      c("Atípicos (criterio 1,5 · IQR)", as.character(atipicos))
    )
    return(list(tipo = tipo, n = n, filas = filas, atipicos = atipicos))
  }

  conteo <- conteo_desc(serie_legible(variable, df))
  k <- length(conteo)
  filas <- list(
    c("Observaciones", as.character(n)),
    c("Categorías", as.character(k)),
    c("Más frecuente", names(conteo)[1]),
    c("Frecuencia", paste0(conteo[[1]], " (", pct(conteo[[1]] / n * 100), ")")),
    c("Menos frecuente", names(conteo)[k]),
    c("Frecuencia", paste0(conteo[[k]], " (", pct(conteo[[k]] / n * 100), ")"))
  )
  if (variable %in% GENE_FEATURES) {
    filas <- append(filas, list(c("Prevalencia de mutación", pct(mean(serie) * 100))), after = 1)
  }
  list(tipo = tipo, n = n, filas = filas, conteo = conteo)
}

distribucion_univariada <- function(variable, ambito = "train") {
  conteo <- conteo_desc(serie_legible(variable, get_df(ambito)))
  data.frame(categoria = names(conteo), pacientes = as.numeric(conteo),
             porcentaje = as.numeric(conteo) / sum(conteo) * 100,
             stringsAsFactors = FALSE)
}

tabla_bivariada <- function(variable, ambito = "train") {
  df <- get_df(ambito)
  categoria <- as.character(serie_legible(variable, df))
  tabla <- as.data.frame(table(categoria = categoria, grade_label = df$grade_label),
                         stringsAsFactors = FALSE)
  names(tabla)[3] <- "pacientes"
  tabla <- tabla[tabla$pacientes > 0, ]
  tabla$total_categoria <- stats::ave(tabla$pacientes, tabla$categoria, FUN = sum)
  tabla$porcentaje <- tabla$pacientes / tabla$total_categoria * 100
  rownames(tabla) <- NULL
  tabla
}

prueba_bivariada <- function(variable, ambito = "train") {
  df <- get_df(ambito)
  tipo <- tipo_variable(variable)
  serie_orden <- if (variable == "Race") race_binaria(df) else df[[variable]]
  rho <- spearman(serie_orden, df$Grade)
  direccion <- if (!is.na(rho) && rho < 0) "LGG" else "GBM"

  if (tipo == TIPO_NUMERICA) {
    lgg <- df[[variable]][df$Grade == 0]
    gbm <- df[[variable]][df$Grade == 1]
    mw <- mann_whitney(lgg, gbm)
    x <- df[[variable]]
    p_normalidad <- if (length(x) >= 3 && length(x) <= 5000) stats::shapiro.test(x)$p.value else NA
    detalle <- list(
      c("Hipótesis nula", "Las dos distribuciones de la variable son iguales en LGG y en GBM."),
      c("Estadístico $U$", miles(mw$estadistico)),
      c("Tamaño del efecto", paste0("$r$ de Spearman $= ", tnum(rho, 3, signo = TRUE), "$")),
      c("Medianas", paste0("LGG ", num(stats::median(lgg)), " · GBM ", num(stats::median(gbm)))),
      c("Normalidad (Shapiro-Wilk)", p_tex(p_normalidad))
    )
    return(list(
      prueba = "U de Mann-Whitney",
      motivo = paste("La variable es continua y no sigue una distribución normal,",
                     "así que se comparan rangos en vez de medias."),
      p_valor = mw$p_valor, rho = rho, v_cramer = NA_real_, magnitud = abs(rho),
      magnitud_nombre = "$|r|$ de Spearman",
      significativa = isTRUE(mw$p_valor < ALFA && abs(rho) >= EFECTO_MINIMO),
      detalle = detalle, direccion = direccion, hay_direccion = TRUE, nota_metodo = ""
    ))
  }

  r <- prueba_independencia(table(serie_para_prueba(variable, df), df$Grade))
  if (r$usar_fisher) {
    nombre_prueba <- "Prueba exacta de Fisher"
    motivo <- paste("La tabla es de 2x2 y alguna frecuencia esperada baja de 5, así",
                    "que la aproximación del chi-cuadrado no es fiable y se calcula",
                    "la probabilidad exacta.")
  } else {
    nombre_prueba <- "Chi-cuadrado de independencia"
    motivo <- paste("Las dos variables son categóricas, así que se compara la tabla",
                    "de contingencia observada con la que cabría esperar si no",
                    "hubiera relación.")
  }
  detalle <- list(
    c("Hipótesis nula", "La variable y el grado tumoral son independientes."),
    c("Estadístico $\\chi^2$", paste0(num(r$chi2, 2), " con ", r$gl, " grado",
                               if (r$gl != 1) "s" else "", " de libertad")),
    c("Frecuencia esperada mínima", num(r$esperada_min, 1)),
    c("Tamaño del efecto", paste("V de Cramér =", num(r$v_cramer, 3))),
    c("Dirección", paste0("$r$ de Spearman $= ", tnum(rho, 3, signo = TRUE), "$",
                          if (variable == "Race") " (sobre la versión reagrupada)" else ""))
  )
  nota <- ""
  if (variable == "Race") {
    nota <- paste(
      "Las cuatro categorías se reagruparon en «White» y «Other racial",
      "groups» antes de la prueba, igual que en el libro: Asian y American",
      "Indian or Alaska Native dejaban casillas con frecuencia esperada",
      "demasiado baja. El gráfico sigue mostrando las cuatro, porque",
      "agrupar es una decisión de la prueba y no de la descripción."
    )
  } else if (r$usar_fisher) {
    nota <- paste0(
      "Se reporta Fisher exacto y no chi-cuadrado porque la tabla tiene ",
      "una frecuencia esperada mínima de ", num(r$esperada_min, 1),
      ", por debajo de 5. Es la misma regla que aplica el notebook."
    )
  }
  list(
    prueba = nombre_prueba, motivo = motivo, p_valor = r$p_valor, rho = rho,
    v_cramer = r$v_cramer, magnitud = abs(rho), magnitud_nombre = "$|r|$ de Spearman",
    significativa = isTRUE(r$p_valor < ALFA && abs(rho) >= EFECTO_MINIMO),
    detalle = detalle, direccion = direccion, hay_direccion = TRUE, nota_metodo = nota
  )
}

lectura_univariada <- function(variable, ambito = "train") {
  resumen <- resumen_univariado(variable, ambito)
  ficha <- ficha_variable(variable)
  df <- get_df(ambito)

  if (resumen$tipo == TIPO_NUMERICA) {
    serie <- df[[variable]]
    sesgo <- asimetria(serie)
    forma <- if (abs(sesgo) < 0.25) "es prácticamente simétrica" else
      if (sesgo > 0) "se alarga hacia la derecha" else "se alarga hacia la izquierda"
    q <- stats::quantile(serie, c(0.25, 0.75), names = FALSE)
    lineas <- c(
      paste0("La distribución ", forma, " (asimetría ", num(sesgo, 2, signo = TRUE),
             ") y se concentra entre los ", sprintf("%.0f", q[1]), " y los ",
             sprintf("%.0f", q[2]), " años, con una mediana de ",
             sprintf("%.0f", stats::median(serie)), "."),
      if (resumen$atipicos > 0) {
        paste0("Hay ", resumen$atipicos, " observaciones atípicas según el criterio ",
               "de 1,5 veces el rango intercuartílico; no se eliminan, porque son ",
               "edades clínicamente posibles.")
      } else {
        paste("No hay observaciones atípicas según el criterio de 1,5 veces el",
              "rango intercuartílico.")
      }
    )
    return(lineas)
  }

  conteo <- resumen$conteo
  total <- resumen$n
  porcentaje <- conteo[[1]] / total * 100
  lineas <- character(0)
  if (ficha$es_gen) {
    prevalencia <- mean(df[[variable]]) * 100
    frecuencia <- if (prevalencia >= 40) "muy frecuente" else if (prevalencia >= 15)
      "frecuente" else if (prevalencia >= 5) "poco frecuente" else "rara"
    lineas <- c(lineas, paste0(
      "La mutación en ", variable, " es ", frecuencia, " en esta cohorte: aparece en ",
      pct(prevalencia), " de los pacientes (", sum(df[[variable]]), " de ", total, ")."
    ))
    if (prevalencia < 5) {
      lineas <- c(lineas, paste("Con tan pocos casos mutados, cualquier asociación que se le",
                                "encuentre descansa sobre un grupo muy pequeño."))
    }
  } else {
    lineas <- c(lineas, paste0("La categoría más frecuente es «", names(conteo)[1], "», con ",
                               pct(porcentaje), " de los ", total, " pacientes."))
    if (porcentaje >= 80) {
      lineas <- c(lineas, paste(
        "El reparto está muy desbalanceado: las categorías minoritarias",
        "tienen tan pocas observaciones que cualquier porcentaje calculado",
        "sobre ellas es inestable."))
    } else if (length(conteo) == 2) {
      lineas <- c(lineas, paste0("La otra categoría, «", names(conteo)[2], "», reúne el ",
                                 pct(conteo[[2]] / total * 100), " restante."))
    }
  }
  lineas
}

lectura_bivariada <- function(variable, ambito = "train") {
  prueba <- prueba_bivariada(variable, ambito)
  tipo <- tipo_variable(variable)

  if (tipo == TIPO_NUMERICA) {
    df <- get_df(ambito)
    lgg <- stats::median(df[[variable]][df$Grade == 0])
    gbm <- stats::median(df[[variable]][df$Grade == 1])
    return(c(
      paste0("La mediana es de ", sprintf("%.0f", lgg), " años en LGG y de ",
             sprintf("%.0f", gbm), " en GBM: una diferencia de ",
             sprintf("%.0f", abs(gbm - lgg)), " años, y la U de Mann-Whitney la ",
             "da por distinguible del azar (", p_valor(prueba$p_valor), ")."),
      paste0("La magnitud de la asociación es $r = ", tnum(prueba$rho, 2, signo = TRUE),
             "$, el correlato clínico más fuerte del panel, aunque no separa los grupos ",
             "por sí sola: las dos distribuciones se solapan ampliamente.")
    ))
  }

  lineas <- character(0)
  tabla <- tabla_bivariada(variable, ambito)
  gbm <- tabla[tabla$grade_label == "GBM", ]
  gbm <- gbm[order(-gbm$porcentaje), ]
  if (nrow(gbm) > 0) {
    alta <- gbm[1, ]
    baja <- gbm[nrow(gbm), ]
    lineas <- c(lineas, paste0(
      "De los pacientes con «", alta$categoria, "», el ", pct(alta$porcentaje),
      " son GBM; con «", baja$categoria, "» son el ", pct(baja$porcentaje),
      ". La brecha es de ", num(alta$porcentaje - baja$porcentaje), " puntos."
    ))
  }
  if (prueba$significativa) {
    lineas <- c(lineas, paste0(
      prueba$prueba, " encuentra asociación (", p_valor(prueba$p_valor),
      ") y la V de Cramér la sitúa en ", num(prueba$v_cramer, 2),
      if (prueba$hay_direccion) paste0(", empujando hacia ", prueba$direccion, ".") else "."
    ))
  } else if (prueba$p_valor < ALFA) {
    lineas <- c(lineas, paste0(
      prueba$prueba, " sale significativa (", p_valor(prueba$p_valor),
      "), pero la magnitud es pequeña (", prueba$magnitud_nombre, " = ",
      num(prueba$magnitud, 2), ", por debajo del umbral de ", num(EFECTO_MINIMO, 2),
      " que usa el proyecto): con esta muestra, una diferencia mínima basta para ",
      "alcanzar significancia."
    ))
  } else {
    lineas <- c(lineas, paste0(
      prueba$prueba, " no encuentra asociación (", p_valor(prueba$p_valor),
      "): esta variable no distingue los dos grados en esta muestra."
    ))
  }
  if (nzchar(prueba$nota_metodo)) lineas <- c(lineas, prueba$nota_metodo)
  lineas
}

# --------------------------------------------------------------------------- #
# Lecturas del EDA (pestaña Resultados): se calculan, no se escriben a mano
# --------------------------------------------------------------------------- #
lectura_cohorte <- function(ambito = "train") {
  prop <- proporcion_grado(ambito)
  rownames(prop) <- prop$grado
  edad <- estadisticas_edad(ambito)
  prueba <- prueba_edad_por_grado(ambito)
  diferencia <- edad["GBM", "mediana"] - edad["LGG", "mediana"]
  c(
    paste0("La cohorte está repartida ", pct(prop["LGG", "porcentaje"]), " LGG y ",
           pct(prop["GBM", "porcentaje"]), " GBM. No hay desbalance severo, así que ",
           "los porcentajes de cada grupo se pueden comparar sin corregir nada."),
    paste0("El GBM se diagnostica ", num(diferencia, 0), " años más tarde: mediana de ",
           num(edad["GBM", "mediana"], 0), " años frente a ", num(edad["LGG", "mediana"], 0),
           " en LGG, y la U de Mann-Whitney descarta que sea casualidad (",
           p_valor(prueba$p_valor), ")."),
    paste("Aun así, las dos distribuciones se solapan de largo: la edad inclina la",
          "balanza, no decide el diagnóstico. Un paciente de 50 años puede tener",
          "cualquiera de los dos tumores.")
  )
}

lectura_clinicas <- function(ambito = "train") {
  lineas <- character(0)
  for (par in list(c("Gender", "El género"), c("Race", "El grupo racial"))) {
    prueba <- prueba_bivariada(par[1], ambito)
    veredicto <- if (prueba$significativa) "sí separa" else "no separa"
    lineas <- c(lineas, paste0(
      par[2], " ", veredicto, " los dos grados: ", tolower(prueba$prueba), " con ",
      p_valor(prueba$p_valor), " y V de Cramér de ", num(prueba$v_cramer, 2), "."
    ))
  }
  tabla <- distribucion_clinica("Race", ambito)
  por_categoria <- tapply(tabla$pacientes, tabla$categoria, sum)
  mayoritaria <- names(por_categoria)[which.max(por_categoria)]
  lineas <- c(lineas, paste0(
    "El reparto racial está muy desbalanceado: «", mayoritaria, "» reúne el ",
    pct(max(por_categoria) / sum(por_categoria) * 100), " de la cohorte. Las categorías ",
    "minoritarias tienen tan pocos pacientes que su porcentaje no es interpretable, y por ",
    "eso este hallazgo no se puede llevar a otra población."
  ))
  lineas
}

lectura_genes <- function(ambito = "train") {
  prevalencia <- prevalencia_genes(ambito)
  mas_comun <- prevalencia[1, ]
  brechas <- prevalencia[order(-abs(prevalencia$diferencia)), ]
  primera <- brechas[1, ]
  segunda <- brechas[2, ]
  raros <- sum(prevalencia$prevalencia < 5)
  hacia <- function(f) if (f$diferencia < 0) "LGG" else "GBM"
  c(
    paste0(mas_comun$gen, " es la mutación más frecuente del panel (",
           pct(mas_comun$prevalencia), " de los pacientes), pero frecuente no ",
           "es lo mismo que informativo: lo que separa los grados es la diferencia ",
           "de prevalencia entre ellos, no la prevalencia global."),
    paste0("Las dos brechas mayores son ", primera$gen, " (",
           num(abs(primera$diferencia), 0), " puntos hacia ", hacia(primera), ") y ",
           segunda$gen, " (", num(abs(segunda$diferencia), 0), " puntos hacia ",
           hacia(segunda), "). Son los marcadores que de verdad distinguen."),
    paste0(raros, " de los ", nrow(prevalencia), " genes están mutados en menos del 5 % ",
           "de la cohorte. Aunque salieran asociados, cualquier conclusión sobre ",
           "ellos descansaría en un puñado de pacientes.")
  )
}

lectura_asociacion <- function(ambito = "train") {
  a <- asociacion_con_grado(ambito)
  con_senal <- a[a$significativa, ]
  sin_senal <- a[!a$significativa, ]
  primera <- a[1, ]
  con_signo <- con_senal[!is.na(con_senal$rho), ]
  hacia_lgg <- sum(con_signo$rho < 0)
  hacia_gbm <- nrow(con_signo) - hacia_lgg
  sin_direccion <- nrow(con_senal) - nrow(con_signo)
  cola <- if (sin_direccion == 1) {
    paste0(", y ", sin_direccion, " no tiene dirección por ser nominal.")
  } else if (sin_direccion > 1) {
    paste0(", y ", sin_direccion, " no tienen dirección por ser nominales.")
  } else "."
  c(
    paste0(nombre_corto(primera$variable), " encabeza el ranking con $r = ", tnum(primera$rho, 2, signo = TRUE),
           "$, de signo ", if (primera$rho < 0) "negativo, así que empuja hacia LGG."
           else "positivo, así que empuja hacia GBM."),
    paste0(nrow(con_senal), " de las ", nrow(a), " predictoras superan a la vez ",
           "los dos filtros del proyecto ($p < ", tnum(ALFA, 2), "$ y magnitud $\\geq ",
           tnum(EFECTO_MINIMO, 2), "$): ", hacia_lgg, " empujan hacia LGG y ", hacia_gbm,
           " hacia GBM", cola),
    paste0("Las ", nrow(sin_senal), " restantes no muestran asociación apreciable por sí ",
           "solas, así que son las primeras candidatas a salir del panel. Con una ",
           "salvedad: esto es un cribado variable a variable, y una que aquí parece ",
           "muda puede aportar acompañada de otras. Quién sobra de verdad lo decide ",
           "un modelo, no esta tabla."),
    paste0("Son ", nrow(a), " contrastes contra el mismo objetivo, así que ",
           "algo de significancia podría aparecer por azar. Corrigiendo por ello ",
           "con Benjamini-Hochberg, que controla la proporción de falsos ",
           "descubrimientos, sobreviven ", sum(a$sobrevive_bh), " de las ",
           sum(a$p_valor < ALFA), " que ya pasaban sin corregir: el panel no depende ",
           "de ese ajuste. El libro aplica la misma corrección.")
  )
}

lectura_correlaciones <- function(ambito = "train") {
  m <- matriz_spearman(ambito)
  con_grado <- m[TARGET, colnames(m) != TARGET]
  mas_fuerte <- names(con_grado)[which.max(abs(con_grado))]
  entre <- m[rownames(m) != TARGET, colnames(m) != TARGET]
  diag(entre) <- 0
  entre[is.na(entre)] <- 0
  # El primer máximo en el orden de numpy (por filas), no el de R (por columnas)
  candidatos <- which(abs(entre) == max(abs(entre)), arr.ind = TRUE)
  candidatos <- candidatos[order(candidatos[, 1], candidatos[, 2]), , drop = FALSE]
  pos <- candidatos[1, ]
  nombres <- colnames(entre)
  superiores <- entre[upper.tri(entre)]
  c(
    paste0("La primera fila y la primera columna son lo que importa para el ",
           "objetivo: la correlación de cada variable con el grado. La mayor es ",
           nombre_corto(mas_fuerte), " con r = ", num(con_grado[[mas_fuerte]], 2, signo = TRUE), "."),
    paste0("Entre predictoras, la correlación más alta es ", nombre_corto(nombres[pos[1]]), " con ",
           nombre_corto(nombres[pos[2]]), " (r = ", num(entre[pos[1], pos[2]], 2, signo = TRUE), "), y ",
           sum(abs(superiores) < 0.20), " de los ", length(superiores),
           " pares se quedan por debajo de 0,20 en valor absoluto. El panel es ",
           "mayoritariamente de variables que aportan información distinta."),
    paste("El grupo racial entra reagrupado en dos niveles, White frente al",
          "resto. Con sus cuatro categorías originales quedaría fuera, porque",
          "correlacionar grupos sin orden no significa nada; con dos, el",
          "coeficiente vuelve a tener sentido. El libro hace lo mismo.")
  )
}

lectura_multicolinealidad <- function(ambito = "train") {
  vif <- vif_predictoras(ambito)
  pares <- matriz_asociacion_genes(ambito, top_n = 12)
  diag(pares) <- 0
  peor <- vif[1, ]
  sobre_cinco <- sum(vif$vif >= 5)
  c(
    paste0("Entre pares, la asociación más fuerte llega a V = ", num(max(pares), 2),
           ", por debajo del umbral de 0,70 que se suele tomar como redundancia."),
    paste("Pero los pares no bastan: una variable puede ser casi predecible a",
          "partir de una combinación de varias sin parecerse a ninguna. Eso lo",
          "mide el factor de inflación de la varianza, que regresa cada",
          "predictora contra todas las demás."),
    if (sobre_cinco == 0) {
      paste0("El VIF más alto es ", num(peor$vif, 2), ", en ", nombre_corto(peor$variable), ", muy ",
             "por debajo de 5. Las 23 predictoras pueden entrar juntas en un modelo ",
             "sin que sus coeficientes se vuelvan inestables.")
    } else {
      paste0(sobre_cinco, " predictoras superan un VIF de 5, la más alta ",
             nombre_corto(peor$variable), " con ", num(peor$vif, 2), ": ahí sí conviene mirar ",
             "antes de meterlas todas juntas en un modelo lineal.")
    }
  )
}

# Exporta los modelos finales y el resumen del capítulo para la app Shiny.
#
# La app no entrena ni carga objetos de glmnet, ranger o xgboost: lee dos JSON
# con los modelos en formato portable (coeficientes de la logística y árboles
# como listas de nodos) y las tablas que muestran las pestañas Modelos y
# Predicción. Así la app solo necesita R base para predecir y explicar, y un
# cambio de versión de esos paquetes no la rompe. Antes de escribir, se
# comprueba que el formato portable reproduce las probabilidades originales.

# ---------------------------------------------------------------------------
# Modelos portables
# ---------------------------------------------------------------------------
portable_logistica <- function(pipe, X_fondo) {
  cf <- coeficientes_logistica(pipe)
  fondo <- colMeans(as.matrix(escalar(pipe, X_fondo)))
  list(tipo = "lineal", coeficientes = unname(cf$beta[PREDICTORAS]),
       intercepto = cf$intercepto, media_fondo = unname(fondo[PREDICTORAS]),
       salida = "log-odds")
}

# Árboles de ranger. La cobertura de cada nodo (lo que pide TreeSHAP) es el
# número de pacientes de la muestra bootstrap de ese árbol que pasan por él:
# se cuenta recorriendo el árbol con los datos con los que se ajustó.
portable_random_forest <- function(pipe) {
  rf <- pipe$ajuste$ajuste
  Xb <- pipe$datos_ajuste$X
  A <- as.matrix(Xb)
  arboles <- lapply(seq_len(rf$num.trees), function(t) {
    ti <- ranger::treeInfo(rf, t)
    n <- nrow(ti)
    izq <- ifelse(is.na(ti$leftChild), -1L, ti$leftChild)
    der <- ifelse(is.na(ti$rightChild), -1L, ti$rightChild)
    var <- ifelse(is.na(ti$splitvarID), -2L, match(ti$splitvarName, PREDICTORAS) - 1L)
    umbral <- ifelse(is.na(ti$splitval), -2, ti$splitval)
    valor <- ifelse(ti$terminal, ti$pred.1, 0)
    # Recorrido de la muestra bootstrap (inbag) para la cobertura
    pesos <- rf$inbag.counts[[t]]
    cobertura <- numeric(n)
    for (i in which(pesos > 0)) {
      nodo <- 0L
      repeat {
        cobertura[nodo + 1] <- cobertura[nodo + 1] + pesos[i]
        if (izq[nodo + 1] == -1L) break
        nodo <- if (A[i, var[nodo + 1] + 1] <= umbral[nodo + 1]) izq[nodo + 1] else der[nodo + 1]
      }
    }
    list(izq = izq, der = der, var = var, umbral = umbral, valor = round(valor, 8),
         cobertura = cobertura)
  })
  list(tipo = "bosque", comparacion = "<=", agregacion = "media", salida = "probabilidad",
       base = 0, arboles = arboles)
}

# Árboles de xgboost, leídos del modelo guardado en JSON (el mismo formato que
# guarda la librería en cualquier lenguaje). En las hojas, split_conditions
# guarda el peso de la hoja; la comparación es estricta (x < umbral va a la
# izquierda) y en float32.
portable_xgboost <- function(pipe) {
  bst <- pipe$ajuste$ajuste
  modelo <- jsonlite::fromJSON(rawToChar(xgboost::xgb.save.raw(bst, raw_format = "json")),
                               simplifyVector = TRUE)
  base <- as.numeric(gsub("\\[|\\]", "", modelo$learner$learner_model_param$base_score))
  arboles <- modelo$learner$gradient_booster$model$trees
  arboles <- lapply(seq_len(nrow(arboles)), function(k) {
    izq <- arboles$left_children[[k]]; der <- arboles$right_children[[k]]
    cond <- arboles$split_conditions[[k]]
    list(izq = izq, der = der, var = arboles$split_indices[[k]], umbral = cond,
         valor = ifelse(izq == -1, cond, 0), cobertura = arboles$sum_hessian[[k]])
  })
  list(tipo = "bosque", comparacion = "<", agregacion = "suma", salida = "log-odds",
       float32 = TRUE, base = log(base / (1 - base)), arboles = arboles)
}

modelos_portables <- function(finales, X_train) {
  pipe_rl <- finales$logistica$pipeline
  salida <- list(predictoras = PREDICTORAS, semilla = SEED,
                 media_edad = unname(pipe_rl$escala["media"]),
                 escala_edad = unname(pipe_rl$escala["de"]), modelos = list())
  for (m in MODELOS) {
    f <- finales[[m]]
    portable <- switch(m,
                       logistica = portable_logistica(f$pipeline, X_train),
                       random_forest = portable_random_forest(f$pipeline),
                       xgboost = portable_xgboost(f$pipeline))
    portable$nombre <- nombre(m)
    portable$balanceo <- f$balanceo
    portable$optimizador <- f$metodo
    portable$hiperparametros <- f$params
    salida$modelos[[m]] <- portable
  }
  salida
}

# Predicción con el formato portable (la misma lógica que modulos/modelo.R de
# la app), para comprobar que reproduce a los modelos originales.
# Redondeo a precisión simple, como compara xgboost (y modulos/modelo.R)
f32 <- function(x) {
  r <- writeBin(as.numeric(x), raw(), size = 4)
  readBin(r, "numeric", size = 4, n = length(x))
}

predecir_portable <- function(portable, X) {
  A <- as.matrix(X[, portable$predictoras])
  A[, 1] <- (A[, 1] - portable$media_edad) / portable$escala_edad
  lapply(portable$modelos, function(mod) {
    if (mod$tipo == "lineal") return(plogis(drop(A %*% mod$coeficientes) + mod$intercepto))
    total <- rep(mod$base, nrow(A))
    for (t in mod$arboles) {
      for (k in seq_len(nrow(A))) {
        n <- 0L
        while (t$izq[n + 1] != -1L) {
          v <- A[k, t$var[n + 1] + 1]; u <- t$umbral[n + 1]
          ir <- if (mod$comparacion == "<") f32(v) < f32(u) else v <= u
          n <- if (ir) t$izq[n + 1] else t$der[n + 1]
        }
        total[k] <- total[k] + t$valor[n + 1]
      }
    }
    if (mod$agregacion == "media") total / length(mod$arboles) else plogis(total)
  })
}

# ---------------------------------------------------------------------------
# Resumen para las pestañas Modelos y Predicción (mismo esquema que el JSON que
# lee el tablero de Dash)
# ---------------------------------------------------------------------------
r4 <- function(v, d = 4) if (is.null(v) || length(v) == 0 || is.na(v)) NULL else round(v, d)

resumen_shiny <- function(finales, d, bloques, recal, semillas, importancia, panel_resumen,
                          historiales, k_min, variables_panel) {
  resumen <- d$resumen; pl <- d$pliegues; oof <- d$oof
  yt <- d$y_test; ytr <- d$y_train
  combos <- lapply(seq_len(nrow(resumen)), function(i) {
    f <- resumen[i, ]
    c(list(modelo = f$modelo, balanceo = f$balanceo, metodo = f$metodo),
      setNames(lapply(as.vector(outer(c("auc", "recall", "precision", "f1", "brier", "accuracy"),
                                      c("media", "de"), paste, sep = "_")),
                      function(k) r4(f[[k]])),
               as.vector(outer(c("auc", "recall", "precision", "f1", "brier", "accuracy"),
                               c("media", "de"), paste, sep = "_"))),
      list(t_total_s = r4(f$t_total_s, 1)))
  })
  proba <- d$proba_test
  modelos <- setNames(lapply(MODELOS, function(m) {
    f <- finales[[m]]
    cid <- id_combo(m, f$balanceo, f$metodo)
    fila <- resumen[resumen$combo == cid, ]
    ic <- ic_bootstrap(function(z) auc_roc(z$y, z$p), data.frame(y = yt, p = proba[[m]]))
    roc <- pROC::roc(yt, proba[[m]], levels = c(0, 1), direction = "<", quiet = TRUE)
    o <- order(roc$specificities, decreasing = TRUE)
    cal_oof <- curva_confiabilidad(ytr, oof[[cid]], 10)
    cal_test <- curva_confiabilidad(yt, proba[[m]], 5)
    list(balanceo = f$balanceo, metodo = f$metodo, hiperparametros = f$params,
         auc_cv_media = r4(fila$auc_media), auc_cv_de = r4(fila$auc_de),
         recall_cv_media = r4(fila$recall_media),
         auc_pliegues = round(pl$auc[pl$combo == cid][order(pl$pliegue[pl$combo == cid])], 4),
         auc_test = r4(auc_roc(yt, proba[[m]])), auc_test_ic = round(ic, 4),
         brier_test = r4(mean((proba[[m]] - yt)^2)), ece_test = r4(ece(yt, proba[[m]], 5)),
         ece_oof = r4(ece(ytr, oof[[cid]])),
         roc = list(fpr = round(1 - roc$specificities[o], 4), tpr = round(roc$sensitivities[o], 4)),
         calibracion_oof = lapply(as.list(cal_oof), function(v) round(v, 4)),
         calibracion_test = lapply(as.list(cal_test), function(v) round(v, 4)))
  }), MODELOS)

  acum <- function(h) { c_ <- cummax(h); c(c_, rep(tail(c_, 1), max(0, PRESUPUESTO - length(c_))))[seq_len(PRESUPUESTO)] }
  anytime <- setNames(lapply(MODELOS, function(m) {
    curvas <- setNames(lapply(OPTIMIZADORES, function(o) {
      do.call(rbind, lapply(BALANCEOS, function(b) do.call(rbind, lapply(historiales[[id_combo(m, b, o)]], acum))))
    }), OPTIMIZADORES)
    ref <- do.call(pmax, lapply(curvas, function(cc) cc[, PRESUPUESTO]))
    lapply(curvas, function(cc) round(colMeans(ref - cc), 5))
  }), MODELOS)
  optimizadores <- setNames(lapply(OPTIMIZADORES, function(o) {
    g <- bloques[bloques$metodo == o, ]
    list(auc_t6 = r4(mean(g$t6)), auc_t12 = r4(mean(g$t12)), auc_t24 = r4(mean(g$t24)),
         area = r4(mean(g$area)), al_mejor = r4(mean(g$llega), 3),
         evals_al_mejor = r4(median(g$evals, na.rm = TRUE), 1), auc_externa = r4(mean(g$auc)),
         optimismo = r4(mean(g$auc_interna - g$auc)), s_por_eval = r4(mean(g$t_por_eval), 3))
  }), OPTIMIZADORES)

  matriz <- tidyr::pivot_wider(pl[, c("pliegue", "combo", "auc")], names_from = combo,
                               values_from = auc)
  fr <- friedman(as.matrix(matriz[, -1]))
  factores <- setNames(lapply(c("modelo", "balanceo", "metodo"), function(factor) {
    niveles <- list(modelo = MODELOS, balanceo = BALANCEOS, metodo = OPTIMIZADORES)[[factor]]
    mat <- matriz_factor(pl, factor, niveles)
    r <- friedman(mat)
    list(chi2 = r4(r$chi2, 2), gl = r$gl, p = r4(r$p), bloques = r$N,
         cd = r4(diferencia_critica(r$k, r$N), 3), rangos = as.list(round(r$rangos, 3)))
  }), c("modelo", "balanceo", "metodo"))
  pares <- list(c("logistica", "random_forest"), c("logistica", "xgboost"),
                c("random_forest", "xgboost"))
  delong_l <- lapply(pares, function(ab) {
    a <- ab[1]; b <- ab[2]
    dl <- delong(yt, proba[[a]], proba[[b]])
    ic <- ic_bootstrap(function(z) auc_roc(z$y, z$a) - auc_roc(z$y, z$b),
                       data.frame(y = yt, a = proba[[a]], b = proba[[b]]))
    sa <- pl$auc[pl$combo == id_combo(a, finales[[a]]$balanceo, finales[[a]]$metodo)]
    sb <- pl$auc[pl$combo == id_combo(b, finales[[b]]$balanceo, finales[[b]]$metodo)]
    list(a = a, b = b, diferencia = r4(dl$diferencia), z = r4(dl$z, 3), p = r4(dl$p),
         ic = round(ic, 4), cliff = r4(delta_cliff(sa, sb), 3))
  })
  holm_p <- p.adjust(vapply(delong_l, function(x) x$p, 0), "holm")
  for (i in seq_along(delong_l)) delong_l[[i]]$p_holm <- r4(holm_p[i])

  sem <- setNames(lapply(MODELOS, function(m) {
    v <- semillas$auc_test[semillas$modelo == m]
    list(mean = r4(mean(v)), std = r4(sd(v)), min = r4(min(v)), max = r4(max(v)))
  }), MODELOS)

  list(
    meta = list(combinaciones = nrow(resumen), configuraciones = sum(resumen$n_eval_total),
                ajustes_internos = sum(resumen$n_eval_total) * K_INTERNO, k_externo = K_EXTERNO,
                k_interno = K_INTERNO, presupuesto = PRESUPUESTO, semilla = SEED,
                minutos = r4(sum(resumen$t_total_s) / 60, 1), n_train = length(ytr),
                n_test = length(yt), gbm_test = sum(yt)),
    combinaciones = combos, modelos = modelos,
    test = c(list(y = yt, indice = d$id_test),
             setNames(lapply(MODELOS, function(m) round(proba[[m]], 4)), MODELOS)),
    importancia = lapply(seq_len(nrow(importancia)), function(i) {
      list(variable = importancia$variable[i], xgboost = round(importancia$XGBoost[i], 2),
           logistica = round(importancia$`Regresión Logística`[i], 2))
    }),
    panel = lapply(as.list(panel_resumen), function(v) round(v, 4)),
    panel_reducido = list(k_min = k_min, variables = variables_panel,
                          auc_k = r4(panel_resumen$auc[panel_resumen$k == k_min]),
                          auc_23 = r4(panel_resumen$auc[panel_resumen$k == 23])),
    anytime = anytime, optimizadores = optimizadores,
    friedman = list(chi2 = r4(fr$chi2, 2), gl = fr$gl, p = r4(fr$p),
                    cd = r4(diferencia_critica(fr$k, fr$N), 2),
                    variacion_dentro = r4(mean(resumen$auc_de)),
                    variacion_entre = r4(sd(resumen$auc_media)),
                    rango_medias = r4(max(resumen$auc_media) - min(resumen$auc_media))),
    factores = factores, delong = delong_l,
    recalibracion = lapply(seq_len(nrow(recal)), function(i) {
      list(Modelo = recal$Modelo[i], `Método` = recal$Metodo[i], auc = r4(recal$auc[i]),
           brier = r4(recal$brier[i]), ece = r4(recal$ece[i]))
    }),
    semillas = sem
  )
}

# Matriz bloques x niveles para el Friedman por factor: cada combinación de los
# otros dos factores con cada pliegue es un bloque.
matriz_factor <- function(pl, factor, niveles) {
  otros <- setdiff(c("modelo", "balanceo", "metodo"), factor)
  d <- pl[, c(otros, "pliegue", factor, "auc")]
  w <- tidyr::pivot_wider(d, names_from = dplyr::all_of(factor), values_from = auc)
  as.matrix(w[, niveles])
}

exportar_shiny <- function(finales, d, destino, ...) {
  portable <- modelos_portables(finales, d$X_train)
  comprobacion <- predecir_portable(portable, d$X_test)
  for (m in MODELOS) {
    error <- max(abs(predecir(finales[[m]]$pipeline, d$X_test) - comprobacion[[m]]))
    message(sprintf("%-14s error máximo portable vs original: %.2e", m, error))
    if (error > 1e-5) stop("El modelo portable de ", m, " no reproduce el original")
  }
  dir.create(destino, recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(portable, file.path(destino, "modelos_finales.json"), auto_unbox = TRUE,
                       digits = NA)
  jsonlite::write_json(resumen_shiny(finales, d, ...), file.path(destino, "resumen_modelado.json"),
                       auto_unbox = TRUE, digits = NA, null = "null", na = "null")
  invisible(file.path(destino, c("modelos_finales.json", "resumen_modelado.json")))
}

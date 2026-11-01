# Análisis del capítulo que van más allá de la validación anidada: modelos
# finales, multi-fidelidad, escalamiento, recalibración, interpretabilidad,
# robustez y panel reducido. Cada función devuelve un data.frame (o una lista)
# y el capítulo la llama a través de en_cache(), de modo que se calcula una
# sola vez.

segundos <- function(t0) as.numeric(Sys.time() - t0, units = "secs")

# Mediana de `rep` repeticiones del tiempo de reloj de f().
tiempo <- function(f, rep = 3) {
  median(vapply(seq_len(rep), function(i) {
    t0 <- Sys.time(); f(); segundos(t0)
  }, numeric(1)))
}

# ---------------------------------------------------------------------------
# Modelos finales
# ---------------------------------------------------------------------------
# Para cada familia, la combinación con mayor AUC media en el bucle externo;
# su optimizador se ejecuta otra vez sobre los 671 pacientes y el pipeline se
# ajusta con todos ellos.
seleccionar <- function(resumen) {
  do.call(rbind, lapply(MODELOS, function(m) {
    r <- resumen[resumen$modelo == m, ]
    r[which.max(r$auc_media), ]
  }))
}

entrenar_finales <- function(seleccion, X, y) {
  setNames(lapply(seq_len(nrow(seleccion)), function(k) {
    f <- seleccion[k, ]
    res <- buscar(f$metodo, f$modelo, f$balanceo, X, y)
    pipe <- ajustar_pipeline(f$modelo, f$balanceo, X, y, res$mejores_params,
                             guardar_inbag = TRUE)
    list(pipeline = pipe, params = res$mejores_params, auc_interna = res$mejor_score,
         balanceo = f$balanceo, metodo = f$metodo)
  }), seleccion$modelo)
}

# ---------------------------------------------------------------------------
# Successive Halving (multi-fidelidad) frente a Random Search en XGBoost
# ---------------------------------------------------------------------------
# La fidelidad es el número de árboles: 81 candidatos con 15 árboles y, en
# cada ronda, el mejor tercio con el triple de árboles (81 -> 27 -> 9 -> 3
# con 15 -> 45 -> 135 -> 405). Se escribe directamente: es un bucle de pocas
# líneas sobre la misma evaluación interna que usan los demás métodos.
successive_halving <- function(X, y, semilla = SEED, n_candidatos = 81, minimo = 15,
                               factor = 3, maximo = 405) {
  cv <- pliegues_estratificados(y, K_INTERNO, semilla)
  set.seed(semilla)
  candidatos <- lapply(seq_len(n_candidatos), function(i) muestrear_config("xgboost"))
  arboles <- minimo
  total_arboles <- 0
  evaluaciones <- 0
  repeat {
    scores <- vapply(candidatos, function(p) {
      p$nrounds <- arboles
      evaluar_config("xgboost", "ninguno", p, X, y, cv, semilla)
    }, numeric(1))
    total_arboles <- total_arboles + arboles * length(candidatos)
    evaluaciones <- evaluaciones + length(candidatos)
    if (length(candidatos) <= factor || arboles * factor > maximo) break
    quedan <- order(scores, decreasing = TRUE)[seq_len(ceiling(length(candidatos) / factor))]
    candidatos <- candidatos[quedan]
    arboles <- arboles * factor
  }
  i <- which.max(scores)
  mejor <- candidatos[[i]]
  mejor$nrounds <- arboles
  list(params = mejor, score = scores[i], evaluaciones = evaluaciones, arboles = total_arboles)
}

comparar_halving <- function(X, y) {
  externo <- pliegues_estratificados(y, K_EXTERNO, SEED)
  do.call(rbind, lapply(c("Random Search", "Successive Halving"), function(metodo) {
    do.call(rbind, lapply(seq_along(externo), function(i) {
      f <- externo[[i]]
      t0 <- Sys.time()
      if (metodo == "Random Search") {
        r <- buscar_random("xgboost", "ninguno", X[f$tr, ], y[f$tr])
        params <- r$mejores_params; score <- r$mejor_score; cand <- r$n_eval
        cv <- pliegues_estratificados(y[f$tr], K_INTERNO, SEED)
        set.seed(SEED)
        arb <- sum(vapply(seq_len(PRESUPUESTO), function(j) muestrear_config("xgboost")$nrounds,
                          numeric(1)))
      } else {
        r <- successive_halving(X[f$tr, ], y[f$tr])
        params <- r$params; score <- r$score; cand <- 81; arb <- r$arboles
      }
      t <- segundos(t0)
      pipe <- ajustar_pipeline("xgboost", "ninguno", X[f$tr, ], y[f$tr], params)
      data.frame(metodo = metodo, pliegue = i - 1L, t = t, auc_interna = score,
                 auc_externa = auc_roc(y[f$va], predecir(pipe, X[f$va, ])),
                 candidatos = cand, arboles = arb)
    }))
  }))
}

# ---------------------------------------------------------------------------
# Optimización computacional
# ---------------------------------------------------------------------------
escalamiento <- function(finales, X, y) {
  set.seed(SEED)
  A <- X
  do.call(rbind, lapply(MODELOS, function(m) {
    f <- finales[[m]]
    ajustar <- function(Xa, ya) ajustar_modelo(m, Xa, ya, f$params, "ninguno", SEED)
    filas_n <- lapply(c(200, 400, 800, 1600, 3200, 6400, 12800, 25600), function(n) {
      idx <- sample(nrow(A), n, replace = TRUE)
      data.frame(modelo = m, eje = "n", valor = n,
                 t = tiempo(function() ajustar(A[idx, ], y[idx])))
    })
    filas_p <- lapply(c(23, 46, 92, 184, 368, 736), function(p) {
      ruido <- matrix(sample(0:1, nrow(A) * (p - 23), replace = TRUE), nrow(A))
      colnames(ruido) <- if (p > 23) paste0("ruido", seq_len(p - 23)) else NULL
      Xp <- if (p > 23) cbind(A, ruido) else A
      data.frame(modelo = m, eje = "p", valor = p, t = tiempo(function() ajustar(Xp, y)))
    })
    do.call(rbind, c(filas_n, filas_p))
  }))
}

pendientes_escalamiento <- function(esc) {
  do.call(rbind, lapply(MODELOS, function(m) do.call(rbind, lapply(c("n", "p"), function(e) {
    d <- tail(esc[esc$modelo == m & esc$eje == e, ], 4)
    data.frame(modelo = m, eje = e, pendiente = unname(coef(lm(log(t) ~ log(valor), d))[2]))
  }))))
}

xgb_metodos <- function(finales, X, y) {
  set.seed(SEED)
  f <- finales$xgboost
  do.call(rbind, lapply(list(c(671, 23), c(6400, 23), c(6400, 184)), function(np) {
    n <- np[1]; p <- np[2]
    idx <- if (n == nrow(X)) seq_len(n) else sample(nrow(X), n, replace = TRUE)
    Xn <- X[idx, ]
    if (p > 23) {
      ruido <- matrix(sample(0:1, n * (p - 23), replace = TRUE), n)
      colnames(ruido) <- paste0("ruido", seq_len(p - 23))
      Xn <- cbind(Xn, ruido)
    }
    do.call(rbind, lapply(c("exact", "hist"), function(tm) {
      data.frame(n = n, p = p, metodo = tm, t = tiempo(function() {
        ajustar_modelo("xgboost", Xn, y[idx], f$params, "ninguno", SEED, tree_method = tm)
      }))
    }))
  }))
}

# Early stopping: el 20 % del pliegue de entrenamiento (estratificado) decide
# cuándo parar, con un máximo de 2 000 árboles y paciencia de 30 rondas.
early_stopping <- function(finales, X, y) {
  f <- finales$xgboost
  cv <- pliegues_estratificados(y, 5, SEED)
  do.call(rbind, lapply(seq_along(cv), function(i) {
    tr <- cv[[i]]$tr; va <- cv[[i]]$va
    do.call(rbind, lapply(c("Fijo (hiperparámetros finales)", "Early stopping (máx. 2000)"),
                          function(variante) {
      t0 <- Sys.time()
      if (startsWith(variante, "Early")) {
        interno <- pliegues_estratificados(y[tr], 5, SEED)[[1]]
        a <- tr[interno$tr]; b <- tr[interno$va]
        pipe <- ajustar_pipeline("xgboost", f$balanceo, X[a, ], y[a], f$params)
        prm <- modifyList(PARAMS_DEFECTO$xgboost, f$params)
        set.seed(SEED)
        bst <- xgboost::xgb.train(
          params = list(objective = "binary:logistic", eval_metric = "logloss",
                        tree_method = "hist", max_depth = prm$max_depth, eta = prm$eta,
                        min_child_weight = prm$min_child_weight, subsample = prm$subsample,
                        colsample_bytree = prm$colsample_bytree, lambda = prm$lambda,
                        nthread = 1),
          data = xgboost::xgb.DMatrix(as.matrix(escalar(pipe, X[a, ])), label = y[a]),
          nrounds = 2000, early_stopping_rounds = 30, verbose = 0,
          evals = list(val = xgboost::xgb.DMatrix(as.matrix(escalar(pipe, X[b, ])),
                                                  label = y[b])))
        pipe$ajuste$ajuste <- bst
        arboles <- xgboost::xgb.attr(bst, "best_iteration")
        arboles <- as.numeric(arboles) + 1
        p <- predict(bst, xgboost::xgb.DMatrix(as.matrix(escalar(pipe, X[va, ]))),
                     iterationrange = c(1, arboles))
      } else {
        pipe <- ajustar_pipeline("xgboost", f$balanceo, X[tr, ], y[tr], f$params)
        arboles <- f$params$nrounds
        p <- predecir(pipe, X[va, ])
      }
      data.frame(variante = variante, pliegue = i - 1L, t = segundos(t0), arboles = arboles,
                 auc = auc_roc(y[va], p))
    }))
  }))
}

paralelo <- function(X, y) {
  do.call(rbind, lapply(1:2, function(procesos) {
    if (procesos > 1) future::plan(future::multisession, workers = procesos)
    t0 <- Sys.time()
    validacion_anidada("random", "xgboost", "ninguno", X, y, procesos = procesos)
    t <- segundos(t0)
    future::plan(future::sequential)
    data.frame(procesos = procesos, t = t)
  }))
}

# Perfilamiento con Rprof de una búsqueda completa (Random Search, logística
# con SMOTE-NC): funciones con más tiempo propio.
perfilar <- function(X, y) {
  archivo <- tempfile(fileext = ".out")
  Rprof(archivo, interval = 0.01)
  t0 <- Sys.time()
  buscar("random", "logistica", "smote", X, y)
  total <- segundos(t0)
  Rprof(NULL)
  s <- summaryRprof(archivo)
  propio <- head(s$by.self, 8)
  list(total = total,
       tabla = data.frame(funcion = rownames(propio), propio_s = propio$self.time,
                          propio_pct = propio$self.pct,
                          total_s = s$by.total[rownames(propio), "total.time"]))
}

desglose <- function(finales, X, y) {
  do.call(rbind, lapply(MODELOS, function(m) {
    f <- finales[[m]]
    pipe <- list(columnas = PREDICTORAS, escala = NULL)
    t0 <- Sys.time()
    edad <- X$Age_at_diagnosis
    pipe$escala <- c(media = mean(edad), de = sqrt(mean((edad - mean(edad))^2)))
    Xe <- escalar(pipe, X)
    t1 <- Sys.time()
    b <- balancear(Xe, y, f$balanceo, SEED)
    t2 <- Sys.time()
    ajustar_modelo(m, b$X, b$y, f$params, f$balanceo, SEED)
    t3 <- Sys.time()
    data.frame(modelo = m, balanceo = f$balanceo,
               escalado_ms = 1000 * as.numeric(t1 - t0, units = "secs"),
               balanceo_ms = 1000 * as.numeric(t2 - t1, units = "secs"),
               modelo_ms = 1000 * as.numeric(t3 - t2, units = "secs"))
  }))
}

# Versión estándar: cada modelo con los valores por defecto de su función en
# R (glm sin penalización, ranger con 500 árboles y mtry = sqrt(p), xgboost
# con 100 rondas de profundidad 6 y eta 0,3), con el mismo escalado.
ajustar_estandar <- function(m, X, y) {
  edad <- X$Age_at_diagnosis
  esc <- c(media = mean(edad), de = sqrt(mean((edad - mean(edad))^2)))
  Xe <- X; Xe$Age_at_diagnosis <- (edad - esc["media"]) / esc["de"]
  ajuste <- switch(m,
    logistica = glm(y ~ ., data = cbind(Xe, y = y), family = binomial),
    random_forest = ranger::ranger(x = Xe, y = factor(y, levels = c(0, 1)), probability = TRUE,
                                   num.threads = 1, seed = SEED),
    xgboost = {
      set.seed(SEED)
      xgboost::xgb.train(params = list(objective = "binary:logistic", nthread = 1),
                         data = xgboost::xgb.DMatrix(as.matrix(Xe), label = y), nrounds = 100,
                         verbose = 0)
    })
  list(m = m, esc = esc, ajuste = ajuste)
}

predecir_estandar <- function(e, X) {
  Xe <- X; Xe$Age_at_diagnosis <- (X$Age_at_diagnosis - e$esc["media"]) / e$esc["de"]
  switch(e$m,
         logistica = as.numeric(predict(e$ajuste, Xe, type = "response")),
         random_forest = predict(e$ajuste, Xe, num.threads = 1)$predictions[, "1"],
         xgboost = predict(e$ajuste, xgboost::xgb.DMatrix(as.matrix(Xe))))
}

comparar_estandar <- function(finales, X, y, X_test) {
  cv <- pliegues_estratificados(y, 10, SEED)
  do.call(rbind, lapply(MODELOS, function(m) {
    f <- finales[[m]]
    versiones <- list(
      "Estándar" = list(ajustar = function(Xa, ya) ajustar_estandar(m, Xa, ya),
                        predecir = predecir_estandar),
      "Optimizado" = list(ajustar = function(Xa, ya) ajustar_pipeline(m, f$balanceo, Xa, ya,
                                                                      f$params),
                          predecir = predecir))
    do.call(rbind, lapply(names(versiones), function(v) {
      V <- versiones[[v]]
      t_fit <- tiempo(function() V$ajustar(X, y))
      ajustado <- V$ajustar(X, y)
      t_pred <- tiempo(function() V$predecir(ajustado, X_test), rep = 10)
      memoria <- as.numeric(bench::mark(V$ajustar(X, y), iterations = 1,
                                        check = FALSE)$mem_alloc) / 2^20
      tam <- length(serialize(if (v == "Optimizado") ajustado$ajuste$ajuste else ajustado$ajuste,
                              NULL)) / 1024
      aucs <- vapply(cv, function(f_) {
        a <- V$ajustar(X[f_$tr, ], y[f_$tr])
        auc_roc(y[f_$va], V$predecir(a, X[f_$va, ]))
      }, numeric(1))
      data.frame(modelo = nombre(m), version = v, entrenamiento_ms = 1000 * t_fit,
                 inferencia_ms = 1000 * t_pred, memoria_mb = memoria, tamano_kb = tam,
                 auc_media = mean(aucs), auc_de = sd(aucs))
    }))
  }))
}

# ---------------------------------------------------------------------------
# Recalibración: Platt e isotónica con validación cruzada de 5 pliegues sobre
# el entrenamiento. En cada pliegue se ajusta el pipeline con 4/5 partes y el
# calibrador con la parte restante; la probabilidad final es la media de los
# cinco pares (pipeline, calibrador), como CalibratedClassifierCV.
# ---------------------------------------------------------------------------
calibrador <- function(p, y, metodo) {
  if (metodo == "Platt") {
    z <- qlogis(pmin(pmax(p, 1e-6), 1 - 1e-6))
    g <- glm(y ~ z, family = binomial)
    return(function(q) as.numeric(predict(g, data.frame(z = qlogis(pmin(pmax(q, 1e-6), 1 - 1e-6))),
                                          type = "response")))
  }
  o <- order(p)
  iso <- isoreg(p[o], y[o])
  ajuste <- iso$yf
  xs <- p[o]
  function(q) approx(xs, ajuste, xout = q, rule = 2, ties = "ordered", method = "constant",
                     f = 0)$y
}

recalibrar <- function(finales, X, y, X_test, y_test) {
  cv <- pliegues_estratificados(y, 5, SEED)
  do.call(rbind, lapply(MODELOS, function(m) {
    f <- finales[[m]]
    p0 <- predecir(f$pipeline, X_test)
    filas <- list(data.frame(Modelo = nombre(m), Metodo = "Sin recalibrar", auc = auc_roc(y_test, p0),
                             brier = mean((p0 - y_test)^2), ece = ece(y_test, p0, 5)))
    for (metodo in c("Platt", "Isotónica")) {
      p <- rowMeans(sapply(cv, function(f_) {
        pipe <- ajustar_pipeline(m, f$balanceo, X[f_$tr, ], y[f_$tr], f$params)
        cal <- calibrador(predecir(pipe, X[f_$va, ]), y[f_$va], metodo)
        cal(predecir(pipe, X_test))
      }))
      filas[[length(filas) + 1]] <- data.frame(Modelo = nombre(m), Metodo = metodo,
                                               auc = auc_roc(y_test, p),
                                               brier = mean((p - y_test)^2),
                                               ece = ece(y_test, p, 5))
    }
    do.call(rbind, filas)
  }))
}

# ---------------------------------------------------------------------------
# Interpretabilidad
# ---------------------------------------------------------------------------
# XGBoost: TreeSHAP exacto de la propia librería (predcontrib = TRUE), en la
# escala del log-odds de GBM. La última columna es el valor base.
shap_xgb <- function(pipe, X) {
  A <- as.matrix(escalar(pipe, X))
  contrib <- predict(pipe$ajuste$ajuste, xgboost::xgb.DMatrix(A), predcontrib = TRUE)
  list(valores = contrib[, colnames(A), drop = FALSE], base = unname(contrib[1, ncol(contrib)]))
}

# Regresión logística: forma cerrada beta_j (x_j - media_j) con la media del
# entrenamiento como fondo.
coeficientes_logistica <- function(pipe) {
  b <- as.matrix(coef(pipe$ajuste$ajuste))
  list(intercepto = b[1, 1], beta = setNames(b[-1, 1], rownames(b)[-1]))
}

shap_logistica <- function(pipe, X, X_fondo) {
  cf <- coeficientes_logistica(pipe)
  A <- as.matrix(escalar(pipe, X))
  media <- colMeans(as.matrix(escalar(pipe, X_fondo)))
  list(valores = sweep(A, 2, media) %*% diag(cf$beta[colnames(A)]) |>
         `colnames<-`(colnames(A)),
       base = unname(cf$intercepto + sum(cf$beta[colnames(A)] * media)))
}

# LIME (paquete lime). Las binarias se presentan como factores, así que LIME
# las perturba muestreando su frecuencia en el entrenamiento; la edad se
# discretiza en cuartiles. Se repite con 10 semillas para medir estabilidad.
model_type.envoltura_lime <- function(x, ...) "classification"
predict_model.envoltura_lime <- function(x, newdata, type, ...) {
  d <- as.data.frame(newdata)
  d[BINARIAS] <- lapply(d[BINARIAS], function(v) as.integer(as.character(v)))
  p <- predecir(x$pipe, d)
  data.frame(`0` = 1 - p, `1` = p, check.names = FALSE)
}

lime_pesos <- function(pipe, X_train, X_paciente, semillas = 0:9) {
  a_factores <- function(d) {
    d[BINARIAS] <- lapply(d[BINARIAS], factor, levels = c(0, 1))
    d
  }
  envoltura <- structure(list(pipe = pipe), class = "envoltura_lime")
  t(vapply(semillas, function(s) {
    set.seed(s)
    explicador <- lime::lime(a_factores(X_train), envoltura, bin_continuous = TRUE,
                             n_bins = 4, quantile_bins = TRUE)
    e <- lime::explain(a_factores(X_paciente), explicador, labels = "1",
                       n_features = length(PREDICTORAS), n_permutations = 5000,
                       feature_select = "none")
    w <- setNames(rep(0, length(PREDICTORAS)), PREDICTORAS)
    w[e$feature] <- e$feature_weight
    w
  }, numeric(length(PREDICTORAS))))
}

# ---------------------------------------------------------------------------
# Robustez
# ---------------------------------------------------------------------------
sensibilidad_semillas <- function(finales, X, y, X_test, y_test) {
  do.call(rbind, lapply(MODELOS, function(m) {
    f <- finales[[m]]
    do.call(rbind, lapply(0:4, function(s) {
      res <- buscar(f$metodo, m, f$balanceo, X, y, semilla = s)
      pipe <- ajustar_pipeline(m, f$balanceo, X, y, res$mejores_params, semilla = s)
      data.frame(modelo = m, semilla = s, auc_interna = res$mejor_score,
                 auc_test = auc_roc(y_test, predecir(pipe, X_test)),
                 params = as.character(jsonlite::toJSON(res$mejores_params, auto_unbox = TRUE)))
    }))
  }))
}

# ---------------------------------------------------------------------------
# Panel reducido: ranking por |SHAP| medio de XGBoost dentro de cada pliegue
# externo y AUC del mejor procedimiento con las k primeras variables.
# ---------------------------------------------------------------------------
KS <- c(1, 2, 3, 4, 5, 6, 8, 10, 13, 16, 23)

curva_panel <- function(finales, mejor, X, y) {
  externo <- pliegues_estratificados(y, K_EXTERNO, SEED)
  fx <- finales$xgboost; fm <- finales[[mejor]]
  rankings <- list(); filas <- list()
  for (i in seq_along(externo)) {
    tr <- externo[[i]]$tr; va <- externo[[i]]$va
    px <- ajustar_pipeline("xgboost", fx$balanceo, X[tr, ], y[tr], fx$params)
    imp <- colMeans(abs(shap_xgb(px, X[tr, ])$valores))
    orden <- names(sort(imp, decreasing = TRUE))
    rankings[[i]] <- orden
    for (k in KS) {
      pipe <- ajustar_pipeline(mejor, fm$balanceo, X[tr, ], y[tr], fm$params,
                               columnas = orden[seq_len(k)])
      p <- predecir(pipe, X[va, ])
      filas[[length(filas) + 1]] <- data.frame(pliegue = i - 1L, k = k, auc = auc_roc(y[va], p),
                                               recall = mean(p[y[va] == 1] >= 0.5))
    }
  }
  list(panel = do.call(rbind, filas), rankings = rankings)
}

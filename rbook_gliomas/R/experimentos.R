# Las 48 combinaciones modelo x balanceo x optimizador y la caché de cálculos.
#
# Cada combinación se guarda en resultados/parciales/<id>.rds en cuanto
# termina, así que una ejecución interrumpida se reanuda donde quedó. Al final
# se consolidan las tablas maestras (resultados/experimentos.csv y
# resultados/pliegues.csv) y los historiales de búsqueda.

# Devuelve el resultado guardado en resultados/cache/<nombre>.rds o lo calcula
# con funcion() y lo guarda. Sirve para que el libro se compile en minutos:
# los cálculos costosos se hacen una sola vez y recalcular = TRUE fuerza a
# repetirlos.
en_cache <- function(nombre, funcion, recalcular = FALSE) {
  ruta <- file.path(DIR_CACHE, paste0(nombre, ".rds"))
  if (file.exists(ruta) && !recalcular) return(readRDS(ruta))
  dir.create(DIR_CACHE, recursive = TRUE, showWarnings = FALSE)
  resultado <- funcion()
  saveRDS(resultado, ruta)
  resultado
}

combinaciones <- function() {
  g <- expand.grid(metodo = OPTIMIZADORES, balanceo = BALANCEOS, modelo = MODELOS,
                   stringsAsFactors = FALSE)
  g[, c("modelo", "balanceo", "metodo")]
}

archivo_parcial <- function(modelo, balanceo, metodo) {
  file.path(DIR_RESULTADOS, "parciales", paste0(modelo, "__", balanceo, "__", metodo, ".rds"))
}

correr_experimento <- function(X, y, procesos = PROCESOS, solo = NULL, verbose = TRUE) {
  dir.create(file.path(DIR_RESULTADOS, "parciales"), recursive = TRUE, showWarnings = FALSE)
  if (procesos > 1) {
    future::plan(future::multisession, workers = procesos)
    on.exit(future::plan(future::sequential), add = TRUE)
  }
  combos <- if (is.null(solo)) combinaciones() else solo
  for (k in seq_len(nrow(combos))) {
    m <- combos$modelo[k]; b <- combos$balanceo[k]; o <- combos$metodo[k]
    archivo <- archivo_parcial(m, b, o)
    if (file.exists(archivo)) next
    t0 <- Sys.time()
    r <- validacion_anidada(o, m, b, X, y, procesos = procesos)
    r$t_total <- as.numeric(Sys.time() - t0, units = "secs")
    r$modelo <- m; r$balanceo <- b; r$metodo <- o; r$semilla <- SEED
    saveRDS(r, archivo)
    if (verbose) message(sprintf("%-40s AUC externa %.4f  %7.1f s", id_combo(m, b, o),
                                 mean(r$filas$auc), r$t_total))
  }
  consolidar()
}

# Une los parciales en las tablas maestras.
consolidar <- function() {
  combos <- combinaciones()
  resumen <- list(); pliegues <- list(); historiales <- list(); geneticos <- list()
  oof <- list()
  for (k in seq_len(nrow(combos))) {
    m <- combos$modelo[k]; b <- combos$balanceo[k]; o <- combos$metodo[k]
    archivo <- archivo_parcial(m, b, o)
    if (!file.exists(archivo)) next
    r <- readRDS(archivo)
    cid <- id_combo(m, b, o)
    df <- r$filas
    pliegues[[cid]] <- cbind(data.frame(combo = cid, modelo = m, balanceo = b, metodo = o), df)
    fila <- data.frame(combo = cid, modelo = m, balanceo = b, metodo = o)
    for (met in c("auc", "accuracy", "precision", "recall", "f1", "brier", "auc_interna")) {
      fila[[paste0(met, "_media")]] <- mean(df[[met]])
      fila[[paste0(met, "_de")]] <- sd(df[[met]])
    }
    fila$n_eval_total <- sum(df$n_eval)
    fila$t_total_s <- r$t_total
    fila$t_busqueda_s <- sum(df$t_busqueda)
    fila$t_por_eval_s <- mean(df$t_por_eval)
    fila$t_prediccion_ms <- mean(df$t_prediccion) * 1000
    fila$semilla <- r$semilla
    resumen[[cid]] <- fila
    historiales[[cid]] <- r$historiales
    if (o == "genetico") {
      geneticos[[cid]] <- lapply(seq_along(r$extras), function(i) {
        g <- r$extras[[i]]$generaciones
        if (is.null(g)) return(NULL)
        cbind(data.frame(modelo = m, corrida = paste0(cid, "|", i - 1L)), g)
      })
    }
    oof[[cid]] <- r$oof
  }
  resumen <- do.call(rbind, unname(resumen))
  pliegues <- do.call(rbind, unname(pliegues))
  dir.create(DIR_RESULTADOS, showWarnings = FALSE)
  write.csv(resumen, file.path(DIR_RESULTADOS, "experimentos.csv"), row.names = FALSE)
  write.csv(pliegues, file.path(DIR_RESULTADOS, "pliegues.csv"), row.names = FALSE)
  list(resumen = resumen, pliegues = pliegues, historiales = historiales,
       geneticos = do.call(rbind, unlist(geneticos, recursive = FALSE)),
       oof = as.data.frame(oof, check.names = FALSE))
}

# Versiones de R y de los paquetes del modelado (para reproducibilidad).
entorno <- function() {
  paquetes <- c("glmnet", "ranger", "xgboost", "themis", "rsample", "ParBayesianOptimization",
                "GA", "lime", "pROC", "boot", "effsize", "future", "future.apply")
  version <- function(p) tryCatch(as.character(packageVersion(p)), error = function(e) "—")
  data.frame(
    componente = c("R", "Plataforma", paquetes, "Semilla", "Pliegues externos",
                   "Pliegues internos", "Presupuesto por búsqueda", "Procesos"),
    valor = c(R.version$version.string, R.version$platform, vapply(paquetes, version, ""),
              SEED, K_EXTERNO, K_INTERNO, PRESUPUESTO, PROCESOS))
}

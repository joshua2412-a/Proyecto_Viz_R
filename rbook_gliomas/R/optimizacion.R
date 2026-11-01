# Optimización de hiperparámetros y validación cruzada anidada.
#
# Los cuatro métodos comparten la misma interfaz (buscar()), el mismo espacio
# de búsqueda (ESPACIOS), el mismo presupuesto de evaluaciones y los mismos
# pliegues internos. Cada evaluación es la AUC media de una validación cruzada
# interna de K_INTERNO pliegues sobre el pipeline completo, así que ninguna
# búsqueda ve los datos de validación al preprocesar.

resultado_busqueda <- function(mejores, score, historial, tiempo, extra = list()) {
  list(mejores_params = mejores, mejor_score = score, historial = historial,
       tiempo = tiempo, extra = extra, n_eval = length(historial))
}

# AUC media de validación interna para una configuración.
evaluar_config <- function(modelo, balanceo, params, X, y, cv, semilla) {
  mean(vapply(cv, function(f) {
    pipe <- ajustar_pipeline(modelo, balanceo, X[f$tr, , drop = FALSE], y[f$tr], params,
                             semilla)
    auc_roc(y[f$va], predecir(pipe, X[f$va, , drop = FALSE]))
  }, numeric(1)))
}

# Evaluador con memoria: una configuración que ya se probó no se vuelve a
# ajustar ni consume presupuesto (lo usan el bayesiano y el genético, cuyos
# puntos continuos pueden decodificarse a la misma configuración).
nuevo_evaluador <- function(modelo, balanceo, X, y, cv, semilla) {
  memoria <- new.env()
  registro <- list(historial = numeric(0), configs = list())
  clave_de <- function(params) paste(names(params), unlist(params), sep = "=", collapse = ";")
  evaluar <- function(params) {
    clave <- clave_de(params)
    if (!is.null(memoria[[clave]])) return(memoria[[clave]])
    v <- evaluar_config(modelo, balanceo, params, X, y, cv, semilla)
    memoria[[clave]] <- v
    registro$historial <<- c(registro$historial, v)
    registro$configs[[length(registro$configs) + 1]] <<- params
    v
  }
  list(evaluar = evaluar, registro = function() registro,
       n = function() length(registro$historial),
       conocida = function(params) !is.null(memoria[[clave_de(params)]]))
}

mejor_del_registro <- function(reg) {
  i <- which.max(reg$historial)
  list(params = reg$configs[[i]], score = reg$historial[i])
}

# ---------------------------------------------------------------------------
# Los cuatro métodos
# ---------------------------------------------------------------------------
buscar_grid <- function(modelo, balanceo, X, y, semilla = SEED, presupuesto = PRESUPUESTO) {
  t0 <- Sys.time()
  cv <- pliegues_estratificados(y, K_INTERNO, semilla)
  configs <- rejilla(modelo)
  hist <- vapply(configs, function(p) evaluar_config(modelo, balanceo, p, X, y, cv, semilla),
                 numeric(1))
  i <- which.max(hist)
  resultado_busqueda(configs[[i]], hist[i], hist, as.numeric(Sys.time() - t0, units = "secs"))
}

buscar_random <- function(modelo, balanceo, X, y, semilla = SEED, presupuesto = PRESUPUESTO) {
  t0 <- Sys.time()
  cv <- pliegues_estratificados(y, K_INTERNO, semilla)
  set.seed(semilla)
  configs <- lapply(seq_len(presupuesto), function(i) muestrear_config(modelo))
  hist <- vapply(configs, function(p) evaluar_config(modelo, balanceo, p, X, y, cv, semilla),
                 numeric(1))
  i <- which.max(hist)
  resultado_busqueda(configs[[i]], hist[i], hist, as.numeric(Sys.time() - t0, units = "secs"))
}

# Optimización bayesiana con ParBayesianOptimization: proceso gaussiano como
# modelo sustituto y Mejora Esperada (EI) como función de adquisición.
# El espacio se presenta al GP como el cubo unidad [0, 1]^d (las escalas
# logarítmicas, los enteros y la penalización categórica se resuelven al
# decodificar), que es la normalización habitual para un GP con núcleo
# estacionario. Las primeras 8 evaluaciones son aleatorias (exploración pura);
# las 16 restantes maximizan la EI sobre el GP ajustado a todo lo evaluado.
buscar_bayesiano <- function(modelo, balanceo, X, y, semilla = SEED,
                             presupuesto = PRESUPUESTO, n_iniciales = 8L) {
  t0 <- Sys.time()
  cv <- pliegues_estratificados(y, K_INTERNO, semilla)
  d <- length(ESPACIOS[[modelo]])
  ev <- nuevo_evaluador(modelo, balanceo, X, y, cv, semilla)
  limites <- setNames(rep(list(c(0, 1)), d), paste0("u", seq_len(d)))
  todas <- numeric(0)
  # bayesOpt empareja los límites con los argumentos de FUN por nombre, así
  # que la función objetivo se construye con argumentos u1, ..., ud.
  objetivo <- function() {
    u <- unlist(mget(names(limites)))
    v <- ev$evaluar(decodificar(u, modelo))
    todas <<- c(todas, v)
    list(Score = v)
  }
  formals(objetivo) <- setNames(rep(list(quote(expr = )), d), names(limites))
  set.seed(semilla)
  estado <- tryCatch({
    ParBayesianOptimization::bayesOpt(
      FUN = objetivo, bounds = limites, initPoints = n_iniciales,
      iters.n = presupuesto - n_iniciales, iters.k = 1, acq = "ei", eps = 0, gsPoints = 20,
      parallel = FALSE, plotProgress = FALSE, verbose = VERBOSO_BO)
    "completa"
  }, error = function(e) conditionMessage(e))
  # Si el GP no puede continuar (p. ej. todos los puntos nuevos ya existen),
  # el presupuesto restante se completa al azar para comparar a igual costo.
  completado <- 0L
  while (length(todas) < presupuesto) {
    v <- ev$evaluar(muestrear_config(modelo))
    todas <- c(todas, v)
    completado <- completado + 1L
  }
  m <- mejor_del_registro(ev$registro())
  resultado_busqueda(m$params, m$score, todas, as.numeric(Sys.time() - t0, units = "secs"),
                     list(estado = estado, completado = completado))
}

# Algoritmo genético con el paquete GA (codificación real en [0, 1]^d). Se usan
# los operadores por defecto de GA para codificación real: selección
# proporcional con escalamiento lineal, cruce aritmético local (prob. 0,8),
# mutación uniforme (prob. 0,1) y elitismo de 1 individuo. Con un presupuesto
# de 24 evaluaciones la población es de 6 individuos. Las configuraciones
# repetidas no consumen presupuesto (se recuerdan) y la búsqueda se detiene en
# cuanto pide la evaluación número 25. Cada generación se registra con su mejor
# aptitud, la media y la diversidad (distancia euclídea media entre genomas,
# normalizada por sqrt(d)).
buscar_genetico <- function(modelo, balanceo, X, y, semilla = SEED,
                            presupuesto = PRESUPUESTO, tam_poblacion = 6L) {
  t0 <- Sys.time()
  cv <- pliegues_estratificados(y, K_INTERNO, semilla)
  d <- length(ESPACIOS[[modelo]])
  ev <- nuevo_evaluador(modelo, balanceo, X, y, cv, semilla)
  diversidad <- function(P) mean(dist(P)) / sqrt(d)
  generaciones <- list()
  aptitud <- function(u) {
    params <- decodificar(u, modelo)
    # Una configuración ya evaluada la responde la memoria sin coste; una
    # nueva con el presupuesto gastado detiene la búsqueda.
    if (ev$n() >= presupuesto && !ev$conocida(params)) {
      stop(structure(class = c("presupuesto_agotado", "error", "condition"),
                     list(message = "presupuesto agotado", call = NULL)))
    }
    ev$evaluar(params)
  }
  monitor <- function(obj) {
    f <- obj@fitness
    generaciones[[length(generaciones) + 1]] <<- data.frame(
      generacion = obj@iter - 1L, evaluaciones = ev$n(),
      mejor = max(ev$registro()$historial), media = mean(f[is.finite(f)]),
      diversidad = diversidad(obj@population))
  }
  estado <- tryCatch({
    # La población pequeña es deliberada (presupuesto fijo): se silencia solo
    # el aviso de GA sobre popSize < 10.
    withCallingHandlers(
      GA::ga(type = "real-valued", fitness = aptitud, lower = rep(0, d), upper = rep(1, d),
             popSize = tam_poblacion, maxiter = 50, run = 50, monitor = monitor,
             seed = semilla),
      warning = function(w) {
        if (grepl("population size is less than 10", conditionMessage(w)))
          invokeRestart("muffleWarning")
      })
    "maxiter"
  }, presupuesto_agotado = function(e) "presupuesto")
  m <- mejor_del_registro(ev$registro())
  resultado_busqueda(m$params, m$score, ev$registro()$historial,
                     as.numeric(Sys.time() - t0, units = "secs"),
                     list(generaciones = do.call(rbind, generaciones), estado = estado))
}

METODOS <- list(grid = buscar_grid, random = buscar_random, bayesiano = buscar_bayesiano,
                genetico = buscar_genetico)

buscar <- function(metodo, modelo, balanceo, X, y, semilla = SEED, presupuesto = PRESUPUESTO) {
  METODOS[[metodo]](modelo, balanceo, X, y, semilla = semilla, presupuesto = presupuesto)
}

# ---------------------------------------------------------------------------
# Validación anidada
# ---------------------------------------------------------------------------
pliegue_externo <- function(i, f, metodo, modelo, balanceo, X, y, semilla) {
  X_tr <- X[f$tr, , drop = FALSE]; y_tr <- y[f$tr]
  X_va <- X[f$va, , drop = FALSE]; y_va <- y[f$va]
  res <- buscar(metodo, modelo, balanceo, X_tr, y_tr, semilla = semilla)
  t0 <- Sys.time()
  pipe <- ajustar_pipeline(modelo, balanceo, X_tr, y_tr, res$mejores_params, semilla)
  t_ajuste <- as.numeric(Sys.time() - t0, units = "secs")
  t0 <- Sys.time()
  p <- predecir(pipe, X_va)
  t_pred <- as.numeric(Sys.time() - t0, units = "secs")
  fila <- data.frame(pliegue = i - 1L, t(metricas(y_va, p)), auc_interna = res$mejor_score,
                     n_eval = res$n_eval, t_busqueda = res$tiempo,
                     t_por_eval = res$tiempo / max(res$n_eval, 1),
                     t_ajuste_final = t_ajuste, t_prediccion = t_pred,
                     params = jsonlite::toJSON(res$mejores_params, auto_unbox = TRUE,
                                               digits = 8))
  list(fila = fila, historial = res$historial, extra = res$extra, va = f$va, p = p)
}

PAQUETES_TRABAJO <- c("glmnet", "ranger", "xgboost", "themis", "rsample",
                      "ParBayesianOptimization", "GA", "jsonlite")

# Validación cruzada anidada K_EXTERNO x K_INTERNO. El bucle externo
# (estratificado) solo mide; el interno vive dentro de buscar(). Los pliegues
# externos son independientes y se reparten entre `procesos` sesiones de R.
validacion_anidada <- function(metodo, modelo, balanceo, X, y, semilla = SEED,
                               procesos = PROCESOS) {
  externo <- pliegues_estratificados(y, K_EXTERNO, semilla)
  # Cada sesión de trabajo es un R nuevo: carga los módulos del capítulo antes
  # de su primer pliegue (las funciones anidadas no viajan solas).
  modulos <- MODULOS_R
  tarea <- function(i) {
    if (!exists(".modulos_en_sesion", envir = globalenv())) {
      for (f in modulos) sys.source(f, envir = globalenv())
      assign(".modulos_en_sesion", TRUE, envir = globalenv())
    }
    pliegue_externo(i, externo[[i]], metodo, modelo, balanceo, X, y, semilla)
  }
  salida <- if (procesos > 1) {
    future.apply::future_lapply(seq_along(externo), tarea, future.seed = TRUE,
                                future.packages = PAQUETES_TRABAJO)
  } else {
    lapply(seq_along(externo), tarea)
  }
  oof <- rep(NA_real_, length(y))
  for (s in salida) oof[s$va] <- s$p
  list(filas = do.call(rbind, lapply(salida, `[[`, "fila")),
       historiales = lapply(salida, `[[`, "historial"),
       extras = lapply(salida, `[[`, "extra"), oof = oof)
}

# Modelos, balanceo, pipelines y espacios de búsqueda.
#
# Cada modelo tiene UN solo espacio de búsqueda (lista de parámetros) del que
# salen las cuatro representaciones que piden los optimizadores: la rejilla de
# Grid Search, el muestreo de Random Search, los límites del optimizador
# bayesiano y la decodificación del genoma en [0, 1]^d del algoritmo genético.
# Así los cuatro métodos exploran exactamente el mismo dominio y sus
# diferencias se deben al método, no al espacio.

# ---------------------------------------------------------------------------
# Espacios de búsqueda
# ---------------------------------------------------------------------------
param <- function(nombre, tipo, bajo = NA, alto = NA, log = FALSE, opciones = NULL,
                  rejilla = NULL) {
  list(nombre = nombre, tipo = tipo, bajo = bajo, alto = alto, log = log,
       opciones = opciones, rejilla = rejilla)
}

ESPACIOS <- list(
  # 2 penalizaciones x 12 valores de lambda = 24 puntos en la rejilla.
  # glmnet minimiza devianza/(2n) + lambda * penalización, así que lambda es
  # la fuerza de la regularización (el inverso de C en liblinear): 1e-4 es casi
  # sin regularizar y 1 anula todos los coeficientes con L1.
  logistica = list(
    param("penalizacion", "cat", opciones = c("l1", "l2"), rejilla = c("l1", "l2")),
    param("lambda", "float", 1e-4, 1, log = TRUE,
          rejilla = signif(10^seq(-4, 0, length.out = 12), 4))
  ),
  # 4 x 3 x 2 = 24. mtry en fracción de las 23 predictoras (0,2 ~ sqrt(23)/23,
  # el valor por defecto de ranger para clasificación).
  random_forest = list(
    param("max.depth", "int", 2, 16, rejilla = c(3, 5, 8, 16)),
    param("min.bucket", "int", 1, 20, rejilla = c(1, 4, 10)),
    param("frac.mtry", "float", 0.1, 1, rejilla = c(0.2, 0.5))
  ),
  # 3 x 2 x 2 x 2 = 24. Los tres últimos solo los exploran los métodos
  # continuos; Grid los deja en su valor por defecto (limitación típica de la
  # rejilla: no puede cubrir siete dimensiones con 24 puntos).
  xgboost = list(
    param("max_depth", "int", 2, 8, rejilla = c(2, 3, 5)),
    param("eta", "float", 0.01, 0.3, log = TRUE, rejilla = c(0.03, 0.1)),
    param("nrounds", "int", 50, 400, rejilla = c(100, 300)),
    param("min_child_weight", "float", 1, 10, log = TRUE, rejilla = c(1, 5)),
    param("subsample", "float", 0.6, 1),
    param("colsample_bytree", "float", 0.5, 1),
    param("lambda", "float", 0.01, 10, log = TRUE)
  )
)

# Rejilla de Grid Search como lista de configuraciones (en el orden de
# expand.grid: el primer parámetro varía más rápido).
rejilla <- function(modelo) {
  con_rejilla <- Filter(function(p) !is.null(p$rejilla), ESPACIOS[[modelo]])
  valores <- setNames(lapply(con_rejilla, `[[`, "rejilla"),
                      vapply(con_rejilla, `[[`, "", "nombre"))
  g <- expand.grid(valores, stringsAsFactors = FALSE)
  lapply(seq_len(nrow(g)), function(i) as.list(g[i, , drop = FALSE]))
}

# Una configuración al azar del dominio continuo (log-uniforme donde log = TRUE).
muestrear_config <- function(modelo) {
  setNames(lapply(ESPACIOS[[modelo]], function(p) {
    if (p$tipo == "cat") return(sample(p$opciones, 1))
    if (p$tipo == "int") return(sample(seq(p$bajo, p$alto), 1))
    if (p$log) exp(runif(1, log(p$bajo), log(p$alto))) else runif(1, p$bajo, p$alto)
  }), vapply(ESPACIOS[[modelo]], `[[`, "", "nombre"))
}

# Traduce un genoma (o un punto del cubo unidad) en [0, 1]^d a hiperparámetros:
# lineal o logarítmico para los reales, redondeando para los enteros y por
# intervalos iguales para los categóricos. Lo usan el genético y el bayesiano.
decodificar <- function(u, modelo) {
  esp <- ESPACIOS[[modelo]]
  setNames(lapply(seq_along(esp), function(j) {
    p <- esp[[j]]
    g <- min(max(u[j], 0), 1)
    if (p$tipo == "cat") return(p$opciones[min(floor(g * length(p$opciones)) + 1,
                                               length(p$opciones))])
    v <- if (p$log) exp(log(p$bajo) + g * (log(p$alto) - log(p$bajo)))
         else p$bajo + g * (p$alto - p$bajo)
    if (p$tipo == "int") round(v) else v
  }), vapply(esp, `[[`, "", "nombre"))
}

# ---------------------------------------------------------------------------
# Balanceo (dentro del pipeline: solo ve el pliegue de entrenamiento)
# ---------------------------------------------------------------------------
# * smote: SMOTE-NC (themis::smotenc), la variante para predictoras mixtas.
#   22 de las 23 predictoras son binarias: el SMOTE clásico generaría
#   pacientes con "media mutación" (IDH1 = 0,37), mientras que SMOTE-NC
#   interpola solo la edad y asigna a cada binaria el valor más frecuente entre
#   los vecinos. Sin variables continuas (paneles sin la edad) sigue siendo
#   aplicable: todas las columnas se tratan como nominales.
# * adasyn: themis::adasyn no tiene variante nominal, así que sí produce
#   valores intermedios en las binarias. Se mantiene como término de
#   comparación y su efecto se discute en el capítulo.
balancear <- function(X, y, balanceo, semilla) {
  if (!balanceo %in% c("smote", "adasyn")) return(list(X = X, y = y))
  set.seed(semilla)
  d <- X
  binarias <- intersect(names(d), BINARIAS)
  if (balanceo == "smote") {
    d[binarias] <- lapply(d[binarias], factor, levels = c(0, 1))
  }
  d$.y <- factor(y, levels = c(0, 1))
  r <- if (balanceo == "smote") {
    themis::smotenc(d, var = ".y", k = 5, over_ratio = 1)
  } else {
    themis::adasyn(d, var = ".y", k = 5, over_ratio = 1)
  }
  r <- as.data.frame(r)
  if (balanceo == "smote") {
    r[binarias] <- lapply(r[binarias], function(v) as.integer(as.character(v)))
  }
  list(X = r[names(X)], y = as.integer(as.character(r$.y)))
}

# Pesos "balanceados": n / (2 * n_clase), el mismo criterio que
# class_weight = "balanced" de scikit-learn.
pesos_clase <- function(y) {
  n <- length(y)
  c(`0` = n / (2 * sum(y == 0)), `1` = n / (2 * sum(y == 1)))
}

# ---------------------------------------------------------------------------
# Clasificadores
# ---------------------------------------------------------------------------
PARAMS_DEFECTO <- list(
  logistica = list(penalizacion = "l2", lambda = 0.01),
  random_forest = list(max.depth = 0, min.bucket = 1, frac.mtry = 0.2),
  xgboost = list(max_depth = 3, eta = 0.1, nrounds = 200, min_child_weight = 1,
                 subsample = 1, colsample_bytree = 1, lambda = 1)
)

ajustar_modelo <- function(modelo, X, y, params, balanceo, semilla, n_arboles = 200L,
                           tree_method = "hist", guardar_inbag = FALSE) {
  params <- modifyList(PARAMS_DEFECTO[[modelo]], params)
  A <- matriz_modelo(X, modelo)
  cw <- if (balanceo == "class_weight") pesos_clase(y) else NULL
  if (modelo == "logistica") {
    w <- if (is.null(cw)) rep(1, length(y)) else unname(cw[as.character(y)])
    ajuste <- glmnet::glmnet(A, y, family = "binomial",
                             alpha = if (params$penalizacion == "l1") 1 else 0,
                             lambda = params$lambda, standardize = FALSE, weights = w)
    return(list(tipo = modelo, ajuste = ajuste, params = params))
  }
  if (modelo == "random_forest") {
    mtry <- max(1L, floor(params$frac.mtry * ncol(A)))
    ajuste <- ranger::ranger(x = X, y = factor(y, levels = c(0, 1)), num.trees = n_arboles,
                             mtry = mtry, max.depth = params$max.depth,
                             min.bucket = params$min.bucket, probability = TRUE,
                             class.weights = if (is.null(cw)) NULL else unname(cw),
                             num.threads = 1, seed = semilla, keep.inbag = guardar_inbag)
    return(list(tipo = modelo, ajuste = ajuste, params = params))
  }
  if (modelo == "xgboost") {
    set.seed(semilla)
    p <- list(objective = "binary:logistic", eval_metric = "logloss",
              tree_method = tree_method, max_depth = params$max_depth, eta = params$eta,
              min_child_weight = params$min_child_weight, subsample = params$subsample,
              colsample_bytree = params$colsample_bytree, lambda = params$lambda,
              nthread = 1)
    if (!is.null(cw)) p$scale_pos_weight <- sum(y == 0) / max(sum(y == 1), 1)
    ajuste <- xgboost::xgb.train(params = p, data = xgboost::xgb.DMatrix(A, label = y),
                                 nrounds = params$nrounds, verbose = 0)
    return(list(tipo = modelo, ajuste = ajuste, params = params))
  }
  stop("Modelo desconocido: ", modelo)
}

# glmnet exige al menos dos columnas: con una sola predictora (paneles de
# k = 1) se añade una columna de ceros, que no cambia el ajuste.
matriz_modelo <- function(X, modelo) {
  A <- as.matrix(X)
  if (modelo == "logistica" && ncol(A) == 1) A <- cbind(A, .cero = 0)
  A
}

predecir_modelo <- function(m, X) {
  A <- matriz_modelo(X, m$tipo)
  switch(m$tipo,
         logistica = as.numeric(predict(m$ajuste, A, type = "response")),
         random_forest = predict(m$ajuste, X, num.threads = 1)$predictions[, "1"],
         xgboost = predict(m$ajuste, xgboost::xgb.DMatrix(A)))
}

# ---------------------------------------------------------------------------
# Pipeline completo: escalado -> balanceo -> clasificador
# ---------------------------------------------------------------------------
# Todo el preprocesamiento se ajusta dentro de cada pliegue (interno o externo)
# con los datos de entrenamiento de ese pliegue: la media y la desviación de la
# edad se calculan sin ver la validación, y el sobremuestreo nunca crea
# pacientes sintéticos a partir de pacientes de validación (solo actúa al
# ajustar, nunca al predecir). Solo se escala la edad: las binarias ya están en
# {0, 1}. Los árboles no lo necesitan, pero mantener el mismo pipeline en los
# tres modelos hace comparables las distancias de SMOTE-NC y ADASYN.
escalar <- function(pipe, X) {
  X <- X[, pipe$columnas, drop = FALSE]
  if (!is.null(pipe$escala)) {
    X$Age_at_diagnosis <- (X$Age_at_diagnosis - pipe$escala["media"]) / pipe$escala["de"]
  }
  X
}

ajustar_pipeline <- function(modelo, balanceo, X, y, params = list(), semilla = SEED,
                             columnas = PREDICTORAS, ...) {
  columnas <- PREDICTORAS[PREDICTORAS %in% columnas]
  pipe <- list(modelo = modelo, balanceo = balanceo, columnas = columnas, escala = NULL)
  if ("Age_at_diagnosis" %in% columnas) {
    edad <- X$Age_at_diagnosis
    pipe$escala <- c(media = mean(edad), de = sqrt(mean((edad - mean(edad))^2)))
  }
  Xe <- escalar(pipe, X)
  b <- balancear(Xe, y, balanceo, semilla)
  pipe$ajuste <- ajustar_modelo(modelo, b$X, b$y, params, balanceo, semilla, ...)
  pipe$datos_ajuste <- if (isTRUE(list(...)$guardar_inbag)) b else NULL
  class(pipe) <- "pipeline_glioma"
  pipe
}

predecir <- function(pipe, X) predecir_modelo(pipe$ajuste, escalar(pipe, X))

# ---------------------------------------------------------------------------
# Métricas
# ---------------------------------------------------------------------------
# AUC por la fórmula de Mann-Whitney (empates cuentan la mitad); es la misma
# cifra que pROC::auc y mucho más rápida dentro de los bucles de búsqueda.
auc_roc <- function(y, p) {
  r <- rank(p)
  n1 <- sum(y == 1)
  n0 <- length(y) - n1
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}

metricas <- function(y, p, umbral = 0.5) {
  pred <- as.integer(p >= umbral)
  tp <- sum(pred == 1 & y == 1)
  fp <- sum(pred == 1 & y == 0)
  fn <- sum(pred == 0 & y == 1)
  precision <- if (tp + fp > 0) tp / (tp + fp) else 0
  recall <- tp / (tp + fn)
  c(auc = auc_roc(y, p), accuracy = mean(pred == y), precision = precision,
    recall = recall,
    f1 = if (precision + recall > 0) 2 * precision * recall / (precision + recall) else 0,
    brier = mean((p - y)^2))
}

# Calibración, pruebas estadísticas y tamaños de efecto.
#
# Se usan las implementaciones estándar de R (pROC para DeLong, boot para los
# intervalos BCa, stats para Friedman, Wilcoxon y Holm, effsize para el delta
# de Cliff); solo la diferencia crítica de Nemenyi se escribe a mano, porque
# es una fórmula cerrada sobre la distribución del rango estudentizado.

# ---------------------------------------------------------------------------
# Calibración
# ---------------------------------------------------------------------------
bins_iguales <- function(p, n_bins) pmin(pmax(findInterval(p, seq(0, 1, length.out = n_bins + 1)[-c(1, n_bins + 1)]), 0), n_bins - 1)

# Error de calibración esperado (Naeini et al., 2015): promedio, ponderado por
# los pacientes de cada bin de igual ancho, de |observada - predicha|.
ece <- function(y, p, n_bins = 10) {
  b <- bins_iguales(p, n_bins)
  sum(vapply(split(seq_along(p), b), function(i) {
    length(i) / length(p) * abs(mean(y[i]) - mean(p[i]))
  }, numeric(1)))
}

curva_confiabilidad <- function(y, p, n_bins = 10) {
  b <- bins_iguales(p, n_bins)
  do.call(rbind, lapply(split(seq_along(p), b), function(i) {
    data.frame(bin = b[i[1]], predicha = mean(p[i]), observada = mean(y[i]), n = length(i))
  }))
}

# ---------------------------------------------------------------------------
# DeLong y bootstrap
# ---------------------------------------------------------------------------
# Prueba de DeLong para dos AUC medidas sobre los mismos pacientes
# (DeLong, DeLong y Clarke-Pearson, 1988), con pROC::roc.test.
delong <- function(y, p_a, p_b) {
  ra <- pROC::roc(y, p_a, levels = c(0, 1), direction = "<", quiet = TRUE)
  rb <- pROC::roc(y, p_b, levels = c(0, 1), direction = "<", quiet = TRUE)
  t <- pROC::roc.test(ra, rb, method = "delong", paired = TRUE)
  list(auc_a = as.numeric(ra$auc), auc_b = as.numeric(rb$auc),
       diferencia = as.numeric(ra$auc - rb$auc), z = unname(t$statistic), p = t$p.value)
}

# Intervalo bootstrap BCa remuestreando pacientes (boot::boot y boot.ci). Las
# columnas de `datos` se remuestrean con los mismos índices, como exige
# comparar dos modelos sobre los mismos pacientes. Si una réplica deja una sola
# clase el estadístico no existe; si BCa no es calculable se usa el intervalo
# de percentiles.
ic_bootstrap <- function(estadistico, datos, n = 2000, semilla = SEED, nivel = 0.95) {
  set.seed(semilla)
  b <- boot::boot(datos, function(d, i) {
    r <- tryCatch(estadistico(d[i, , drop = FALSE]), error = function(e) NA_real_)
    if (is.finite(r)) r else NA_real_
  }, R = n)
  ic <- tryCatch(boot::boot.ci(b, conf = nivel, type = "bca")$bca[4:5],
                 error = function(e) NULL)
  if (is.null(ic) || any(!is.finite(ic))) {
    ic <- quantile(b$t, c((1 - nivel) / 2, 1 - (1 - nivel) / 2), na.rm = TRUE, names = FALSE)
  }
  ic
}

# ---------------------------------------------------------------------------
# Friedman, Nemenyi y diferencia crítica (Demšar, 2006)
# ---------------------------------------------------------------------------
# `matriz`: filas = bloques (pliegues externos), columnas = procedimientos;
# valores = la métrica (más alto es mejor). Rango 1 = mejor.
friedman <- function(matriz) {
  matriz <- as.matrix(matriz)
  N <- nrow(matriz); k <- ncol(matriz)
  rangos <- t(apply(-matriz, 1, rank))
  prueba <- friedman.test(matriz)
  chi2 <- unname(prueba$statistic)
  ff <- (N - 1) * chi2 / (N * (k - 1) - chi2)
  list(chi2 = chi2, gl = k - 1, p = prueba$p.value, F_ID = ff,
       p_ID = pf(ff, k - 1, (k - 1) * (N - 1), lower.tail = FALSE), N = N, k = k,
       rangos = sort(colMeans(rangos)))
}

# CD = q_alfa * sqrt(k (k + 1) / (6 N)), con q_alfa el cuantil del rango
# estudentizado con infinitos grados de libertad dividido entre sqrt(2).
diferencia_critica <- function(k, N, alfa = 0.05) {
  qtukey(1 - alfa, k, Inf) / sqrt(2) * sqrt(k * (k + 1) / (6 * N))
}

# p-valores de Nemenyi para todos los pares a partir de los rangos promedio.
nemenyi <- function(rangos, N) {
  k <- length(rangos)
  se <- sqrt(k * (k + 1) / (6 * N))
  p <- outer(rangos, rangos, function(a, b) {
    ptukey(abs(a - b) / se * sqrt(2), k, Inf, lower.tail = FALSE)
  })
  dimnames(p) <- list(names(rangos), names(rangos))
  p
}

# ---------------------------------------------------------------------------
# Tamaño del efecto
# ---------------------------------------------------------------------------
# Delta de Cliff, P(a > b) - P(a < b), con effsize::cliff.delta; magnitud con
# los umbrales de Romano et al. (2006): 0,147, 0,33 y 0,474.
delta_cliff <- function(a, b) unname(effsize::cliff.delta(a, b)$estimate)

magnitud_cliff <- function(d) {
  d <- abs(d)
  if (d < 0.147) "despreciable" else if (d < 0.33) "pequeño" else if (d < 0.474) "mediano"
  else "grande"
}

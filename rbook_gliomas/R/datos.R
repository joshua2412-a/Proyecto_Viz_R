# Carga de datos y matriz de predictoras.
#
# La partición 80/20 estratificada es la del EDA: se lee de train_data.csv y
# test_data.csv, de modo que los 168 pacientes de prueba son los mismos que el
# análisis exploratorio dejó sin mirar.

preparar_predictoras <- function(df) {
  # Agrupa Race en Race_binary (0 = White, 1 = otros grupos), la misma
  # agrupación que el EDA usó en la prueba de independencia: dos de las cuatro
  # categorías originales tienen 14 y 1 pacientes y no permiten estimar un
  # efecto propio.
  df$Race_binary <- as.integer(as.integer(df$Race) != 0L)
  X <- df[, PREDICTORAS]
  X[BINARIAS] <- lapply(X[BINARIAS], as.integer)
  X$Age_at_diagnosis <- as.numeric(X$Age_at_diagnosis)
  as.data.frame(X)
}

# Identifica a cada paciente por su fila en TCGA_InfoWithGrade.csv (0-based,
# como el índice de pandas del Jupyter Book y del dashboard). Las filas de
# train/test se emparejan con el dataset completo por su contenido; si hay
# perfiles repetidos, cada copia se asigna a una fila distinta.
indice_en_dataset <- function(parte, completo) {
  clave <- function(d) do.call(paste, c(d[, names(completo)], sep = "|"))
  k_comp <- clave(completo)
  k_part <- clave(parte)
  usados <- logical(length(k_comp))
  vapply(k_part, function(k) {
    i <- which(k_comp == k & !usados)[1]
    usados[i] <<- TRUE
    i - 1L
  }, integer(1), USE.NAMES = FALSE)
}

cargar_particion <- function() {
  completo <- read.csv(RUTA_DATASET)
  tr <- read.csv(RUTA_TRAIN)
  te <- read.csv(RUTA_TEST)
  list(X_train = preparar_predictoras(tr), y_train = as.integer(tr[[OBJETIVO]]),
       X_test = preparar_predictoras(te), y_test = as.integer(te[[OBJETIVO]]),
       id_train = indice_en_dataset(tr, completo),
       id_test = indice_en_dataset(te, completo))
}

# Pliegues estratificados (rsample::vfold_cv). Devuelve una lista con los
# índices de entrenamiento y de validación de cada pliegue.
pliegues_estratificados <- function(y, k, semilla) {
  set.seed(semilla)
  cv <- rsample::vfold_cv(data.frame(y = factor(y)), v = k, strata = y)
  lapply(cv$splits, function(s) {
    list(tr = sort(s$in_id), va = sort(setdiff(seq_along(y), s$in_id)))
  })
}

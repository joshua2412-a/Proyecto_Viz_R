# Configuración global del modelado: semilla, rutas, variables y colores.
#
# Todo lo que deba ser idéntico entre el capítulo, las búsquedas y la
# exportación para la app Shiny se define aquí una sola vez.

# ---------------------------------------------------------------------------
# Reproducibilidad
# ---------------------------------------------------------------------------
# Semilla global. Es la misma del EDA y se pasa de forma explícita a los
# pliegues, a los modelos, al sobremuestreo y a los dos optimizadores con azar.
SEED <- 42L

# Pliegues del bucle externo e interno de la validación anidada.
K_EXTERNO <- 10L
K_INTERNO <- 3L

# Evaluaciones (configuraciones de hiperparámetros) por búsqueda. Es igual
# para los cuatro optimizadores, así la comparación es a presupuesto fijo.
PRESUPUESTO <- 24L

# Procesos para repartir los pliegues externos (future, sesiones separadas:
# funciona igual en Windows, macOS y Linux).
PROCESOS <- 2L

# ---------------------------------------------------------------------------
# Rutas (relativas a la carpeta del libro, que es donde knitr ejecuta)
# ---------------------------------------------------------------------------
RUTA_TRAIN     <- "dataset/train_data.csv"
RUTA_TEST      <- "dataset/test_data.csv"
RUTA_DATASET   <- "dataset/TCGA_InfoWithGrade.csv"
DIR_RESULTADOS <- "resultados"
DIR_CACHE      <- file.path(DIR_RESULTADOS, "cache")

# ---------------------------------------------------------------------------
# Variables
# ---------------------------------------------------------------------------
OBJETIVO <- "Grade"
GENES <- c("IDH1", "TP53", "ATRX", "PTEN", "EGFR", "CIC", "MUC16", "PIK3CA", "NF1",
           "PIK3R1", "FUBP1", "RB1", "NOTCH1", "BCOR", "CSMD3", "SMARCA4", "GRIN2A",
           "IDH2", "FAT4", "PDGFRA")
NUMERICAS <- "Age_at_diagnosis"
# Binarias en el orden en que entran al modelo (Race_binary sustituye a Race:
# blanco = 0, otros grupos = 1, la misma agrupación del EDA).
BINARIAS    <- c("Gender", "Race_binary", GENES)
PREDICTORAS <- c(NUMERICAS, BINARIAS)

# ---------------------------------------------------------------------------
# Nombres legibles y colores
# ---------------------------------------------------------------------------
MODELOS       <- c("logistica", "random_forest", "xgboost")
BALANCEOS     <- c("ninguno", "smote", "adasyn", "class_weight")
OPTIMIZADORES <- c("grid", "random", "bayesiano", "genetico")

NOMBRE <- c(logistica = "Regresión Logística", random_forest = "Random Forest",
            xgboost = "XGBoost", ninguno = "Sin balanceo", smote = "SMOTE-NC",
            adasyn = "ADASYN", class_weight = "Pesos de clase", grid = "Grid Search",
            random = "Random Search", bayesiano = "Bayesiano (GP)",
            genetico = "Genético (GA)")
CORTO <- c(logistica = "RL", random_forest = "RF", xgboost = "XGB", ninguno = "Sin",
           smote = "SMOTE", adasyn = "ADASYN", class_weight = "Pesos", grid = "Grid",
           random = "Random", bayesiano = "GP", genetico = "GA")

# Un color por modelo: paleta fría validada para daltonismo (todas las
# parejas con ΔE OKLab >= 9 sobre fondo claro), la misma del dashboard.
COLOR_MODELO_ID <- c(logistica = "#1f5fa8", random_forest = "#199e70", xgboost = "#9085e9")
COLOR_OPTIMIZADOR_ID <- c(grid = "#8a94a6", random = "#1f5fa8", bayesiano = "#199e70",
                          genetico = "#9085e9")
COLOR_MODELO      <- setNames(unname(COLOR_MODELO_ID), NOMBRE[MODELOS])
COLOR_OPTIMIZADOR <- setNames(unname(COLOR_OPTIMIZADOR_ID), NOMBRE[OPTIMIZADORES])
TINTA <- "#2b2f36"

nombre <- function(x) unname(NOMBRE[x])
id_combo <- function(modelo, balanceo, metodo) paste(modelo, balanceo, metodo, sep = "|")

# Mensajes de ParBayesianOptimization (0 = ninguno)
VERBOSO_BO <- 0

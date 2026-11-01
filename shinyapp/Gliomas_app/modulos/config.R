# =========================================================================== #
# Configuración central del tablero
#
# Rutas, enlaces, esquema del dataset y etiquetas legibles. Es la versión en R
# de utils/config.py del tablero de Dash: mismas variables, mismo orden, mismas
# etiquetas, así que las cifras y los textos de las dos versiones coinciden.
# =========================================================================== #

# --------------------------------------------------------------------------- #
# Rutas (relativas a la carpeta de la app, que es el directorio de trabajo
# cuando se lanza con shiny::runApp("shinyapp/Gliomas_app"))
# --------------------------------------------------------------------------- #
DATA_DIR <- "data"
DATA_PATH <- file.path(DATA_DIR, "TCGA_InfoWithGrade.csv")
TRAIN_PATH <- file.path(DATA_DIR, "train_data.csv")
TEST_PATH <- file.path(DATA_DIR, "test_data.csv")
MODELOS_PATH <- file.path(DATA_DIR, "modelos_finales.json")
RESUMEN_PATH <- file.path(DATA_DIR, "resumen_modelado.json")

# Operador "si es nulo, usa esto" (viene en R >= 4.4; aquí para versiones previas)
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}

# --------------------------------------------------------------------------- #
# Enlaces del proyecto
# --------------------------------------------------------------------------- #
URL_LIBRO <- "https://joshua2412-a.github.io/Proyecto_Viz_R/"
URL_LIBRO_EDA <- paste0(URL_LIBRO, "intro.html")
URL_LIBRO_MODELADO <- paste0(URL_LIBRO, "modelado.html")
URL_REPO <- "https://github.com/joshua2412-a/Proyecto_Viz_R"
URL_LIBRO_PY <- "https://joshua2412-a.github.io/Proyecto_Viz/"
URL_DASH_PY <- "https://gliomas-dashboard-984926604434.us-central1.run.app"
URL_DATASET <- paste0(
  "https://archive.ics.uci.edu/dataset/759/",
  "glioma+grading+clinical+and+mutation+features+dataset"
)

AUTORES <- list(
  list(
    nombre = "Alejandro Cantillo Escorcia",
    github = "https://github.com/Alej0126",
    linkedin = "https://www.linkedin.com/in/alejandro-cantillo-a8a13b3a6"
  ),
  list(
    nombre = "Joshua Hincapie LLorente",
    github = "https://github.com/joshua2412-a",
    linkedin = paste0(
      "https://www.linkedin.com/in/",
      "joshua-alessandro-hincapie-llorente-74b305441"
    )
  )
)

# --------------------------------------------------------------------------- #
# Partición: la misma 80/20 estratificada del libro, leída de
# data/train_data.csv y data/test_data.csv (los archivos del proyecto).
# --------------------------------------------------------------------------- #
TEST_SIZE <- 0.20

# --------------------------------------------------------------------------- #
# Esquema del dataset
# --------------------------------------------------------------------------- #
TARGET <- "Grade"
NUMERIC_FEATURES <- "Age_at_diagnosis"
CATEGORICAL_FEATURES <- c("Gender", "Race")
GENE_FEATURES <- c(
  "IDH1", "TP53", "ATRX", "PTEN", "EGFR", "CIC", "MUC16", "PIK3CA", "NF1",
  "PIK3R1", "FUBP1", "RB1", "NOTCH1", "BCOR", "CSMD3", "SMARCA4", "GRIN2A",
  "IDH2", "FAT4", "PDGFRA"
)
FEATURES <- c(NUMERIC_FEATURES, CATEGORICAL_FEATURES, GENE_FEATURES)

GRADE_LABELS <- c("0" = "LGG", "1" = "GBM")
GRADE_NOMBRES <- c(
  "0" = "Glioma de bajo grado (LGG)",
  "1" = "Glioblastoma multiforme (GBM)"
)
GENDER_LABELS <- c("0" = "Masculino", "1" = "Femenino")
RACE_LABELS <- c(
  "0" = "White",
  "1" = "Black or African American",
  "2" = "Asian",
  "3" = "American Indian or Alaska Native"
)
MUTACION_LABELS <- c("0" = "No mutado (wildtype)", "1" = "Mutado")
RACE_AGRUPADA <- c(
  "0" = "White",
  "1" = "Other racial groups",
  "2" = "Other racial groups",
  "3" = "Other racial groups"
)

LABELS <- c(
  Grade = "Grado del glioma",
  Age_at_diagnosis = "Edad al diagnóstico (años)",
  Gender = "Género",
  Race = "Grupo racial reportado",
  grade_label = "Grado",
  stats::setNames(paste("Mutación en", GENE_FEATURES), GENE_FEATURES)
)

etiqueta <- function(variable) {
  if (variable %in% names(LABELS)) unname(LABELS[[variable]]) else variable
}

CLINICA_DESCRIPCION <- c(
  Age_at_diagnosis = paste(
    "Edad del paciente en el momento del diagnóstico. Lleva",
    "decimales porque recoge los días exactos, no solo el año."
  ),
  Gender = "Sexo registrado del paciente en la historia clínica de TCGA.",
  Race = paste(
    "Grupo racial reportado, en las cuatro categorías que usa TCGA. La",
    "cohorte está muy desbalanceada hacia el grupo White."
  )
)

VARIABLE_PAPEL <- c(
  Age_at_diagnosis = "Predictora clínica · se contrasta con la U de Mann-Whitney",
  Gender = "Predictora clínica · chi-cuadrado y V de Cramér",
  Race = "Predictora clínica · chi-cuadrado (categorías muy desbalanceadas)"
)

GENE_DESCRIPCION <- c(
  IDH1 = paste("Enzima del metabolismo celular. Su mutación es el marcador",
               "característico de los gliomas de bajo grado."),
  TP53 = paste("Gen supresor tumoral: controla el ciclo celular y la respuesta al",
               "daño del ADN."),
  ATRX = "Mantiene la estructura de la cromatina y la estabilidad del genoma.",
  PTEN = "Gen supresor tumoral que regula crecimiento y supervivencia celular.",
  EGFR = "Receptor de señales de crecimiento y proliferación celular.",
  CIC = "Regula la expresión de otros genes y el desarrollo celular.",
  MUC16 = "Proteína de gran tamaño asociada a la superficie celular.",
  PIK3CA = "Vía PI3K/AKT: crecimiento, supervivencia y proliferación.",
  NF1 = "Regulador negativo de señales de crecimiento celular.",
  PIK3R1 = "Subunidad reguladora de la vía PI3K.",
  FUBP1 = "Regula la expresión génica y el crecimiento celular.",
  RB1 = "Gen supresor tumoral: controla la progresión hacia la división celular.",
  NOTCH1 = "Vía Notch: diferenciación, proliferación y supervivencia.",
  BCOR = "Regula la expresión génica mediante complejos de cromatina.",
  CSMD3 = "Proteína de gran tamaño; muta en distintos tipos de tumor.",
  SMARCA4 = "Remodelación de la cromatina y regulación de la expresión génica.",
  GRIN2A = "Subunidad de un receptor de glutamato (señalización neuronal).",
  IDH2 = "Enzima metabólica relacionada con IDH1.",
  FAT4 = "Organización, adhesión y crecimiento celular.",
  PDGFRA = "Receptor de señales de crecimiento y proliferación."
)

# Nombre corto en español de las variables clínicas, para tablas, ejes y
# lecturas donde no cabe la etiqueta larga (como nombre_corto() en Dash). Los
# genes se quedan con su símbolo; las columnas del CSV siguen en inglés.
NOMBRE_CORTO <- c(Grade = "Grado", Age_at_diagnosis = "Edad", Gender = "Género",
                  Race = "Grupo racial", "Race (binaria)" = "Grupo racial",
                  Race_binary = "Grupo racial")
nombre_corto <- function(variable) {
  ifelse(variable %in% names(NOMBRE_CORTO), NOMBRE_CORTO[variable], variable)
}

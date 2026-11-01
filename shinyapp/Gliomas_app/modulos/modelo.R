# =========================================================================== #
# Los tres clasificadores finales del capítulo de modelado, para predecir y
# explicar
#
# La app no entrena nada ni carga objetos de Python: lee data/modelos_finales.json,
# que exporta el capítulo de modelado con los coeficientes de la regresión
# logística y los árboles de Random Forest y XGBoost como listas de nodos. Con
# eso basta R base para reproducir sus probabilidades (coinciden con las del
# notebook hasta 1e-7), así que no hace falta instalar scikit-learn ni xgboost.
#
# Las explicaciones son valores de Shapley exactos:
#   * regresión logística: forma cerrada, beta_j * (x_j - media_j), en log-odds;
#   * árboles: TreeSHAP dependiente de la trayectoria (Lundberg, Erion y Lee,
#     2018, algoritmo 2), el mismo de shap.TreeExplainer. En XGBoost sale en
#     log-odds; en Random Forest, que promedia probabilidades, en probabilidad.
# En los tres casos las contribuciones suman la predicción menos el valor base.
# =========================================================================== #

ORDEN_MODELOS <- c("logistica", "random_forest", "xgboost")
NOMBRES_MODELO <- c(logistica = "Regresión logística", random_forest = "Random Forest",
                    xgboost = "XGBoost")
NOMBRE_BALANCEO <- c(ninguno = "sin balanceo", smote = "SMOTE-NC", adasyn = "ADASYN",
                     class_weight = "pesos de clase")
NOMBRE_OPTIMIZADOR <- c(grid = "Grid Search", random = "Random Search",
                        bayesiano = "Bayesiano (GP)", genetico = "Genético (GA)")
UMBRAL <- 0.5

# --------------------------------------------------------------------------- #
# Carga
# --------------------------------------------------------------------------- #
preparar_arbol <- function(t) {
  izq <- as.integer(unlist(t$izq))
  arbol <- list(
    hoja = izq == -1L,
    # Índices de nodo y de variable vienen de Python (desde 0): se pasan a R
    izq = ifelse(izq == -1L, NA_integer_, izq + 1L),
    der = ifelse(as.integer(unlist(t$der)) == -1L, NA_integer_, as.integer(unlist(t$der)) + 1L),
    var = as.integer(unlist(t$var)) + 1L,
    umbral = as.numeric(unlist(t$umbral)),
    valor = as.numeric(unlist(t$valor)),
    cobertura = as.numeric(unlist(t$cobertura))
  )
  arbol$esperado <- sum(arbol$valor[arbol$hoja] * arbol$cobertura[arbol$hoja]) / arbol$cobertura[1]
  arbol
}

cargar_modelos <- function() {
  if (!file.exists(MODELOS_PATH)) stop("No se encontró ", MODELOS_PATH, ".")
  datos <- jsonlite::fromJSON(MODELOS_PATH, simplifyVector = FALSE)
  datos$predictoras <- unlist(datos$predictoras)
  for (clave in names(datos$modelos)) {
    m <- datos$modelos[[clave]]
    if (m$tipo == "lineal") {
      m$coeficientes <- as.numeric(unlist(m$coeficientes))
      m$media_fondo <- as.numeric(unlist(m$media_fondo))
    } else {
      m$arboles <- lapply(m$arboles, preparar_arbol)
    }
    m$float32 <- isTRUE(m$float32)
    datos$modelos[[clave]] <- m
  }
  datos
}

cargar_resumen <- function() {
  if (!file.exists(RESUMEN_PATH)) stop("No se encontró ", RESUMEN_PATH, ".")
  jsonlite::fromJSON(RESUMEN_PATH, simplifyVector = FALSE)
}

MODELOS <- cargar_modelos()
RESUMEN <- cargar_resumen()

# Vectores numéricos del resumen (el JSON llega como listas)
vec <- function(x) as.numeric(unlist(x))

# Panel reducido del libro: el número mínimo de variables (regla de un error
# estándar) y cuáles son, en el orden del ranking SHAP.
K_PANEL <- as.integer(RESUMEN$panel_reducido$k_min %||% 4L)
VARIABLES_PANEL <- unlist(RESUMEN$panel_reducido$variables) %||%
  vapply(utils::head(RESUMEN$importancia, K_PANEL), `[[`, character(1), "variable")
NUMEROS <- c("una", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez")
numero_texto <- function(n) if (n >= 1 && n <= length(NUMEROS)) NUMEROS[n] else as.character(n)
nombres_panel <- function() {
  ifelse(VARIABLES_PANEL == "Age_at_diagnosis", "edad", VARIABLES_PANEL)
}
# "la edad y tres genes", "cuatro genes"...: el panel dicho en palabras
panel_en_palabras <- function() {
  genes <- sum(VARIABLES_PANEL %in% GENE_FEATURES)
  con_edad <- "Age_at_diagnosis" %in% VARIABLES_PANEL
  texto_genes <- paste(numero_texto(genes), if (genes == 1) "gen" else "genes")
  if (con_edad) paste("la edad y", texto_genes) else texto_genes
}

# El peor error de XGBoost en prueba (mayor distancia entre el grado real y la
# probabilidad): el caso difícil que se ofrece en Predicción. Se numera como en
# el libro, por su fila en TCGA_InfoWithGrade.csv empezando en 0.
PACIENTE_DIFICIL <- local({
  y <- as.numeric(unlist(RESUMEN$test$y))
  p <- as.numeric(unlist(RESUMEN$test$xgboost))
  as.integer(unlist(RESUMEN$test$indice))[which.max(abs(y - p))]
})

mejor_modelo_test <- function() {
  aucs <- vapply(ORDEN_MODELOS, function(m) RESUMEN$modelos[[m]]$auc_test, numeric(1))
  list(clave = names(which.max(aucs)), auc = max(aucs))
}

# --------------------------------------------------------------------------- #
# Del formulario al vector del modelo
# --------------------------------------------------------------------------- #
# `paciente`: lista con Age_at_diagnosis, Gender, Race (0-3) y los 20 genes (0/1).
# Race se agrupa en Race_binary igual que en el libro y la edad se estandariza
# con la media y la desviación del entrenamiento.
vector_paciente <- function(paciente) {
  vapply(MODELOS$predictoras, function(variable) {
    if (variable == "Age_at_diagnosis") {
      (as.numeric(paciente$Age_at_diagnosis) - MODELOS$media_edad) / MODELOS$escala_edad
    } else if (variable == "Race_binary") {
      if (as.integer(paciente$Race %||% 0) == 0) 0 else 1
    } else {
      as.numeric(paciente[[variable]] %||% 0)
    }
  }, numeric(1), USE.NAMES = FALSE)
}

# XGBoost compara en float32: sin esto, un paciente justo en un umbral podría
# ir a la rama contraria que en el modelo original.
a_float32 <- function(x) readBin(writeBin(as.numeric(x), raw(), size = 4), "double",
                                 n = length(x), size = 4)

preparar_x <- function(modelo, x) if (modelo$float32) a_float32(x) else x

va_izquierda <- function(v, umbral, estricto) if (estricto) v < umbral else v <= umbral

hoja_de <- function(arbol, x, estricto) {
  n <- 1L
  while (!arbol$hoja[n]) {
    n <- if (va_izquierda(x[arbol$var[n]], arbol$umbral[n], estricto)) arbol$izq[n] else arbol$der[n]
  }
  n
}

sigmoide <- function(z) 1 / (1 + exp(-z))

# Probabilidad de GBM según cada uno de los tres modelos
predecir <- function(paciente) {
  x <- vector_paciente(paciente)
  salida <- vapply(ORDEN_MODELOS, function(clave) {
    m <- MODELOS$modelos[[clave]]
    if (m$tipo == "lineal") {
      return(sigmoide(sum(x * m$coeficientes) + m$intercepto))
    }
    xp <- preparar_x(m, x)
    estricto <- m$comparacion == "<"
    total <- m$base + sum(vapply(m$arboles, function(a) a$valor[hoja_de(a, xp, estricto)], numeric(1)))
    if (m$agregacion == "media") total / length(m$arboles) else sigmoide(total)
  }, numeric(1))
  as.list(salida)
}

# --------------------------------------------------------------------------- #
# TreeSHAP (Lundberg, Erion y Lee, 2018; algoritmo 2)
#
# El camino se guarda como cuatro vectores paralelos: variable (d), fracción
# "cero" (z), fracción "uno" (o) y peso (w). La posición 1 es el elemento raíz
# ficticio del algoritmo (variable 0), como el índice 0 en la versión de Python.
# --------------------------------------------------------------------------- #
extender <- function(camino, pz, po, pi) {
  l <- length(camino$d)
  camino$d <- c(camino$d, pi)
  camino$z <- c(camino$z, pz)
  camino$o <- c(camino$o, po)
  camino$w <- c(camino$w, if (l == 0) 1 else 0)
  if (l > 0) {
    for (i in (l - 1):0) {
      # i es el índice de Python; en R la posición es i + 1
      camino$w[i + 2] <- camino$w[i + 2] + po * camino$w[i + 1] * (i + 1) / (l + 1)
      camino$w[i + 1] <- pz * camino$w[i + 1] * (l - i) / (l + 1)
    }
  }
  camino
}

deshacer <- function(camino, i) {
  # i: índice de Python del elemento a quitar
  l <- length(camino$d) - 1
  n <- camino$w[l + 1]
  pz <- camino$z[i + 1]
  po <- camino$o[i + 1]
  if (l > 0) {
    for (j in (l - 1):0) {
      if (po != 0) {
        t <- camino$w[j + 1]
        camino$w[j + 1] <- n * (l + 1) / ((j + 1) * po)
        n <- t - camino$w[j + 1] * pz * (l - j) / (l + 1)
      } else {
        camino$w[j + 1] <- camino$w[j + 1] * (l + 1) / (pz * (l - j))
      }
    }
  }
  if (i < l) {
    for (j in i:(l - 1)) {
      camino$d[j + 1] <- camino$d[j + 2]
      camino$z[j + 1] <- camino$z[j + 2]
      camino$o[j + 1] <- camino$o[j + 2]
    }
  }
  quedan <- seq_len(l)
  list(d = camino$d[quedan], z = camino$z[quedan], o = camino$o[quedan], w = camino$w[quedan])
}

peso_deshecho <- function(camino, i) {
  l <- length(camino$d) - 1
  n <- camino$w[l + 1]
  pz <- camino$z[i + 1]
  po <- camino$o[i + 1]
  total <- 0
  if (l > 0) {
    for (j in (l - 1):0) {
      if (po != 0) {
        w <- n * (l + 1) / ((j + 1) * po)
        total <- total + w
        n <- camino$w[j + 1] - w * pz * (l - j) / (l + 1)
      } else {
        total <- total + camino$w[j + 1] * (l + 1) / (pz * (l - j))
      }
    }
  }
  total
}

treeshap_arbol <- function(arbol, x, estricto, n_variables) {
  phi <- numeric(n_variables)
  recorrer <- function(j, camino, pz, po, pi) {
    camino <- extender(camino, pz, po, pi)
    if (arbol$hoja[j]) {
      largo <- length(camino$d)
      if (largo > 1) {
        for (i in 1:(largo - 1)) {
          w <- peso_deshecho(camino, i)
          d <- camino$d[i + 1]
          phi[d] <<- phi[d] + w * (camino$o[i + 1] - camino$z[i + 1]) * arbol$valor[j]
        }
      }
      return(invisible(NULL))
    }
    d <- arbol$var[j]
    if (va_izquierda(x[d], arbol$umbral[j], estricto)) {
      caliente <- arbol$izq[j]
      frio <- arbol$der[j]
    } else {
      caliente <- arbol$der[j]
      frio <- arbol$izq[j]
    }
    iz <- 1
    io <- 1
    # ¿La variable ya está en el camino? (se ignora el elemento raíz ficticio)
    k <- if (length(camino$d) > 1) which(camino$d[-1] == d)[1] else NA
    if (!is.na(k)) {
      iz <- camino$z[k + 1]
      io <- camino$o[k + 1]
      camino <- deshacer(camino, k)
    }
    recorrer(caliente, camino, iz * arbol$cobertura[caliente] / arbol$cobertura[j], io, d)
    recorrer(frio, camino, iz * arbol$cobertura[frio] / arbol$cobertura[j], 0, d)
  }
  recorrer(1L, list(d = integer(0), z = numeric(0), o = numeric(0), w = numeric(0)), 1, 1, 0L)
  phi
}

.cache_explicaciones <- new.env(parent = emptyenv())

explicar_vector <- function(clave, x) {
  llave <- paste(clave, paste(sprintf("%.10g", x), collapse = ","))
  if (exists(llave, envir = .cache_explicaciones, inherits = FALSE)) {
    return(get(llave, envir = .cache_explicaciones))
  }
  m <- MODELOS$modelos[[clave]]
  if (m$tipo == "lineal") {
    base <- m$intercepto + sum(m$coeficientes * m$media_fondo)
    phi <- m$coeficientes * (x - m$media_fondo)
  } else {
    xp <- preparar_x(m, x)
    estricto <- m$comparacion == "<"
    phi <- numeric(length(x))
    esperado <- 0
    for (arbol in m$arboles) {
      phi <- phi + treeshap_arbol(arbol, xp, estricto, length(x))
      esperado <- esperado + arbol$esperado
    }
    if (m$agregacion == "media") {
      phi <- phi / length(m$arboles)
      esperado <- esperado / length(m$arboles)
    }
    base <- m$base + esperado
  }
  resultado <- list(base = base, phi = phi)
  if (length(ls(.cache_explicaciones)) > 512) rm(list = ls(.cache_explicaciones), envir = .cache_explicaciones)
  assign(llave, resultado, envir = .cache_explicaciones)
  resultado
}

# Contribución de cada variable a la predicción de un modelo. Base + suma de
# contribuciones = predicción del modelo en su escala ("log-odds" o "probabilidad").
explicar <- function(paciente, clave) {
  r <- explicar_vector(clave, vector_paciente(paciente))
  list(base = r$base, contribuciones = stats::setNames(r$phi, MODELOS$predictoras),
       escala = MODELOS$modelos[[clave]]$salida)
}

# --------------------------------------------------------------------------- #
# Dentro de cada modelo: lo que dice cada árbol y el camino de un paciente
# (mismas funciones que utils/modelo.py del Dash). Los índices de árbol que
# reciben y devuelven estas funciones empiezan en 1.
# --------------------------------------------------------------------------- #

# Probabilidad de GBM que da cada árbol del Random Forest a este paciente. El
# bosque no cuenta votos: su predicción es la media de estas probabilidades.
probas_arboles <- function(paciente, clave = "random_forest") {
  m <- MODELOS$modelos[[clave]]
  xp <- preparar_x(m, vector_paciente(paciente))
  estricto <- m$comparacion == "<"
  vapply(m$arboles, function(a) a$valor[hoja_de(a, xp, estricto)], numeric(1))
}

# El árbol cuya respuesta más se parece a la del bosque
arbol_representativo <- function(paciente, clave = "random_forest") {
  p <- probas_arboles(paciente, clave)
  which.min(abs(p - mean(p)))
}

# Deshace la estandarización de la edad para mostrar el corte en años
umbral_en_unidades <- function(variable, umbral) {
  if (variable != "Age_at_diagnosis") return(umbral)
  umbral * MODELOS$escala_edad + MODELOS$media_edad
}

# Nodos por los que pasa el paciente en un árbol, de la raíz a la hoja
camino_paciente <- function(paciente, indice, clave = "random_forest") {
  m <- MODELOS$modelos[[clave]]
  a <- m$arboles[[indice]]
  xp <- preparar_x(m, vector_paciente(paciente))
  estricto <- m$comparacion == "<"
  n <- 1L
  nodos <- n
  pasos <- list()
  while (!a$hoja[n]) {
    variable <- MODELOS$predictoras[a$var[n]]
    izquierda <- va_izquierda(xp[a$var[n]], a$umbral[n], estricto)
    pasos[[length(pasos) + 1]] <- list(nodo = n, variable = variable,
                                       umbral = umbral_en_unidades(variable, a$umbral[n]),
                                       izquierda = izquierda)
    n <- if (izquierda) a$izq[n] else a$der[n]
    nodos <- c(nodos, n)
  }
  list(nodos = nodos, pasos = pasos, hoja = n,
       proba = a$valor[n],            # en XGBoost: aporte en log-odds
       cobertura = a$cobertura[n],
       menor = estricto,              # TRUE: el corte es "<", no "<="
       escala = m$salida)
}

# XGBoost no promedia: arranca en el valor base y cada árbol suma el valor de
# la hoja a la que llega el paciente (en log-odds)
aportes_arboles <- function(paciente, clave = "xgboost") {
  m <- MODELOS$modelos[[clave]]
  xp <- preparar_x(m, vector_paciente(paciente))
  estricto <- m$comparacion == "<"
  list(base = m$base,
       aportes = vapply(m$arboles, function(a) a$valor[hoja_de(a, xp, estricto)], numeric(1)))
}

# El árbol de XGBoost que más mueve la predicción de este paciente
arbol_mas_influyente <- function(paciente, clave = "xgboost") {
  which.max(abs(aportes_arboles(paciente, clave)$aportes))
}

# Suma de efectos de la regresión logística: intercepto + coeficientes · x
logodds_logistica <- function(paciente) {
  m <- MODELOS$modelos$logistica
  sum(vector_paciente(paciente) * m$coeficientes) + m$intercepto
}

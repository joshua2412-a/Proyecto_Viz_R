# =========================================================================== #
# Glía, la asistente de la app · lógica (sin interfaz)
#
# Versión en R de utils/asistente.py del tablero de Dash. Responde en dos modos
# y elige solo:
#
# 1. Respuestas guiadas. Preguntas frecuentes, un glosario y la lectura de la
#    predicción, escritas aquí y con las cifras calculadas desde los mismos
#    datos que pintan las pestañas (asociacion_con_grado y el resumen que
#    exporta el libro de R). Funcionan siempre, sin red y sin coste.
# 2. Gemini, si el servidor tiene la clave GEMINI_API_KEY (la gratuita de
#    Google AI Studio). En shinyapps.io va en un archivo .Renviron junto a la
#    app, que git ignora: nunca en el código. El texto libre lo responde
#    Gemini con todo lo anterior como contexto. Si la llamada falla (sin red,
#    sin cuota), se cae a la respuesta guiada. En el plan gratuito Google
#    puede usar lo que se le envía para mejorar sus productos (aquí son cifras
#    públicas y las preguntas).
#
# Nunca pasan por la IA: las preguntas sugeridas (fijas a propósito), las
# órdenes de navegación («llévame a Modelos») y la lectura de la predicción,
# cuyas cifras salen de los propios modelos.
#
# La interfaz y la conversación están en modulos/asistente_panel.R.
# =========================================================================== #

# Modelos de Gemini, en orden. Se puede fijar uno con ASISTENTE_MODELO_GEMINI.
# El cupo gratuito es por modelo y por proyecto: si uno responde «sin cupo»
# (429) o «no existe» (404), se prueba el siguiente.
MODELOS_GEMINI <- if (nzchar(Sys.getenv("ASISTENTE_MODELO_GEMINI"))) {
  Sys.getenv("ASISTENTE_MODELO_GEMINI")
} else {
  c("gemini-3.5-flash-lite", "gemini-3.1-flash-lite", "gemini-3.6-flash")
}
MAX_PREGUNTA <- 500       # caracteres: una pregunta, no un documento
MAX_TURNOS_IA <- 8        # mensajes previos que se le pasan a la IA
# Gemini 3 piensa antes de responder y ese pensamiento cuenta dentro del
# máximo de salida: con 700 a veces se quedaba sin sitio para la respuesta.
MAX_TOKENS_GEMINI <- 2048

# --------------------------------------------------------------------------- #
# Utilidades de texto
# --------------------------------------------------------------------------- #
# Minúsculas, sin tildes ni signos: «¿Qué es IDH1?» -> «que es idh1»
normalizar <- function(texto) {
  t <- tolower(texto %||% "")
  t <- chartr("áéíóúüàèìòù", "aeiouuaeiou", t)
  t <- gsub("[^a-z0-9ñ ]+", " ", t)
  trimws(gsub(" +", " ", t))
}

# La clave aparece al principio de una palabra («limitac» casa con
# «limitaciones», pero «pten» no casa dentro de otra palabra)
contiene <- function(texto_norm, clave) {
  grepl(paste0("(^|[^a-z0-9])", gsub("([][{}()+*^$|\\\\?.])", "\\\\\\1", clave)), texto_norm, perl = TRUE)
}

# Una respuesta de Glía. `tono` («lgg», «gbm» o NULL) y `fuerza` (0-1) la tiñen:
# su núcleo toma el color del grado del que habla (www/glia.js).
respuesta_glia <- function(texto, ir = NULL, fuente = "guiada", navegar = FALSE, tono = NULL,
                           fuerza = 0) {
  list(texto = trimws(texto), ir = ir, fuente = fuente, navegar = navegar, tono = tono,
       fuerza = round(as.numeric(fuerza), 2))
}

# --------------------------------------------------------------------------- #
# Cifras (las mismas que muestran las pestañas), calculadas una vez
# --------------------------------------------------------------------------- #
.glia <- new.env()

hechos_glia <- function() {
  if (!is.null(.glia$hechos)) return(.glia$hechos)
  tabla <- asociacion_con_grado("train")
  k <- vec(RESUMEN$panel$k)
  auc <- vec(RESUMEN$panel$auc)
  con <- tabla[tabla$significativa, ]
  .glia$hechos <- list(
    meta = RESUMEN$meta,
    auc = vapply(ORDEN_MODELOS, function(m) RESUMEN$modelos[[m]]$auc_test, numeric(1)),
    ic = lapply(stats::setNames(ORDEN_MODELOS, ORDEN_MODELOS), function(m) vec(RESUMEN$modelos[[m]]$auc_test_ic)),
    delong_p_min = min(vapply(RESUMEN$delong, function(d) d$p, numeric(1))),
    panel4 = auc[k == 4],
    panel23 = auc[k == max(k)],
    panel_k = K_PANEL,
    panel_auc = auc[k == K_PANEL],
    importancia = RESUMEN$importancia,
    con_senal = stats::setNames(con$rho, con$variable),
    sin_senal = tabla$variable[!tabla$significativa],
    rho = stats::setNames(tabla$rho, tabla$variable),
    p = stats::setNames(tabla$p_valor, tabla$variable)
  )
  .glia$hechos
}

# Hacia qué grado empuja una variable en la muestra, y con qué fuerza
tono_gen <- function(variable) {
  h <- hechos_glia()
  rho <- h$rho[variable]; p <- h$p[variable]
  if (is.na(rho) || is.na(p) || p >= 0.05 || abs(rho) < 0.10) return(list(tono = NULL, fuerza = 0))
  list(tono = if (rho < 0) "lgg" else "gbm", fuerza = min(1, abs(rho) / 0.7))
}

nombre_glia <- function(variable) {
  if (variable == "Race_binary") return("grupo racial")
  tryCatch(nombre_variable(variable), error = function(e) variable)
}

# En LaTeX, como el resto de la app (MathJax compone los globos)
rho_tex <- function(v) paste0("$r = ", tnum(v, 2, signo = TRUE), "$")
UMBRAL_RHO <- "$|r| \\geq 0{,}10$"
auc_txt <- function(v) num(v, 3)

# El panel reducido en palabras: «cinco variables (IDH1, edad, TP53, PTEN e IDH2)»
panel_glia <- function() {
  paste0(numero_texto(K_PANEL), " variables (", lista_y_e(nombres_panel()), ")")
}
# lista_y con «e» delante de una palabra que empieza por i («PTEN e IDH2»)
lista_y_e <- function(v) {
  v <- as.character(v)
  if (length(v) < 2) return(paste(v, collapse = ""))
  ultimo <- v[length(v)]
  y <- if (grepl("^[iI]", ultimo) && !grepl("^[iI][aeouAEOU]", ultimo)) " e " else " y "
  paste0(paste(v[-length(v)], collapse = ", "), y, ultimo)
}

# --------------------------------------------------------------------------- #
# Preguntas frecuentes
#   id: lo usan las sugerencias de cada pestaña.
#   claves: comienzos de palabra (ya normalizados) que delatan la pregunta en
#           texto libre. Cuantas más coinciden, mejor la candidata.
#   texto: función que recibe hechos_glia() y escribe la respuesta en Markdown.
# --------------------------------------------------------------------------- #
r_proyecto <- function(h) paste0(
  "Esta aplicación estudia si un **subconjunto pequeño de variables** basta para distinguir un ",
  "glioma de bajo grado (**LGG**) de un glioblastoma (**GBM**). Usa ",
  h$meta$n_train + h$meta$n_test, " pacientes de TCGA con su edad, género, grupo racial y 20 ",
  "mutaciones.\n\n",
  "Se recorre en cuatro etapas: **el contexto** (por qué importa), **el método** (variables y ",
  "pruebas), **los resultados** (análisis, modelos y un simulador de predicción) y **el cierre** ",
  "(limitaciones y conclusiones).")

r_lgg_gbm <- function(h) paste0(
  "Los dos son gliomas, tumores que nacen de las células gliales del sistema nervioso central, ",
  "pero se comportan muy distinto:\n\n",
  "- **LGG (glioma de bajo grado):** crecimiento lento y supervivencia larga. Aparece más en ",
  "adultos jóvenes y suele portar la mutación de **IDH1**.\n",
  "- **GBM (glioblastoma):** el glioma más agresivo, de crecimiento rápido y pronóstico corto. Se ",
  "concentra en edades avanzadas y presenta con más frecuencia alteraciones en **PTEN** y **EGFR**.\n\n",
  "La clasificación actual de la OMS combina la histología con el perfil molecular, que es la ",
  "premisa de este proyecto.")

r_datos <- function(h) {
  m <- h$meta
  paste0(
    "El dataset es *Glioma Grading Clinical and Mutation Features* (UCI), con **",
    m$n_train + m$n_test, " pacientes** de TCGA-LGG y TCGA-GBM: 487 LGG y 352 GBM.\n\n",
    "Tiene **23 predictoras**: edad al diagnóstico, género, grupo racial y el estado (mutado o no) ",
    "de 20 genes. No hay valores faltantes.\n\n",
    "Se partió 80/20 de forma estratificada: **", m$n_train, " de entrenamiento** y **", m$n_test,
    " de prueba** (", m$gbm_test, " GBM en prueba).\n\n",
    "Fuente: [UCI Machine Learning Repository](", URL_DATASET, ").")
}

r_particion <- function(h) paste0(
  "Se reservó el 20 % de los pacientes (", h$meta$n_test, ") **antes de calcular el primer ",
  "estadístico**. Así ninguna decisión del análisis exploratorio ni del modelado se contamina con ",
  "los datos con los que después se evalúan los modelos.\n\n",
  "La partición es estratificada (mantiene la proporción LGG/GBM) y es la misma del libro en R, ",
  "que lee los mismos train_data.csv y test_data.csv.")

r_asociadas <- function(h) {
  top <- utils::head(h$con_senal, 6)
  lineas <- paste0("- **", vapply(names(top), nombre_glia, character(1)), "** (",
                   vapply(top, rho_tex, character(1)), ", hacia ",
                   ifelse(top > 0, "GBM", "LGG"), ")", collapse = "\n")
  paste0(
    "Ordenadas por la fuerza de su asociación con el grado (Spearman con el grado, sobre ",
    "entrenamiento):\n\n", lineas, "\n\n",
    "**IDH1** es el caso extremo: su mutación es casi una firma de LGG. La **edad** es el principal ",
    "correlato clínico. En total, ", length(h$con_senal), " de 23 variables son significativas *y* ",
    "tienen ", UMBRAL_RHO, ".")
}

r_no_aportan <- function(h) paste0(
  "Estas ", length(h$sin_senal), " variables no muestran una asociación apreciable con el grado ",
  "($p \\geq 0{,}05$ o $|r| < 0{,}10$):\n\n",
  lista_y(vapply(h$sin_senal, nombre_glia, character(1))), ".\n\n",
  "Que no discriminen el grado **no** las hace irrelevantes en oncología: pueden importar para el ",
  "pronóstico o el tratamiento, que este análisis no mide.")

r_pruebas <- function(h) paste0(
  "Cada variable se contrasta con la prueba que corresponde a su naturaleza ($\\alpha = 0{,}05$):\n\n",
  "- **Edad:** U de Mann-Whitney, porque Shapiro-Wilk rechaza la normalidad (es bimodal).\n",
  "- **Género, grupo racial y mutaciones:** chi-cuadrado (o Fisher exacto si hay frecuencias ",
  "esperadas < 5), con la **V de Cramér** como tamaño del efecto.\n",
  "- **Spearman** aporta el signo: si la variable empuja hacia LGG o hacia GBM.\n",
  "- **Benjamini-Hochberg** corrige por contrastes múltiples (q-valor).\n",
  "- **V de Cramér entre genes y VIF** descartan la multicolinealidad.\n\n",
  "Una variable tiene señal si es significativa **y** ", UMBRAL_RHO, ": con muestras de este ",
  "tamaño la significancia sola no basta.")

r_modelos <- function(h) {
  filas <- vapply(ORDEN_MODELOS, function(m) {
    paste0("- **", NOMBRES_MODELO[[m]], ":** AUC ", auc_txt(h$auc[[m]]), " (IC 95 %: ",
           auc_txt(h$ic[[m]][1]), "–", auc_txt(h$ic[[m]][2]), ")")
  }, character(1))
  paste0(
    "Se entrenaron tres clasificadores y se evaluaron en los ", h$meta$n_test,
    " pacientes de prueba:\n\n", paste(filas, collapse = "\n"), "\n\n",
    "La prueba de DeLong **no separa** ningún par (p ≥ ", num(h$delong_p_min, 2), "): con estos ",
    "datos los tres son equivalentes. Por eso la elección puede hacerse por interpretabilidad, y la ",
    "regresión logística no pierde nada.")
}

r_entrenamiento <- function(h) {
  m <- h$meta
  paste0(
    "No se entrenó un modelo, sino **", m$combinaciones, " combinaciones**: 3 modelos (logística, ",
    "Random Forest y XGBoost) × 4 balanceos (ninguno, pesos de clase, SMOTE-NC y ADASYN) × 4 ",
    "optimizadores (Grid, Random, Bayesiano y Genético).\n\n",
    "Cada una se evaluó con **validación cruzada anidada** (", m$k_externo, " pliegues externos × ",
    m$k_interno, " internos): ", miles(m$configuraciones), " configuraciones en unos ",
    num(m$minutos, 0), " minutos. El preprocesamiento y el balanceo van siempre dentro del pipeline, ",
    "así que ningún paciente de validación influye en el entrenamiento.\n\n",
    "La mejor de cada familia se reajustó con los ", m$n_train, " de entrenamiento y se aplicó **una ",
    "sola vez** al conjunto de prueba.")
}

r_panel <- function(h) paste0(
  "Sí, en estos datos. Con la regla de un error estándar, el panel más pequeño que no se distingue ",
  "del completo tiene **", panel_glia(), "**: AUC de **", auc_txt(h$panel_auc), "** en validación ",
  "cruzada, frente a **", auc_txt(h$panel23), "** con las 23.\n\n",
  "Es la hipótesis central del proyecto: la mayor parte del panel no añade capacidad para decidir el ",
  "grado. Ojo: se midió en pacientes a los que se secuenció todo; convertirlo en un ahorro real ",
  "exige un estudio prospectivo.")

r_importancia <- function(h) {
  top <- utils::head(h$importancia, 5)
  lineas <- vapply(top, function(f) {
    paste0("- **", nombre_glia(f$variable), ":** ", pct(f$xgboost, 1), " en XGBoost · ",
           pct(f$logistica, 1), " en la logística")
  }, character(1))
  paste0(
    "Importancia medida con valores **SHAP** (porcentaje del total de |SHAP| medio):\n\n",
    paste(lineas, collapse = "\n"), "\n\n",
    "Un modelo lineal y uno de árboles, con supuestos opuestos, coinciden en el orden: el hallazgo ",
    "no es un artefacto de uno de ellos.")
}

r_usar_prediccion <- function(h) paste0(
  "En **Predicción** describes un paciente y ves qué le predicen los tres modelos:\n\n",
  "1. Elige un perfil de ejemplo o un paciente de prueba (o arma uno tú).\n",
  "2. Ajusta la edad, el género, el grupo racial y marca los genes mutados.\n",
  "3. Pulsa **Predecir**: verás la probabilidad de GBM de cada modelo, el veredicto y qué ",
  "variables empujan hacia cada grado.\n",
  "4. Cambia de modelo arriba para ver *cómo decide* cada uno.\n\n",
  "Después pregúntame **«Interpreta mi predicción»** y te la explico.")

r_limitaciones <- function(h) paste0(
  "Las principales:\n\n",
  "- **Una sola fuente:** todos los pacientes son de TCGA (centros sobre todo de EE. UU.) y no hay ",
  "validación externa.\n",
  "- **Sesgo de composición:** más de nueve de cada diez pacientes son del grupo White; la ",
  "asociación con el grupo racial no debe leerse como efecto biológico.\n",
  "- **Asociación no es causalidad.**\n",
  "- **Solo el grado histológico:** nada sobre supervivencia ni respuesta al tratamiento.\n",
  "- **Mutaciones como 0/1:** no se distingue el tipo de variante.\n",
  "- **El panel reducido** se midió con datos donde se secuenció todo.\n\n",
  "La pestaña Predicción es para entender los modelos, **no** un apoyo al diagnóstico.")

r_conclusiones <- function(h) paste0(
  "Un puñado de marcadores concentra casi toda la información sobre el grado:\n\n",
  "- Solo ", length(h$con_senal), " de 23 variables muestran una asociación significativa y ",
  "apreciable; **IDH1** y la **edad** encabezan la lista.\n",
  "- Los tres modelos separan los grados con AUC de ", auc_txt(min(h$auc)), " a ",
  auc_txt(max(h$auc)), " en pacientes nuevos, sin diferencias significativas.\n",
  "- Con ", panel_glia(), " el AUC es ", auc_txt(h$panel_auc), " frente a ", auc_txt(h$panel23),
  " con las 23: la diferencia no se distingue de la variación entre pliegues.\n",
  "- La señal coincide con la literatura clínica: IDH1, IDH2, ATRX, CIC y TP53 hacia LGG; edad, ",
  "PTEN y EGFR hacia GBM.")

r_documentacion <- function(h) paste0(
  "- **Libro en R (bookdown)** con el análisis y el modelado completos: [abrir el libro](", URL_LIBRO, ")\n",
  "- **Repositorio** con el código del libro y de esta app: [GitHub](", URL_REPO, ")\n\n",
  "La pestaña **Documentación** explica cómo ejecutar la app y compilar el libro.")

r_autores <- function(h) paste0(
  "El proyecto es de **", lista_y(vapply(AUTORES, `[[`, character(1), "nombre")), "**, para el ",
  "curso de Visualización de datos.")

r_diagnostico <- function(h) paste0(
  "**No.** Es una herramienta docente: muestra cómo razonan unos modelos entrenados con ",
  h$meta$n_train, " pacientes de una sola cohorte (TCGA), sin validación externa. El diagnóstico de ",
  "un glioma lo establece un equipo médico con histología, imagen y pruebas moleculares.")

r_navegar <- function(h) paste0(
  "- El **índice** (arriba a la izquierda) muestra las 13 pestañas agrupadas por etapa y dónde estás.\n",
  "- **Anterior / siguiente** avanzan en orden; los **peldaños** de la doble hélice de la cabecera ",
  "también son clicables.\n",
  "- El botón de la **luna / sol** cambia entre modo claro y oscuro.\n",
  "- Y puedes pedirme: *«llévame a Modelos»*.")

FAQ_GLIA <- list(
  list(id = "proyecto", pregunta = "¿De qué trata esta aplicación?",
       claves = c("trata", "proyecto", "dashboard", "tablero", "aplicacion", "objetivo general",
                  "para que sirve", "resumen"),
       texto = r_proyecto, ir = "tab-introduccion"),
  list(id = "lgg_gbm", pregunta = "¿Qué diferencia hay entre LGG y GBM?",
       claves = c("diferencia", "lgg", "gbm", "glioblastoma", "bajo grado", "glioma", "grado"),
       texto = r_lgg_gbm, ir = "tab-contexto"),
  list(id = "datos", pregunta = "¿Qué datos se usaron?",
       claves = c("datos", "dataset", "pacientes", "muestra", "tcga", "cohorte", "cuantos"),
       texto = r_datos, ir = "tab-metodologia"),
  list(id = "particion", pregunta = "¿Por qué se separaron entrenamiento y prueba?",
       claves = c("particion", "entrenamiento", "prueba", "test", "separ", "reserv", "80"),
       texto = r_particion, ir = "tab-metodologia"),
  list(id = "asociadas", pregunta = "¿Qué variables se asocian más con el grado?",
       claves = c("asoci", "importan", "relacion", "influy", "variable", "marcador", "señal",
                  "senal", "correl", "spearman"),
       texto = r_asociadas, ir = "tab-resultados"),
  list(id = "no_aportan", pregunta = "¿Qué variables no aportan?",
       claves = c("no aport", "no sirv", "irrelev", "sin asoci", "sin señal", "descart", "no influy"),
       texto = r_no_aportan, ir = "tab-problema"),
  list(id = "pruebas", pregunta = "¿Qué pruebas estadísticas se usaron?",
       claves = c("prueba", "estadistic", "contraste", "test", "chi", "mann", "metodo", "hipotesis"),
       texto = r_pruebas, ir = "tab-metodologia"),
  list(id = "modelos", pregunta = "¿Qué modelo funciona mejor?",
       claves = c("modelo", "mejor", "auc", "rinde", "rendimiento", "precision", "logistic", "random",
                  "xgboost", "compar"),
       texto = r_modelos, ir = "tab-modelos"),
  list(id = "entrenamiento", pregunta = "¿Cómo se entrenaron los modelos?",
       claves = c("entren", "validacion", "cruzada", "anidada", "hiperpar", "optimiz", "balance",
                  "smote", "combinacion"),
       texto = r_entrenamiento, ir = "tab-modelos"),
  list(id = "panel", pregunta = "¿Bastan pocas variables para decidir el grado?",
       claves = c("panel", "pocas", "bastan", "cinco", "cuatro", "reduc", "ahorr", "coste",
                  "costo", "minim"),
       texto = r_panel, ir = "tab-modelos"),
  list(id = "importancia", pregunta = "¿Qué variables pesan más en los modelos?",
       claves = c("pesan", "peso", "shap", "importancia", "contribu"),
       texto = r_importancia, ir = "tab-modelos"),
  list(id = "usar_prediccion", pregunta = "¿Cómo uso la pestaña Predicción?",
       claves = c("como uso", "usar", "simulador", "predecir", "formulario", "probar"),
       texto = r_usar_prediccion, ir = "tab-prediccion"),
  list(id = "limitaciones", pregunta = "¿Cuáles son las limitaciones?",
       claves = c("limitac", "sesgo", "debilidad", "confiable", "fiable", "generaliz", "externa"),
       texto = r_limitaciones, ir = "tab-limitaciones"),
  list(id = "conclusiones", pregunta = "¿Cuáles son las conclusiones?",
       claves = c("conclus", "hallazgo", "result", "aprend", "concluy"),
       texto = r_conclusiones, ir = "tab-conclusiones"),
  list(id = "documentacion", pregunta = "¿Dónde están el código y el libro?",
       claves = c("codigo", "bookdown", "libro", "github", "repositorio", "rmd", "reproduc", "documentac"),
       texto = r_documentacion, ir = "tab-documentacion"),
  list(id = "autores", pregunta = "¿Quién hizo el proyecto?",
       claves = c("autor", "quien", "hizo", "creador", "equipo"),
       texto = r_autores, ir = NULL),
  list(id = "diagnostico", pregunta = "¿Puedo usarlo para diagnosticar?",
       claves = c("diagnost", "medico", "clinic", "usar en", "paciente real", "tratamiento"),
       texto = r_diagnostico, ir = "tab-limitaciones"),
  list(id = "navegar", pregunta = "¿Cómo me muevo por la aplicación?",
       claves = c("naveg", "mover", "muevo", "indice", "pestañas", "pestanas", "modo oscuro", "ayuda"),
       texto = r_navegar, ir = NULL)
)
names(FAQ_GLIA) <- vapply(FAQ_GLIA, `[[`, character(1), "id")

# --------------------------------------------------------------------------- #
# Glosario: alias (formas normalizadas), definición y pestaña
# --------------------------------------------------------------------------- #
GLOSARIO <- list(
  "AUC" = list(c("auc", "area bajo la curva", "roc"), paste(
    "Área bajo la curva ROC. Mide si el modelo **ordena bien** a los pacientes por riesgo de GBM:",
    "0,5 es azar y 1 es perfecto. No depende del umbral ni se distorsiona con el desbalance, por eso",
    "es el criterio principal del proyecto."), "tab-modelos"),
  "Spearman" = list(c("spearman", "rho", "correlacion de spearman"), paste(
    "Correlación de rangos entre −1 y 1. Aquí aporta el **signo**: negativo, la variable empuja hacia",
    "LGG; positivo, hacia GBM. No exige linealidad ni normalidad. El umbral de relevancia del",
    "proyecto es", paste0(UMBRAL_RHO, ".")), "tab-marco"),
  "V de Cramér" = list(c("cramer", "v de cramer"), paste(
    "Tamaño del efecto del chi-cuadrado, entre 0 y 1: el chi-cuadrado dice **si** hay asociación y",
    "la V dice **cuánta**. Se usa contra el grado y entre pares de genes (redundancia si supera 0,70)."),
    "tab-marco"),
  "U de Mann-Whitney" = list(c("mann", "whitney", "u de mann"), paste(
    "Prueba no paramétrica que compara las distribuciones completas de dos grupos. Se usa para la",
    "edad porque no sigue una distribución normal (es bimodal)."), "tab-marco"),
  "Chi-cuadrado" = list(c("chi cuadrado", "chi2", "chi"), paste(
    "Contrasta si la distribución de una variable categórica cambia entre LGG y GBM. Si alguna",
    "frecuencia esperada de una tabla 2×2 baja de 5, se usa la prueba exacta de Fisher."), "tab-marco"),
  "p-valor" = list(c("p valor", "pvalor", "p value", "significancia", "significativ"), paste(
    "Probabilidad de ver una asociación al menos tan fuerte si en realidad no la hubiera. Por debajo",
    "de $\\alpha = 0{,}05$ se considera significativa, pero con muestras grandes conviene mirar",
    "también el tamaño del efecto."), "tab-marco"),
  "q-valor (Benjamini-Hochberg)" = list(c("q valor", "benjamini", "hochberg", "falsos descubrimientos", "fdr"),
    paste("p-valor corregido por hacer muchos contrastes a la vez: controla la proporción de falsos",
          "descubrimientos. En este proyecto ninguna asociación del panel se cae al corregir."),
    "tab-resultados"),
  "VIF" = list(c("vif", "inflacion de la varianza", "multicolinealidad", "colinealidad"), paste(
    "Factor de inflación de la varianza: mide cuánto se explica una predictora con todas las demás.",
    "Por encima de 5 inestabilizaría un modelo; aquí el más alto es el de IDH1, cerca de 3,1."),
    "tab-resultados"),
  "SHAP" = list(c("shap", "valores shap", "contribucion"), paste(
    "Reparte la predicción de un modelo entre las variables: cuánto empuja cada una hacia GBM o hacia",
    "LGG respecto a la predicción media. Sumadas al valor base dan exactamente la predicción."),
    "tab-prediccion"),
  "Validación cruzada anidada" = list(c("validacion cruzada", "anidada", "nested", "pliegue"), paste(
    "Dos bucles: el interno elige los hiperparámetros y el externo mide el rendimiento con datos que",
    "esa elección no vio. Evita sobreestimar el desempeño."), "tab-modelos"),
  "DeLong" = list(c("delong"), paste(
    "Prueba que compara las AUC de dos modelos evaluados sobre los mismos pacientes. Aquí no",
    "encuentra diferencias entre los tres modelos."), "tab-modelos"),
  "SMOTE / ADASYN" = list(c("smote", "adasyn", "balanceo", "desbalance", "pesos de clase", "class weight"),
    paste("Técnicas de balanceo: crean pacientes sintéticos de la clase minoritaria (SMOTE-NC,",
          "ADASYN) o pesan más sus errores (pesos de clase). En este proyecto casi no mueven la AUC;",
          "cambian el recall."), "tab-modelos"),
  "Recall" = list(c("recall", "sensibilidad", "exhaustividad"), paste(
    "De los GBM reales, qué proporción detecta el modelo. Es la métrica clínica clave: un",
    "glioblastoma clasificado como LGG retrasa un tratamiento urgente."), "tab-modelos"),
  "Brier y calibración" = list(c("brier", "calibra", "ece"), paste(
    "La calibración mide si una probabilidad del 70 % acierta de verdad un 70 % de las veces. El",
    "Brier y el ECE la resumen: más bajo es mejor."), "tab-modelos"),
  "Umbral" = list(c("umbral", "punto de corte", "0 5"), paste(
    "Probabilidad a partir de la cual el modelo dice GBM. Se usa 0,5; fijarlo según el coste de cada",
    "error es una decisión clínica."), "tab-modelos"),
  "Random Forest" = list(c("random forest", "bosque"), paste(
    "Cientos de árboles de decisión entrenados con muestras distintas que votan: la predicción es la",
    "media de sus votos."), "tab-prediccion"),
  "XGBoost" = list(c("xgboost", "boosting", "gradient"), paste(
    "Árboles que se construyen uno tras otro, cada uno corrigiendo los errores de los anteriores;",
    "sus aportes se suman en log-odds."), "tab-prediccion"),
  "Regresión logística" = list(c("logistica", "regresion logistica"), paste(
    "Modelo lineal: suma las variables con un peso cada una y pasa el resultado por una curva en S",
    "para obtener una probabilidad. Es el más interpretable de los tres."), "tab-prediccion"),
  "TCGA" = list(c("tcga", "cancer genome atlas"), paste(
    "The Cancer Genome Atlas: proyecto público de EE. UU. que caracterizó molecularmente miles de",
    "tumores. De ahí vienen los 839 pacientes (TCGA-LGG y TCGA-GBM)."), "tab-introduccion")
)

# Los genes más largos primero, para que «PIK3R1» no se lea como otro
GENES_GLIA <- names(GENE_DESCRIPCION)[order(-nchar(names(GENE_DESCRIPCION)))]

definicion_gen <- function(gen) {
  h <- hechos_glia()
  rho <- h$rho[gen]; p <- h$p[gen]
  texto <- paste0("**", gen, ".** ", GENE_DESCRIPCION[[gen]])
  if (!is.na(rho)) {
    if (!is.na(p) && p < 0.05 && abs(rho) >= 0.10) {
      texto <- paste0(texto, "\n\nEn esta muestra, su mutación se asocia con **",
                      if (rho < 0) "LGG" else "GBM", "** (", rho_tex(rho), ", sobre entrenamiento).")
    } else {
      texto <- paste0(texto, "\n\nEn esta muestra **no** muestra una asociación apreciable con el ",
                      "grado (", rho_tex(rho), ").")
    }
  }
  texto
}

respuesta_gen <- function(gen) {
  tg <- tono_gen(gen)
  respuesta_glia(definicion_gen(gen), "tab-marco", tono = tg$tono, fuerza = tg$fuerza)
}

respuesta_termino <- function(termino) {
  g <- GLOSARIO[[termino]]
  respuesta_glia(paste0("**", termino, ".** ", g[[2]]), ir = g[[3]])
}

buscar_gen <- function(texto_norm) {
  for (gen in GENES_GLIA) {
    if (grepl(paste0("(^|[^a-z0-9])", tolower(gen), "($|[^a-z0-9])"), texto_norm, perl = TRUE)) return(gen)
  }
  NULL
}

buscar_termino <- function(texto_norm) {
  mejor <- NULL; largo <- 0
  for (termino in names(GLOSARIO)) {
    for (a in GLOSARIO[[termino]][[1]]) {
      if (contiene(texto_norm, a) && nchar(a) > largo) { mejor <- termino; largo <- nchar(a) }
    }
  }
  mejor
}

# --------------------------------------------------------------------------- #
# Navegación: «llévame a Modelos»
# --------------------------------------------------------------------------- #
ALIAS_PESTANA <- list(
  "tab-introduccion" = c("introduccion", "inicio", "portada"),
  "tab-contexto" = c("contexto", "clinico"),
  "tab-problema" = "problema",
  "tab-objetivos" = "objetivo",
  "tab-marco" = c("marco", "teori"),
  "tab-metodologia" = c("metodologia", "metodo"),
  "tab-exploracion" = c("exploracion", "explorar"),
  "tab-resultados" = "resultado",
  "tab-modelos" = "modelos",
  "tab-prediccion" = c("prediccion", "predecir", "simulador"),
  "tab-limitaciones" = "limitacion",
  "tab-conclusiones" = "conclusion",
  "tab-documentacion" = c("documentacion", "libro", "bookdown")
)
VERBOS_IR <- "(^|[^a-z])(llevame|lleva me|llevar|ir a|vamos a|ve a|abre|abrir|abreme|muestrame|ensename|quiero ver|navega)"

destino_glia <- function(texto_norm) {
  if (!grepl(VERBOS_IR, texto_norm, perl = TRUE)) return(NULL)
  for (tab in names(ALIAS_PESTANA)) {
    if (any(vapply(ALIAS_PESTANA[[tab]], function(a) contiene(texto_norm, a), logical(1)))) return(tab)
  }
  NULL
}

# --------------------------------------------------------------------------- #
# Lectura de la predicción
# --------------------------------------------------------------------------- #
INTENCION_PREDICCION <- paste0(
  "(interpret|explica\\w* (mi|la|esta|este|el)|mi (prediccion|perfil|paciente|resultado)",
  "|este (perfil|paciente|resultado)|esta prediccion|por que (el|los|la) model",
  "|por que (dice|sale|predice|da)|que significa (mi|este|esta|el resultado))")

gen_o_variable <- function(v, paciente) {
  if (v == "Age_at_diagnosis") return(paste0("la edad (", num(paciente$Age_at_diagnosis, 0), " años)"))
  if (v == "Gender") return("el género")
  if (v == "Race_binary") return("el grupo racial")
  paste(v, if (isTRUE(paciente[[v]] == 1)) "mutado" else "sin mutar")
}

# Explica en palabras la predicción que está en la pestaña Predicción. Las
# cifras salen de los mismos modelos y la misma explicación SHAP que la pestaña.
interpretar_prediccion <- function(estado) {
  if (is.null(estado) || is.null(estado$paciente)) {
    return(respuesta_glia(paste(
      "Todavía no has armado ningún perfil. Abre **Predicción**, elige un paciente o marca sus",
      "mutaciones, pulsa **Predecir** y vuelve a preguntarme."), ir = "tab-prediccion"))
  }
  paciente <- estado$paciente
  modelo <- if (isTRUE(estado$modelo %in% ORDEN_MODELOS)) estado$modelo else "logistica"
  probas <- unlist(predecir(paciente))
  exp <- explicar(paciente, modelo)

  mutados <- names(GENE_DESCRIPCION)[vapply(names(GENE_DESCRIPCION),
                                            function(g) isTRUE(paciente[[g]] == 1), logical(1))]
  perfil <- paste0("Paciente de **", num(paciente$Age_at_diagnosis, 0), " años** con ",
                   if (length(mutados)) paste0("**", lista_y(mutados), "** mutado",
                                               if (length(mutados) > 1) "s" else "", ".")
                   else "**ninguna mutación** del panel.")
  gbm <- ORDEN_MODELOS[probas[ORDEN_MODELOS] >= UMBRAL]
  veredicto <- if (length(gbm) == 3) "Los tres modelos apuntan a **glioblastoma (GBM)**." else
    if (length(gbm) == 0) "Los tres modelos apuntan a **glioma de bajo grado (LGG)**." else
      paste0("Los modelos **no coinciden**: ", lista_y(NOMBRES_MODELO[gbm]), " ",
             if (length(gbm) == 1) "dice" else "dicen", " GBM.")
  cifras <- paste(vapply(ORDEN_MODELOS, function(m) paste(NOMBRES_MODELO[[m]], pct(probas[[m]] * 100, 0)),
                         character(1)), collapse = " · ")
  minimo <- min(probas); maximo <- max(probas)
  firmeza <- if (maximo < 0.35 || minimo > 0.65) {
    "Es una predicción **firme**: las tres probabilidades están lejos del umbral de 50 %."
  } else if (minimo >= 0.35 && maximo <= 0.65) {
    paste("Está en **zona de incertidumbre**: las tres rondan el 50 %. Con estas variables no se",
          "puede clasificar con confianza.")
  } else "Alguna probabilidad está cerca del umbral, así que conviene mirar qué la empuja."

  c_ <- exp$contribuciones
  minimo_c <- if (exp$escala == "log-odds") 0.05 else 0.01
  hacia_gbm <- utils::head(names(sort(c_[c_ >= minimo_c], decreasing = TRUE)), 3)
  hacia_lgg <- utils::head(names(sort(c_[c_ <= -minimo_c])), 3)
  empujes <- c(
    if (length(hacia_gbm)) paste0("- Hacia **GBM**: ", lista_y(vapply(hacia_gbm, gen_o_variable, character(1),
                                                                     paciente = paciente)), "."),
    if (length(hacia_lgg)) paste0("- Hacia **LGG**: ", lista_y(vapply(hacia_lgg, gen_o_variable, character(1),
                                                                     paciente = paciente)), ".")
  )
  real <- ""
  cargado <- estado$cargado
  if (!is.null(cargado) && !is.null(cargado$grado) && mismo_paciente(cargado$paciente, paciente)) {
    grado <- cargado$grado
    aciertos <- ORDEN_MODELOS[as.integer(probas[ORDEN_MODELOS] >= UMBRAL) == grado]
    real <- paste0("\n\nEs el paciente ", cargado$indice, " de prueba y su diagnóstico real en TCGA es **",
                   if (grado == 1) "GBM" else "LGG", "**: ",
                   if (length(aciertos) == 3) "aciertan los tres modelos." else
                     if (length(aciertos) == 0) "no acierta ningún modelo." else
                       paste0("acierta", if (length(aciertos) > 1) "n" else "", " ",
                              lista_y(NOMBRES_MODELO[aciertos]), "."))
  }
  texto <- paste0(
    perfil, "\n\n", veredicto, " Probabilidad de GBM: ", cifras, ".\n\n", firmeza, "\n\n",
    "Según **", NOMBRES_MODELO[[modelo]], "** (el modelo elegido en la pestaña), lo que más pesa:\n\n",
    if (length(empujes)) paste(empujes, collapse = "\n") else "- Ninguna variable empuja de forma apreciable.",
    real, "\n\n*Es una lectura de los modelos, no un diagnóstico.*")
  p <- probas[[modelo]]
  respuesta_glia(texto, ir = "tab-prediccion", fuente = "prediccion",
                 tono = if (p >= UMBRAL) "gbm" else "lgg", fuerza = abs(p - 0.5) * 2)
}

# --------------------------------------------------------------------------- #
# Sugerencias por pestaña (ids de FAQ, «g:<término>» del glosario o «prediccion»)
# --------------------------------------------------------------------------- #
SUGERENCIAS_GLIA <- list(
  "tab-introduccion" = c("proyecto", "navegar", "datos"),
  "tab-contexto" = c("lgg_gbm", "g:IDH1", "datos"),
  "tab-problema" = c("no_aportan", "panel", "asociadas"),
  "tab-objetivos" = c("panel", "particion", "proyecto"),
  "tab-marco" = c("pruebas", "g:Spearman", "g:V de Cramér"),
  "tab-metodologia" = c("particion", "pruebas", "g:q-valor (Benjamini-Hochberg)"),
  "tab-exploracion" = c("asociadas", "g:p-valor", "g:U de Mann-Whitney"),
  "tab-resultados" = c("asociadas", "no_aportan", "g:VIF"),
  "tab-modelos" = c("modelos", "entrenamiento", "g:AUC"),
  "tab-prediccion" = c("prediccion", "usar_prediccion", "g:SHAP"),
  "tab-limitaciones" = c("limitaciones", "diagnostico", "datos"),
  "tab-conclusiones" = c("conclusiones", "panel", "importancia"),
  "tab-documentacion" = c("documentacion", "autores", "entrenamiento")
)

texto_sugerencia <- function(clave) {
  if (clave == "prediccion") return("Interpreta mi predicción")
  if (startsWith(clave, "g:")) return(paste0("¿Qué es ", substring(clave, 3), "?"))
  FAQ_GLIA[[clave]]$pregunta
}

sugerencias_glia <- function(pestana) {
  claves <- SUGERENCIAS_GLIA[[pestana %||% ""]] %||% SUGERENCIAS_GLIA[["tab-introduccion"]]
  stats::setNames(vapply(claves, texto_sugerencia, character(1)), claves)
}

SIN_RESPUESTA <- paste(
  "No tengo una respuesta preparada para eso. Puedo explicarte **los datos**, **las pruebas**,",
  "**los modelos**, **las conclusiones**, definir un término (*«¿qué es el AUC?»*), interpretar",
  "**tu predicción** o llevarte a una pestaña (*«llévame a Modelos»*).")

# Respuesta fija de una sugerencia: nunca pasa por la IA
responder_sugerencia <- function(clave, estado_pred) {
  if (clave == "prediccion") return(interpretar_prediccion(estado_pred))
  if (startsWith(clave, "g:")) {
    termino <- substring(clave, 3)
    if (termino %in% names(GLOSARIO)) return(respuesta_termino(termino))
    if (termino %in% names(GENE_DESCRIPCION)) return(respuesta_gen(termino))
  }
  faq <- FAQ_GLIA[[clave]]
  if (!is.null(faq)) return(respuesta_glia(faq$texto(hechos_glia()), ir = faq$ir))
  respuesta_glia(SIN_RESPUESTA)
}

# --------------------------------------------------------------------------- #
# Respuesta guiada a texto libre
# --------------------------------------------------------------------------- #
PIDE_DEFINICION <- "(que es|que son|que significa|significado|define|definicion|para que sirve|explica)"

respuesta_guiada <- function(pregunta, estado_pred = NULL, minimo = 1) {
  t <- normalizar(pregunta)
  if (!nzchar(t)) return(respuesta_glia(SIN_RESPUESTA))
  # Un término por el que se pregunta directamente («¿qué es la V de Cramér?»,
  # «IDH1») gana a las preguntas frecuentes, que son más amplias.
  corta <- length(strsplit(t, " ")[[1]]) <= 3
  gen <- buscar_gen(t)
  termino <- buscar_termino(t)
  if ((!is.null(gen) || !is.null(termino)) && (corta || grepl(PIDE_DEFINICION, t))) {
    if (!is.null(gen)) return(respuesta_gen(gen))
    return(respuesta_termino(termino))
  }
  # Una clave de varias palabras («no sirv») pesa más que una suelta
  # («variables»): es mucho más específica de su pregunta.
  puntos <- vapply(FAQ_GLIA, function(f) {
    sum(vapply(f$claves, function(cl) (1 + lengths(regmatches(cl, gregexpr(" ", cl)))) * contiene(t, cl),
               numeric(1)))
  }, numeric(1))
  if (max(puntos) >= minimo) {
    faq <- FAQ_GLIA[[which.max(puntos)]]
    return(respuesta_glia(faq$texto(hechos_glia()), ir = faq$ir))
  }
  if (!is.null(gen)) return(respuesta_gen(gen))
  if (!is.null(termino)) return(respuesta_termino(termino))
  respuesta_glia(SIN_RESPUESTA)
}

# --------------------------------------------------------------------------- #
# Gemini
# --------------------------------------------------------------------------- #
clave_gemini <- function() {
  k <- Sys.getenv("GEMINI_API_KEY")
  if (!nzchar(k)) k <- Sys.getenv("GOOGLE_API_KEY")
  if (nzchar(k)) k else NULL
}
# Se mira en cada pregunta: añadir la clave no exige tocar el código
ia_disponible <- function() !is.null(clave_gemini()) && requireNamespace("httr2", quietly = TRUE)

# Todo lo que la IA puede citar. Es fijo por proceso, así que se reutiliza
# entre preguntas (Gemini guarda en caché el principio común de las peticiones).
contexto_ia <- function() {
  if (!is.null(.glia$contexto)) return(.glia$contexto)
  h <- hechos_glia()
  pestanas <- paste0("- ", ORDEN, ": ", ETIQUETAS[ORDEN], " — ", DESCRIPCIONES[ORDEN], collapse = "\n")
  asociacion <- paste0("- ", vapply(names(h$rho), nombre_glia, character(1)), " (", names(h$rho), "): r = ",
                       vapply(h$rho, num, character(1), 3), ", p = ", formatC(h$p, format = "e", digits = 2),
                       ", ", ifelse(names(h$rho) %in% names(h$con_senal), "con señal", "sin señal apreciable"),
                       collapse = "\n")
  faq <- paste0(vapply(FAQ_GLIA, function(f) {
    paste0("### ", f$pregunta, "\nPestaña: ", f$ir %||% "ninguna", "\n", f$texto(h))
  }, character(1)), collapse = "\n\n")
  glosario <- paste0("- **", names(GLOSARIO), "**: ", vapply(GLOSARIO, `[[`, character(1), 2), collapse = "\n")
  genes <- paste0("- ", names(GENE_DESCRIPCION), ": ", GENE_DESCRIPCION, collapse = "\n")
  autores <- lista_y(vapply(AUTORES, `[[`, character(1), "nombre"))
  .glia$contexto <- paste0(
    "Eres Glía, la asistente virtual de una aplicación académica en Shiny (R) sobre gliomas: ",
    "«Caracterización Clínico-Molecular del Grado Tumoral en Gliomas» (LGG frente a GBM, datos TCGA), de ",
    autores, ". En la aplicación te dibujan como una célula glial (un astrocito) hecha de partículas, ",
    "porque los gliomas nacen de células como tú. Los modelos y las cifras de modelado son los del ",
    "libro en R (bookdown) del proyecto.\n\n",
    "Cómo responder:\n",
    "- Hablas en primera persona como Glía, con cercanía, pero sin exagerar el personaje: lo importante es la explicación, no la célula.\n",
    "- No saludes ni te presentes en cada respuesta: entra directo a la explicación.\n",
    "- Cuando expliques un gráfico, no describas detalles visuales (colores, rayas, formas, posición) que no aparezcan en su nota: no ves el gráfico, solo su título, su nota y las cifras de este contexto.\n",
    "- Siempre en español, con coma decimal (0,923) y en un tono claro y cercano.\n",
    "- Breve: normalmente 2 a 6 frases o una lista corta. Markdown sencillo (negritas y listas); nada de tablas ni encabezados.\n",
    "- La notación matemática va en LaTeX entre $...$, como en el resto de la aplicación (por ejemplo $r = -0{,}70$ o $|r| \\geq 0{,}10$); la correlación de Spearman se escribe r, nunca ρ.\n",
    "- No uses rayas (—) en el texto: usa comas o paréntesis.\n",
    "- Usa SOLO las cifras y hechos de este contexto. Si algo no está aquí, dilo con franqueza y sugiere la pestaña o el libro donde podría estar. No inventes cifras.\n",
    "- Solo hablas de esta aplicación y de lo que hace falta para entenderla (gliomas, estadística, modelos). Si la pregunta es ajena (cultura general, tareas, programación de otra cosa…), no la respondas: di en una frase que solo puedes ayudar con la aplicación y sugiere algo que sí puedes explicar.\n",
    "- No des consejo médico ni diagnósticos: es una herramienta docente. Si alguien describe un caso real, recomiéndale hablar con su equipo médico.\n",
    "- Si la respuesta está desarrollada en una pestaña concreta, termina con una única etiqueta [[ir:ID]] usando uno de estos ID. Si la pregunta coincide con una de las preguntas frecuentes, usa la pestaña indicada en su «Pestaña:». No escribas la etiqueta en ningún otro caso.\n\n",
    "Pestañas (ID: nombre — contenido):\n", pestanas, "\n\n",
    "Asociación de cada predictora con el grado (Spearman con el grado sobre los ", h$meta$n_train,
    " de entrenamiento; r negativo empuja hacia LGG, positivo hacia GBM; «con señal» = p < 0,05 y |r| ≥ 0,10):\n",
    asociacion, "\n\n",
    "Función de los genes del panel:\n", genes, "\n\n",
    "Glosario:\n", glosario, "\n\n",
    "Preguntas frecuentes con su respuesta de referencia:\n", faq, "\n\n",
    "Enlaces: libro en R ", URL_LIBRO, " · Repositorio ", URL_REPO, " · Dataset ", URL_DATASET, "\n")
  .glia$contexto
}

# Lo que cambia en cada pregunta: dónde está el usuario y su predicción
situacion_ia <- function(pestana, estado_pred, extra = "") {
  partes <- paste0("El usuario está en la pestaña: ", ETIQUETAS[pestana %||% TAB_INICIAL] %||% "Introducción", ".")
  if (nzchar(extra)) partes <- c(partes, extra)
  if (!is.null(estado_pred) && !is.null(estado_pred$paciente)) {
    partes <- c(partes, paste0("Lectura de la predicción que tiene ahora en la pestaña Predicción ",
                               "(cifras exactas de los modelos):\n", interpretar_prediccion(estado_pred)$texto))
  }
  paste(partes, collapse = "\n\n")
}

# La conversación como turnos alternos que empiezan por el usuario, lista para
# la API. Dos mensajes seguidos del mismo rol se unen y el saludo inicial no cuenta.
turnos_ia <- function(pregunta, historial) {
  turnos <- list()
  for (m in utils::tail(historial, MAX_TURNOS_IA)) {
    if (isTRUE(m$inicial) || !nzchar(m$texto %||% "")) next
    rol <- if (identical(m$rol, "usuario")) "user" else "model"
    n <- length(turnos)
    if (n && turnos[[n]]$role == rol) {
      turnos[[n]]$text <- paste0(turnos[[n]]$text, "\n\n", m$texto)
    } else {
      turnos[[n + 1]] <- list(role = rol, text = m$texto)
    }
  }
  while (length(turnos) && turnos[[1]]$role != "user") turnos[[1]] <- NULL
  n <- length(turnos)
  if (n && turnos[[n]]$role == "user") {
    turnos[[n]]$text <- paste0(turnos[[n]]$text, "\n\n", pregunta)
  } else {
    turnos[[n + 1]] <- list(role = "user", text = pregunta)
  }
  turnos
}

# Modelo de Gemini -> hasta cuándo no se prueba (Sys.time() en segundos)
.glia$pausa <- list()

llamar_gemini <- function(turnos, situacion) {
  cuerpo <- list(
    # El contexto fijo va primero y la situación al final: Gemini reutiliza
    # en caché el principio común de las peticiones.
    systemInstruction = list(parts = list(list(text = paste0(contexto_ia(), "\n\n", situacion)))),
    contents = lapply(turnos, function(t) list(role = t$role, parts = list(list(text = t$text)))),
    generationConfig = list(maxOutputTokens = MAX_TOKENS_GEMINI, temperature = 0.3)
  )
  ultimo_error <- NULL
  for (modelo in MODELOS_GEMINI) {
    if (isTRUE((.glia$pausa[[modelo]] %||% 0) > as.numeric(Sys.time()))) next
    peticion <- httr2::request(paste0("https://generativelanguage.googleapis.com/v1beta/models/",
                                      modelo, ":generateContent")) |>
      httr2::req_headers(`x-goog-api-key` = clave_gemini()) |>
      httr2::req_body_json(cuerpo, auto_unbox = TRUE) |>
      # 25 s: más vale caer a la respuesta guiada que dejar al usuario esperando
      httr2::req_timeout(25) |>
      httr2::req_error(is_error = function(r) FALSE)
    r <- httr2::req_perform(peticion)
    estado <- httr2::resp_status(r)
    if (estado == 200) {
      datos <- httr2::resp_body_json(r)
      partes <- datos$candidates[[1]]$content$parts
      texto <- paste(vapply(partes %||% list(), function(p) p$text %||% "", character(1)), collapse = "")
      return(texto)
    }
    detalle <- tryCatch(httr2::resp_body_string(r), error = function(e) "")
    # 402: proyecto con facturación de prepago y sin saldo. Afecta a todos los
    # modelos, así que no se prueba otro.
    if (estado == 402 || grepl("prepayment", detalle, ignore.case = TRUE)) stop("402 ", detalle)
    if (!estado %in% c(404, 429)) stop(estado, " ", substr(detalle, 1, 300))
    # Sin cupo: se deja de probar un rato; si no existe, hasta reiniciar.
    .glia$pausa[[modelo]] <- as.numeric(Sys.time()) + if (estado == 429) 600 else 1e9
    message("Glía: ", modelo, " no disponible (", estado, "); pruebo el siguiente modelo.")
    ultimo_error <- paste(estado, substr(detalle, 1, 300))
  }
  stop(ultimo_error %||% "429 RESOURCE_EXHAUSTED: ningún modelo de Gemini con cupo")
}

ETIQUETA_IR <- "\\[\\[\\s*ir\\s*:\\s*([a-zA-Z-]+)\\s*\\]\\]"

responder_ia <- function(pregunta, historial, pestana, estado_pred, extra = "") {
  texto <- llamar_gemini(turnos_ia(pregunta, historial), situacion_ia(pestana, estado_pred, extra))
  ir <- NULL
  encontrada <- regmatches(texto, regexec(ETIQUETA_IR, texto, perl = TRUE))[[1]]
  if (length(encontrada) == 2 && encontrada[2] %in% ORDEN) ir <- encontrada[2]
  texto <- trimws(gsub(ETIQUETA_IR, "", texto, perl = TRUE))
  if (!nzchar(texto)) stop("la IA devolvió una respuesta vacía")
  r <- respuesta_glia(texto, ir = ir, fuente = "ia")
  r$proveedor <- "Gemini"
  r
}

aviso_ia <- function(error) {
  detalle <- conditionMessage(error)
  if (grepl("402|prepayment", detalle, ignore.case = TRUE)) {
    "La clave de Gemini es de un proyecto con facturación sin saldo; te dejo la respuesta guiada."
  } else if (grepl("429|RESOURCE_EXHAUSTED", detalle)) {
    "Gemini alcanzó su límite de uso por ahora; te dejo la respuesta guiada."
  } else "No pude contactar con Gemini; te dejo la respuesta guiada."
}

# --------------------------------------------------------------------------- #
# Punto de entrada: una pregunta escrita por el usuario
#   Orden: navegación y lectura de la predicción (deterministas), Gemini si hay
#   clave y, si no la hay o falla, la respuesta guiada.
# --------------------------------------------------------------------------- #
responder_glia <- function(pregunta, historial = list(), pestana = NULL, estado_pred = NULL) {
  pregunta <- substr(trimws(pregunta %||% ""), 1, MAX_PREGUNTA)
  t <- normalizar(pregunta)
  destino <- destino_glia(t)
  if (!is.null(destino)) {
    return(respuesta_glia(paste0("Te llevo a **", ETIQUETAS[[destino]], "**: ", DESCRIPCIONES[[destino]]),
                          ir = destino, navegar = TRUE))
  }
  if (grepl(INTENCION_PREDICCION, t, perl = TRUE)) return(interpretar_prediccion(estado_pred))
  # Saludos y agradecimientos: no merecen una llamada a la IA
  if (grepl("^(hola|buenas|buenos dias|buenas tardes|buenas noches|hey|gracias|muchas gracias|ok|vale|listo)( .{0,20})?$", t)) {
    if (grepl("^(gracias|muchas|ok|vale|listo)", t)) {
      return(respuesta_glia("¡Con gusto! Si quieres seguir, elige una de las preguntas sugeridas o escríbeme otra."))
    }
    return(respuesta_glia(paste("¡Hola! Soy Glía, la asistente de la aplicación.", sub("^[^.]*\\. ", "", SIN_RESPUESTA))))
  }
  if (ia_disponible()) {
    return(tryCatch(responder_ia(pregunta, historial, pestana, estado_pred), error = function(e) {
      message("Glía: Gemini no respondió (", conditionMessage(e), "); uso la respuesta guiada.")
      g <- respuesta_guiada(pregunta, estado_pred)
      g$aviso <- aviso_ia(e)
      g
    }))
  }
  respuesta_guiada(pregunta, estado_pred)
}

# «Pregúntale a Glía» junto a un gráfico: con IA, el título y la nota van como
# contexto; sin IA, la nota de la tarjeta y la pregunta frecuente que mejor
# encaja con el título, para que el chip nunca se quede sin respuesta.
explicar_grafico <- function(titulo, nota, historial = list(), pestana = NULL, estado_pred = NULL) {
  pregunta <- paste0("Explícame el gráfico «", titulo, "»")
  if (ia_disponible()) {
    extra <- paste0("El usuario pulsó «Pregúntale a Glía» junto al gráfico «", titulo, "».",
                    if (nzchar(nota)) paste0(" La nota al pie de ese gráfico dice: ", gsub("\\$([^$]*)\\$", "\\1", nota)) else "",
                    " Explica qué muestra y cómo leerlo en 3 a 5 frases, con las cifras del contexto.")
    r <- tryCatch(responder_ia(pregunta, historial, pestana, estado_pred, extra), error = function(e) {
      message("Glía: no pude explicar el gráfico con IA (", conditionMessage(e), ").")
      NULL
    })
    if (!is.null(r)) return(r)
  }
  partes <- paste0("**", titulo, ".**", if (nzchar(nota)) paste0(" ", nota) else "")
  # Solo se añade una pregunta frecuente si el título encaja con claridad (dos
  # claves o más): con una sola, cualquier gráfico acababa en el ranking.
  relacionada <- respuesta_guiada(titulo, estado_pred, minimo = 2)
  partes <- c(partes, if (relacionada$texto != SIN_RESPUESTA) relacionada$texto else
    "Si quieres más detalle, pregúntame por una variable o una prueba concreta del gráfico y te la explico.")
  respuesta_glia(paste(partes, collapse = "\n\n"))
}

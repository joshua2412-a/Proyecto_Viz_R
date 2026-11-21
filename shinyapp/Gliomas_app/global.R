# =========================================================================== #
# Grado tumoral en gliomas · aplicación Shiny
#
# Versión en R del tablero de Dash del proyecto: las mismas trece pestañas, la
# misma estadística y los mismos tres modelos ya entrenados.
#
# Shiny ejecuta este archivo una vez, antes que ui.R y server.R: aquí se cargan
# los paquetes, los datos, la partición y los modelos, y se definen todas las
# funciones de modulos/. Lo que se calcula aquí lo comparten todas las sesiones.
#
# Ejecución, desde la carpeta del repositorio:
#   shiny::runApp("shinyapp/Gliomas_app")
# =========================================================================== #

library(shiny)
library(bslib)
library(plotly)
library(jsonlite)

options(scipen = 999)

# El orden importa: cada módulo usa lo definido en los anteriores
for (modulo in c("config.R", "formato.R", "tema.R", "componentes.R", "navegacion.R",
                 "datos.R", "figuras.R", "modelo.R", "figuras_modelo.R", "portada.R")) {
  source(file.path("modulos", modulo), local = FALSE, encoding = "UTF-8")
}

# Una función ui_<nombre>() por pestaña y, si tiene partes interactivas,
# server_<nombre>(input, output, session)
for (archivo in sort(list.files(file.path("modulos", "pestanas"), pattern = "\\.R$", full.names = TRUE))) {
  source(archivo, local = FALSE, encoding = "UTF-8")
}

# Qué construye cada pestaña. Añadir una = crear modulos/pestanas/mi_pestana.R,
# sumarla a PESTANAS (modulos/navegacion.R) y aquí.
CONSTRUCTORES <- list(
  "tab-introduccion" = ui_introduccion,
  "tab-contexto" = ui_contexto,
  "tab-problema" = ui_problema,
  "tab-objetivos" = ui_objetivos,
  "tab-marco" = ui_marco,
  "tab-metodologia" = ui_metodologia,
  "tab-exploracion" = ui_exploracion,
  "tab-resultados" = ui_resultados,
  "tab-limitaciones" = ui_limitaciones,
  "tab-conclusiones" = ui_conclusiones,
  "tab-modelos" = ui_modelos,
  "tab-prediccion" = ui_prediccion,
  "tab-documentacion" = ui_documentacion
)
stopifnot(setequal(names(CONSTRUCTORES), ORDEN))

# Glía, la asistente: va después de las pestañas porque usa funciones de
# Predicción (mismo_paciente) y de los módulos
for (modulo in c("asistente.R", "asistente_panel.R")) {
  source(file.path("modulos", modulo), local = FALSE, encoding = "UTF-8")
}

# --------------------------------------------------------------------------- #
# Lo que no depende de la sesión se calcula una vez por proceso
#
# Ninguna pestaña depende de quién la mira: su contenido sale de los datos y
# de los modelos, que son fijos. Antes se reconstruía en cada visita, y en
# shinyapps.io (CPU modesta y lejos del visitante) cada cambio de pestaña
# tardaba de 1 a 3 s, y hasta 20-25 s la primera vez. Ahora cada pestaña y cada
# figura sin controles se construye una sola vez, al arrancar el proceso, y
# cambiar de pestaña solo envía lo ya hecho.
# --------------------------------------------------------------------------- #
.pestanas_hechas <- new.env()
pestana_ui <- function(id) {
  if (is.null(.pestanas_hechas[[id]])) .pestanas_hechas[[id]] <- CONSTRUCTORES[[id]]()
  .pestanas_hechas[[id]]
}

# Cada pestaña en su contenedor (ui.R los pone todos en la página). Si una no
# se puede construir, se ve el aviso de error en su lugar y el resto funciona.
panel_pestana <- function(id) {
  div(class = "pestana-panel", `data-pestana` = id,
      tryCatch(pestana_ui(id), error = aviso_error))
}

# Figuras que no cambian con ningún control (las demás se calculan al vuelo)
FIGURAS_FIJAS <- list(
  ctx_cohorte = function() fig_distribucion_grado(AMBITO_EDA),
  ctx_edad = function() fig_edad_por_grado(AMBITO_EDA),
  ctx_caja = function() fig_boxplot_edad(AMBITO_EDA),
  prob_prevalencia = function() fig_prevalencia_genes(AMBITO_EDA, top_n = 12),
  res_cohorte = function() fig_distribucion_grado("train"),
  res_edad = function() fig_edad_por_grado("train"),
  res_caja = function() fig_boxplot_edad("train"),
  res_genero = function() fig_clinica_por_grado("Gender", "train"),
  res_raza = function() fig_clinica_por_grado("Race", "train"),
  res_prevalencia = function() fig_prevalencia_genes("train", top_n = 12),
  res_asociacion = function() fig_asociacion_grado("train", top_n = 14),
  res_spearman = function() fig_matriz_spearman("train"),
  res_genes = function() fig_matriz_genes("train", top_n = 12),
  mod_roc = function() fig_roc(),
  mod_importancia = function() fig_importancia(),
  mod_panel = function() fig_panel_reducido(),
  mod_pliegues = function() fig_pliegues()
)
.figuras_hechas <- new.env()
figura_fija <- function(clave) {
  if (is.null(.figuras_hechas[[clave]])) .figuras_hechas[[clave]] <- FIGURAS_FIJAS[[clave]]()
  .figuras_hechas[[clave]]
}

# Las librerías de Plotly van en la cabecera de la página (ui.R), cargadas con
# defer, mientras se ve la portada. Si no, llegaban con el primer gráfico y el
# navegador tardaba casi un segundo en leer los 3,5 MB de plotly.js justo al
# abrir la primera pestaña con gráficos.
DEPENDENCIAS_PLOTLY <- lapply(
  plotly::plotly_build(plotly::plot_ly(x = 1, y = 1, type = "scatter", mode = "markers"))$dependencies,
  function(d) {
    d$script <- lapply(unlist(d$script), function(s) list(src = s, defer = NA))
    d
  }
)

# La salida de una figura fija, ya serializada. renderPlotly vuelve a
# construir y a pasar a JSON la figura en cada sesión (medio segundo largo para
# las nueve de Resultados en shinyapps.io); con bindCache se hace una vez por
# proceso y cada visita recibe lo ya hecho.
salida_figura_fija <- function(clave) {
  shiny::bindCache(plotly::renderPlotly(figura_fija(clave)), clave)
}

# Se calienta todo al arrancar: así ni el primer visitante espera
local({
  for (id in ORDEN) tryCatch(pestana_ui(id), error = function(e) message("No se pudo preparar ", id, ": ", conditionMessage(e)))
  for (clave in names(FIGURAS_FIJAS)) tryCatch(figura_fija(clave), error = function(e) message("No se pudo preparar la figura ", clave))
  invisible(tryCatch(contexto_ia(), error = function(e) NULL))
})

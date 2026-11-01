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

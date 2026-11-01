# Carga los módulos del capítulo de modelado. Lo usan el capítulo (02-Modelado.Rmd),
# el script que precalcula la caché y las sesiones de trabajo en paralelo.
MODULOS_R <- normalizePath(file.path("R", c("config.R", "datos.R", "modelos.R",
                                            "optimizacion.R", "evaluacion.R",
                                            "experimentos.R", "analisis.R",
                                            "exportar_shiny.R")))
for (f in MODULOS_R) source(f, encoding = "UTF-8")

# Los modelos guardados en la caché son objetos de glmnet, ranger y xgboost.
# predict() solo encuentra sus métodos (predict.lognet, predict.ranger...) si
# el paquete está cargado, y leer el .rds no lo carga. Se cargan aquí para que
# el capítulo funcione igual con RECALCULAR = FALSE.
for (p in c("glmnet", "ranger", "xgboost")) loadNamespace(p)
rm(p)

# Caracterización Clínico-Molecular del Grado Tumoral en Gliomas

Libro en R (bookdown) del proyecto de clasificación de gliomas de bajo grado (LGG) frente a glioblastoma (GBM) con datos de TCGA.

**Autores:** Alejandro Cantillo Escorcia y Joshua Hincapie Llorente.

## Contenido

| Archivo / carpeta | Qué es |
|---|---|
| `index.Rmd` | Introducción, objetivos y ficha técnica del dataset |
| `01-EDA.Rmd` | Capítulo 1: análisis exploratorio |
| `02-Modelado.Rmd` | Capítulo 2: modelado predictivo, calculado en R de principio a fin |
| `R/` | Código del modelado: pipelines, optimizadores, validación anidada, pruebas estadísticas y exportación para la app Shiny |
| `resultados/` | Resultados del modelado: tablas maestras (`experimentos.csv`, `pliegues.csv`), una búsqueda por combinación (`parciales/`) y la caché de los demás cálculos (`cache/`) |
| `dataset/` | Dataset original y partición 80/20 (`train_data.csv`, `test_data.csv`) |
| `textos/` | Interpretaciones de cada gen que usa el capítulo 1 |
| `header.html`, `style.css` | Código plegable, títulos de tabla y estilos |
| `_bookdown.yml`, `_output.yml` | Configuración del libro |

## Compilar

Abre `rbook_gliomas.Rproj` en RStudio y usa *Build Book*, o ejecuta:

```r
bookdown::render_book("index.Rmd")
```

El libro se genera en la carpeta `docs/` de la raíz del repositorio, la que publica GitHub Pages (Settings → Pages → Branch: main · Folder: /docs). Al compilar, el capítulo 2 también escribe los modelos y el resumen que usa la app Shiny en `../shinyapp/Gliomas_app/data/`.

Paquetes necesarios:

```r
install.packages(c(
  "bookdown", "tidyverse", "knitr", "kableExtra", "patchwork", "scales", "jsonlite",
  # modelado
  "rsample", "glmnet", "ranger", "xgboost", "themis", "ParBayesianOptimization", "GA",
  "lime", "pROC", "boot", "effsize", "bench", "future", "future.apply"
))
```

## El modelado

El capítulo 2 no depende de ningún resultado externo: parte de `train_data.csv` y `test_data.csv` y calcula todo en R (las 48 combinaciones de modelo × balanceo × optimizador con validación cruzada anidada, los modelos finales, la calibración, SHAP, LIME, las pruebas estadísticas y el panel reducido).

Los cálculos costosos se guardan la primera vez en `resultados/` y se reutilizan en las compilaciones siguientes, así que el libro se compila en pocos minutos. Para repetirlos desde cero, pon `RECALCULAR <- TRUE` al inicio de `02-Modelado.Rmd` (unas tres horas con dos procesos). El experimento de las 48 combinaciones también se puede lanzar aparte, y se reanuda donde quedó si se interrumpe:

```bash
Rscript R/precalcular.R      # desde la carpeta rbook_gliomas
```

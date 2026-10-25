# Caracterización Clínico-Molecular del Grado Tumoral en Gliomas

Libro en R (bookdown) del proyecto de clasificación de gliomas de bajo grado (LGG) frente a glioblastoma (GBM) con datos de TCGA.

**Autores:** Alejandro Cantillo Escorcia y Joshua Hincapie Llorente.

## Contenido

| Archivo / carpeta | Qué es |
|---|---|
| `index.Rmd` | Introducción, objetivos y ficha técnica del dataset |
| `01-EDA.Rmd` | Capítulo 1: análisis exploratorio |
| `02-Modelado.Rmd` | Capítulo 2: modelado predictivo |
| `dataset/` | Dataset original y partición 80/20 (`train_data.csv`, `test_data.csv`) |
| `modelado/` | Resultados del capítulo 2 exportados desde el proyecto de Python |
| `textos/` | Interpretaciones de cada gen que usa el capítulo 1 |
| `header.html`, `style.css` | Código plegable, títulos de tabla y estilos |
| `_bookdown.yml`, `_output.yml` | Configuración del libro |

## Compilar

Abre `rbook_gliomas.Rproj` en RStudio y usa *Build Book*, o ejecuta:

```r
bookdown::render_book("index.Rmd")
```

El libro se genera en la carpeta `docs/` de la raíz del repositorio, la que publica GitHub Pages (Settings → Pages → Branch: main · Folder: /docs).

Paquetes necesarios: `bookdown`, `tidyverse`, `knitr`, `kableExtra`, `patchwork`, `scales` y `jsonlite`.

## Actualizar los resultados del modelado

El capítulo 2 no reentrena modelos: lee las tablas de `modelado/`, generadas a partir del Jupyter Book del proyecto (repositorio `Proyecto_Viz`). Después de volver a ejecutar `02_Modelado.ipynb`, desde la carpeta `jbook/` de ese repositorio:

```bash
python -m modelado.exportar_r --destino ../../Proyecto_Viz_R/rbook_gliomas/modelado
```

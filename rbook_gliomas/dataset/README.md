# dataset/

Esta carpeta guarda la **única copia** de los datos del proyecto. La leen tanto
el dashboard (`utils/data_loader.py`) como los notebooks del Jupyter Book
(`jbook/01_EDA.ipynb`), de modo que ninguna cifra pueda desincronizarse entre
las dos piezas.

## Archivo necesario

```
dataset/TCGA_InfoWithGrade.csv
```

- **Dataset:** Glioma Grading Clinical and Mutation Features
- **Fuente:** [UCI Machine Learning Repository, dataset 759](https://archive.ics.uci.edu/dataset/759/glioma+grading+clinical+and+mutation+features+dataset)
- **Origen de los datos:** proyectos TCGA-LGG y TCGA-GBM de *The Cancer Genome Atlas*
- **Dimensiones esperadas:** 839 filas × 24 columnas, sin valores faltantes

## Esquema esperado

| Columna | Tipo | Codificación |
|---|---|---|
| `Grade` | binaria | 0 = LGG · 1 = GBM |
| `Gender` | binaria | 0 = masculino · 1 = femenino |
| `Age_at_diagnosis` | continua | años, con decimales |
| `Race` | categórica | 0 = White · 1 = Black or African American · 2 = Asian · 3 = American Indian or Alaska Native |
| `IDH1`, `TP53`, `ATRX`, `PTEN`, `EGFR`, `CIC`, `MUC16`, `PIK3CA`, `NF1`, `PIK3R1`, `FUBP1`, `RB1`, `NOTCH1`, `BCOR`, `CSMD3`, `SMARCA4`, `GRIN2A`, `IDH2`, `FAT4`, `PDGFRA` | binarias | 0 = no mutado (wildtype) · 1 = mutado |

`utils/data_loader.py` valida el esquema al cargar: si falta alguna columna, el
error dice exactamente cuál en lugar de fallar más adelante con un gráfico
vacío.

## Después de colocar el archivo

```bash
python app.py                 # http://127.0.0.1:8080
```

No hace falta nada más: la app solo lee este CSV. Si el archivo no está, arranca
igual y cada pestaña con datos avisa de que falta.

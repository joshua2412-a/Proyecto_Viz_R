# Grado tumoral en gliomas · app Shiny

Versión en R del tablero de Dash del proyecto: las mismas trece pestañas, el
mismo diseño, la misma estadística y los tres modelos ya entrenados (regresión
logística, Random Forest y XGBoost), que aquí predicen y se explican con R base.

## Qué hay en esta versión

Va a la par del tablero de Dash (estado del 1 de noviembre):

* **Portada animada**: un cerebro de partículas con el glioma dentro
  (`www/intro.js`, WebGL sin librerías; `modulos/portada.R` arma el marcado).
  Se gira arrastrando y el interruptor GBM | LGG cambia la lesión.
* **Estilo B+**: la barra superior, el índice y la cabecera de cada pestaña
  forman una banda azul noche. Las mismas partículas de la portada recorren
  la banda (`www/banda.js`): una figura con acción en cada pestaña (el cursor
  de la edad, la diana, la sonda de genes, el Plinko de los modelos...) y una
  barra de progreso en doble hélice con un peldaño clicable por pestaña.
* **Modo claro y oscuro**: botón junto al enlace al libro (`www/tema.js`);
  sigue al sistema hasta que se elige uno. Todo el estilo está en el bloque
  «Modo oscuro» de `www/style.css`.
* **Notación en LaTeX**: cualquier texto con `$...$` se compone con MathJax
  (`www/formulas.js` lo aplica a cada pestaña al montarse).
* **Predicción**: el formulario no recalcula nada hasta pulsar *Predecir*;
  las probabilidades salen en mosaicos, «Dónde cae este perfil» es un enjambre
  de partículas con los pacientes de prueba (`www/enjambre.js`) y la sección
  "Cómo decide" sigue al modelo elegido arriba.
* **Glía, la asistente**: un astrocito de partículas en la esquina inferior
  derecha (`www/glia.js`) que abre un panel de conversación en todas las
  pestañas, con preguntas sugeridas por pestaña, un chip «Pregúntale a Glía»
  en cada gráfico y la lectura de la predicción. Las respuestas guiadas
  (`modulos/asistente.R`) funcionan siempre; si el servidor tiene la clave
  `GEMINI_API_KEY`, el texto libre lo responde Gemini (ver «Activar Gemini»).
* **Navegación**: el título de la barra vuelve a la portada, y el viaje de
  las partículas entre pestañas es el largo del tablero (1,7 s).
* **Orden de las pestañas**: Inicio · 01 El contexto · 02 El método ·
  03 Los resultados (Resultados, Modelos, Predicción) · 04 El cierre
  (Limitaciones, Conclusiones) · Anexo.

`www/style.css`, `banda.js`, `tema.js`, `enjambre.js`, `intro.js`,
`indice.js` y `contador.js` son los archivos del tablero de Dash; los pocos
ajustes para Shiny están señalados en ellos. `www/shiny.css` adapta los
controles de Shiny y `www/navegacion.js` hace en el navegador lo que en Dash
hacen los callbacks de cliente.

## Ejecutar

Desde la carpeta raíz del repositorio (`Proyecto_Viz_R`):

```r
install.packages(c("shiny", "bslib", "plotly", "jsonlite", "httr2"))
shiny::runApp("shinyapp/Gliomas_app")
```

O abre `ui.R` en RStudio y pulsa **Run App**.

## Publicar en shinyapps.io

```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name = "<cuenta>", token = "<token>", secret = "<secreto>")
rsconnect::deployApp("shinyapp/Gliomas_app", appName = "gliomas")
```

Los datos van dentro de `data/`, así que la app no depende de ninguna otra
carpeta del repositorio.

## Activar Gemini en Glía

Sin clave, Glía da sus respuestas guiadas. Para que responda el texto libre con
Gemini (clave gratuita de Google AI Studio, de un proyecto sin facturación):

1. Crea el archivo `shinyapp/Gliomas_app/.Renviron` con una sola línea:
   `GEMINI_API_KEY=tu_clave`. Git lo ignora: la clave nunca va al repositorio.
2. En local, reinicia R para que lo lea. En shinyapps.io, `deployApp()` sube
   el archivo con la app y el servidor lo lee al arrancar.

Si se agota la cuota o falla la red, Glía lo dice y responde con la guía. En el
plan gratuito, Google puede usar lo que se le envía para mejorar sus productos
(aquí, cifras públicas y las preguntas).

## Estructura

```
Gliomas_app/
├── global.R            paquetes, datos, partición, modelos y módulos (una vez)
├── ui.R                barra superior, navegación, contenido y pie
├── server.R            enruta la pestaña activa y registra las salidas
├── modulos/
│   ├── config.R        rutas, enlaces, esquema y etiquetas
│   ├── formato.R       números en español (coma decimal, "42,0 %")
│   ├── tema.R          paleta y plantilla de plotly
│   ├── componentes.R   tarjetas, KPI, tablas, lecturas guiadas...
│   ├── navegacion.R    orden de las pestañas, bloques, barra e índice
│   ├── datos.R         carga, partición y toda la estadística del EDA
│   ├── figuras.R       figuras del EDA
│   ├── modelo.R        predicción con los modelos exportados y TreeSHAP
│   ├── figuras_modelo.R figuras de Modelos y Predicción
│   ├── asistente.R     Glía: preguntas frecuentes, glosario, predicción y Gemini
│   ├── asistente_panel.R Glía: el panel y la conversación
│   └── pestanas/       una función ui_*() (y server_*() si hace falta) por pestaña
├── www/                style.css (la del Dash), shiny.css, JS de navegación y de Glía, portada.svg
└── data/
    ├── TCGA_InfoWithGrade.csv
    ├── train_data.csv          partición 80/20 del proyecto (671 pacientes)
    ├── test_data.csv           y sus 168 pacientes de prueba
    ├── modelos_finales.json    coeficientes y árboles de los tres modelos
    └── resumen_modelado.json   métricas, curvas y pruebas del capítulo de modelado
```

## De dónde salen los modelos

Los entrena el capítulo de modelado del libro en R (`rbook_gliomas/02-Modelado.Rmd`),
que al compilarse escribe aquí `data/modelos_finales.json` y
`data/resumen_modelado.json`: los coeficientes de la regresión logística (glmnet)
y los árboles de Random Forest (ranger) y XGBoost como listas de nodos, más las
tablas de las pestañas Modelos y Predicción. `modulos/modelo.R` recorre esos
árboles y calcula los valores SHAP exactos con el algoritmo TreeSHAP; el libro
comprueba antes de exportar que el formato portable reproduce las probabilidades
de los modelos originales. Si se reentrenan, basta volver a compilar el libro.

La partición es la de `train_data.csv` y `test_data.csv`, los mismos archivos
que usa el libro: cada fila se localiza en el dataset completo por su contenido.

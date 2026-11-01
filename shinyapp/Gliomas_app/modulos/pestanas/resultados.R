# =========================================================================== #
# Pestaña 8 · Resultados
# El análisis exploratorio completo, siempre sobre el conjunto de entrenamiento:
# cruzar las predictoras con el grado en el conjunto reservado gastaría la
# reserva que después hace falta para evaluar los modelos.
# =========================================================================== #

FRANJA_PARTIDA <- paste0("linear-gradient(180deg, ", COLOR_LGG, " 0 50%, ", COLOR_GBM, " 50% 100%)")

resultados_kpis <- function() {
  a <- asociacion_con_grado("train")
  edad <- estadisticas_edad("train")
  prev <- prevalencia_genes("train")
  idh1 <- prev[prev$gen == "IDH1", ]
  prop <- proporcion_grado("train")
  mas_fuerte <- a[1, ]
  list(
    list(valor = num(mas_fuerte$rho, 2, signo = TRUE),
         etiqueta = paste("Asociación de", mas_fuerte$variable),
         detalle = "El marcador con mayor capacidad discriminativa"),
    list(valor = paste(num(edad["GBM", "media"] - edad["LGG", "media"], 1), "años"),
         etiqueta = "Diferencia de edad", color = FRANJA_PARTIDA,
         detalle = paste("GBM", num(edad["GBM", "media"], 1), "frente a LGG",
                         num(edad["LGG", "media"], 1))),
    list(valor = paste(pct(idh1$LGG, 0), "vs", pct(idh1$GBM, 0)),
         etiqueta = "IDH1 mutado: LGG vs GBM",
         detalle = "Prevalencia de la mutación en cada grado", color = FRANJA_PARTIDA),
    list(valor = paste(sum(a$significativa), "de", nrow(a)), etiqueta = "Variables con señal",
         detalle = paste("Sobre", sum(prop$pacientes), "pacientes de entrenamiento"))
  )
}

resultados_tabla_asociacion <- function(ambito) {
  datos <- utils::head(asociacion_con_grado(ambito), 10)
  formato_p <- function(v) if (v < 0.0001) "< 0,0001" else num(v, 4)
  filas <- lapply(seq_len(nrow(datos)), function(i) {
    f <- datos[i, ]
    c(nombre_corto(f$variable), f$prueba, num(f$rho, 3, signo = TRUE),
      if (is.na(f$v_cramer)) "—" else num(f$v_cramer, 3),
      formato_p(f$p_valor), formato_p(f$q_valor),
      if (is.na(f$rho)) "—" else if (f$rho < 0) "LGG" else "GBM")
  })
  data_table(c("Variable", "Prueba", "$r$ de Spearman", "V de Cramér", "p-valor", "q (BH)", "Empuja hacia"),
             filas)
}

resultados_v_par <- function(ambito, a, b) num(matriz_asociacion_genes(ambito)[a, b], 2)

resultados_tabla_vif <- function(ambito) {
  datos <- utils::head(vif_predictoras(ambito), 5)
  data_table(
    c("Predictora", "$R^2$ contra las demás", "VIF", "Lectura"),
    lapply(seq_len(nrow(datos)), function(i) {
      f <- datos[i, ]
      c(nombre_corto(f$variable), num(f$r2, 3), num(f$vif, 2),
        if (f$vif < 5) "Sin problema" else if (f$vif < 10) "Conviene mirarlo" else "Inestable")
    })
  )
}

resultados_bloque_eda <- function(ambito) {
  prueba_edad <- prueba_edad_por_grado(ambito)
  nombre_ambito <- AMBITO_NOMBRES[[ambito]]
  tagList(
    fila(
      columna(lg = 4, class = "mb-4",
              graph_card("fig_res_cohorte", "Distribución de la variable objetivo",
                         paste0("Composición del ", nombre_ambito, "."))),
      columna(lg = 8, class = "mb-4",
              graph_card("fig_res_edad", "Edad al diagnóstico por grado",
                         paste0("Prueba U de Mann-Whitney: ", p_valor(prueba_edad$p_valor), ".")))
    ),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_res_caja", "Dispersión de la edad",
                         "Los puntos son atípicos dentro de cada grado, no en la distribución global.")),
      columna(lg = 5, class = "mb-4",
              lectura_guiada(lectura_cohorte(ambito), titulo = "Lectura · la cohorte y la edad"))
    ),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_res_genero", "Género dentro de cada grado",
                         "Predominio masculino en ambos grados, más marcado en GBM.")),
      columna(lg = 5, class = "mb-4",
              lectura_guiada(lectura_clinicas(ambito), titulo = "Lectura · las variables demográficas"))
    ),
    fila(
      columna(lg = 12, class = "mb-4",
              graph_card("fig_res_raza", "Grupo racial reportado dentro de cada grado",
                         paste("Más de nueve de cada diez pacientes son del grupo White: las",
                               "categorías minoritarias tienen muy pocas observaciones.")))
    ),
    section_title("Mutaciones: prevalencia y capacidad discriminativa"),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_res_prevalencia", "Prevalencia de mutación por gen y grado",
                         "Los doce genes que más separan a los dos grupos.")),
      columna(lg = 5, class = "mb-4",
              lectura_guiada(lectura_genes(ambito), titulo = "Lectura · el panel de mutaciones"))
    ),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_res_asociacion", "Asociación de cada variable con el grado",
                         paste("Signo negativo = empuja hacia LGG. Las barras rayadas no superan",
                               "los dos filtros ($p \\geq 0{,}05$ o $|r| < 0{,}10$). El grupo racial",
                               "aparece reagrupado en dos niveles, que es como el libro lo",
                               "correlaciona."))),
      columna(lg = 5, class = "mb-4",
              lectura_guiada(lectura_asociacion(ambito), titulo = "Lectura · qué merece la pena medir"))
    ),
    card(
      resultados_tabla_asociacion(ambito),
      titulo = "Las variables más asociadas al grado",
      subtitulo = paste0(toupper(substr(nombre_ambito, 1, 1)), substring(nombre_ambito, 2),
                         " · las diez de mayor $|r|$"),
      class = "mb-4"
    ),
    section_title("Correlación entre todas las variables"),
    fila(
      columna(lg = 8, class = "mb-4",
              graph_card("fig_res_spearman", "Matriz de Spearman del grado y las 23 predictoras",
                         paste("Solo el triángulo inferior: el superior es su reflejo. El grupo",
                               "racial entra reagrupado en dos niveles, igual que en el libro."))),
      columna(lg = 4, class = "mb-4",
              lectura_guiada(lectura_correlaciones(ambito), titulo = "Lectura · el mapa completo"))
    ),
    section_title("Multicolinealidad entre mutaciones"),
    fila(
      columna(lg = 7, class = "mb-4",
              graph_card("fig_res_genes", "V de Cramér entre pares de genes",
                         "La diagonal se omite (vale 1 por definición).")),
      columna(
        lg = 5, class = "mb-4",
        card(
          paragraph(paste(
            "La matriz responde a una pregunta concreta: ¿hay pares de mutaciones tan",
            "redundantes que no convenga meterlas juntas en un modelo lineal? Solo a esa:",
            "la multicolinealidad completa se mide más abajo, con el factor de inflación",
            "de la varianza.")),
          bullet_list(list(
            paste0("ATRX-TP53 es la co-ocurrencia más fuerte (V = ",
                   resultados_v_par(ambito, "ATRX", "TP53"),
                   "): el eje clásico de co-mutación en la astrocitogénesis."),
            paste0("FUBP1-CIC (V = ", resultados_v_par(ambito, "FUBP1", "CIC"), ") y ATRX-IDH1 (V = ",
                   resultados_v_par(ambito, "ATRX", "IDH1"), ") reafirman las firmas del linaje ",
                   "oligodendroglial y de los gliomas de bajo grado."),
            paste0("PTEN-IDH1 (V = ", resultados_v_par(ambito, "PTEN", "IDH1"), ") refleja ",
                   "exclusión mutua: IDH1 mutado caracteriza LGG, PTEN alterado caracteriza GBM.")
          )),
          callout(
            paste("Ningún par supera el umbral de redundancia (V > 0,70). Eso descarta la",
                  "redundancia por parejas, que no es lo mismo que descartar la",
                  "multicolinealidad: una variable puede ser casi predecible a partir de",
                  "varias a la vez sin parecerse a ninguna por separado."),
            titulo = "Qué descarta esto, y qué no"
          ),
          titulo = "Cómo se lee la matriz",
          tono = "acento"
        )
      )
    ),
    section_title("Multicolinealidad completa: factor de inflación de la varianza"),
    callout(
      paste("Las dos figuras de arriba miran pares, y la multicolinealidad no es un",
            "fenómeno por pares: una variable puede ser casi predecible a partir de una",
            "combinación de otras sin parecerse a ninguna por separado. El VIF responde a",
            "esa pregunta."),
      titulo = "Por qué no basta con mirar pares"
    ),
    fila(
      columna(lg = 7, class = "mb-4",
              card(resultados_tabla_vif(ambito), titulo = "Las cinco predictoras con más VIF",
                   subtitulo = "$\\mathrm{VIF} = 1/(1 - R^2)$ de regresar cada predictora contra las otras 22")),
      columna(lg = 5, class = "mb-4",
              lectura_guiada(lectura_multicolinealidad(ambito),
                             titulo = "Lectura · se pisan o no las predictoras"))
    )
  )
}

ui_resultados <- function() {
  div(
    class = "vista-pestana",
    titular(
      "03 Los resultados · Análisis exploratorio",
      "Qué dicen los datos,",
      "variable a variable",
      texto = "Distribuciones, contrastes y fuerza de cada asociación con el grado tumoral."
    ),
    kpi_row(resultados_kpis()),
    section_title("Análisis exploratorio"),
    callout(
      paste0("Todo lo que sigue se calcula sobre el conjunto de entrenamiento (",
             tamanos_particion()[["train"]], " pacientes). El de prueba se apartó antes ",
             "de mirar nada y no se cruza con el grado, para que la evaluación de ",
             "los modelos siga siendo independiente."),
      titulo = "Sobre qué datos está hecho este análisis"
    ),
    div(class = "mb-3", series_chips()),
    resultados_bloque_eda("train")
  )
}

server_resultados <- function(input, output, session) {
  output$fig_res_cohorte <- plotly::renderPlotly(fig_distribucion_grado("train"))
  output$fig_res_edad <- plotly::renderPlotly(fig_edad_por_grado("train"))
  output$fig_res_caja <- plotly::renderPlotly(fig_boxplot_edad("train"))
  output$fig_res_genero <- plotly::renderPlotly(fig_clinica_por_grado("Gender", "train"))
  output$fig_res_raza <- plotly::renderPlotly(fig_clinica_por_grado("Race", "train"))
  output$fig_res_prevalencia <- plotly::renderPlotly(fig_prevalencia_genes("train", top_n = 12))
  output$fig_res_asociacion <- plotly::renderPlotly(fig_asociacion_grado("train", top_n = 14))
  output$fig_res_spearman <- plotly::renderPlotly(fig_matriz_spearman("train"))
  output$fig_res_genes <- plotly::renderPlotly(fig_matriz_genes("train", top_n = 12))
}

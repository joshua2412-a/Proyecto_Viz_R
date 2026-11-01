# =========================================================================== #
# Mapa de navegación: orden de las pestañas, nombres, iconos y bloques
#
# Única fuente de esta información: la leen la barra de pestañas, el índice
# desplegable, la guía de la portada y www/navegacion.js (que la recibe como
# JSON incrustado en la página).
# =========================================================================== #

PESTANAS <- list(
  list(id = "tab-introduccion", etiqueta = "Introducción", icono = "home",
       descripcion = "Portada, ficha del dataset y guía de lectura."),
  list(id = "tab-contexto", etiqueta = "Contexto clínico", icono = "monitor_heart",
       descripcion = "Qué son LGG y GBM y por qué importa distinguirlos."),
  list(id = "tab-problema", etiqueta = "Problema", icono = "error",
       descripcion = "El coste de la secuenciación completa como cuello de botella."),
  list(id = "tab-objetivos", etiqueta = "Objetivos", icono = "track_changes",
       descripcion = "Qué se propone resolver el proyecto y con qué criterio."),
  list(id = "tab-marco", etiqueta = "Marco teórico", icono = "menu_book",
       descripcion = "Las variables del panel y las pruebas estadísticas."),
  list(id = "tab-metodologia", etiqueta = "Metodología", icono = "assignment",
       descripcion = "Partición, control de calidad y contrastes aplicados."),
  list(id = "tab-exploracion", etiqueta = "Exploración", icono = "tune",
       descripcion = "Cada variable por separado, con su ficha y sus pruebas."),
  list(id = "tab-resultados", etiqueta = "Resultados", icono = "bar_chart",
       descripcion = "El análisis exploratorio, variable a variable."),
  list(id = "tab-modelos", etiqueta = "Modelos", icono = "account_tree",
       descripcion = "Cómo se entrenaron, evaluaron y compararon los tres clasificadores."),
  list(id = "tab-prediccion", etiqueta = "Predicción", icono = "calculate",
       descripcion = "Describe un paciente y mira qué predicen los modelos y por qué."),
  list(id = "tab-limitaciones", etiqueta = "Limitaciones", icono = "warning",
       descripcion = "Qué NO se puede concluir del análisis ni de los modelos."),
  list(id = "tab-conclusiones", etiqueta = "Conclusiones", icono = "check_circle",
       descripcion = "Lo que deja el proyecto, del EDA al clasificador."),
  list(id = "tab-documentacion", etiqueta = "Documentación", icono = "book",
       descripcion = "El libro en R, el repositorio y cómo ejecutar la app.")
)

ORDEN <- vapply(PESTANAS, `[[`, character(1), "id")
ETIQUETAS <- stats::setNames(vapply(PESTANAS, `[[`, character(1), "etiqueta"), ORDEN)
ICONOS <- stats::setNames(vapply(PESTANAS, `[[`, character(1), "icono"), ORDEN)
DESCRIPCIONES <- stats::setNames(vapply(PESTANAS, `[[`, character(1), "descripcion"), ORDEN)
TAB_INICIAL <- ORDEN[1]

BLOQUES <- list(
  list(numero = "", titulo = "Inicio", resumen = "",
       pestanas = "tab-introduccion", en_guia = FALSE),
  list(numero = "01", titulo = "El contexto",
       resumen = paste("Qué está en juego al distinguir los dos grados y por qué",
                       "hacerlo bien sale caro hoy."),
       pestanas = c("tab-contexto", "tab-problema", "tab-objetivos"), en_guia = TRUE),
  list(numero = "02", titulo = "El método",
       resumen = paste("Las variables del panel, la partición de los datos y las",
                       "pruebas que sostienen cada cifra del tablero."),
       pestanas = c("tab-marco", "tab-metodologia", "tab-exploracion"), en_guia = TRUE),
  list(numero = "03", titulo = "Los resultados",
       resumen = paste("El análisis variable a variable y los clasificadores que",
                       "salen de él: cómo rinden en pacientes nuevos y qué predicen",
                       "para un perfil concreto."),
       pestanas = c("tab-resultados", "tab-modelos", "tab-prediccion"), en_guia = TRUE),
  list(numero = "04", titulo = "El cierre",
       resumen = paste("Lo que se puede concluir del trabajo completo y, sobre",
                       "todo, lo que no."),
       pestanas = c("tab-limitaciones", "tab-conclusiones"), en_guia = TRUE),
  list(numero = "", titulo = "Anexo", resumen = "",
       pestanas = "tab-documentacion", en_guia = FALSE)
)

local({
  en_bloques <- unlist(lapply(BLOQUES, `[[`, "pestanas"))
  if (!setequal(en_bloques, ORDEN) || length(en_bloques) != length(ORDEN)) {
    stop("BLOQUES y PESTANAS no coinciden en modulos/navegacion.R")
  }
})

# --------------------------------------------------------------------------- #
# Piezas de la estructura (barra superior, pestañas, índice y pie)
# --------------------------------------------------------------------------- #
barra_superior <- function() {
  div(
    id = "barra-superior", class = "topbar",
    div(
      class = "container-fluid",
      fila(
        class = "align-items-center",
        columna(
          md = 8,
          div(class = "brand-eyebrow", "Dashboard analítico · TCGA-LGG y TCGA-GBM"),
          tags$h1(class = "brand-title", "Grado tumoral en gliomas"),
          div(class = "brand-subtitle",
              paste("Análisis exploratorio y modelado de la edad, el perfil",
                    "demográfico y 20 mutaciones genéticas en 839 pacientes de",
                    "TCGA-LGG y TCGA-GBM"))
        ),
        columna(
          md = 4, class = "d-flex align-items-end justify-content-md-end",
          # Solo el cambio de tema y el enlace al libro: lo que de verdad hace algo
          div(
            class = "tech-pills",
            # Claro / oscuro. Los dos iconos están siempre y se cruzan con CSS;
            # el texto accesible y el estado los pone www/tema.js.
            tags$button(
              type = "button", id = "boton-tema", class = "boton-tema",
              title = "Cambiar entre modo claro y oscuro",
              `aria-label` = "Cambiar entre modo claro y oscuro",
              icono("dark_mode", "tema-icono tema-icono-luna"),
              icono("light_mode", "tema-icono tema-icono-sol")
            ),
            tags$a(
              href = URL_LIBRO, target = "_blank", rel = "noopener noreferrer",
              class = "boton-libro", title = "Abrir el libro en R",
              icono("menu_book", "me-2"),
              span(class = "boton-libro-largo", "Abrir el"),
              "libro en R"
            )
          )
        )
      )
    )
  )
}

panel_indice <- function() {
  lapply(seq_along(BLOQUES), function(i) {
    bloque <- BLOQUES[[i]]
    div(
      class = paste0("indice-bloque indice-bloque-", i - 1),
      div(class = "indice-bloque-titulo",
          if (nzchar(bloque$numero)) span(class = "indice-bloque-num", bloque$numero),
          bloque$titulo),
      lapply(bloque$pestanas, function(tab_id) {
        tags$button(
          type = "button", class = "indice-item", `data-ir` = tab_id,
          span(class = "indice-item-nombre", ETIQUETAS[[tab_id]]),
          span(class = "indice-item-texto", DESCRIPCIONES[[tab_id]])
        )
      })
    )
  })
}

barra_indice <- function() {
  div(
    class = "indice-barra",
    tags$details(
      id = "indice", class = "indice",
      tags$summary(
        class = "indice-boton",
        icono("menu", "indice-icono"),
        span(id = "indice-posicion", class = "indice-posicion"),
        span(id = "indice-actual", class = "indice-actual"),
        icono("expand_more", "indice-flecha")
      ),
      div(class = "indice-panel", panel_indice())
    ),
    div(class = "indice-progreso", div(id = "indice-progreso", class = "indice-progreso-relleno")),
    div(
      class = "indice-vecinos",
      tags$button(type = "button", id = "indice-anterior", class = "indice-vecino"),
      tags$button(type = "button", id = "indice-siguiente", class = "indice-vecino")
    )
  )
}

barra_navegacion <- function() {
  div(
    id = "barra-navegacion", class = "navbar-tabs",
    div(
      class = "container-fluid",
      div(
        class = "nav-pestanas",
        # El id es el de dbc.Tabs en Dash: www/banda.js lee de aquí la pestaña
        # activa, que cambia en el mismo clic.
        tags$ul(
          id = "tabs-principal",
          class = "nav nav-tabs main-tabs", role = "tablist",
          lapply(ORDEN, function(tab_id) {
            tags$li(
              class = "nav-item", role = "presentation",
              tags$a(
                href = "#", role = "tab", `data-ir` = tab_id,
                class = paste("nav-link main-tab",
                              if (tab_id == TAB_INICIAL) "active main-tab-active"),
                ETIQUETAS[[tab_id]]
              )
            )
          })
        )
      ),
      div(class = "nav-indice", barra_indice())
    )
  )
}

pie_pagina <- function() {
  div(
    class = "footer",
    div(
      class = "container-fluid",
      span("Proyecto de visualización y analítica de datos"),
      span(class = "footer-sep", " · "),
      span("Dataset: Glioma Grading Clinical and Mutation Features (TCGA, 839 pacientes)"),
      span(class = "footer-sep", " · "),
      span(paste("Autores:", lista_y(lapply(AUTORES, `[[`, "nombre")))),
      span(class = "footer-sep", " · "),
      tags$a("Documentación completa", href = URL_LIBRO, target = "_blank",
             rel = "noopener noreferrer", class = "footer-link")
    )
  )
}

# Lo que necesita www/navegacion.js para pintar el índice sin ir al servidor
datos_navegacion_js <- function() {
  tags$script(
    id = "datos-navegacion", type = "application/json",
    HTML(as.character(jsonlite::toJSON(
      list(orden = ORDEN, etiquetas = as.list(ETIQUETAS), inicial = TAB_INICIAL),
      auto_unbox = TRUE
    )))
  )
}

# Cifras de los modelos que dibuja www/banda.js en la banda de Modelos (Plinko
# con la matriz de confusión de la regresión logística en prueba) y de
# Conclusiones (el panel reducido con la importancia SHAP de cada variable en
# XGBoost). Salen del resumen que exporta el libro de R, así que si se
# reentrenan los modelos la banda cambia sola.
datos_banda_js <- function() {
  y <- as.integer(unlist(RESUMEN$test$y))
  p <- as.numeric(unlist(RESUMEN$test$logistica))
  plinko <- list(lgg = sum(y == 0), okLgg = sum(y == 0 & p < UMBRAL),
                 gbm = sum(y == 1), okGbm = sum(y == 1 & p >= UMBRAL))
  pr <- RESUMEN$panel_reducido
  if (is.null(pr)) {
    return(tags$script(HTML(paste0("window.DATOS_BANDA = ",
                                   jsonlite::toJSON(list(plinko = plinko), auto_unbox = TRUE), ";"))))
  }
  imp <- RESUMEN$importancia
  imp_xgb <- stats::setNames(vapply(imp, function(f) f$xgboost, numeric(1)),
                             vapply(imp, function(f) f$variable, character(1)))
  a <- asociacion_con_grado("train")
  quedan <- lapply(unlist(pr$variables), function(v) {
    rho <- a$rho[a$variable == v]
    list(nombre = if (v == "Age_at_diagnosis") "Edad" else v, imp = unname(imp_xgb[v]),
         lado = if (length(rho) && rho < 0) "g" else "l")
  })
  panel <- list(quedan = quedan,
                rotulo = paste0("AUC ", num(pr$auc_k, 3), " con ", pr$k_min, " · ",
                                num(pr$auc_23, 3), " con 23"))
  tags$script(HTML(paste0("window.DATOS_BANDA = ",
                          jsonlite::toJSON(list(plinko = plinko, panel = panel), auto_unbox = TRUE),
                          ";")))
}

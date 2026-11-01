# =========================================================================== #
# Glía · interfaz y conversación
#
# Versión en Shiny de utils/asistente_panel.py del tablero de Dash, con el
# mismo marcado (ids y clases), de modo que www/glia.js (el dibujo de Glía) y
# los estilos de www/style.css son los del tablero sin cambios.
#
# Reparto entre navegador y servidor:
# - Abrir, cerrar, Escape, foco, la pista de cada pestaña y mandar una
#   pregunta van en www/asistente.js; el dibujo y los estados de Glía, en
#   www/glia.js.
# - Responder va en el servidor (server_asistente), porque necesita los
#   modelos y, si hay clave, la API de Gemini, que nunca debe tocar el
#   navegador. La respuesta vuelve como HTML con el mensaje "glia-respuesta".
# - Los botones «Ir a ...» llevan data-ir, así que los atiende
#   www/navegacion.js como cualquier otro enlace a una pestaña.
# =========================================================================== #

# Mensajes que se guardan como máximo por sesión
MAX_MENSAJES_GLIA <- 40

BIENVENIDA_GLIA <- list(
  rol = "asistente", inicial = TRUE, fuente = "guiada",
  texto = paste("Puedo **explicarte los resultados**, **definir conceptos**, **interpretar tu",
                "predicción** y **llevarte a cualquier pestaña**. Elige una pregunta, escríbeme",
                "o pulsa «Pregúntale a Glía» junto a cualquier gráfico.")
)

ETIQUETA_FUENTE_GLIA <- list(
  ia = c("auto_awesome", "Respondido con IA"),
  prediccion = c("calculate", "Calculado con los modelos")
)

# Markdown -> HTML sin tocar las fórmulas: los $...$ se apartan antes, porque
# Markdown leería sus guiones bajos o asteriscos como cursivas.
markdown_glia <- function(texto) {
  formulas <- regmatches(texto, gregexpr("\\$[^$]+\\$", texto))[[1]]
  for (i in seq_along(formulas)) {
    texto <- sub(formulas[i], paste0("GLIAFORMULA", i, "X"), texto, fixed = TRUE)
  }
  html <- commonmark::markdown_html(texto, extensions = FALSE)
  for (i in seq_along(formulas)) {
    # Se escapa como texto HTML (por si una fórmula lleva < o &)
    html <- sub(paste0("GLIAFORMULA", i, "X"), htmltools::htmlEscape(formulas[i]), html, fixed = TRUE)
  }
  # Los enlaces, en otra pestaña
  gsub("<a href=", "<a target=\"_blank\" rel=\"noopener noreferrer\" href=", html, fixed = TRUE)
}

avatar_glia <- function(tono = NULL, fuerza = 0, tamano = 28) {
  tags$canvas(class = "glia-avatar", width = tamano, height = tamano,
              `data-tono` = tono %||% "", `data-fuerza` = as.character(fuerza %||% 0),
              `aria-hidden` = "true")
}

# Un mensaje. Los de Glía admiten Markdown y fórmulas en LaTeX.
globo_glia <- function(m) {
  if (identical(m$rol, "usuario")) {
    return(div(class = "asistente-mensaje asistente-mensaje-usuario", div(class = "asistente-globo", m$texto)))
  }
  pie <- list()
  if (!is.null(m$ir) && m$ir %in% ORDEN) {
    pie <- c(pie, list(tags$button(type = "button", class = "asistente-ir", `data-ir` = m$ir,
                                   paste("Ir a", ETIQUETAS[[m$ir]]), icono("arrow_forward", "ms-1"))))
  }
  fuente <- ETIQUETA_FUENTE_GLIA[[m$fuente %||% ""]]
  if (!is.null(fuente)) {
    texto <- if (!is.null(m$proveedor)) paste("Respondido con", m$proveedor) else fuente[2]
    pie <- c(pie, list(span(class = "asistente-fuente", icono(fuente[1], "me-1"), texto)))
  }
  div(
    class = "asistente-mensaje asistente-mensaje-bot",
    span(class = "asistente-avatar", avatar_glia(m$tono, m$fuerza)),
    div(class = "asistente-globo",
        if (!is.null(m$aviso)) div(class = "asistente-aviso-ia", icono("info", "me-1"), m$aviso),
        div(class = "asistente-markdown", HTML(markdown_glia(m$texto))),
        if (length(pie)) div(class = "asistente-pie-mensaje", pie))
  )
}

modo_glia <- function() if (ia_disponible()) "Tu guía por la aplicación · con Gemini" else "Tu guía por la aplicación"

# Botón flotante, pista y panel (el mismo marcado que en Dash)
glia_ui <- function() {
  div(
    id = "asistente", class = "asistente",
    # Pista de cada pestaña: la escribe el servidor y la enseña asistente.js
    div(class = "glia-pista", `aria-live` = "polite",
        tags$button(id = "glia-pista", class = "glia-pista-texto", type = "button"),
        tags$button(class = "glia-pista-cerrar", type = "button", title = "Descartar",
                    `aria-label` = "Descartar la pista", icono("close"))),
    tags$button(
      id = "asistente-abrir", class = "asistente-fab", type = "button",
      title = "Pregúntale a Glía, la asistente de la aplicación",
      `aria-label` = "Abrir a Glía, la asistente de la aplicación", `aria-expanded` = "false",
      `aria-controls` = "asistente-panel",
      tags$canvas(id = "glia-fab", class = "glia-fab-lienzo", width = 42, height = 42, `aria-hidden` = "true"),
      span(class = "asistente-fab-texto", "Pregúntale a Glía")
    ),
    tags$section(
      id = "asistente-panel", class = "asistente-panel", role = "dialog",
      `aria-label` = "Glía, la asistente de la aplicación", `aria-hidden` = "true",
      # Partículas que derivan al fondo del panel (glia.js)
      tags$canvas(id = "asistente-fondo", class = "asistente-fondo", `aria-hidden` = "true"),
      tags$button(id = "asistente-cerrar", class = "asistente-cerrar", type = "button", title = "Cerrar",
                  `aria-label` = "Cerrar a Glía", icono("close")),
      # Glía preside el panel; al conversar se encoge (clase `conversando`)
      div(class = "asistente-heroe",
          tags$canvas(id = "glia-heroe", class = "glia-heroe-lienzo", width = 84, height = 84,
                      `aria-hidden` = "true"),
          div(class = "asistente-cabecera-texto",
              div(class = "asistente-titulo", "Glía"),
              div(class = "asistente-lema", "Los gliomas nacen de células como yo. Te ayudo a leer esta aplicación."),
              div(id = "asistente-modo", class = "asistente-modo", modo_glia()))),
      div(id = "asistente-mensajes", class = "asistente-mensajes", role = "log", `aria-live` = "polite",
          globo_glia(BIENVENIDA_GLIA)),
      div(id = "asistente-escribiendo", class = "asistente-escribiendo", `aria-hidden` = "true",
          span(), span(), span(), tags$em(class = "asistente-escribiendo-texto", "Glía está pensando…")),
      div(id = "asistente-sugerencias", class = "asistente-sugerencias"),
      div(class = "asistente-formulario",
          tags$input(id = "asistente-entrada", type = "text", class = "asistente-entrada",
                     placeholder = "Pregúntale a Glía…", maxlength = MAX_PREGUNTA, autocomplete = "off",
                     `aria-label` = "Tu pregunta para Glía"),
          tags$button(id = "asistente-enviar", class = "asistente-enviar", type = "button", title = "Enviar",
                      `aria-label` = "Enviar la pregunta", icono("send"))),
      div(class = "asistente-nota", "Herramienta docente: no sustituye un diagnóstico médico.")
    )
  )
}

# Botones de las tres preguntas sugeridas de una pestaña
sugerencias_html <- function(pestana) {
  s <- sugerencias_glia(pestana)
  as.character(tagList(lapply(names(s), function(clave) {
    tags$button(type = "button", class = "asistente-sugerencia", `data-clave` = clave, s[[clave]])
  })))
}

# `pestana`: reactivo con la pestaña activa. `estado_pred`: reactivo con el
# perfil de Predicción (NULL hasta que se visita esa pestaña).
server_asistente <- function(input, output, session, pestana, estado_pred) {
  historial <- reactiveVal(list(BIENVENIDA_GLIA))

  # Tres preguntas pensadas para la pestaña y la pista que Glía ofrece al
  # entrar (no en la introducción: allí se acaba de llegar de la portada)
  observe({
    p <- pestana()
    s <- sugerencias_glia(p)
    session$sendCustomMessage("glia-sugerencias", list(
      html = sugerencias_html(p),
      pista = if (identical(p, "tab-introduccion")) "" else paste0("Pregúntame: «", s[[1]], "»")
    ))
  })

  responder_y_pintar <- function(pregunta, calcular) {
    h <- historial()
    respuesta <- tryCatch(calcular(h), error = function(e) {
      message("Glía: error al responder (", conditionMessage(e), ")")
      respuesta_glia(SIN_RESPUESTA)
    })
    usuario <- list(rol = "usuario", texto = substr(pregunta, 1, MAX_PREGUNTA))
    bot <- c(list(rol = "asistente"), respuesta[setdiff(names(respuesta), "navegar")])
    historial(utils::tail(c(h, list(usuario, bot)), MAX_MENSAJES_GLIA))
    session$sendCustomMessage("glia-respuesta", list(
      html = as.character(tagList(globo_glia(usuario), globo_glia(bot))),
      modo = modo_glia()
    ))
    # «Llévame a ...»: se cambia de pestaña como con cualquier enlace
    if (isTRUE(respuesta$navegar) && !is.null(respuesta$ir) && !identical(respuesta$ir, pestana())) {
      session$sendCustomMessage("ir-a-pestana", respuesta$ir)
    }
  }

  observeEvent(input$glia_pregunta, {
    texto <- trimws(input$glia_pregunta$texto %||% "")
    if (!nzchar(texto)) return()
    responder_y_pintar(texto, function(h) responder_glia(texto, h, pestana(), estado_pred()))
  })

  observeEvent(input$glia_sugerencia, {
    clave <- input$glia_sugerencia$clave %||% ""
    if (!clave %in% unlist(SUGERENCIAS_GLIA)) return()
    responder_y_pintar(texto_sugerencia(clave), function(h) responder_sugerencia(clave, estado_pred()))
  })

  observeEvent(input$glia_grafico, {
    titulo <- input$glia_grafico$titulo %||% ""
    if (!nzchar(titulo)) return()
    nota <- input$glia_grafico$nota %||% ""
    responder_y_pintar(paste0("Explícame el gráfico «", titulo, "»"),
                       function(h) explicar_grafico(titulo, nota, h, pestana(), estado_pred()))
  })
}

# =========================================================================== #
# Servidor: enruta la pestaña activa y registra las salidas de cada pestaña
#
# input$pestana lo envía www/navegacion.js al pulsar cualquier elemento con
# data-ir="tab-...". Las salidas de todas las pestañas se registran una vez;
# Shiny solo calcula las que están en pantalla.
# =========================================================================== #

function(input, output, session) {
  pestana <- reactiveVal(TAB_INICIAL)

  observeEvent(input$pestana, {
    if (input$pestana %in% ORDEN) pestana(input$pestana)
  })

  output$contenido <- renderUI({
    constructor <- CONSTRUCTORES[[pestana()]]
    tryCatch(constructor(), error = aviso_error)
  })

  server_contexto(input, output, session)
  server_problema(input, output, session)
  server_exploracion(input, output, session)
  server_resultados(input, output, session)
  server_modelos(input, output, session)
  server_prediccion(input, output, session)
}

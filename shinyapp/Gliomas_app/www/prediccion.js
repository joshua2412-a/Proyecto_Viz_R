/* ==========================================================================
   Predicción: al cargar un perfil, los genes se encienden en cascada.

   server_prediccion() envía el mensaje "cargando_perfil" justo antes de
   actualizar las casillas; aquí se marca el formulario un momento y el CSS
   (www/shiny.css) escalona la animación de cada ficha.
   ========================================================================== */
(function registrar() {
  "use strict";
  if (!window.Shiny || !window.Shiny.addCustomMessageHandler) {
    window.setTimeout(registrar, 50);
    return;
  }
  var temporizador = null;
  window.Shiny.addCustomMessageHandler("cargando_perfil", function (_mensaje) {
    var f = document.querySelector(".formulario-prediccion");
    if (!f) return;
    f.classList.remove("cargando-perfil");
    void f.offsetWidth;
    f.classList.add("cargando-perfil");
    window.clearTimeout(temporizador);
    temporizador = window.setTimeout(function () {
      f.classList.remove("cargando-perfil");
    }, 1800);
  });
})();

/* ==========================================================================
   Fórmulas: MathJax compone el LaTeX ($...$) de lo que Shiny acaba de montar

   Cada pestaña llega con renderUI, después de que MathJax pasara por la
   página; sin esto, las fórmulas de las pestañas se verían como texto plano.

   Dos detalles:
   - Shiny lanza shiny:value con jQuery, y un evento lanzado así no llega a
     document.addEventListener: hay que escucharlo con $(document).on(...).
     (Con addEventListener, este archivo no componía nada.)
   - renderUI puede insertar el contenido algo después del evento (espera a
     sus dependencias), así que se compone varias veces. MathJax salta lo que
     ya está compuesto, así que repetir no cuesta nada.
   ========================================================================== */
(function () {
  "use strict";
  function componer(id) {
    if (!window.MathJax || !window.MathJax.Hub) return;
    var destino = (id && document.getElementById(id)) || document.body;
    window.MathJax.Hub.Queue(["Typeset", window.MathJax.Hub, destino]);
  }
  function escuchar() {
    if (!window.jQuery) { setTimeout(escuchar, 50); return; }
    window.jQuery(document).on("shiny:value", function (evento) {
      var id = evento.name || (evento.target && evento.target.id);
      [0, 250, 900, 2500].forEach(function (ms) { setTimeout(function () { componer(id); }, ms); });
    });
  }
  escuchar();
})();

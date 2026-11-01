/* ==========================================================================
   Fórmulas: MathJax compone el LaTeX ($...$) de lo que Shiny acaba de montar

   Cada pestaña llega con renderUI, después de que MathJax pasara por la
   página; sin esto, las fórmulas de las pestañas se verían como texto plano.
   ========================================================================== */
(function () {
  "use strict";
  document.addEventListener("shiny:value", function (evento) {
    if (!window.MathJax || !window.MathJax.Hub) return;
    // Shiny inserta el contenido después de este evento: se espera un ciclo
    setTimeout(function () {
      var destino = document.getElementById(evento.target.id) || document.body;
      window.MathJax.Hub.Queue(["Typeset", window.MathJax.Hub, destino]);
    }, 0);
  });
})();

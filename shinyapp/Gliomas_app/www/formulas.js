/* ==========================================================================
   Fórmulas: MathJax compone el LaTeX ($...$) de lo que Shiny acaba de montar

   Las pestañas guardadas no se componen al cargar la página (lo oculto se
   compone mal y cuesta): se componen al abrirlas. Y las salidas de texto de
   Shiny llegan después de que MathJax pasara por la página.

   - Shiny lanza shiny:value con jQuery, y un evento lanzado así no llega a
     document.addEventListener: se escucha con $(document).on(...).
   - renderUI inserta el contenido algo después del evento, así que se espera
     a que el contenido cambie (MutationObserver) y se compone una sola vez.
     Componer varias veces la pestaña entera congelaba la página: medido en
     Resultados, hasta 2,8 s sin pintar un solo fotograma.
   - Solo se compone si hay fórmulas, y nunca dentro de un gráfico.
   ========================================================================== */
(function () {
  "use strict";

  function tieneFormulas(el) {
    var t = el.textContent || "";
    return t.indexOf("$") >= 0 || t.indexOf("\\(") >= 0;
  }

  function componer(el, intentos) {
    if (!el || !el.isConnected || !tieneFormulas(el)) return;
    // MathJax se carga con defer y puede llegar después que la primera pestaña
    if (!window.MathJax || !window.MathJax.Hub) {
      if ((intentos || 0) < 40) setTimeout(function () { componer(el, (intentos || 0) + 1); }, 150);
      return;
    }
    window.MathJax.Hub.Queue(["Typeset", window.MathJax.Hub, el]);
  }

  // Compone el elemento cuando su contenido nuevo ya está en la página
  function alLlegar(el) {
    if (el.__esperaFormulas) return;
    var hecho = false;
    var terminar = function () {
      if (hecho) return;
      hecho = true;
      observador.disconnect();
      clearTimeout(respaldo);
      el.__esperaFormulas = false;
      // Un respiro: que el navegador pinte primero el contenido
      requestAnimationFrame(function () { setTimeout(function () { componer(el); }, 0); });
    };
    var calma = null;
    var observador = new MutationObserver(function () {
      clearTimeout(calma);
      calma = setTimeout(terminar, 40);
    });
    observador.observe(el, { childList: true, subtree: true });
    var respaldo = setTimeout(terminar, 1500);
    el.__esperaFormulas = true;
  }

  // Las pestañas esperan ocultas en #pestanas-guardadas (tex2jax_ignore) y
  // se componen la primera vez que se abren (navegacion.js avisa)
  document.addEventListener("pestana-mostrada", function (evento) {
    var panel = evento.detail && evento.detail.panel;
    if (!panel || panel.__formulas) return;
    panel.__formulas = true;
    requestAnimationFrame(function () { setTimeout(function () { componer(panel); }, 0); });
  });

  function escuchar() {
    if (!window.jQuery) { setTimeout(escuchar, 50); return; }
    window.jQuery(document).on("shiny:value", function (evento) {
      var el = document.getElementById(evento.name);
      // Solo las salidas de HTML o texto: los gráficos no llevan LaTeX
      if (!el || !(el.classList.contains("shiny-html-output") || el.classList.contains("shiny-text-output"))) return;
      alLlegar(el);
    });
  }
  escuchar();
})();

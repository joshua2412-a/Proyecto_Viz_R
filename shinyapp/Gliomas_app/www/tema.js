/* ==========================================================================
   Modo claro / oscuro

   Se carga desde ui.R (www/tema.js). Es el mismo archivo del tablero de Dash,
   con los pocos ajustes que pide Shiny señalados en su sitio.

   El atributo data-tema="claro|oscuro" de <html> lo pone ya un script
   mínimo del <head> (app.index_string en app.py), antes del primer pintado.
   Aquí se atiende el botón de la barra superior y se sigue al sistema:

   - Mientras no pulses el botón, el tablero va como tu sistema operativo y
     cambia con él.
   - Al pulsarlo, tu elección se guarda en este navegador (localStorage) y
     manda sobre el sistema.

   Los colores están en style.css (bloque «Modo oscuro»): tokens en
   [data-tema="oscuro"] y, para los gráficos de Plotly, reglas que cambian
   las tintas neutras del SVG. Los colores de LGG y GBM no cambian.
   ========================================================================== */

(function () {
  "use strict";

  var raiz = document.documentElement;
  var sistema = window.matchMedia ? window.matchMedia("(prefers-color-scheme: dark)") : null;

  function guardado() {
    try { return localStorage.getItem("tema"); } catch (e) { return null; }
  }

  function actual() {
    return raiz.getAttribute("data-tema") === "oscuro" ? "oscuro" : "claro";
  }

  function rotular() {
    var boton = document.getElementById("boton-tema");
    if (!boton) return;
    var oscuro = actual() === "oscuro";
    var texto = oscuro ? "Cambiar a modo claro" : "Cambiar a modo oscuro";
    boton.setAttribute("aria-label", texto);
    boton.setAttribute("title", texto);
    boton.setAttribute("aria-pressed", String(oscuro));
  }

  function aplicar(tema) {
    if (tema === actual()) { rotular(); return; }
    // Sin transiciones durante el cambio: cada tarjeta y botón tiene las
    // suyas (0,15 s en el fondo o el borde) y, sin esto, la página se
    // repintaba a trozos en vez de cambiar de golpe.
    raiz.classList.add("tema-cambiando");
    raiz.setAttribute("data-tema", tema);
    rotular();
    window.requestAnimationFrame(function () {
      window.requestAnimationFrame(function () { raiz.classList.remove("tema-cambiando"); });
    });
  }

  document.addEventListener("click", function (evento) {
    if (!evento.target.closest("#boton-tema")) return;
    var nuevo = actual() === "oscuro" ? "claro" : "oscuro";
    try { localStorage.setItem("tema", nuevo); } catch (e) {}
    aplicar(nuevo);
  });

  // Seguir al sistema mientras no haya una elección guardada
  if (sistema) {
    var seguir = function (e) { if (!guardado()) aplicar(e.matches ? "oscuro" : "claro"); };
    if (sistema.addEventListener) sistema.addEventListener("change", seguir);
    else if (sistema.addListener) sistema.addListener(seguir);
  }

  // El botón lo monta React después de este archivo: se rotula cuando aparece
  new MutationObserver(function (cambios, obs) {
    if (document.getElementById("boton-tema")) { rotular(); obs.disconnect(); }
  }).observe(document.body, { childList: true, subtree: true });
  rotular();
})();

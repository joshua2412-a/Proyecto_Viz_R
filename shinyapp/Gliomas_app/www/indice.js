/* ==========================================================================
   Índice desplegable

   Se carga desde ui.R (www/indice.js).

   El índice es un <details> nativo, que ya se abre y se cierra solo. Esto
   añade lo que un desplegable necesita y <details> no trae: cerrarse al
   elegir una pestaña, al pulsar Escape y al hacer clic fuera de él. El cambio
   de pestaña en sí lo hace www/navegacion.js.

   El cierre se anima aquí (la apertura la anima style.css con
   @starting-style): <details> oculta el panel en el mismo instante en que
   pierde `open`, así que hay que encoger el panel ANTES de quitarlo. Es más
   corto que la apertura (140 frente a 200 ms): cerrar tiene que quitarse de
   en medio, no lucirse.
   ========================================================================== */

(function () {
  "use strict";

  var CIERRE = 140; // ms

  function indice() {
    return document.getElementById("indice");
  }

  function reducirMovimiento() {
    return (
      window.matchMedia &&
      window.matchMedia("(prefers-reduced-motion: reduce)").matches
    );
  }

  function cerrar(d) {
    var panel = d.querySelector(".indice-panel");
    if (d.dataset.cerrando) return;
    if (!panel || !panel.animate || reducirMovimiento()) {
      d.open = false;
      return;
    }
    d.dataset.cerrando = "1";
    var anim = panel.animate(
      [
        { opacity: 1, transform: "none" },
        { opacity: 0, transform: "translateY(-4px) scale(0.99)" }
      ],
      { duration: CIERRE, easing: "cubic-bezier(0.22, 1, 0.36, 1)" }
    );
    function fin() {
      d.open = false;
      delete d.dataset.cerrando;
    }
    anim.onfinish = fin;
    anim.oncancel = fin;
  }

  document.addEventListener("click", function (evento) {
    var d = indice();
    if (!d || !d.open) return;
    // Clic en el propio botón con el panel abierto: <details> lo cerraría de
    // golpe; se intercepta para cerrarlo con la misma animación.
    if (evento.target.closest(".indice-boton") && d.contains(evento.target)) {
      evento.preventDefault();
      cerrar(d);
      return;
    }
    if (evento.target.closest(".indice-item") || !d.contains(evento.target)) {
      cerrar(d);
    }
  });

  document.addEventListener("keydown", function (evento) {
    var d = indice();
    if (evento.key !== "Escape" || !d || !d.open) return;
    cerrar(d);
    d.querySelector("summary").focus();
  });
})();

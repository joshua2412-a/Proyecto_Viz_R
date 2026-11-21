/* ==========================================================================
   Gráficos de Plotly: se dibujan cuando hacen falta, como en el tablero de Dash

   Es la misma idea que assets/graficos.js del Dash, adaptada a Shiny (aquí se
   envuelve el renderValue de la vinculación de Plotly con Shiny en vez de
   Plotly.react).

   Por qué: Shiny dibuja de golpe todos los gráficos que llegan en un mismo
   mensaje. En Resultados son nueve, y el navegador pasaba más de 2 s sin
   pintar un fotograma: la banda de partículas se quedaba congelada.

   Qué hace:
   - Los gráficos que están a la vista (o a punto de estarlo) se dibujan ya, de
     uno en uno y de arriba abajo, con un fotograma entre uno y otro.
   - Los que quedan lejos de la pantalla no se dibujan todavía: se reserva su
     alto (para que la página no salte después) y se dibujan cuando se acercan
     a la vista (a 1.000 px, antes de que lleguen) o en los ratos libres del
     navegador, uno cada vez, a partir de 1,6 s de entrar en la pestaña, cuando
     ya terminó el viaje de las partículas.
   - Lo que llega a una pestaña que ya no se ve (se cambió antes de que
     terminara de llegar) espera a que esa pestaña vuelva a abrirse.
   - Al volver a una pestaña solo se reajustan los gráficos cuyo tamaño cambió
     mientras estaba oculta (navegacion.js no lanza el reajuste general de
     Shiny, que redibujaba todos los gráficos de la pestaña).
   ========================================================================== */
(function () {
  "use strict";

  // Los gráficos no le piden a Shiny su tamaño ni los colores del tema (las
  // figuras son las mismas a cualquier ancho y el modo oscuro lo resuelve el
  // CSS). Shiny 1.14 vigila cada salida que los pide y, al cambiar de
  // pestaña, lee el tamaño y los estilos de cada gráfico uno detrás de otro;
  // en la app publicada se midieron, alguna vez al volver a Resultados, hasta
  // 0,9 s de recálculo de estilos dentro de esa rutina de Shiny. Se quitan
  // las clases antes de que Shiny enlace las salidas (este archivo va con
  // defer, así que corre antes).
  var conTema = document.querySelectorAll(".html-widget-output.shiny-report-size, .html-widget-output.shiny-report-theme");
  for (var c = 0; c < conTema.length; c++) {
    conTema[c].classList.remove("shiny-report-size", "shiny-report-theme");
  }

  var MARGEN = 1000;          // px: se dibuja antes de entrar en la vista
  var ESPERA_LIBRE = 1600;    // ms desde que se entró en la pestaña

  var cola = [];              // { el, hacer } pendientes
  var programado = false, libre = null;
  var ultimo = 0;             // cuándo se entró en la pestaña o llegó algo lejano
  var observador = "IntersectionObserver" in window ?
    new IntersectionObserver(function (entradas) {
      var alguno = false;
      entradas.forEach(function (e) { e.target.__cerca = e.isIntersecting; if (e.isIntersecting) alguno = true; });
      if (alguno) programar();
    }, { rootMargin: MARGEN + "px 0px" }) : null;

  function oculto(el) {
    return !el.isConnected || !!el.closest("#pestanas-guardadas") || el.offsetParent === null;
  }

  // A la vista o a menos de 1.000 px (lo marca el observador)
  function cerca(el) {
    if (el.__cerca) return true;
    var r = el.getBoundingClientRect();
    return r.top < window.innerHeight + MARGEN * 0.25 && r.bottom > -MARGEN;
  }

  // El primero en la página de los que cumplen la condición
  function primero(cumple) {
    var mejor = -1;
    for (var i = 0; i < cola.length; i++) {
      var el = cola[i].el;
      if (oculto(el) || !cumple(el)) continue;
      if (mejor < 0 || (cola[mejor].el.compareDocumentPosition(el) & Node.DOCUMENT_POSITION_PRECEDING)) mejor = i;
    }
    return mejor;
  }

  function hacer(i) {
    var tarea = cola.splice(i, 1)[0];
    if (observador) observador.unobserve(tarea.el);
    if (tarea.reservado) tarea.el.style.minHeight = "";
    try { tarea.hacer(); } catch (e) { if (window.console) console.error(e); }
  }

  function programar() {
    if (programado) return;
    programado = true;
    // Un fotograma pintado antes de cada gráfico
    requestAnimationFrame(function () { setTimeout(siguiente, 0); });
  }

  function siguiente() {
    programado = false;
    cola = cola.filter(function (t) { return t.el.isConnected; });
    var i = primero(cerca);
    if (i >= 0) {
      hacer(i);
      if (primero(cerca) >= 0) { programar(); return; }
    }
    vaciarEnRatosLibres();
  }

  // Los lejanos, en los ratos libres y después del viaje de las partículas
  function vaciarEnRatosLibres() {
    clearTimeout(libre);
    if (primero(function () { return true; }) < 0) return;
    var falta = ESPERA_LIBRE - (performance.now() - ultimo);
    libre = setTimeout(function () {
      var pedir = window.requestIdleCallback || function (f) { return setTimeout(f, 50); };
      pedir(function () {
        var i = primero(function () { return true; });
        if (i >= 0) hacer(i);
        libre = setTimeout(vaciarEnRatosLibres, 120);
      }, { timeout: 2000 });
    }, Math.max(falta, 0));
  }

  // Reservar el alto que tendrá, para que la página no salte al dibujarlo (y
  // para que los de abajo no parezcan cerca solo por medir 0 px)
  function reservar(tarea) {
    var el = tarea.el;
    if (tarea.reservado || !tarea.alto || el.offsetHeight || oculto(el)) return;
    el.style.minHeight = tarea.alto + "px";
    tarea.reservado = true;
  }

  function encolar(el, hacerlo, alto) {
    // Si ya había algo pendiente para este gráfico, manda lo último
    cola = cola.filter(function (t) { return t.el !== el; });
    var tarea = { el: el, hacer: hacerlo, alto: alto };
    reservar(tarea);
    cola.push(tarea);
    if (observador) observador.observe(el);
    programar();
  }

  // Envuelve el renderValue de la vinculación de Plotly con Shiny
  function parchear() {
    if (!window.Shiny || !Shiny.outputBindings || !Shiny.outputBindings.getBindings) return false;
    var lista = Shiny.outputBindings.getBindings();
    for (var i = 0; i < lista.length; i++) {
      var b = lista[i].binding;
      if (!b || b.name !== "plotly") continue;
      if (b.__diferido) return true;
      var original = b.renderValue;
      b.renderValue = function (el, datos) {
        var self = this;
        var lay = datos && datos.x && datos.x.layout;
        encolar(el, function () { original.call(self, el, datos); }, (lay && lay.height) || 400);
      };
      b.__diferido = true;
      return true;
    }
    return false;
  }
  (function esperar(n) {
    if (!parchear() && n > 0) setTimeout(function () { esperar(n - 1); }, 30);
  })(400);

  // Al abrir una pestaña empieza el viaje de las partículas: lo lejano espera
  // a que termine. Lo pendiente de la pestaña se dibuja, y se reajustan los
  // gráficos que cambiaron de tamaño mientras estaba oculta.
  document.addEventListener("pestana-mostrada", function (evento) {
    ultimo = performance.now();
    var panel = evento.detail && evento.detail.panel;
    if (!panel) return;
    cola.forEach(reservar);
    var graficos = panel.querySelectorAll(".js-plotly-plot");
    for (var i = 0; i < graficos.length; i++) {
      (function (g) {
        var fl = g._fullLayout;
        if (!fl || (Math.abs(fl.width - g.clientWidth) < 2 && Math.abs(fl.height - g.clientHeight) < 2)) return;
        encolar(g, function () {
          var adaptador = window.jQuery && window.jQuery(g).data("shiny-output-binding");
          if (adaptador && adaptador.onResize) adaptador.onResize();
          else if (window.Plotly) window.Plotly.Plots.resize(g);
        });
      })(graficos[i]);
    }
    programar();
  });
})();

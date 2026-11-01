/* ==========================================================================
   Contador de los indicadores (KPI)

   Se carga desde ui.R (www/contador.js).

   Que hace: cuando aparece una cifra de KPI en pantalla, la anima desde cero
   hasta su valor. Las pestanas se construyen bajo demanda, asi que los nodos
   no existen al cargar la pagina; por eso se vigila el DOM con un
   MutationObserver en vez de recorrerlo una sola vez al arrancar.

   Que NO hace: inventarse el formato. Se conserva el separador decimal, el
   numero de decimales y todo lo que venga detras de la cifra ("%", "pp",
   "de 23"...), de modo que el valor final es exactamente el que escribio
   Python. Si la cifra no empieza por un numero, se deja intacta.

   Accesibilidad: con prefers-reduced-motion la animacion no se ejecuta.
   ========================================================================== */

(function () {
  "use strict";

  var DURACION = 700; // ms

  function reducirMovimiento() {
    return (
      window.matchMedia &&
      window.matchMedia("(prefers-reduced-motion: reduce)").matches
    );
  }

  function suavizar(t) {
    // easeOutCubic: arranca rapido y frena al final
    return 1 - Math.pow(1 - t, 3);
  }

  // KPI que ya se contaron en esta visita. React vuelve a montar las
  // tarjetas cada vez que entras en una pestaña, y contar desde cero en cada
  // visita convertía una cifra que ya conoces en una espera (y durante 0,7 s
  // enseñaba valores intermedios falsos: «575» pacientes camino de 671). Se
  // cuenta la primera vez; al volver, la cifra aparece ya puesta. Las cifras
  // de Predicción (.cifra-animada) sí se cuentan siempre: cambian con cada
  // perfil, y ahí el recuento es la respuesta a lo que el usuario hizo.
  var vistas = {};

  function claveKpi(el, texto) {
    var tarjeta = el.closest(".kpi-card");
    return tarjeta ? tarjeta.textContent.replace(texto, "").trim() + "|" + texto : null;
  }

  function animar(el) {
    if (el.dataset.contado) return;
    el.dataset.contado = "1";

    var texto = el.textContent.trim();
    if (el.classList.contains("kpi-value")) {
      var clave = claveKpi(el, texto);
      if (clave) {
        if (vistas[clave]) return;
        vistas[clave] = true;
      }
    }
    var partido = texto.match(/^(-?\d+(?:[.,]\d+)?)([\s\S]*)$/);
    if (!partido) return; // no empieza por un numero: se deja como esta

    var crudo = partido[1];
    var resto = partido[2];
    var destino = parseFloat(crudo.replace(",", "."));
    if (!isFinite(destino)) return;

    var separador = crudo.indexOf(",") >= 0 ? "," : ".";
    var trozos = crudo.split(separador);
    var decimales = trozos.length > 1 ? trozos[1].length : 0;

    if (reducirMovimiento()) return;

    var inicio = null;
    var terminado = false;

    function terminar() {
      // El valor exacto lo pone Python, no el navegador
      terminado = true;
      el.textContent = texto;
    }

    function paso(marca) {
      if (terminado) return;
      if (inicio === null) inicio = marca;
      var t = Math.min((marca - inicio) / DURACION, 1);
      var valor = destino * suavizar(t);
      el.textContent = valor.toFixed(decimales).replace(".", separador) + resto;
      if (t < 1) window.requestAnimationFrame(paso);
      else terminar();
    }

    el.textContent = (0).toFixed(decimales).replace(".", separador) + resto;
    window.requestAnimationFrame(paso);

    // Red de seguridad: si el navegador no dibuja fotogramas (pestana en segundo
    // plano, panel oculto), requestAnimationFrame se congela y la cifra se
    // quedaria en 0 para siempre. Este temporizador garantiza el valor final.
    window.setTimeout(function () {
      if (!terminado) terminar();
    }, DURACION + 400);
  }

  function barrer() {
    var cifras = document.querySelectorAll(".kpi-value:not([data-contado]), .cifra-animada:not([data-contado])");
    for (var i = 0; i < cifras.length; i++) animar(cifras[i]);
  }

  function arrancar() {
    barrer();
    // Las pestanas se montan y desmontan: hay que seguir vigilando.
    //
    // El barrido es SINCRONO a proposito, sin debounce ni requestAnimationFrame:
    // el callback de un MutationObserver corre antes del siguiente pintado, asi
    // que la cifra se pone a cero antes de que el usuario llegue a ver el valor
    // final. Con un retardo, aparecia el valor completo y saltaba a 0 un instante
    // despues. Barrer es barato (un querySelectorAll) y cada cifra se marca con
    // data-contado, de modo que las mutaciones que provoca el propio contador no
    // encadenan trabajo.
    new MutationObserver(barrer).observe(document.body, {
      childList: true,
      subtree: true
    });
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", arrancar);
  } else {
    arrancar();
  }
})();

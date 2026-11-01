/* ==========================================================================
   Navegación entre pestañas

   Todo lo que lleva el atributo data-ir="tab-..." cambia de pestaña: las
   pestañas de arriba, el índice desplegable, los botones anterior/siguiente
   y las fichas de la guía de la portada. Este archivo:

     1. pinta al instante el estado de la navegación (pestaña activa, índice,
        barra de progreso, vecinos, barra superior fina), sin esperar al
        servidor, que es lo que en Dash hacían los callbacks de cliente;
     2. avisa a Shiny con input$pestana, y el servidor monta el contenido;
     3. vuelve arriba de la página, para no dejar al visitante a media pestaña.

   El orden y los nombres llegan en el JSON #datos-navegacion, que se genera
   desde modulos/navegacion.R: no hay una segunda copia que mantener.
   ========================================================================== */

(function () {
  "use strict";

  var NAV = null;
  var actual = null;

  function datos() {
    if (!NAV) {
      var nodo = document.getElementById("datos-navegacion");
      NAV = nodo ? JSON.parse(nodo.textContent) : { orden: [], etiquetas: {}, inicial: "" };
    }
    return NAV;
  }

  function pintar(destino) {
    var nav = datos();
    var posicion = nav.orden.indexOf(destino);
    if (posicion < 0) return;
    var total = nav.orden.length;
    var inicial = destino === nav.inicial;

    // Barra superior completa solo en la portada; fina en el resto
    var barra = document.getElementById("barra-superior");
    if (barra) barra.className = inicial ? "topbar" : "topbar topbar-fina";

    // Pestañas en la portada, índice en las secciones
    var navegacion = document.getElementById("barra-navegacion");
    if (navegacion) navegacion.className = inicial ? "navbar-tabs" : "navbar-tabs en-seccion";

    document.querySelectorAll(".main-tabs .nav-link").forEach(function (a) {
      var activa = a.getAttribute("data-ir") === destino;
      a.classList.toggle("active", activa);
      a.classList.toggle("main-tab-active", activa);
      a.setAttribute("aria-selected", activa ? "true" : "false");
    });

    document.querySelectorAll(".indice-item").forEach(function (b) {
      b.classList.toggle("indice-item-activo", b.getAttribute("data-ir") === destino);
    });

    var texto = function (id, valor) {
      var el = document.getElementById(id);
      if (el) el.textContent = valor;
    };
    texto("indice-posicion", (posicion + 1) + " de " + total);
    texto("indice-actual", nav.etiquetas[destino]);

    var progreso = document.getElementById("indice-progreso");
    if (progreso) progreso.style.width = ((posicion + 1) / total * 100).toFixed(1) + "%";

    var vecino = function (id, indice, rotulo, flechaDelante) {
      var b = document.getElementById(id);
      if (!b) return;
      var existe = indice >= 0 && indice < total;
      b.disabled = !existe;
      var nombre = existe ? nav.etiquetas[nav.orden[indice]] : rotulo;
      if (existe) b.setAttribute("data-ir", nav.orden[indice]);
      else b.removeAttribute("data-ir");
      b.innerHTML = flechaDelante
        ? '<span class="material-symbols-rounded me-1" aria-hidden="true">arrow_back</span>' + nombre
        : nombre + '<span class="material-symbols-rounded ms-1" aria-hidden="true">arrow_forward</span>';
    };
    vecino("indice-anterior", posicion - 1, "Inicio", true);
    vecino("indice-siguiente", posicion + 1, "Fin", false);
  }

  function ir(destino) {
    if (!destino || destino === actual) return;
    actual = destino;
    pintar(destino);
    if (window.Shiny && Shiny.setInputValue) {
      Shiny.setInputValue("pestana", destino, { priority: "event" });
    }
    // Suave solo cerca de arriba: desde el pie de una pestaña larga, el
    // desplazamiento suave recorría miles de píxeles justo mientras se monta
    // la pestaña nueva y se veía a saltos.
    var suave = !(window.matchMedia &&
      window.matchMedia("(prefers-reduced-motion: reduce)").matches) &&
      window.scrollY < window.innerHeight * 0.6;
    window.scrollTo({ top: 0, behavior: suave ? "smooth" : "auto" });
  }

  document.addEventListener("click", function (evento) {
    var origen = evento.target.closest("[data-ir]");
    if (!origen || origen.disabled) return;
    evento.preventDefault();
    ir(origen.getAttribute("data-ir"));
  });

  // El servidor también puede mandar a otra pestaña (no se usa hoy, pero así
  // un botón de cualquier módulo puede hacerlo sin conocer este archivo).
  document.addEventListener("DOMContentLoaded", function () {
    actual = datos().inicial;
    pintar(actual);
    if (window.Shiny && Shiny.addCustomMessageHandler) {
      Shiny.addCustomMessageHandler("ir-a-pestana", function (destino) {
        ir(destino);
      });
    }
  });
})();

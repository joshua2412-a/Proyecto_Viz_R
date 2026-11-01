/*
 * Glía: el asistente dibujado como un astrocito de partículas.
 *
 * Las gliomas nacen de las células gliales, así que el asistente es una: un
 * núcleo cálido y siete prolongaciones de partículas, con los mismos colores
 * y el mismo brillo que la portada (PALETA de intro.js) para que se lea como
 * otra forma de LAS MISMAS partículas.
 *
 * Qué hace este archivo (la conversación en sí la lleva asistente.js):
 *  - Dibuja a Glía en el botón flotante (#glia-fab) y en la cabecera del
 *    panel (#glia-heroe), animada, y en cada avatar de mensaje
 *    (canvas.glia-avatar), quieta.
 *  - Estados (A1): reposo (prolongaciones recogidas, respira), escuchando
 *    (se orientan hacia el campo de texto), pensando (señales que recorren
 *    las prolongaciones) y responde (se estira y vuelve al reposo).
 *  - Tinte (A2): el núcleo toma el azul LGG o el coral GBM según de qué grado
 *    habla la última respuesta (data-tono y data-fuerza en su avatar).
 *  - Fondo del panel: partículas que derivan despacio (#asistente-fondo).
 *  - Cuando la portada termina de recogerse, Glía se forma en su botón: las
 *    prolongaciones llegan girando desde fuera y se recogen en la célula.
 *
 * Solo se pinta lo que se ve: el botón cuando la portada ya no lo tapa y el
 * panel está cerrado; la cabecera y el fondo cuando el panel está abierto.
 *
 * Fluidez (medido en el navegador del autor, 30-oct): una primera versión
 * hacía viajar las partículas desde el cerebro en una capa a pantalla
 * completa justo durante la salida de la portada, y vigilaba todos los
 * cambios de clase del documento. La salida pasó a dar 35 tirones en 3 s.
 * Ahora nada de Glía se dibuja ni se observa durante esa transición: se
 * forma después, dentro de su propio lienzo de 42 px, y los observadores
 * miran solo el cuerpo (clase) y la raíz del asistente.
 */
(function () {
  "use strict";

  var AZUL = "#7DB4FF", CORAL = "#FF9472", EDEMA = "#F6C98E", BLANCO = "#FFFFFF";
  var quieto = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  // ---- forma: siete prolongaciones de cuatro partículas -------------------
  var BRAZOS = 7, TRAMOS = 4;

  function mezclar(a, b, t) {
    var pa = parseInt(a.slice(1), 16), pb = parseInt(b.slice(1), 16);
    var r = Math.round(((pa >> 16) & 255) * (1 - t) + ((pb >> 16) & 255) * t);
    var g = Math.round(((pa >> 8) & 255) * (1 - t) + ((pb >> 8) & 255) * t);
    var bl = Math.round((pa & 255) * (1 - t) + (pb & 255) * t);
    return "rgb(" + r + "," + g + "," + bl + ")";
  }

  function colorNucleo(tono, fuerza) {
    if (tono === "lgg") return mezclar(CORAL, AZUL, 0.55 + 0.45 * fuerza);
    if (tono === "gbm") return mezclar(CORAL, "#FF5E36", fuerza);
    return CORAL;
  }

  // Posiciones de las partículas para un tamaño S y unos parámetros:
  // largo (cuánto se extienden), hacia (ángulo al que se orientan, con
  // peso w), luz (0–1, señales encendidas), t (segundos).
  function particulas(S, p, t) {
    var R = S / 2, c = R, lista = [];
    var respira = quieto ? 1 : 1 + 0.035 * Math.sin(t * 1.3);
    // p.forma (0–1): al formarse, las prolongaciones llegan de fuera girando
    var f = p.forma == null ? 1 : p.forma;
    var fe = 1 - Math.pow(1 - f, 3);
    var largo = p.largo * respira * (1 + (1 - fe) * 1.6);
    var giro = (1 - fe) * 2.2;
    lista.push({ x: c, y: c, d: R * 0.44, color: colorNucleo(p.tono, p.fuerza) });
    lista.push({ x: c - R * 0.1, y: c + R * 0.08, d: R * 0.2, color: EDEMA });
    var tinteBrazo = p.tono === "gbm" ? CORAL : null;
    for (var b = 0; b < BRAZOS; b++) {
      var ang = b / BRAZOS * Math.PI * 2 + 0.35 + (b % 2) * 0.12 + giro;
      if (p.w > 0.001) {
        var dif = Math.atan2(Math.sin(p.hacia - ang), Math.cos(p.hacia - ang));
        ang += dif * 0.38 * p.w;
      }
      var encendida = -1;
      if (p.luz > 0.01) encendida = Math.floor((t * 5 + b * 0.7) % (TRAMOS + 2));
      for (var k = 1; k <= TRAMOS; k++) {
        var r = R * (0.23 + k * 0.155 * largo);
        var curva = Math.sin(k * 0.9 + b) * 0.12 + (quieto ? 0 : Math.sin(t * 0.8 + b) * 0.04);
        var d = Math.max(R * (0.17 - k * 0.028), 1.4);
        var color = AZUL;
        if (tinteBrazo && ((b * 4 + k) * 37 % 100) / 100 < p.fuerza * 0.85) color = tinteBrazo;
        if (encendida === k) color = mezclar(color, BLANCO, p.luz);
        lista.push({ x: c + Math.cos(ang + curva) * r, y: c + Math.sin(ang + curva) * r, d: d, color: color });
      }
    }
    return lista;
  }

  function pintarParticula(ctx, x, y, d, color) {
    ctx.shadowColor = color;
    ctx.shadowBlur = d * 1.6;
    ctx.fillStyle = color;
    ctx.beginPath();
    ctx.arc(x, y, d / 2, 0, Math.PI * 2);
    ctx.fill();
  }

  // Ajusta el lienzo a su tamaño en pantalla y a la densidad de píxeles
  function preparar(lienzo) {
    var S = lienzo.clientWidth || parseInt(lienzo.getAttribute("width"), 10) || 40;
    var dpr = Math.min(window.devicePixelRatio || 1, 2);
    if (lienzo._S !== S || lienzo._dpr !== dpr) {
      lienzo.width = Math.round(S * dpr);
      lienzo.height = Math.round(S * dpr);
      lienzo._S = S; lienzo._dpr = dpr;
    }
    var ctx = lienzo.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, S, S);
    ctx.globalCompositeOperation = "lighter";
    return { ctx: ctx, S: S };
  }

  function pintarGlia(lienzo, p, t) {
    if (!lienzo || !lienzo.isConnected) return;
    var l = preparar(lienzo);
    var lista = particulas(l.S, p, t);
    l.ctx.globalAlpha = p.forma == null ? 1 : Math.min(1, p.forma * 1.4);
    for (var i = 0; i < lista.length; i++) pintarParticula(l.ctx, lista[i].x, lista[i].y, lista[i].d, lista[i].color);
    l.ctx.globalAlpha = 1;
  }

  // ---- estado ---------------------------------------------------------------
  var OBJETIVOS = {
    reposo: { largo: 0.74, w: 0, luz: 0 },
    escuchando: { largo: 1.0, w: 1, luz: 0 },
    pensando: { largo: 0.96, w: 0, luz: 1 },
    responde: { largo: 1.12, w: 0, luz: 0 }
  };
  var est = {
    estado: "reposo", hasta: 0,
    actual: { largo: 0.74, w: 0, luz: 0, hacia: Math.PI / 2, tono: null, fuerza: 0 },
    tono: null, fuerza: 0
  };

  function ponerEstado(nombre, duracion) {
    est.estado = nombre;
    est.hasta = duracion ? performance.now() + duracion : 0;
  }

  function tenir(tono, fuerza) {
    est.tono = tono || null;
    est.fuerza = tono ? Math.max(0, Math.min(1, fuerza == null ? 0.6 : fuerza)) : 0;
  }

  // ---- fondo del panel -----------------------------------------------------
  var fondo = [];
  function sembrarFondo() {
    var s = 11;
    var azar = function () { s = (s * 9301 + 49297) % 233280; return s / 233280; };
    for (var i = 0; i < 64; i++) {
      fondo.push({ x: azar(), y: azar(), d: 1 + azar() * 2, a: 0.22 + azar() * 0.35,
                   v: 0.004 + azar() * 0.01, color: azar() < 0.25 ? CORAL : AZUL, f: azar() * 6 });
    }
  }
  sembrarFondo();

  function pintarFondo(lienzo, t) {
    if (!lienzo || !lienzo.isConnected) return;
    var W = lienzo.clientWidth, H = lienzo.clientHeight;
    if (!W || !H) return;
    var dpr = Math.min(window.devicePixelRatio || 1, 2);
    if (lienzo._W !== W || lienzo._H !== H) {
      lienzo.width = Math.round(W * dpr); lienzo.height = Math.round(H * dpr);
      lienzo._W = W; lienzo._H = H;
    }
    var ctx = lienzo.getContext("2d");
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    ctx.clearRect(0, 0, W, H);
    ctx.globalCompositeOperation = "lighter";
    for (var i = 0; i < fondo.length; i++) {
      var p = fondo[i];
      var y = ((p.y - (quieto ? 0 : t * p.v)) % 1 + 1) % 1;
      var x = p.x + (quieto ? 0 : Math.sin(t * 0.3 + p.f) * 0.01);
      ctx.globalAlpha = p.a;
      pintarParticula(ctx, x * W, y * H, p.d, p.color);
    }
    ctx.globalAlpha = 1;
  }

  // ---- avatares de los mensajes (quietos) -------------------------------
  // Glía dorada para los chips de los gráficos en modo claro. Sobre blanco,
  // las partículas azul claro con mezcla aditiva casi no se ven y el disco
  // azul noche que las sostenía parecía un aro oscuro pegado a la tarjeta.
  // Se pinta sin disco y con tinta: núcleo coral (el de GBM en el tablero)
  // y prolongaciones doradas, del tono del edema de la portada oscurecido
  // hasta leerse sobre blanco.
  var DORADOS = ["#C8922E", "#D9A441", "#B98322"];
  function pintarGliaClara(lienzo) {
    if (!lienzo || !lienzo.isConnected) return;
    var l = preparar(lienzo);
    l.ctx.globalCompositeOperation = "source-over";
    var lista = particulas(l.S, { largo: 0.9, w: 0, luz: 0, hacia: 0, tono: null, fuerza: 0 }, 0);
    for (var i = 0; i < lista.length; i++) {
      var p = lista[i];
      var color = i === 0 ? "#E0785A" : i === 1 ? "#F2C46B" : DORADOS[i % DORADOS.length];
      l.ctx.shadowColor = i === 0 ? "rgba(224, 120, 90, 0.45)" : "rgba(217, 164, 65, 0.4)";
      l.ctx.shadowBlur = p.d * 0.9;
      l.ctx.fillStyle = color;
      l.ctx.beginPath();
      l.ctx.arc(p.x, p.y, p.d / 2, 0, Math.PI * 2);
      l.ctx.fill();
    }
  }
  function temaClaro() { return document.documentElement.getAttribute("data-tema") !== "oscuro"; }

  function pintarAvatares() {
    var avatares = document.querySelectorAll("canvas.glia-avatar:not([data-pintado])");
    var claro = temaClaro();
    for (var i = 0; i < avatares.length; i++) {
      var a = avatares[i];
      if (claro && a.closest(".glia-chip")) {
        pintarGliaClara(a);
      } else {
        var tono = a.getAttribute("data-tono") || null;
        var fuerza = parseFloat(a.getAttribute("data-fuerza") || "0");
        pintarGlia(a, { largo: 0.9, w: 0, luz: 0, hacia: 0, tono: tono, fuerza: fuerza }, 0);
      }
      a.setAttribute("data-pintado", "1");
    }
  }

  // Al cambiar de tema, los chips se repintan con su paleta
  new MutationObserver(function () {
    var chips = document.querySelectorAll(".glia-chip canvas.glia-avatar[data-pintado]");
    for (var i = 0; i < chips.length; i++) chips[i].removeAttribute("data-pintado");
    pintarAvatares();
  }).observe(document.documentElement, { attributes: true, attributeFilter: ["data-tema"] });

  // ---- bucle ----------------------------------------------------------------
  function panelAbierto() {
    var r = document.getElementById("asistente");
    return !!(r && r.classList.contains("asistente-abierto"));
  }
  function portadaVisible() {
    return !!document.getElementById("intro") && !document.body.classList.contains("intro-cerrada");
  }

  var formacion = { esperando: false, inicio: 0 };
  var t0 = performance.now();
  function fotograma(ahora) {
    requestAnimationFrame(fotograma);
    if (document.hidden) return;
    var t = (ahora - t0) / 1000;
    if (est.hasta && ahora > est.hasta) { est.estado = "reposo"; est.hasta = 0; }

    var obj = OBJETIVOS[est.estado] || OBJETIVOS.reposo, a = est.actual, k = quieto ? 1 : 0.09;
    a.largo += (obj.largo - a.largo) * k;
    a.w += (obj.w - a.w) * k;
    a.luz += (obj.luz - a.luz) * k;
    a.fuerza += (est.fuerza - a.fuerza) * (quieto ? 1 : 0.05);
    if (est.tono) a.tono = est.tono; else if (a.fuerza < 0.02) a.tono = null;

    if (formacion.inicio) {
      a.forma = Math.min(1, (ahora - formacion.inicio) / 900);
      if (a.forma >= 1) formacion.inicio = 0;
    }
    var abierto = panelAbierto();
    if (abierto) {
      pintarGlia(document.getElementById("glia-heroe"), a, t);
      pintarFondo(document.getElementById("asistente-fondo"), t);
    } else if (!portadaVisible() && !formacion.esperando) {
      pintarGlia(document.getElementById("glia-fab"), a, t);
    }
  }
  requestAnimationFrame(fotograma);

  // ---- señales de la conversación -------------------------------------------
  // Se observan el documento entero porque Dash monta y vuelve a montar los
  // nodos; cada rama filtra por id para no costar nada en el resto.
  var mensajesPrevios = 0;
  function revisarMensajes() {
    var lista = document.getElementById("asistente-mensajes");
    if (!lista) return;
    var n = lista.children.length;
    var panel = document.getElementById("asistente-panel");
    if (panel) panel.classList.toggle("conversando", n > 1);
    if (n > mensajesPrevios && mensajesPrevios > 0) {
      var ultimo = lista.lastElementChild;
      var avatar = ultimo && ultimo.querySelector("canvas.glia-avatar");
      if (avatar) {
        tenir(avatar.getAttribute("data-tono"), parseFloat(avatar.getAttribute("data-fuerza") || "0"));
        ponerEstado("responde", 1500);
      }
    }
    mensajesPrevios = n;
  }

  // Avatares nuevos (mensajes y chips de los gráficos): se pintan en un rato
  // libre, una vez por lote de cambios, sin recorrer los registros.
  var avataresPendientes = false;
  function pedirAvatares() {
    if (avataresPendientes) return;
    avataresPendientes = true;
    var hacer = function () { avataresPendientes = false; pintarAvatares(); };
    if (window.requestIdleCallback) window.requestIdleCallback(hacer, { timeout: 600 });
    else setTimeout(hacer, 120);
  }
  new MutationObserver(pedirAvatares).observe(document.body, { childList: true, subtree: true });

  // Solo la clase del <body>: aquí llegan las señales de la portada
  new MutationObserver(function () {
    if (document.body.classList.contains("intro-cerrada")) formar();
  }).observe(document.body, { attributes: true, attributeFilter: ["class"] });

  // La conversación: mensajes nuevos y «pensando», dentro del asistente
  function vigilarAsistente() {
    var raiz = document.getElementById("asistente");
    if (!raiz) { setTimeout(vigilarAsistente, 300); return; }
    new MutationObserver(function (cambios) {
      for (var i = 0; i < cambios.length; i++) {
        var c = cambios[i], t = c.target;
        if (c.type === "attributes" && t.id === "asistente-escribiendo") {
          if (t.classList.contains("asistente-escribiendo-activo")) ponerEstado("pensando");
          else if (est.estado === "pensando") ponerEstado("reposo");
        } else if (c.type === "childList" && t.id === "asistente-mensajes") {
          revisarMensajes();
        }
      }
    }).observe(raiz, { childList: true, subtree: true, attributes: true, attributeFilter: ["class"] });
  }
  vigilarAsistente();

  document.addEventListener("focusin", function (e) {
    if (e.target && e.target.id === "asistente-entrada" && est.estado !== "pensando") ponerEstado("escuchando");
  });
  document.addEventListener("focusout", function (e) {
    if (e.target && e.target.id === "asistente-entrada" && est.estado === "escuchando") ponerEstado("reposo");
  });

  // ---- Glía se forma al terminar la portada ------------------------------
  // Espera a que la portada se haya recogido del todo (intro-cerrada) y un
  // respiro más, para no competir con la explosión del cerebro ni con la
  // hélice que se forma en la banda. Después, todo ocurre en su lienzo.
  var formada = false;
  function raizAsistente() { return document.getElementById("asistente"); }
  function ocultarBoton() { var r = raizAsistente(); if (r) r.classList.add("glia-oculta"); }
  function mostrarBoton() { var r = raizAsistente(); if (r) r.classList.remove("glia-oculta"); }

  function formar() {
    if (formada) return;
    formada = true;
    formacion.esperando = true;
    setTimeout(function () {
      formacion.esperando = false;
      if (!quieto) { est.actual.forma = 0; formacion.inicio = performance.now(); }
      mostrarBoton();
    }, quieto ? 0 : 450);
  }

  (function esperarRaiz() {
    if (!raizAsistente()) { setTimeout(esperarRaiz, 200); return; }
    if (portadaVisible()) ocultarBoton();
    else formada = true;
    pintarAvatares();
  })();

  window.glia = { estado: ponerEstado, tenir: tenir };
})();

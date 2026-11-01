/* ==========================================================================
   Predicción · «Dónde cae este perfil», con partículas

   Se carga desde ui.R (www/enjambre.js). Es el mismo archivo del tablero de Dash,
   con los pocos ajustes que pide Shiny señalados en su sitio.

   Sustituye al gráfico de puntos de Plotly (fig_contexto) en la misma
   tarjeta. Cada punto es uno de los 168 pacientes de prueba, colocado en la
   probabilidad EXACTA de GBM que le da el modelo (x) y en la fila de su grado
   real; la altura dentro de la fila solo evita que se pisen (enjambre, nada
   aleatorio). Es un gráfico, no un adorno: los pacientes no brillan, hay eje
   y umbral, y la lectura va escrita debajo. Solo brilla el perfil.

   - Al cambiar de modelo, los 168 se desplazan a la probabilidad que les da
     el otro: se ve quién cruza el umbral.
   - Al predecir, el perfil viaja en arco de su sitio al nuevo y se encienden
     un momento los pacientes con una probabilidad parecida (±5 puntos).

   Lo alimenta el mensaje "pred-contexto" que envía server_prediccion()
   (modulos/pestanas/prediccion.R): el manejador del final llama a
   `window.enjambrePrediccion.actualizar({modelo, p})` y pone en la página la
   lectura que devuelve. Los datos de prueba vienen en `data-prueba` del
   contenedor. Como la pestaña se monta de nuevo en cada visita, si el
   contenedor cambia se vuelve a empezar con el nuevo.
   ========================================================================== */

(function () {
  "use strict";

  var reducir = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;
  // Colores de LGG y GBM validados para el fondo azul noche (los mismos que
  // la banda); tinta clara para el perfil y los rótulos
  var C = { lgg: "#4E8CD9", gbm: "#DA7454", tinta: "238, 243, 249", suave: "185, 199, 222", eje: "rgba(143, 184, 242, 0.28)" };
  var NOMBRE = { logistica: "la regresión logística", random_forest: "Random Forest", xgboost: "XGBoost" };

  // Curva fuerte para movimiento en pantalla (Emil Kowalski), tabulada
  var CURVA = (function () {
    var t = [];
    function bz(a, b, s) { return 3 * a * s * (1 - s) * (1 - s) + 3 * b * s * s * (1 - s) + s * s * s; }
    for (var k = 0; k <= 128; k++) {
      var x = k / 128, lo = 0, hi = 1, s = 0.5;
      for (var it = 0; it < 20; it++) { s = (lo + hi) / 2; if (bz(0.77, 0.175, s) < x) lo = s; else hi = s; }
      t.push(bz(0, 1, s));
    }
    return t;
  })();
  function curva(k) { if (k <= 0) return 0; if (k >= 1) return 1; var f = k * 128, i = Math.floor(f); return CURVA[i] + (CURVA[i + 1] - CURVA[i]) * (f - i); }
  function pct(v) { return Math.round(v * 100) + " %"; }

  var G = null;   // el gráfico montado ahora (uno a la vez)

  function crear(el) {
    var datos;
    try { datos = JSON.parse(el.getAttribute("data-prueba")); } catch (e) { return null; }
    var g = {
      el: el, cv: el.querySelector("canvas"), punta: el.querySelector(".enjambre-punta"), datos: datos,
      N: datos.y.length, modelo: "logistica", W: 0, H: 0, R: 3.6, pts: [],
      paciente: { p: 0.5, desde: 0.5, t0: -10, x: 0, y: 0 }, trans: { t0: -10 }, tLlega: -10,
      reloj: 0, antes: 0, visible: false, sobre: null, iniciado: false
    };
    g.ctx = g.cv.getContext("2d");
    for (var i = 0; i < g.N; i++) g.pts.push({ i: i, gbm: datos.y[i] === 1, x: 0, y: 0, x0: 0, y0: 0, tx: 0, ty: 0, ret: 0 });

    g.el.addEventListener("pointermove", function (e) {
      var r = g.el.getBoundingClientRect(), x = e.clientX - r.left, y = e.clientY - r.top, mejor = null, md = 100;
      g.pts.forEach(function (q) { var d = (q.x - x) * (q.x - x) + (q.y - y) * (q.y - y); if (d < md) { md = d; mejor = q; } });
      g.sobre = mejor;
      if (mejor) {
        g.punta.textContent = (mejor.gbm ? "GBM" : "LGG") + " real · P(GBM) = " + pct(g.datos[g.modelo][mejor.i]);
        g.punta.style.left = mejor.x + "px"; g.punta.style.top = mejor.y + "px"; g.punta.classList.add("visible");
      } else g.punta.classList.remove("visible");
    });
    g.el.addEventListener("pointerleave", function () { g.sobre = null; g.punta.classList.remove("visible"); });

    // Solo se anima mientras se ve: fuera de la vista no gasta nada
    g.obs = new IntersectionObserver(function (en) {
      var antes = g.visible;
      g.visible = en[0].isIntersecting;
      if (g.visible && !antes) { g.antes = 0; requestAnimationFrame(function (t) { cuadro(g, t); }); }
    }, { rootMargin: "60px" });
    g.obs.observe(el);
    g.ro = new ResizeObserver(function () { medir(g); });
    g.ro.observe(el);
    return g;
  }

  function caja(g) {
    var W = g.W, H = g.H, izq = W < 520 ? 14 : 60, der = W < 520 ? 14 : 24;
    return { x0: izq, x1: W - der, yG: H * 0.27, yL: H * 0.7, yP: H * 0.485, base: H - 30, banda: H * 0.17 };
  }
  function xDe(p, b) { return b.x0 + p * (b.x1 - b.x0); }

  // Enjambre: cada paciente en su probabilidad exacta, apilado sin solaparse
  function enjambre(g) {
    var b = caja(g), d = g.R * 2 + 1.2, P = g.datos[g.modelo];
    [false, true].forEach(function (gbm) {
      var fila = g.pts.filter(function (q) { return q.gbm === gbm; });
      fila.sort(function (a, c) { return P[a.i] - P[c.i]; });
      var puestos = [], cy = gbm ? b.yG : b.yL;
      fila.forEach(function (q) {
        var x = xDe(P[q.i], b), dy = 0, k = 0;
        for (;;) {
          var y = cy + dy, libre = true;
          for (var j = puestos.length - 1; j >= 0 && puestos[j][0] > x - d; j--) {
            var ex = puestos[j][0] - x, ey = puestos[j][1] - y;
            if (ex * ex + ey * ey < d * d) { libre = false; break; }
          }
          if (libre || Math.abs(dy) > b.banda) break;
          k++; dy = (k % 2 ? 1 : -1) * Math.ceil(k / 2) * d * 0.62;
        }
        q.tx = x; q.ty = cy + dy; puestos.push([x, cy + dy]);
      });
    });
  }

  function medir(g) {
    var r = g.el.getBoundingClientRect();
    if (!r.width || !r.height) return;
    var dpr = Math.min(window.devicePixelRatio || 1, 2);
    g.W = r.width; g.H = r.height;
    g.cv.width = Math.round(g.W * dpr); g.cv.height = Math.round(g.H * dpr);
    g.ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    g.R = g.W < 520 ? 3 : 3.6;
    enjambre(g);
    g.pts.forEach(function (q) { q.x = q.x0 = q.tx; q.y = q.y0 = q.ty; });
    g.trans.t0 = -10;
    if (!g.visible) dibujar(g);
  }

  function parecidos(g) {
    var p = g.paciente.p, lo = Math.max(0, p - 0.05), hi = Math.min(1, p + 0.05), l = 0, n = 0, P = g.datos[g.modelo];
    for (var i = 0; i < g.N; i++) { var v = P[i]; if (v >= lo && v <= hi) { if (g.datos.y[i]) n++; else l++; } }
    return { lo: lo, hi: hi, l: l, g: n };
  }

  // La lectura escrita, en HTML (en Shiny la escribe el manejador de abajo)
  function lectura(g) {
    var s = parecidos(g), tot = s.l + s.g;
    function span(clase, texto) { return '<span class="' + clase + '">' + texto + "</span>"; }
    var partes = ["Este perfil: ", span("enjambre-dato", pct(g.paciente.p)), " de probabilidad de GBM con " + NOMBRE[g.modelo] + ". "];
    if (tot) partes.push("Entre los pacientes de prueba con una probabilidad de " + pct(s.lo) + " a " + pct(s.hi) + " había ",
                         span("lgg", s.l + " LGG"), " y ", span("gbm", s.g + " GBM"), ".");
    else partes.push("Ningún paciente de prueba recibió una probabilidad entre " + pct(s.lo) + " y " + pct(s.hi) + ".");
    return partes.join("");
  }

  function cuadro(g, t) {
    if (!g.visible || G !== g || !g.el.isConnected) return;
    requestAnimationFrame(function (t2) { cuadro(g, t2); });
    var dt = g.antes ? Math.min(0.05, (t - g.antes) / 1000) : 0.016;
    g.antes = t; g.reloj += dt;
    dibujar(g);
  }

  function dibujar(g) {
    var ctx = g.ctx, W = g.W, H = g.H, b = caja(g), reloj = g.reloj, pa = g.paciente;
    if (!W) return;
    ctx.clearRect(0, 0, W, H);
    // Eje, marcas y umbral
    ctx.fillStyle = C.eje; ctx.fillRect(b.x0, b.base, b.x1 - b.x0, 1);
    ctx.font = "500 11px 'IBM Plex Mono', ui-monospace, monospace"; ctx.textAlign = "center";
    for (var m = 0; m <= 100; m += (W < 520 ? 25 : 20)) {
      var xm = xDe(m / 100, b);
      ctx.fillStyle = C.eje; ctx.fillRect(xm - 0.5, b.base, 1, 4);
      ctx.fillStyle = "rgba(" + C.suave + ", 0.75)"; ctx.fillText(m + " %", Math.min(W - 18, Math.max(18, xm)), b.base + 17);
    }
    var xu = xDe(0.5, b);
    ctx.setLineDash([3, 4]); ctx.strokeStyle = "rgba(" + C.suave + ", 0.45)"; ctx.lineWidth = 1;
    ctx.beginPath(); ctx.moveTo(xu + 0.5, 16); ctx.lineTo(xu + 0.5, b.base); ctx.stroke(); ctx.setLineDash([]);
    ctx.fillStyle = "rgba(" + C.suave + ", 0.7)"; ctx.fillText("umbral 50 %", xu, 11);
    if (W >= 520) {
      ctx.textAlign = "left"; ctx.font = "600 12px 'Plus Jakarta Sans', system-ui, sans-serif";
      ctx.fillStyle = "rgba(" + C.suave + ", 0.85)";
      ctx.fillText("GBM", 12, b.yG + 4); ctx.fillText("LGG", 12, b.yL + 4);
      ctx.font = "500 10px 'Plus Jakarta Sans', system-ui, sans-serif"; ctx.fillStyle = "rgba(" + C.suave + ", 0.55)";
      ctx.fillText("real", 12, b.yG + 17); ctx.fillText("real", 12, b.yL + 17);
    }
    // Perfil: viaje en arco
    var kp = reducir ? 1 : curva((reloj - pa.t0) / 0.75);
    var pp = pa.desde + (pa.p - pa.desde) * kp;
    pa.x = xDe(pp, b); pa.y = b.yP - (kp < 1 ? Math.sin(Math.PI * kp) * 26 : 0);
    // Onda de «parecidos» al llegar
    var onda = reloj > g.tLlega ? Math.max(0, 1 - (reloj - g.tLlega) / 1.6) : 0, s = parecidos(g), P = g.datos[g.modelo];
    for (var i = 0; i < g.N; i++) {
      var q = g.pts[i], k = reducir ? 1 : curva((reloj - g.trans.t0 - q.ret) / 0.8);
      q.x = q.x0 + (q.tx - q.x0) * k; q.y = q.y0 + (q.ty - q.y0) * k;
      var v = P[q.i], dentro = v >= s.lo && v <= s.hi;
      ctx.globalAlpha = dentro && onda > 0 ? 1 : 0.88;
      ctx.fillStyle = q.gbm ? C.gbm : C.lgg;
      ctx.beginPath(); ctx.arc(q.x, q.y, g.R * (1 + (dentro ? 0.45 * onda : 0)), 0, 6.2832); ctx.fill();
      if (dentro && onda > 0) { ctx.globalAlpha = onda * 0.9; ctx.strokeStyle = "rgba(" + C.tinta + ", 1)"; ctx.lineWidth = 1.2; ctx.stroke(); }
      if (q === g.sobre) { ctx.globalAlpha = 1; ctx.strokeStyle = "rgba(" + C.tinta + ", 1)"; ctx.lineWidth = 1.5; ctx.stroke(); }
    }
    ctx.globalAlpha = 1;
    // Guía y perfil: el único con brillo
    var gr = ctx.createLinearGradient(0, 16, 0, b.base);
    gr.addColorStop(0, "rgba(" + C.tinta + ", 0)"); gr.addColorStop(0.5, "rgba(" + C.tinta + ", 0.45)"); gr.addColorStop(1, "rgba(" + C.tinta + ", 0.1)");
    ctx.fillStyle = gr; ctx.fillRect(pa.x - 0.75, 16, 1.5, b.base - 16);
    var halo = ctx.createRadialGradient(pa.x, pa.y, 0, pa.x, pa.y, 22);
    halo.addColorStop(0, "rgba(" + C.tinta + ", 0.55)"); halo.addColorStop(1, "rgba(" + C.tinta + ", 0)");
    ctx.fillStyle = halo; ctx.beginPath(); ctx.arc(pa.x, pa.y, 22, 0, 6.2832); ctx.fill();
    ctx.fillStyle = "rgba(" + C.tinta + ", 1)"; ctx.beginPath(); ctx.arc(pa.x, pa.y, 5.5, 0, 6.2832); ctx.fill();
    // El rótulo del perfil, siempre al mismo lado salvo si no cabe
    var der = pa.x > W - 140;
    ctx.font = "600 12px 'Plus Jakarta Sans', system-ui, sans-serif"; ctx.textAlign = der ? "right" : "left";
    ctx.lineWidth = 3; ctx.lineJoin = "round"; ctx.strokeStyle = "rgba(11, 23, 48, 0.85)";
    var rot = "este perfil · " + pct(pp), xr = pa.x + (der ? -12 : 12);
    ctx.strokeText(rot, xr, pa.y + 4);
    ctx.fillStyle = "rgba(" + C.tinta + ", 0.95)"; ctx.fillText(rot, xr, pa.y + 4);
  }

  function actualizar(estado) {
    var el = document.getElementById("pred-enjambre");
    if (!el) return null;
    // Pestaña montada de nuevo: se empieza con el contenedor nuevo
    if (!G || G.el !== el) {
      if (G) { G.obs.disconnect(); G.ro.disconnect(); }
      G = crear(el);
      if (!G) return null;
    }
    var g = G, p = +estado.p, modelo = estado.modelo in g.datos ? estado.modelo : "logistica";
    if (!g.iniciado) {
      // Primera vez: todo en su sitio, sin viaje
      g.iniciado = true; g.modelo = modelo; g.paciente.p = g.paciente.desde = p;
      medir(g);
    } else {
      if (modelo !== g.modelo) {
        g.pts.forEach(function (q) { q.x0 = q.x; q.y0 = q.y; q.ret = reducir ? 0 : Math.random() * 0.22; });
        g.modelo = modelo; enjambre(g); g.trans.t0 = g.reloj;
      }
      if (Math.abs(p - g.paciente.p) > 1e-6) {
        var k = curva((g.reloj - g.paciente.t0) / 0.75);
        g.paciente.desde += (g.paciente.p - g.paciente.desde) * k;
        g.paciente.p = p; g.paciente.t0 = g.reloj; g.tLlega = g.reloj + 0.75;
      }
      if (!g.visible) dibujar(g);
    }
    return lectura(g);
  }

  window.enjambrePrediccion = { actualizar: actualizar };

  // Shiny: server_prediccion() manda {modelo, p} con el mensaje
  // "pred-contexto" cada vez que cambia el perfil o el modelo. Si la tarjeta
  // aún no está en la página (la pestaña se está montando), se reintenta.
  function aplicar(estado, intentos) {
    var html = actualizar(estado);
    var destino = document.getElementById("pred-contexto-lectura");
    if (html === null || !destino) {
      if (intentos > 0) setTimeout(function () { aplicar(estado, intentos - 1); }, 120);
      return;
    }
    destino.innerHTML = html;
  }
  (function registrar() {
    if (!window.Shiny || !window.Shiny.addCustomMessageHandler) { setTimeout(registrar, 50); return; }
    window.Shiny.addCustomMessageHandler("pred-contexto", function (estado) { aplicar(estado, 25); });
  })();
})();

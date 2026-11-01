/* ==========================================================================
   Portada del tablero: un cerebro de partículas con el glioma dentro

   Se carga desde ui.R (www/intro.js); en el tablero de Dash lo carga /assets. Este archivo no se
   importa desde ninguna parte. Dibuja con WebGL puro (sin three.js ni CDN):
   un solo programa de sombreado y un búfer por atributo.

   Qué se ve
   * Un cerebro hecho de partículas (hemisferios con circunvoluciones, cisura
     interhemisférica, cisura de Silvio, cerebelo y tronco).
   * Dentro, la lesión del grado elegido con su aspecto típico en resonancia:
       GBM  anillo grueso e irregular que capta contraste, centro necrótico
            casi vacío, edema vasogénico alrededor (con forma de dedos, solo en
            sustancia blanca), prolongaciones infiltrantes y efecto de masa
            (el tejido de alrededor se desplaza).
       LGG  lesión homogénea, difusa y sin realce, de bordes relativamente
            definidos, córtico-subcortical frontal, sin necrosis ni edema.
   * Una partícula grande por paciente de TCGA: las del grado elegido forman
     la lesión y las del otro grado quedan repartidas por el cerebro.

   La ubicación es ilustrativa: el dataset no dice dónde estaba cada tumor.

   Interacción: el cerebro se inclina hacia el cursor, las partículas cercanas
   al puntero se apartan y brillan, y al pasar sobre la lesión aparece su
   etiqueta. El botón de entrada dispersa las partículas y abre el tablero.

   Accesibilidad: con prefers-reduced-motion no gira ni respira. Sin WebGL se
   queda la portada estática (texto y botón), que funciona igual.
   ========================================================================== */

(function () {
  "use strict";

  // ------------------------------------------------------------------ util
  var semilla = 7;
  function azar() {
    // Generador determinista: el cerebro sale igual en cada visita.
    semilla = (semilla * 16807) % 2147483647;
    return (semilla - 1) / 2147483646;
  }
  function gauss() {
    return Math.sqrt(-2 * Math.log(azar() + 1e-9)) * Math.cos(6.2831853 * azar());
  }
  function dirAzar() {
    var z = 2 * azar() - 1, t = 6.2831853 * azar(), r = Math.sqrt(1 - z * z);
    return [r * Math.cos(t), z, r * Math.sin(t)];
  }
  function largo(v) { return Math.sqrt(v[0] * v[0] + v[1] * v[1] + v[2] * v[2]); }
  function norm(v) { var l = largo(v) || 1; return [v[0] / l, v[1] / l, v[2] / l]; }
  function suma(a, b) { return [a[0] + b[0], a[1] + b[1], a[2] + b[2]]; }
  function esc(v, s) { return [v[0] * s, v[1] * s, v[2] * s]; }
  function dist(a, b) { return largo([a[0] - b[0], a[1] - b[1], a[2] - b[2]]); }
  function suave(a, b, x) {
    var t = Math.min(1, Math.max(0, (x - a) / (b - a)));
    return t * t * (3 - 2 * t);
  }
  function hex(c, a) {
    var n = parseInt(c.slice(1), 16);
    return [(n >> 16 & 255) / 255, (n >> 8 & 255) / 255, (n & 255) / 255, a];
  }

  // Ruido suave: suma de ondas con fases fijas. Basta para lobular formas.
  var ONDAS = [];
  for (var k = 0; k < 7; k++) {
    ONDAS.push([gauss(), gauss(), gauss(), 6.2831853 * azar(), 1 + k * 0.55]);
  }
  function ruido(v, f) {
    var s = 0, peso = 0;
    for (var i = 0; i < ONDAS.length; i++) {
      var o = ONDAS[i], w = 1 / o[4];
      s += w * Math.sin(f * o[4] * (v[0] * o[0] + v[1] * o[1] + v[2] * o[2]) + o[3]);
      peso += w;
    }
    return s / peso; // aprox. en [-1, 1]
  }

  // --------------------------------------------------------- anatomía
  // Ejes: x izquierda-derecha (x > 0 hemisferio derecho), y arriba,
  // z delante (lóbulo frontal en z > 0).
  function radioCerebro(d) {
    var x = d[0], y = d[1], z = d[2], ax = Math.abs(x);
    var a = 0.70 * (z > 0 ? 1 - 0.13 * z : 1 - 0.04 * z);   // frontal más estrecho
    var b = 0.46 + 0.17 * suave(-0.3, 0.3, y);            // base aplanada, sin arista
    var c = 0.80;
    var r = 1 / Math.sqrt((x / a) * (x / a) + (y / b) * (y / b) + (z / c) * (z / c));
    // Cisura interhemisférica: surco profundo en la línea media, sobre todo arriba
    r *= 1 - 0.30 * Math.exp(-(x * x) / 0.0030) * (y > -0.15 ? 1 : 0.35);
    // Lóbulos temporales: abultan abajo, a los lados y hacia delante
    var t1 = dist(d, norm([0.78, -0.55, 0.22])), t2 = dist(d, norm([-0.78, -0.55, 0.22]));
    r *= 1 + 0.16 * Math.exp(-t1 * t1 / 0.05) + 0.16 * Math.exp(-t2 * t2 / 0.05);
    // Cisura de Silvio: separa el temporal del frontal y parietal
    var silvio = y - (-0.10 + 0.38 * z);
    r *= 1 - 0.14 * Math.exp(-silvio * silvio / 0.0026) * suave(0.45, 0.72, ax) * (z > -0.35 ? 1 : 0);
    // Polo occipital algo más alto que la base
    if (z < -0.5 && y < 0) r *= 0.93;
    // Cara orbitaria del lóbulo frontal, más plana
    if (z > 0.3 && y < -0.1) r *= 0.95;
    return r;
  }

  function surco(p) {
    // Circunvoluciones: líneas sinuosas donde el campo cruza el cero.
    var q = esc(p, 23);
    var wx = q[0] + 1.8 * Math.sin(q[1] * 0.45 + q[2] * 0.3);
    var wy = q[1] + 1.8 * Math.sin(q[2] * 0.4 + q[0] * 0.35);
    var wz = q[2] + 1.8 * Math.sin(q[0] * 0.42 + q[1] * 0.3);
    var g = Math.sin(wx) * Math.cos(wy * 1.1) + Math.sin(wy * 0.9 + wz) * Math.cos(wz * 0.8 - wx * 0.3);
    return 1 - suave(0.24, 0.58, Math.abs(g)); // surcos: donde el campo cruza el cero // 1 en el fondo del surco
  }

  function dentroCerebro(p, margen) {
    var l = largo(p);
    return l < margen * radioCerebro(esc(p, 1 / (l || 1)));
  }

  // --------------------------------------------------------- lesiones
  var C_GBM = [0.33, -0.07, 0.20];   // frontotemporal derecho, sustancia blanca
  var C_LGG = [0.34, 0.15, 0.40];    // frontal derecho, córtico-subcortical

  function radioAnillo(u) {
    return 0.185 * (1 + 0.30 * ruido(u, 2.1) + 0.16 * ruido(esc(u, 2.7), 4.2));
  }
  function grosorAnillo(u) {
    return 0.022 + 0.055 * Math.pow(0.5 + 0.5 * ruido(esc(u, -1.3), 4.0), 2); // grueso y nodular
  }
  function radioEdema(u) {
    // Dedos de edema que siguen la sustancia blanca
    return 0.10 + 0.42 * Math.pow(Math.max(0, ruido(esc(u, 1.7), 1.9) + 0.15), 1.2);
  }
  function radioLgg(u) {
    var e = [u[0] / 0.17, u[1] / 0.15, u[2] / 0.20];
    return (1 / largo(e)) * (1 + 0.09 * ruido(u, 1.4)); // contorno suave
  }

  function puntoAnilloGbm() {
    var u = dirAzar(), R = radioAnillo(u), t = grosorAnillo(u);
    return suma(C_GBM, esc(u, R + t * Math.pow(azar(), 0.8)));
  }
  function puntoNecrosis() {
    var u = dirAzar(), R = radioAnillo(u);
    return suma(C_GBM, esc(u, R * 0.85 * Math.cbrt(azar())));
  }
  function puntoEdema() {
    for (var i = 0; i < 60; i++) {
      var u = dirAzar(), R = radioAnillo(u) + grosorAnillo(u);
      var p = suma(C_GBM, esc(u, R + radioEdema(u) * Math.pow(azar(), 1.25)));
      if (dentroCerebro(p, 0.88)) return p; // el edema respeta la corteza
    }
    return puntoAnilloGbm();
  }
  var TENTACULOS = [];
  for (var tt = 0; tt < 7; tt++) {
    // Infiltración: hacia la línea media (cuerpo calloso) y a lo largo de la
    // sustancia blanca, no hacia la superficie.
    var dir = norm(suma(norm([-1, 0.15 + 0.5 * gauss() * 0.3, 0.2 * gauss()]), esc(dirAzar(), 0.8)));
    TENTACULOS.push(dir);
  }
  function puntoInfiltracion() {
    var d = TENTACULOS[Math.floor(azar() * TENTACULOS.length)];
    var s = azar();
    var base = suma(C_GBM, esc(d, radioAnillo(d) + 0.02 + s * 0.30));
    var curva = esc([Math.sin(s * 5 + d[0] * 3), Math.cos(s * 4 + d[2] * 2), Math.sin(s * 3)], 0.035 * s);
    var p = suma(suma(base, curva), esc(dirAzar(), 0.012 + 0.02 * s));
    return dentroCerebro(p, 0.86) ? p : puntoEdema();
  }
  function puntoLgg() {
    for (var i = 0; i < 60; i++) {
      var u = dirAzar(), R = radioLgg(u);
      var p = suma(C_LGG, esc(u, R * Math.cbrt(azar())));   // relleno homogéneo
      if (dentroCerebro(p, 0.97)) return p;               // llega a la corteza
    }
    return C_LGG.slice();
  }
  function bordeLgg(p) {
    // 0 en el centro, 1 en el contorno: para difuminar el margen
    var v = [p[0] - C_LGG[0], p[1] - C_LGG[1], p[2] - C_LGG[2]];
    var l = largo(v);
    return l / radioLgg(esc(v, 1 / (l || 1)));
  }

  // Efecto de masa: el tejido cercano a la lesión se desplaza hacia fuera.
  function empujar(p, centro, fuerza, alcance) {
    var v = [p[0] - centro[0], p[1] - centro[1], p[2] - centro[2]];
    var l = largo(v);
    if (l > alcance) return p;
    var f = fuerza * (1 - l / alcance) * (1 - l / alcance);
    return suma(p, esc(v, f / (l || 1)));
  }

  // ------------------------------------------------------- partículas
  var PALETA = ["#8FB8F2", "#A7A2F4", "#6FD0E0", "#DCE7FF", "#7C93E8"];

  function construir(nLgg, nGbm) {
    var P = { posA: [], posB: [], colA: [], colB: [], tam: [], azar: [], forma: [], norm: [] };
    var N = [0, 0, 0]; // normal de la partícula que se pone (0 = sin realce de silueta)
    function poner(pa, pb, ca, cb, ta, tb, lesA, lesB, forma, anA, anB) {
      P.posA.push(pa[0], pa[1], pa[2]);
      P.posB.push(pb[0], pb[1], pb[2]);
      P.colA.push(ca[0], ca[1], ca[2], ca[3]);
      P.colB.push(cb[0], cb[1], cb[2], cb[3]);
      P.tam.push(ta, tb, lesA, lesB);
      var d = dirAzar();
      P.azar.push(d[0], d[1], d[2], azar() * 6.2831853);
      P.forma.push(forma + 2 * (anA || 0) + 4 * (anB || 0));
      P.norm.push(N[0], N[1], N[2]);
    }
    function colorCerebro(brillo, alfa) {
      var c = hex(PALETA[Math.floor(azar() * PALETA.length)], alfa);
      return [c[0] * brillo, c[1] * brillo, c[2] * brillo, c[3]];
    }
    function superficie() {
      // Corteza con circunvoluciones: menos partículas en el fondo de los surcos
      for (var i = 0; i < 80; i++) {
        var d = dirAzar(), r = radioCerebro(d), p = esc(d, r);
        var s = surco(p);
        if (azar() < 0.04 + 0.96 * Math.pow(1 - s, 2.5)) {
          return { p: esc(p, 1 - 0.07 * s), brillo: 0.25 + 0.75 * (1 - s), d: d };
        }
      }
      return { p: esc(d, r), brillo: 0.7, d: d };
    }
    function conMasa(p, modo) {
      return modo === "gbm" ? empujar(p, C_GBM, 0.045, 0.55) : empujar(p, C_LGG, 0.015, 0.35);
    }

    // Corteza
    for (var i = 0; i < 56000; i++) {
      var s = superficie();
      var c = colorCerebro(s.brillo, 0.7);
      var tam = 2.0 + azar() * 1.6;
      N = s.d;
      poner(conMasa(s.p, "gbm"), conMasa(s.p, "lgg"), c, c, tam, tam, 0, 0, azar() < 0.28 ? 1 : 0);
      N = [0, 0, 0];
    }
    // Cerebelo: folias finas horizontales y vermis en la línea media
    var CC = [0, -0.38, -0.45], RC = [0.40, 0.19, 0.25];
    for (i = 0; i < 5000; i++) {
      var d = dirAzar(), p = suma(CC, [d[0] * RC[0], d[1] * RC[1], d[2] * RC[2]]);
      var folia = Math.abs(Math.sin(p[1] * 75 + p[2] * 22));
      if (azar() > 0.3 + 0.7 * folia) { i--; continue; }
      p = suma(p, esc(d, -0.025 * Math.exp(-p[0] * p[0] / 0.003)));
      c = colorCerebro(0.6 + 0.4 * folia, 0.7);
      N = d;
      poner(p, p, c, c, 2.6, 2.6, 0, 0, 0);
      N = [0, 0, 0];
    }
    // Tronco del encéfalo
    for (i = 0; i < 1800; i++) {
      var h = azar(), ang = 6.2831853 * azar(), rr = (0.115 - 0.03 * h) * Math.sqrt(0.35 + 0.65 * azar());
      p = [rr * Math.cos(ang), -0.28 - 0.55 * h, -0.17 - 0.12 * h + rr * Math.sin(ang)];
      c = colorCerebro(0.7, 0.6);
      poner(p, p, c, c, 2.4, 2.4, 0, 0, 0);
    }
    // Sustancia blanca: un velo tenue que da volumen
    for (i = 0; i < 1600; i++) {
      d = dirAzar();
      p = esc(d, radioCerebro(d) * 0.88 * Math.cbrt(azar()));
      c = colorCerebro(0.8, 0.12);
      poner(conMasa(p, "gbm"), conMasa(p, "lgg"), c, c, 2.6, 2.6, 0, 0, 0);
    }
    // Polvo de fondo
    for (i = 0; i < 450; i++) {
      p = esc(dirAzar(), 1.4 + 1.6 * azar());
      c = colorCerebro(0.9, 0.22);
      poner(p, p, c, c, 2 + azar() * 2.5, 2 + azar() * 2.5, 0, 0, 1);
    }

    // Lesión: partículas decorativas con un destino por grado
    var RIM = hex("#FF8A66", 0.9), NEC = hex("#7A3B46", 0.25), EDE = hex("#F6C98E", 0.55);
    var INF = hex("#F08A6A", 0.45), LGG = hex("#6FA6F2", 0.62), GHOST = [0, 0, 0, 0];
    var decor = [];
    for (i = 0; i < 2600; i++) decor.push([puntoAnilloGbm(), RIM, 3.0, 1, 1]);
    for (i = 0; i < 260; i++) decor.push([puntoNecrosis(), NEC, 2.6, 0.4]);
    for (i = 0; i < 5200; i++) decor.push([puntoEdema(), EDE, 5.0, 0.5]);
    for (i = 0; i < 420; i++) decor.push([puntoInfiltracion(), INF, 2.6, 0.7]);
    for (i = 0; i < decor.length; i++) {
      var g = decor[i], pb, cb, tb, lb;
      if (i < 2300) {
        pb = puntoLgg();
        var borde = suave(0.75, 1.0, bordeLgg(pb));
        cb = [LGG[0], LGG[1], LGG[2], LGG[3] * (1 - 0.6 * borde)];
        tb = 3.8; lb = 1;
      } else {
        // Sobran para el LGG: se funden con la corteza y se apagan
        pb = superficie().p; cb = GHOST; tb = 1; lb = 0;
      }
      poner(g[0], pb, g[1], cb, g[2], tb, g[3], lb, 0, g[4] || 0, 0);
    }

    // Una partícula grande por paciente
    var PG = hex("#FF9472", 0.95), PL = hex("#7DB4FF", 0.8);
    var PGs = hex("#E0785A", 0.55), PLs = hex("#6FA4EC", 0.55);
    for (i = 0; i < nGbm; i++) {
      poner(puntoAnilloGbm(), superficie().p, PG, PGs, 6.5, 4.5, 1, 0, 1, 1, 0);
    }
    for (i = 0; i < nLgg; i++) {
      poner(superficie().p, puntoLgg(), PLs, PL, 4.5, 4.8, 0, 1, 1);
    }
    return P;
  }

  // ------------------------------------------------------------ WebGL
  var VERT = [
    "attribute vec3 aPosA; attribute vec3 aPosB;",
    "attribute vec4 aColA; attribute vec4 aColB;",
    "attribute vec4 aTam; attribute vec4 aAzar; attribute float aForma; attribute vec3 aNorm;",
    "uniform mat3 uRot; uniform float uMix, uT, uAspecto, uExpl, uBrillo, uPix, uFocal, uRespira;",
    "uniform vec2 uRaton, uDesp; uniform float uRatonOn; uniform vec3 uCentro;",
    "varying vec4 vCol; varying float vForma;",
    "void main(){",
    "  float m = uMix;",
    "  vec3 p = mix(aPosA, aPosB, m);",
    "  float les = mix(aTam.z, aTam.w, m);",
    "  p *= 1.0 + uRespira * 0.010 * sin(uT * 1.1 + aAzar.w);",
    "  p += aAzar.xyz * uRespira * 0.004 * sin(uT * 0.7 + aAzar.w * 3.0);",
    "  p += aAzar.xyz * uExpl * (2.0 + 2.0 * fract(aAzar.w));",
    "  vec3 r = uRot * p;",
    "  float z = r.z + 3.1;",
    "  vec2 ndc = r.xy * uFocal / z;",
    "  ndc.x /= uAspecto;",
    "  ndc += uDesp;",
    "  vec2 d = ndc - uRaton; d.x *= uAspecto;",
    "  float dd = length(d);",
    "  float inf = uRatonOn * smoothstep(0.16, 0.0, dd);",
    "  vec2 empuje = (d / (dd + 1e-4)) * inf * 0.035; empuje.x /= uAspecto;",
    "  ndc += empuje;",
    "  gl_Position = vec4(ndc, 0.0, 1.0);",
    "  float pulso = 1.0 + les * (0.08 * sin(uT * 2.0) + 0.18 * uBrillo);",
    "  gl_PointSize = mix(aTam.x, aTam.y, m) * uPix * (uFocal / z) * (1.0 + inf * 0.5) * pulso;",
    "  vec4 c = mix(aColA, aColB, m);",
    "  float anA = mod(floor(aForma / 2.0), 2.0), anB = floor(aForma / 4.0);",
    "  vec3 rc = uRot * (p - uCentro);",
    "  float lim = 1.0 - abs(normalize(rc + 1e-5).z);",
    "  c.a *= mix(1.0, 0.12 + 1.7 * lim * lim, mix(anA, anB, m));",
    "  float tieneN = step(0.01, dot(aNorm, aNorm));",
    "  vec3 nv = uRot * aNorm;",
    "  float fres = 1.0 - abs(nv.z);",
    "  float cara = nv.z < 0.0 ? 1.0 : 0.32;",
    "  c.a *= mix(1.0, (0.85 + 0.35 * fres * fres) * cara, tieneN);",
    "  float prof = 0.12 + 0.88 * smoothstep(3.75, 2.55, z);",
    "  c.a *= prof * (1.0 + inf * (0.7 - 0.5 * step(0.01, les)) + les * uBrillo * 0.35) * (1.0 - uExpl);",
    "  vCol = c; vForma = mod(aForma, 2.0);",
    "}"
  ].join("\n");

  var FRAG = [
    "precision mediump float;",
    "varying vec4 vCol; varying float vForma;",
    "void main(){",
    "  vec2 q = gl_PointCoord * 2.0 - 1.0; q.y = -q.y;",
    "  float a;",
    "  if (vForma < 0.5) {",
    "    a = smoothstep(1.0, 0.15, length(q));",
    "  } else {",
    "    float d = max(abs(q.x) * 0.866 + q.y * 0.5, -q.y) - 0.42;",
    "    a = smoothstep(0.10, -0.06, d);",
    "  }",
    "  if (a < 0.01) discard;",
    "  gl_FragColor = vec4(vCol.rgb, vCol.a * a);",
    "}"
  ].join("\n");

  function compilar(gl, tipo, src) {
    var s = gl.createShader(tipo);
    gl.shaderSource(s, src);
    gl.compileShader(s);
    if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(s));
    return s;
  }

  function rotacion(yaw, pitch) {
    var cy = Math.cos(yaw), sy = Math.sin(yaw), cp = Math.cos(pitch), sp = Math.sin(pitch);
    // R = Rx(pitch) * Ry(yaw), por columnas para WebGL
    return [
      cy, sp * sy, -cp * sy,
      0, cp, sp,
      sy, -sp * cy, cp * cy
    ];
  }
  function aplicar(m, p) {
    return [
      m[0] * p[0] + m[3] * p[1] + m[6] * p[2],
      m[1] * p[0] + m[4] * p[1] + m[7] * p[2],
      m[2] * p[0] + m[5] * p[1] + m[8] * p[2]
    ];
  }

  // ----------------------------------------------------------- arranque
  function iniciar(raiz) {
    if (raiz.dataset.iniciada) return;
    raiz.dataset.iniciada = "1";

    var lienzo = raiz.querySelector(".intro-lienzo");
    var etiqueta = raiz.querySelector(".intro-etiqueta");
    var botonEntrar = raiz.querySelector(".intro-entrar");
    var botonesModo = raiz.querySelectorAll("[data-modo]");
    var nLgg = parseInt(raiz.dataset.lgg || "487", 10);
    var nGbm = parseInt(raiz.dataset.gbm || "352", 10);
    var quieto = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

    var estado = {
      modo: "gbm", mix: 0, mixObj: 0, yaw: 1.95, pitch: -0.34, rx: 0, ry: 0,
      raton: [9, 9], ratonOn: 0, brillo: 0, expl: 0, saliendo: false, vivo: true,
      dyaw: 0, dpitch: 0, vyaw: 0, vpitch: 0, arrastre: false, ax: 0, ay: 0
    };

    // ---- entrar al tablero (va primero: funciona aunque WebGL falle)
    function cerrar() {
      document.body.classList.add("intro-cerrada");
      document.body.classList.remove("portada-saliendo");
      estado.vivo = false;
      try { window.dispatchEvent(new Event("resize")); } catch (e) { /* nada */ }
    }
    function entrar() {
      if (estado.saliendo) return;
      estado.saliendo = true;
      if (quieto) { cerrar(); return; }
      // La capa se recoge hasta el alto de la cabecera del tablero, que es del
      // mismo azul tinta: el oscuro de la portada se convierte en la cabecera.
      // Se recoge hasta el final del índice: barra e índice son ya azul noche.
      var cabecera = document.getElementById("barra-superior");
      var nav = document.getElementById("barra-navegacion");
      var alto = cabecera ? cabecera.getBoundingClientRect().height : 72;
      var hasta = nav ? nav.getBoundingClientRect().bottom : alto;
      raiz.style.setProperty("--alto-cabecera", Math.round(hasta) + "px");
      raiz.style.setProperty("--alto-barra", Math.round(alto) + "px");
      document.body.classList.add("portada-saliendo");
      try { window.scrollTo(0, 0); } catch (e) { /* nada */ }
      raiz.classList.add("intro-saliendo");
      setTimeout(function () { raiz.classList.add("intro-fundido"); }, 980);
      setTimeout(cerrar, 1300);
    }
    botonEntrar.addEventListener("click", entrar);
    document.addEventListener("keydown", function (e) {
      if (document.body.classList.contains("intro-cerrada")) return;
      var enModo = e.target && e.target.closest && e.target.closest(".intro-modo");
      if (e.key === "Escape" || (e.key === "Enter" && !enModo)) entrar();
    });

    function ponerModo(modo) {
      estado.modo = modo;
      estado.mixObj = modo === "gbm" ? 0 : 1;
      raiz.dataset.modo = modo;
      botonesModo.forEach(function (b) {
        var activo = b.dataset.modo === modo;
        b.classList.toggle("activo", activo);
        b.setAttribute("aria-pressed", activo ? "true" : "false");
      });
    }
    botonesModo.forEach(function (b) {
      b.addEventListener("click", function () { ponerModo(b.dataset.modo); });
    });
    ponerModo("gbm");

    // ---- WebGL
    var gl = null;
    try {
      gl = lienzo.getContext("webgl", { antialias: false, alpha: true, premultipliedAlpha: false }) ||
           lienzo.getContext("experimental-webgl");
    } catch (e) { gl = null; }
    if (!gl) { raiz.classList.add("intro-sin-3d"); return; }

    var prog, U = {}, total;
    try {
      prog = gl.createProgram();
      gl.attachShader(prog, compilar(gl, gl.VERTEX_SHADER, VERT));
      gl.attachShader(prog, compilar(gl, gl.FRAGMENT_SHADER, FRAG));
      gl.linkProgram(prog);
      if (!gl.getProgramParameter(prog, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(prog));
      gl.useProgram(prog);

      var P = construir(nLgg, nGbm);
      total = P.forma.length;
      [["aPosA", P.posA, 3], ["aPosB", P.posB, 3], ["aColA", P.colA, 4], ["aColB", P.colB, 4],
       ["aTam", P.tam, 4], ["aAzar", P.azar, 4], ["aForma", P.forma, 1], ["aNorm", P.norm, 3]].forEach(function (a) {
        var loc = gl.getAttribLocation(prog, a[0]);
        if (loc < 0) return;
        var buf = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, buf);
        gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(a[1]), gl.STATIC_DRAW);
        gl.enableVertexAttribArray(loc);
        gl.vertexAttribPointer(loc, a[2], gl.FLOAT, false, 0, 0);
      });
      ["uRot", "uMix", "uT", "uAspecto", "uExpl", "uBrillo", "uPix", "uFocal", "uRaton",
       "uRatonOn", "uDesp", "uRespira", "uCentro"].forEach(function (n) { U[n] = gl.getUniformLocation(prog, n); });
      gl.disable(gl.DEPTH_TEST);
      gl.enable(gl.BLEND);
      gl.blendFunc(gl.SRC_ALPHA, gl.ONE); // aditivo: las partículas se iluminan al juntarse
    } catch (e) {
      if (window.console) console.warn("Portada sin 3D:", e);
      raiz.classList.add("intro-sin-3d");
      return;
    }
    raiz.classList.add("intro-con-3d");

    var ancho = 0, alto = 0, dpr = 1, focal = 2.6, desp = [0, 0];
    function medir() {
      dpr = Math.min(window.devicePixelRatio || 1, 2);
      ancho = lienzo.clientWidth; alto = lienzo.clientHeight;
      lienzo.width = Math.round(ancho * dpr);
      lienzo.height = Math.round(alto * dpr);
      gl.viewport(0, 0, lienzo.width, lienzo.height);
      var ancha = ancho > 900;
      // En pantalla ancha el cerebro va a la derecha del texto; en móvil, arriba.
      desp = ancha ? [0.36, -0.02] : [0.0, 0.36];
      focal = ancha ? 2.95 : Math.min(2.6, 3.3 * ancho / alto);
    }
    medir();
    window.addEventListener("resize", medir);

    function ratonANdc(e) {
      var r = lienzo.getBoundingClientRect();
      return [((e.clientX - r.left) / r.width) * 2 - 1, -(((e.clientY - r.top) / r.height) * 2 - 1)];
    }
    // Arrastrar para girar (ratón o dedo). No empieza sobre el panel de texto.
    raiz.addEventListener("pointerdown", function (e) {
      if (e.target.closest && e.target.closest(".intro-panel, button")) return;
      estado.arrastre = true;
      estado.ax = e.clientX; estado.ay = e.clientY;
      estado.vyaw = 0; estado.vpitch = 0;
      raiz.classList.add("intro-arrastrando", "intro-girado");
      try { raiz.setPointerCapture(e.pointerId); } catch (err) { /* nada */ }
    });
    function soltar(e) {
      if (!estado.arrastre) return;
      estado.arrastre = false;
      raiz.classList.remove("intro-arrastrando");
      try { raiz.releasePointerCapture(e.pointerId); } catch (err) { /* nada */ }
    }
    raiz.addEventListener("pointerup", soltar);
    raiz.addEventListener("pointercancel", soltar);
    raiz.addEventListener("pointermove", function (e) {
      if (estado.arrastre) {
        var dx = e.clientX - estado.ax, dy = e.clientY - estado.ay;
        estado.ax = e.clientX; estado.ay = e.clientY;
        estado.vyaw = dx * 0.0075; estado.vpitch = dy * 0.0055;
        estado.dyaw += estado.vyaw;
        estado.dpitch += estado.vpitch;
      }
      estado.raton = ratonANdc(e);
      estado.ratonOn = (e.pointerType === "touch" || estado.arrastre) ? 0 : 1;
    });
    raiz.addEventListener("pointerleave", function () {
      if (estado.arrastre) return;
      estado.ratonOn = 0; estado.raton = [9, 9];
    });

    function proyectar(p, rot) {
      var r = aplicar(rot, p), z = r[2] + 3.1;
      var x = (r[0] * focal / z) / (ancho / alto) + desp[0], y = r[1] * focal / z + desp[1];
      return [(x + 1) / 2 * ancho, (1 - y) / 2 * alto];
    }

    var t0 = performance.now(), antes = t0;
    function cuadro(ahora) {
      if (!estado.vivo) return;
      var dt = Math.min(0.05, (ahora - antes) / 1000); antes = ahora;
      var t = quieto ? 0 : (ahora - t0) / 1000;

      // Inclinación hacia el cursor, suavizada
      var objX = estado.ratonOn ? estado.raton[0] - desp[0] : 0;
      var objY = estado.ratonOn ? estado.raton[1] - desp[1] : 0;
      estado.rx += (objX - estado.rx) * Math.min(1, dt * 3);
      estado.ry += (objY - estado.ry) * Math.min(1, dt * 3);
      // Inercia al soltar: el giro se frena poco a poco
      if (!estado.arrastre) {
        estado.dyaw += estado.vyaw; estado.vyaw *= 0.93;
        estado.dpitch += estado.vpitch; estado.vpitch *= 0.93;
      }
      var yaw = estado.yaw + estado.dyaw + (quieto ? 0 : 0.30 * Math.sin(t * 0.16)) + estado.rx * 0.35;
      var pitch = estado.pitch + estado.dpitch + estado.ry * -0.22;
      if (pitch > 1.35) { estado.dpitch -= pitch - 1.35; pitch = 1.35; }
      if (pitch < -1.35) { estado.dpitch -= pitch + 1.35; pitch = -1.35; }
      var rot = rotacion(yaw, pitch);

      // Cambio de grado: transición con aceleración suave
      estado.mix += (estado.mixObj - estado.mix) * Math.min(1, dt * 2.2);

      // ¿El cursor está sobre la lesión?
      var centro = [
        C_GBM[0] + (C_LGG[0] - C_GBM[0]) * estado.mix,
        C_GBM[1] + (C_LGG[1] - C_GBM[1]) * estado.mix,
        C_GBM[2] + (C_LGG[2] - C_GBM[2]) * estado.mix
      ];
      var pc = proyectar(centro, rot);
      var radioPx = 0.22 * focal / 3.1 * alto / 2;
      var rr = lienzo.getBoundingClientRect();
      var mx = (estado.raton[0] + 1) / 2 * rr.width, my = (1 - estado.raton[1]) / 2 * rr.height;
      var sobre = estado.ratonOn && Math.hypot(mx - pc[0], my - pc[1]) < radioPx;
      estado.brillo += ((sobre ? 1 : 0) - estado.brillo) * Math.min(1, dt * 5);
      if (etiqueta) {
        etiqueta.style.transform = "translate(" + Math.round(pc[0] + radioPx * 0.9) + "px," +
          Math.round(pc[1] - radioPx * 0.9) + "px)";
        etiqueta.classList.toggle("visible", sobre);
      }

      if (estado.saliendo) estado.expl = Math.min(1, estado.expl + dt * 1.4);

      gl.clearColor(0, 0, 0, 0);
      gl.clear(gl.COLOR_BUFFER_BIT);
      gl.uniformMatrix3fv(U.uRot, false, new Float32Array(rot));
      gl.uniform1f(U.uMix, estado.mix);
      gl.uniform1f(U.uT, t);
      gl.uniform1f(U.uAspecto, ancho / alto);
      gl.uniform1f(U.uExpl, estado.expl * estado.expl);
      gl.uniform1f(U.uBrillo, estado.brillo);
      gl.uniform1f(U.uPix, dpr);
      gl.uniform1f(U.uFocal, focal);
      gl.uniform2f(U.uRaton, estado.raton[0], estado.raton[1]);
      gl.uniform1f(U.uRatonOn, estado.ratonOn);
      gl.uniform2f(U.uDesp, desp[0], desp[1]);
      gl.uniform1f(U.uRespira, quieto ? 0 : 1);
      gl.uniform3f(U.uCentro, centro[0], centro[1], centro[2]);
      gl.drawArrays(gl.POINTS, 0, total);
      requestAnimationFrame(cuadro);
    }
    requestAnimationFrame(cuadro);

    // Pestaña oculta: no gastar batería
    document.addEventListener("visibilitychange", function () {
      if (!document.hidden && estado.vivo) { antes = performance.now(); }
    });
  }

  // La página puede montarse después de cargar los scripts: se espera a que exista.
  function buscar() {
    var raiz = document.getElementById("intro");
    if (raiz) { iniciar(raiz); return true; }
    return false;
  }
  if (!buscar()) {
    var obs = new MutationObserver(function () { if (buscar()) obs.disconnect(); });
    obs.observe(document.documentElement, { childList: true, subtree: true });
  }
})();

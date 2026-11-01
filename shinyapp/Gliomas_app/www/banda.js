/* ==========================================================================
   Partículas de la banda nocturna (estilo B+)

   Se carga desde ui.R (www/banda.js). Es el mismo archivo del tablero de Dash,
   con los pocos ajustes que pide Shiny señalados en su sitio.

   Las mismas partículas recorren todo el tablero
   La portada dispersa su cerebro (intro.js). Desde ese clic, estas partículas,
   con el mismo color, tamaño, forma y brillo que las de la portada, se
   reúnen en la banda azul noche y toman una figura distinta en cada bloque
   de las pestañas. Cada pestaña tiene su figura, y todas dicen algo de lo que
   trata la pestaña:

     Introducción            doble hélice que fluye; tres tramos en el coral
                             del GBM son genes mutados
     Contexto clínico        el cursor de la edad: las densidades de LGG y
                             GBM y un cursor (o tu ratón) que dice qué parte
                             de los pacientes de esa edad es de cada grado
     Problema                el secuenciador: la hélice entra por una ranura
                             que lee de una en una; detrás se acumula la cola
     Objetivos               lanzamientos a la diana: diez intentos, cada uno
                             más cerca del centro (el panel mínimo)
     Marco teórico           la sonda que se detiene en ocho genes de los 23
                             cromosomas y dice su ρ
     Metodología             barajar y repartir: 839 pacientes se reparten
                             en entrenamiento (671) y prueba (168)
     Exploración             una lupa recorre las 23 variables y agranda la
                             que tiene debajo, con su nombre
     Resultados              barras de ρ que crecen una a una y se leen
     Modelos                 Plinko: los 168 de prueba caen por un árbol a
                             hojas LGG o GBM (147 aciertos de la logística)
     Predicción              tu paciente (blanca) entre las cohortes, según la
                             P(GBM) del modelo elegido en la pestaña
     Limitaciones            rebotes en la frontera de lo que esta cohorte
                             permite concluir; alguna partícula se escapa
     Conclusiones            de 23 variables a 4: IDH1, edad, TP53 y PTEN
                             crecen hasta su importancia; las demás caen
     Documentación           un libro abierto con renglones de partículas y
                             una página que pasa

   Si la pestaña nueva tiene la misma figura, se desliza entera hasta su
   sitio; si no, las partículas viajan de una figura a la otra. El
   viaje no empieza hasta que la pestaña nueva está montada y dibujada
   (`pestanaLista`): dibujar sus gráficos ocupa el navegador y congelaba el
   viaje a medias. Mientras tanto la figura sigue viva donde estaba.

   Mientras se espera, la figura vieja se recoge un poco hacia su centro:
   es la respuesta inmediata al clic, aunque el viaje aún no pueda empezar.

   El reloj de las animaciones es virtual (`est.tv`): avanza como mucho 50 ms
   por fotograma. Si el navegador se congela medio segundo montando una
   pestaña, las partículas siguen desde donde estaban en vez de saltar medio
   segundo de golpe.

   La barra de progreso del índice es una doble hélice quieta de 13 peldaños,
   agrupados como los bloques del índice (Inicio, 01-04, Anexo). No late: la
   pestaña actual se marca quieta y solo se enciende una vez al cambiar. Cada
   peldaño es un botón (lleva a su pestaña y dice su nombre al pasar el
   ratón). Cuando nada cambia (cabecera fuera de la vista, ratón lejos de la
   barra) el lienzo no se vuelve a pintar.

   Todo se dibuja en un único lienzo fijo con mezcla aditiva, como la
   portada. Cada fotograma lee dónde están la barra, el índice y la cabecera.

   Accesibilidad: con prefers-reduced-motion nada fluye y los cambios son
   inmediatos. El lienzo es decorativo (aria-hidden).
   ========================================================================== */

(function () {
  "use strict";

  var N_PESTANAS = 13;
  // Figura de cada pestaña, en el orden del índice (utils/navegacion.py)
  var FIGURA = [null, "helice", "edad", "secuenciador", "dardos", "sonda", "baraja",
                "lupa", "barras", "plinko", "paciente", "rebotes", "veintitres", "libro"];
  var reducir = window.matchMedia && window.matchMedia("(prefers-reduced-motion: reduce)").matches;

  // ------------------------------------------------- aspecto de la portada
  var PALETA = [[143, 184, 242], [167, 162, 244], [111, 208, 224], [220, 231, 255], [124, 147, 232]];
  var AZULES = [[143, 184, 242], [124, 147, 232], [111, 166, 242]];
  var RIM = [255, 138, 102], EDEMA = [246, 201, 142], INFIL = [240, 138, 106];
  var PAC_GBM = [255, 148, 114], PAC_LGG = [125, 180, 255];

  // Datos reales del proyecto
  var CROM = [248, 242, 198, 190, 181, 171, 159, 145, 138, 134, 135, 133, 114, 107, 102, 90, 83, 80, 59, 64, 47, 51, 156];
  // cromosoma, posición relativa aproximada (banda citogenética), dirección (-1 LGG, 1 GBM, 0 sin destacar)
  var GENES = [[2, 0.85, -1], [17, 0.08, -1], [23, 0.52, -1], [10, 0.65, 1], [7, 0.35, 1], [19, 0.72, -1], [15, 0.9, -1],
               [17, 0.36, 0], [13, 0.42, 1], [9, 0.96, -1], [1, 0.3, -1], [3, 0.86, 0], [5, 0.4, 0], [19, 0.12, 0],
               [23, 0.28, 0], [8, 0.78, 0], [19, 0.2, 0], [16, 0.12, 0], [4, 0.66, 0], [4, 0.29, 0]];
  var RHO = [["IDH1", -0.703], ["Edad", 0.527], ["PTEN", 0.367], ["ATRX", -0.327], ["CIC", -0.313],
             ["EGFR", 0.209], ["FUBP1", -0.195], ["NOTCH1", -0.194], ["RB1", 0.183]];
  var N_LGG = 487, N_GBM = 352;

  var semilla = 29;
  function azar() { semilla = (semilla * 16807) % 2147483647; return (semilla - 1) / 2147483646; }
  function gauss() { return Math.sqrt(-2 * Math.log(azar() + 1e-9)) * Math.cos(6.2831853 * azar()); }
  function suave(a, b, x) { var t = Math.min(1, Math.max(0, (x - a) / (b - a))); return t * t * (3 - 2 * t); }
  // Curva de los viajes: ease-in-out fuerte, cubic-bezier(0.77, 0, 0.175, 1),
  // la que recomienda Emil Kowalski para movimiento en pantalla. Las curvas de
  // serie se quedan cortas. Se tabula una vez (257 puntos) en vez de
  // resolver la Bézier para cada partícula en cada fotograma.
  var CURVA = (function () {
    var x1 = 0.77, y1 = 0, x2 = 0.175, y2 = 1, tabla = [];
    function bz(a, b, s) { return 3 * a * s * (1 - s) * (1 - s) + 3 * b * s * s * (1 - s) + s * s * s; }
    for (var i = 0; i <= 256; i++) {
      var x = i / 256, lo = 0, hi = 1, s = 0.5;
      for (var it = 0; it < 22; it++) { s = (lo + hi) / 2; if (bz(x1, x2, s) < x) lo = s; else hi = s; }
      tabla.push(bz(y1, y2, s));
    }
    return tabla;
  })();
  function curvaViaje(k) {
    if (k <= 0) return 0; if (k >= 1) return 1;
    var f = k * 256, i = Math.floor(f);
    return CURVA[i] + (CURVA[i + 1] - CURVA[i]) * (f - i);
  }
  function elegir(lista) { return lista[Math.floor(azar() * lista.length)]; }

  // Tres estilos por partícula, sacados de la portada: corteza (c), lesión
  // del GBM (l) y paciente o tejido LGG (g). Cada figura elige cuál mostrar.
  function estiloCorteza() { return { col: elegir(PALETA), tam: 2.2 + azar() * 1.8, alfa: 0.75, tri: azar() < 0.28, halo: azar() < 0.08 }; }
  function estiloLesion() {
    var r = azar();
    if (r < 0.55) return { col: RIM, tam: 3.4, alfa: 0.95, tri: false, halo: azar() < 0.5, les: 1 };
    if (r < 0.8) return { col: EDEMA, tam: 5.5, alfa: 0.5, tri: false, halo: false, les: 0.5 };
    if (r < 0.9) return { col: INFIL, tam: 2.8, alfa: 0.5, tri: false, halo: false, les: 0.7 };
    return { col: PAC_GBM, tam: 7.0, alfa: 0.95, tri: true, halo: true, les: 1 };
  }
  function estiloLgg() {
    if (azar() < 0.12) return { col: PAC_LGG, tam: 4.8, alfa: 0.85, tri: true, halo: true, les: 0 };
    return { col: elegir(AZULES), tam: 2.6 + azar() * 1.6, alfa: 0.82, tri: azar() < 0.2, halo: azar() < 0.1, les: 0 };
  }

  var N = 1800;
  var parts = [];
  for (var i = 0; i < N; i++) {
    // b: blanca, la que se destaca (tu paciente, los últimos intentos)
    var e = { c: estiloCorteza(), l: estiloLesion(), g: estiloLgg(),
              b: { col: [238, 243, 249], tam: 4.2, alfa: 1, tri: false, halo: true, les: 0 } };
    parts.push({ i: i, r1: azar(), r2: azar(), r3: azar(), r4: azar(), g1: gauss(), g2: gauss(), f: azar() * 6.28,
                 est: e, col: e.c.col.slice(), tam: e.c.tam, tri: e.c.tri });
  }

  // ------------------------------------------------------------ figuras
  // Cada figura coloca la partícula: p.tx, p.ty (destino en pantalla), p.ta
  // (opacidad 0-1), p.tz (profundidad 0-1, encoge y apaga) y p.te (estilo).
  var PELDANOS = 24;
  function mutado(u) { return (u > 0.16 && u < 0.21) || (u > 0.52 && u < 0.555) || (u > 0.79 && u < 0.81); }

  function figHelice(p, c, t) {
    var flujo = reducir ? 0 : t * 0.010, giro = reducir ? 0 : t * 0.28;
    var largoX = c.hx1 - c.hx0;
    if (p.r1 > 0.94) {   // polvo alrededor
      p.tx = c.hx0 - largoX * 0.1 + p.r2 * largoX * 1.15 + (reducir ? 0 : Math.sin(t * 0.2 + p.f) * 6);
      p.ty = c.R.top + p.r3 * c.R.height; p.ta = 0.3; p.tz = 0.6; p.te = "c"; return;
    }
    var peldano = p.r1 > 0.82;
    var u = peldano ? (Math.floor(p.r2 * PELDANOS) + 0.5) / PELDANOS : p.r2;
    var s = u + flujo; s -= Math.floor(s);
    var env = suave(0, 0.24, s) * suave(1, 0.82, s), disp = 1 - env;
    var ang = s * c.vueltas * 6.2831853 + giro, z;
    var x = c.hx0 + s * largoX, y;
    if (!peldano) {
      var a = ang + (p.r3 < 0.5 ? 0 : Math.PI);
      y = c.hcy + Math.sin(a) * c.amp; z = 0.5 + 0.5 * Math.cos(a);
    } else {
      var y1 = c.hcy + Math.sin(ang) * c.amp, y2 = c.hcy + Math.sin(ang + Math.PI) * c.amp;
      y = y1 + (y2 - y1) * p.r3; z = 0.3 + 0.3 * (0.5 + 0.5 * Math.cos(ang));
    }
    p.tx = x + p.g1 * disp * 44; p.ty = y + p.g2 * disp * 26;
    p.ta = Math.pow(env, 0.65); p.tz = z; p.te = mutado(u) ? "l" : "c";
  }

  // Exploración: una lupa recorre las 23 variables (en el orden del CSV) y
  // agranda la que tiene debajo. Las que empujan hacia LGG se encienden en
  // azul y las que empujan hacia GBM en coral, con el signo de ρ del EDA.
  var VARIABLES = ["Género", "Edad", "Raza", "IDH1", "TP53", "ATRX", "PTEN", "EGFR", "CIC", "MUC16", "PIK3CA",
                   "NF1", "PIK3R1", "FUBP1", "RB1", "NOTCH1", "BCOR", "CSMD3", "SMARCA4", "GRIN2A", "IDH2", "FAT4", "PDGFRA"];
  var DIRECCION = { "Edad": "l", "IDH1": "g", "TP53": "g", "ATRX": "g", "PTEN": "l", "EGFR": "l", "CIC": "g",
                    "FUBP1": "g", "RB1": "l", "NOTCH1": "g", "IDH2": "g" };
  function centroLupa(c, t) {
    return [c.x + c.w * (0.5 + 0.44 * Math.sin((reducir ? 0.6 : t) * 0.32)), c.y + c.h * 0.5];
  }
  function figLupa(p, c, t) {
    if (p.r1 > 0.97) { polvo(p, c, t); return; }
    var L = centroLupa(c, t), RL = Math.min(c.h * 0.36, c.w * 0.2);
    if (p.r1 > 0.88) {
      // El aro de la lupa y su mango
      if (p.r2 < 0.82) {
        var a = p.r3 * 6.2831853;
        p.tx = L[0] + Math.cos(a) * (RL + p.g1 * 1.2); p.ty = L[1] + Math.sin(a) * (RL + p.g1 * 1.2);
      } else {
        p.tx = L[0] + RL * 0.72 + p.r3 * RL * 0.6 + p.g1; p.ty = L[1] + RL * 0.72 + p.r3 * RL * 0.6 + p.g2;
      }
      p.te = "c"; p.ta = 1; p.tz = 1; return;
    }
    var k = Math.floor(p.r2 * VARIABLES.length), paso = c.w / VARIABLES.length;
    var cx = c.x + paso * (k + 0.5), cy = c.y + c.h * 0.5, rb = Math.min(paso * 0.42, c.h * 0.16);
    var an = p.r3 * 6.2831853 + (reducir ? 0 : t * 0.2), rr = rb * Math.sqrt(p.r4);
    var x = cx + Math.cos(an) * rr, y = cy + Math.sin(an) * rr;
    // Ojo de pez: dentro de la lupa todo se separa del centro y crece
    var dx = x - L[0], dy = y - L[1], d = Math.sqrt(dx * dx + dy * dy), u = d / RL, dentro = u < 1;
    if (dentro && d > 0) {
      var f = 3.2 * u / (1 + 2.2 * u);
      x = L[0] + dx / d * f * RL * 0.95; y = L[1] + dy / d * f * RL * 0.95;
    }
    p.tx = x; p.ty = y;
    p.te = dentro ? (DIRECCION[VARIABLES[k]] || "c") : "c";
    p.ta = dentro ? 1 : 0.55; p.tz = dentro ? 1 : 0.45;
  }

  // Documentación: un libro abierto, renglones de partículas y una página que
  // pasa cada pocos segundos (el Jupyter Book con el análisis completo)
  function paginaLibro(c) {
    var pw = Math.min(c.w * 0.24, c.h * 0.62), ph = Math.min(pw * 1.3, c.h * 0.86);
    return { cx: c.x + c.w * 0.5, top: c.y + (c.h - ph) / 2, pw: pw, ph: ph };
  }
  function figLibro(p, c, t) {
    if (p.r1 > 0.95) { polvo(p, c, t); return; }
    var B = paginaLibro(c), lado = p.r2 < 0.5 ? -1 : 1;
    function curva(u) { return Math.sin(u * Math.PI) * B.ph * 0.035; }
    if (p.r1 < 0.2) {
      // Bordes de las dos páginas
      var e = p.r3, x, y;
      if (p.r4 < 0.5) { x = B.cx + lado * e * B.pw; y = p.r4 < 0.25 ? B.top - curva(e) : B.top + B.ph - curva(e) * 0.5; }
      else { x = p.r4 < 0.75 ? B.cx : B.cx + lado * B.pw; y = B.top + e * B.ph; }
      p.tx = x + p.g1 * 0.6; p.ty = y + p.g2 * 0.6; p.te = "c"; p.ta = 1; p.tz = 1; return;
    }
    if (p.r1 < 0.78) {
      // Renglones: nueve por página, con huecos entre palabras
      var linea = Math.floor(p.r3 * 9), uu = 0.1 + p.r4 * 0.8;
      var hueco = (uu * 7.3 + linea * 1.7) % 1 < 0.16;
      p.tx = B.cx + lado * uu * B.pw; p.ty = B.top + B.ph * (0.14 + linea * 0.09) - curva(uu) * 0.6 + p.g1 * 0.6;
      var cita = lado < 0 && (linea === 3 || linea === 4);
      p.te = cita ? "l" : "c"; p.ta = hueco ? 0.05 : (cita ? 0.95 : 0.6); p.tz = 0.5; return;
    }
    // La página que pasa: de derecha a izquierda cada 7 s
    var fase = reducir ? 0 : (t / 7) % 1, giro = suave(0.1, 0.7, fase) * Math.PI;
    var ancho = Math.cos(giro) * B.pw;
    p.tx = B.cx + p.r3 * ancho; p.ty = B.top + p.r4 * B.ph - Math.sin(giro) * B.ph * 0.06 * p.r3;
    p.te = "c"; p.ta = (fase > 0.08 && fase < 0.74) ? 0.75 : 0; p.tz = 0.8;
  }

  // ===================================================== figuras con acción
  // Elegidas por el autor en el prototipo «Figuras nuevas para elegir»
  // (28-oct): como la lupa y el libro, cada una tiene un objeto que se
  // reconoce y una acción que se repite en ciclos lentos. Los ciclos cuentan
  // desde que la figura llega (est.tf), así que cada visita empieza el
  // relato desde el principio. Las partículas que una figura no usa se
  // apagan (p.ta = 0) y no se dibujan: varias figuras usan menos de las
  // 1.800, y eso aligera el lienzo.
  function lerp(a, b, k) { return a + (b - a) * k; }
  function coma(v, d) { return v.toFixed(d).replace(".", ","); }
  function apagada(p, c, t) { polvo(p, c, t); p.ta = 0; }
  // Estado de las figuras que lo necesitan (cursor, sonda, rebotes...)
  var EF = {};

  // ---- Contexto clínico: el cursor de la edad ---------------------------
  // Densidad de la edad en entrenamiento (KDE, ancho 0,3, de 10 a 90 años en
  // 33 puntos), los dos grados en la misma escala: LGG (389, mediana 41,9) y
  // GBM (282, mediana 61,5). Un cursor recorre las edades como la lupa y dice
  // qué proporción de pacientes de esa edad es de cada grado; con el ratón
  // encima de la banda, el cursor lo lleva el ratón.
  var DENS = {
    g: [0.01, 0.02, 0.05, 0.10, 0.19, 0.30, 0.44, 0.61, 0.79, 0.94, 1.00, 0.98, 0.91, 0.82, 0.75, 0.70, 0.68,
        0.68, 0.66, 0.62, 0.56, 0.48, 0.39, 0.29, 0.21, 0.15, 0.09, 0.05, 0.02, 0.01, 0.01, 0.01, 0.01],
    l: [0.00, 0.00, 0.01, 0.02, 0.04, 0.06, 0.08, 0.10, 0.13, 0.17, 0.20, 0.22, 0.23, 0.27, 0.36, 0.49, 0.64,
        0.78, 0.88, 0.95, 0.99, 1.00, 0.96, 0.89, 0.82, 0.76, 0.69, 0.59, 0.44, 0.30, 0.20, 0.13, 0.07]
  };
  function densidad(k, u) {
    var d = DENS[k], f = Math.min(1, Math.max(0, u)) * (d.length - 1), i = Math.min(d.length - 2, Math.floor(f));
    return d[i] + (d[i + 1] - d[i]) * (f - i);
  }
  function acumulada(d) {
    var a = [0];
    for (var i = 1; i < d.length; i++) a.push(a[i - 1] + (d[i - 1] + d[i]) / 2);
    return a.map(function (v) { return v / a[a.length - 1]; });
  }
  var ACUM = { g: acumulada(DENS.g), l: acumulada(DENS.l) };
  function cuantil(k, q) {
    var a = ACUM[k], i = 1;
    while (i < a.length - 1 && a[i] < q) i++;
    return (i - 1 + (q - a[i - 1]) / ((a[i] - a[i - 1]) || 1)) / (a.length - 1);
  }
  function ejeEdad(c) { return { x0: c.x + c.w * 0.05, an: c.w * 0.9, base: c.y + c.h * 0.86, alto: c.h * 0.52 }; }
  function pasoEdad(c, t) {
    var A = ejeEdad(c), auto = 0.5 + 0.45 * Math.sin(t * 0.33), mano = null;
    if (raton.on && raton.y > c.R.top && raton.y < c.R.bottom && raton.x > A.x0 && raton.x < A.x0 + A.an) mano = (raton.x - A.x0) / A.an;
    // El rótulo va siempre a la derecha del cursor (el autor no quiere que
    // cambie de lado): el cursor no pasa de donde ese rótulo aún cabe
    var tope = Math.min(1, (c.x + c.w - 74 - A.x0) / A.an);
    var meta = Math.min(tope, mano !== null ? mano : 0.05 + (auto - 0.05) * (tope - 0.05) / 0.9);
    EF.uEdad = EF.uEdad === undefined || reducir ? meta : lerp(EF.uEdad, meta, mano !== null ? 0.25 : 0.06);
  }
  function figEdad(p, c, t) {
    if (p.i >= 1300) { apagada(p, c, t); return; }
    if (p.r1 > 0.95) { polvo(p, c, t); return; }
    var A = ejeEdad(c), k = p.r1 < 0.5 ? "g" : "l";
    if (p.edadU === undefined) p.edadU = cuantil(k, p.r2);
    var u = p.edadU, alto = densidad(k, u) * A.alto, x = A.x0 + u * A.an;
    var cerca = Math.exp(-Math.pow((u - (EF.uEdad || 0.5)) / 0.035, 2));
    if (p.r3 < 0.3) { p.tx = x + p.g1 * 0.8; p.ty = A.base - alto + p.g2; p.ta = 0.85 + 0.15 * cerca; p.tz = 1; }
    else {
      var s = p.r4 + (reducir ? 0.5 : t * 0.07); s -= Math.floor(s);
      p.tx = x + p.g1 * 1.2; p.ty = A.base - s * alto;
      p.ta = (0.08 + 0.45 * suave(0, 0.15, s) * suave(1, 0.75, s)) * (1 + 1.6 * cerca); p.tz = 0.5 + 0.5 * cerca;
    }
    p.tb = 1 + 0.5 * cerca; p.te = k;
  }

  // ---- Problema: el secuenciador ----------------------------------------
  // Metáfora del cuello de botella (no son costes reales): la doble hélice
  // entra por una ranura que lee de una en una; detrás se acumula la cola y
  // delante sale la secuencia leída, con los genes mutados encendidos.
  function figSecuenciador(p, c, t) {
    if (p.i >= 1300) { apagada(p, c, t); return; }
    if (p.r1 > 0.95) { polvo(p, c, t); return; }
    var ranura = c.x + c.w * 0.5, x0 = c.x - 10, x1 = c.x + c.w, cy = c.y + c.h * 0.5, amp = c.h * 0.32;
    if (p.r1 > 0.86) {
      p.tx = ranura + (p.r4 - 0.5) * 8; p.ty = p.r2 < 0.5 ? cy - 12 - p.r3 * c.h * 0.3 : cy + 12 + p.r3 * c.h * 0.3;
      p.te = "c"; p.ta = 0.75; p.tz = 1; return;
    }
    var s = p.r2 + (reducir ? 0 : t * 0.045); s -= Math.floor(s);
    var corte = 0.62;
    if (s < corte) {
      var a = s / corte, xq = x0 + (ranura - x0) * (1 - Math.pow(1 - a, 2.6));
      var giro = a * 9 + (reducir ? 0 : t * 0.6), estre = 1 - Math.pow(a, 6) * 0.9;
      var hebra = p.r3 < 0.5 ? 0 : Math.PI, peld = p.r3 > 0.86;
      var y1 = cy + Math.sin(giro + hebra) * amp * estre;
      if (peld) { var ya = cy + Math.sin(giro) * amp * estre, yb = cy + Math.sin(giro + Math.PI) * amp * estre; y1 = ya + (yb - ya) * p.r4; }
      p.tx = xq + p.g1 * 0.8; p.ty = y1 + p.g2 * 0.8;
      p.ta = suave(0, 0.08, a) * (peld ? 0.55 : 1); p.tz = 0.5 + 0.5 * Math.cos(giro + hebra); p.te = "c"; return;
    }
    var b = (s - corte) / (1 - corte), gen = Math.floor(b * 20), mut = [0, 1, 3, 6, 7, 11].indexOf(gen % 12) >= 0;
    p.tx = ranura + 14 + b * (x1 - ranura - 14) + p.g1 * 0.6;
    p.ty = cy + p.g2 * 2.4 + (Math.floor(p.r4 * 3) - 1) * 5;
    p.ta = 0.9 * suave(0, 0.04, b) * suave(1, 0.85, b); p.tz = 0.9; p.te = mut ? "l" : "c"; p.tb = mut ? 1.15 : 0.9;
  }

  // ---- Objetivos: lanzamientos a la diana -------------------------------
  // Diez intentos, cada uno más cerca del centro (la búsqueda que acerca el
  // modelo a su objetivo) hasta dar en el panel mínimo. Ilustrativa.
  var TIROS = 10, DUR_TIRO = 0.7, CICLO_DIANA = TIROS * DUR_TIRO + 2.4, ANG_TIRO = [];
  for (var at = 0; at < TIROS; at++) ANG_TIRO.push(azar() * 6.2831853);
  function centroDiana(c) { return { cx: c.x + c.w * 0.5, cy: c.y + c.h * 0.5, R: Math.min(c.h * 0.46, c.w * 0.2) }; }
  function figDardos(p, c, t) {
    if (p.i >= 1100) { apagada(p, c, t); return; }
    var D = centroDiana(c), fase = (est.tf || 0) % CICLO_DIANA, cierre = suave(CICLO_DIANA - 0.9, CICLO_DIANA, fase);
    if (p.i < 560) {
      if (p.r1 < 0.12) {
        var rn = D.R * 0.13 * Math.sqrt(p.r3), an = p.r4 * 6.283 + (reducir ? 0 : t * 0.3);
        p.tx = D.cx + Math.cos(an) * rn; p.ty = D.cy + Math.sin(an) * rn; p.te = "l"; p.ta = 0.9; p.tz = 1; return;
      }
      var k = Math.floor(p.r2 * 4), a = p.r3 * 6.2831853, r = D.R * [0.32, 0.56, 0.8, 1.0][k] + p.g1 * 1.2;
      p.tx = D.cx + Math.cos(a) * r; p.ty = D.cy + Math.sin(a) * r; p.te = "c"; p.ta = 0.35 + 0.2 * (1 - k / 4); p.tz = 0.6; return;
    }
    if (p.i < 600) { polvo(p, c, t); return; }
    var j = Math.floor((p.i - 600) / 50) % TIROS, sale = j * DUR_TIRO;
    var rj = D.R * 0.92 * Math.pow(0.66, j), aj = ANG_TIRO[j];
    var lx = D.cx + Math.cos(aj) * rj, ly = D.cy + Math.sin(aj) * rj;
    var ox = c.x + 10, oy = c.y + c.h * (0.25 + 0.5 * ((j * 37) % 10) / 10);
    var k2 = reducir ? 1.5 : (fase - sale) / 0.5;
    if (k2 <= 0) { p.ta = 0; p.tx = ox; p.ty = oy; p.te = "b"; p.tz = 0.5; return; }
    var e = 1 - Math.pow(1 - Math.min(1, k2), 3), disp = (1 - Math.min(1, k2)) * 14 + 2.2;
    p.tx = lerp(ox, lx, e) + p.g1 * disp; p.ty = lerp(oy, ly, e) - Math.sin(Math.PI * e) * c.h * 0.22 + p.g2 * disp;
    p.te = j === TIROS - 1 ? "l" : (j > TIROS - 4 ? "b" : "c");
    p.ta = (k2 < 1 ? 0.8 : 0.95) * (1 - cierre); p.tz = 1;
    if (k2 >= 1) p.tb = 1 + 0.8 * Math.exp(-(k2 - 1) * 3);
  }

  // ---- Marco teórico: la sonda que se detiene ---------------------------
  // Los 23 cromosomas; la sonda salta de un gen con señal a otro, se para,
  // lo enciende en el color del grado al que empuja y dice su ρ.
  var LARGO_TOTAL = CROM.reduce(function (a, b) { return a + b; }, 0);
  var SONDA = [["FUBP1", 1, 0.27, -0.195], ["IDH1", 2, 0.82, -0.703], ["EGFR", 7, 0.35, 0.209], ["NOTCH1", 9, 0.97, -0.194],
               ["PTEN", 10, 0.66, 0.367], ["RB1", 13, 0.42, 0.183], ["CIC", 19, 0.72, -0.313], ["ATRX", 23, 0.52, -0.327]];
  var PARADA = 1.6, VIAJE_S = 0.9, CICLO_S = SONDA.length * (PARADA + VIAJE_S);
  function pasoSonda(c, t) {
    var paso = c.w / CROM.length, f = (est.tf || 0) % CICLO_S, k = Math.floor(f / (PARADA + VIAJE_S)), r = f - k * (PARADA + VIAJE_S);
    function gx(i) { return c.x + paso * (SONDA[i][1] - 0.5); }
    if (r < PARADA || reducir) { EF.sonda = { x: gx(k), k: k, en: reducir ? 1 : suave(0, 0.25, r) * suave(PARADA, PARADA - 0.25, r) }; return; }
    var e = (r - PARADA) / VIAJE_S; e = e < 0.5 ? 4 * e * e * e : 1 - Math.pow(-2 * e + 2, 3) / 2;
    EF.sonda = { x: lerp(gx(k), gx((k + 1) % SONDA.length), e), k: -1, en: 0 };
  }
  function figSonda(p, c, t) {
    if (p.i >= 1300) { apagada(p, c, t); return; }
    if (p.r1 > 0.92) { polvo(p, c, t); return; }
    var S = EF.sonda || { x: -1e4, k: -1, en: 0 };
    var obj = p.r2 * LARGO_TOTAL, k = 0, acc = 0;
    while (k < CROM.length - 1 && acc + CROM[k] < obj) { acc += CROM[k]; k++; }
    var paso = c.w / CROM.length, cx = c.x + paso * (k + 0.5), alto = c.h * 0.78 * CROM[k] / 248, centro = 0.4;
    var yc = c.y + c.h * 0.46, y0 = yc - alto * centro, f = p.r3;
    var pinza = Math.abs(f - centro) < 0.035 ? 0.25 : Math.min(1, Math.min(f, 1 - f) * 9);
    p.tx = cx + (p.r4 < 0.5 ? -1 : 1) * paso * 0.12 * pinza + p.g1 * paso * 0.035; p.ty = y0 + f * alto;
    var gen = null;
    for (var g = 0; g < SONDA.length; g++) if (SONDA[g][1] === k + 1 && Math.abs(f - SONDA[g][2]) < 0.05) { gen = g; break; }
    var luz = Math.exp(-Math.pow((p.tx - S.x) / 18, 2)), activo = gen !== null && S.k === gen ? S.en : 0;
    p.te = gen !== null && (activo > 0.05 || luz > 0.5) ? (SONDA[gen][3] < 0 ? "g" : "l") : "c";
    p.ta = (Math.floor(f * 14) % 2 ? 0.75 : 0.42) * (0.75 + 0.6 * luz) + activo * 0.4; p.tz = 0.6 + 0.4 * luz;
    p.tb = 1 + activo * 0.9;
  }

  // ---- Metodología: barajar y repartir ----------------------------------
  // 839 pacientes; se barajan y se reparten uno a uno: 671 a entrenamiento
  // (389 LGG, 282 GBM) y 168 a prueba (98 LGG, 70 GBM), estratificado.
  var CICLO_P = 10;
  (function () {
    var idx = []; for (var i = 0; i < 839; i++) idx.push(i);
    for (var j = idx.length - 1; j > 0; j--) { var r = Math.floor(azar() * (j + 1)), x = idx[j]; idx[j] = idx[r]; idx[r] = x; }
    var n = { tl: 0, tg: 0, pl: 0, pg: 0 };
    idx.forEach(function (pi, orden) {
      var p = parts[pi], lgg = pi < N_LGG;
      var prueba = lgg ? n.pl < 98 && (orden % 5 === 0 || n.tl >= 389) : n.pg < 70 && (orden % 4 === 0 || n.tg >= 282);
      if (prueba) { if (lgg) n.pl++; else n.pg++; } else { if (lgg) n.tl++; else n.tg++; }
      p.mtLgg = lgg; p.mtPrueba = prueba; p.mtOrden = orden; p.mtPos = prueba ? n.pl + n.pg - 1 : n.tl + n.tg - 1;
    });
  })();
  function figBaraja(p, c, t) {
    if (p.i >= 900) { apagada(p, c, t); return; }
    if (p.i >= 839) { polvo(p, c, t); return; }
    var f = (est.tf || 0) % CICLO_P, mazoX = c.x + c.w * 0.14, mazoY = c.y + c.h * 0.5;
    p.te = p.mtLgg ? "g" : "l"; p.tz = 0.9;
    var a = p.r3 * 6.2831853 + (f < 2.2 && !reducir ? f * (2 + p.r4 * 3) * (p.r1 < 0.5 ? 1 : -1) : 0), rr = Math.sqrt(p.r4) * Math.min(c.h * 0.42, c.w * 0.1);
    var mx = mazoX + Math.cos(a) * rr * 1.25, my = mazoY + Math.sin(a) * rr;
    var base = c.estrecho ? 48 : 84, ancho = c.w * (c.estrecho ? 0.55 : 0.5), cols = p.mtPrueba ? base / 2 : base;
    var x0 = c.x + c.w * 0.42, sep = ancho / base;
    var px = x0 + (p.mtPos % cols) * sep, py = (p.mtPrueba ? c.y + c.h * 0.78 : c.y + c.h * 0.06) + 4 + Math.floor(p.mtPos / cols) * sep;
    if (c.estrecho) p.tb = 0.75;
    if (reducir) { p.tx = px; p.ty = py; p.ta = 0.95; return; }
    if (f > CICLO_P - 1.1) { var v = suave(CICLO_P - 1.1, CICLO_P - 0.1, f); p.tx = lerp(px, mx, v); p.ty = lerp(py, my, v); p.ta = 0.9; return; }
    var k = (f - (2.2 + p.mtOrden / 839 * 4.4)) / 0.45;
    if (k <= 0) { p.tx = mx; p.ty = my; p.ta = 0.85; return; }
    var e = 1 - Math.pow(1 - Math.min(1, k), 3);
    p.tx = lerp(mx, px, e); p.ty = lerp(my, py, e) - Math.sin(Math.PI * e) * 26; p.ta = 0.95; p.tb = (p.tb || 1) * (k < 1 ? 1.3 : 1);
  }

  // ---- Resultados: barras que crecen y se leen --------------------------
  // ρ de Spearman con el grado de las 9 variables con más señal. Crecen una
  // a una, de la más fuerte a la más débil, y una lectura recorre las filas.
  var PESO_RHO = RHO.map(function (r) { return Math.abs(r[1]); });
  var SUMA_RHO = PESO_RHO.reduce(function (a, b) { return a + b; }, 0);
  var CICLO_R = RHO.length * 1.4 + 1.6;
  function lecturaRho(k, f) {
    if (reducir) return k === 0 ? 1 : 0;
    var r = f - 1.4 - k * 1.4;
    return Math.floor((f - 1.4) / 1.4) === k ? suave(0, 0.2, r) * suave(1.4, 1.2, r) : 0;
  }
  function figBarrasLectura(p, c, t) {
    if (p.i >= 1200) { apagada(p, c, t); return; }
    if (p.r1 > 0.92) { polvo(p, c, t); return; }
    var obj = p.r2 * SUMA_RHO, k = 0, acc = 0;
    while (k < RHO.length - 1 && acc + PESO_RHO[k] < obj) { acc += PESO_RHO[k]; k++; }
    var f = (est.tf || 0) % CICLO_R, crece = reducir ? 1 : suave(0.2 + k * 0.25, 1.2 + k * 0.25, f) * suave(CICLO_R, CICLO_R - 0.8, f);
    var on = lecturaRho(k, f), fila = c.h / RHO.length, yc = c.y + fila * (k + 0.5), eje = c.x + c.w * 0.5;
    var largo = Math.abs(RHO[k][1]) / 0.75 * c.w * 0.44 * crece, lado = RHO[k][1] < 0 ? -1 : 1;
    p.tx = eje + lado * p.r3 * largo; p.ty = yc + p.g1 * fila * (0.13 + 0.06 * on);
    p.te = lado < 0 ? "g" : "l"; p.ta = (0.55 + 0.45 * on) * suave(0, 0.04, p.r3); p.tz = 0.7 + 0.3 * on; p.tb = 1 + 0.35 * on;
  }

  // ---- Modelos: Plinko de decisión --------------------------------------
  // Los 168 pacientes de prueba caen por un árbol y se apilan en hojas LGG o
  // GBM según la matriz de confusión de la regresión logística en prueba:
  // Shiny: las cifras llegan en window.DATOS_BANDA (modulos/navegacion.R),
  // calculadas con los modelos del libro de R; si faltan, las del tablero.
  var DATOS_BANDA = window.DATOS_BANDA || {};
  var PK = DATOS_BANDA.plinko || { lgg: 98, okLgg: 83, gbm: 70, okGbm: 64 };
  var N_PK = PK.lgg + PK.gbm;
  var HOJAS = ["g", "l", "g", "g", "l", "l", "g", "l"], CICLO_K = 11;
  (function () {
    var cont = [0, 0, 0, 0, 0, 0, 0, 0], hojasG = [0, 2, 3, 6], hojasL = [1, 4, 5, 7];
    for (var i = 0; i < N_PK; i++) {
      var p = parts[i], lgg = i < PK.lgg, bien = lgg ? i < PK.okLgg : (i - PK.lgg) < PK.okGbm;
      var cand = (lgg === bien) ? hojasG : hojasL, hoja = cand[Math.floor(azar() * 4)];
      p.pkLgg = lgg; p.pkHoja = hoja; p.pkAltura = cont[hoja]++;
    }
    var orden = []; for (var j = 0; j < N_PK; j++) orden.push(j);
    for (var k = orden.length - 1; k > 0; k--) { var r = Math.floor(azar() * (k + 1)), x = orden[k]; orden[k] = orden[r]; orden[r] = x; }
    orden.forEach(function (pi, o) { parts[pi].pkSale = 0.4 + o * 0.042; });
  })();
  function arbolPlinko(c) {
    var ancho = Math.min(c.w * 0.86, 640), alt = c.h * 0.58, y0 = c.y + 6, cx = c.x + c.w * 0.5;
    return { ancho: ancho, alt: alt, y0: y0, nodo: function (niv, j) { return [cx + ((j + 0.5) / Math.pow(2, niv) - 0.5) * ancho, y0 + niv * alt / 3]; } };
  }
  function figPlinko(p, c, t) {
    if (p.i >= 760) { apagada(p, c, t); return; }
    var A = arbolPlinko(c), nodo = A.nodo;
    if (p.i >= N_PK) {
      if (p.i < 468) {
        var e = Math.floor(p.r2 * 7), niv = e < 1 ? 0 : (e < 3 ? 1 : 2), jj = e - (niv === 0 ? 0 : niv === 1 ? 1 : 3), a = nodo(niv, jj);
        if (p.r1 < 0.45) { var an = p.r3 * 6.283; p.tx = a[0] + Math.cos(an) * 5 * Math.sqrt(p.r4); p.ty = a[1] + Math.sin(an) * 5 * Math.sqrt(p.r4); p.ta = 1; p.tz = 1; p.te = "c"; p.tb = 1.25; return; }
        var hijo = nodo(niv + 1, jj * 2 + (p.r1 < 0.72 ? 0 : 1));
        p.tx = lerp(a[0], hijo[0], p.r3); p.ty = lerp(a[1], hijo[1], p.r3); p.ta = 0.45; p.te = "c"; p.tz = 0.6; return;
      }
      if (p.i < 688) {
        var h = Math.floor(p.r2 * 8), w = A.ancho / 8 * 0.7;
        p.tx = nodo(3, h)[0] - w / 2 + p.r3 * w; p.ty = c.y + c.h - 2 + p.g1 * 0.6; p.te = HOJAS[h]; p.ta = 0.5; p.tz = 0.6; return;
      }
      polvo(p, c, t); return;
    }
    var f = (est.tf || 0) % CICLO_K, k = reducir ? 9 : f - p.pkSale, salida = reducir ? 0 : suave(CICLO_K - 0.8, CICLO_K, f);
    p.te = p.pkLgg ? "g" : "l"; p.tz = 1;
    if (k < 0) { p.ta = 0; p.tx = c.x + c.w * 0.5; p.ty = A.y0 - 10; return; }
    var tn = 0.32, seg = Math.floor(k / tn), u = k / tn - seg;
    function paso(n) { return n < 3 ? nodo(n, p.pkHoja >> (3 - n)) : [nodo(3, p.pkHoja)[0], A.y0 + A.alt]; }
    var pila = nodo(3, p.pkHoja), wcol = A.ancho / 8 * 0.7, porFila = 5, celda = wcol / porFila;
    var finX = pila[0] - wcol / 2 + ((p.pkAltura % porFila) + 0.5) * celda, finY = c.y + c.h - 8 - Math.floor(p.pkAltura / porFila) * celda;
    if (seg >= 3) {
      var cae = Math.min(1, (k - 3 * tn) / 0.35), desde = paso(3);
      p.tx = lerp(desde[0], finX, cae); p.ty = lerp(desde[1], finY, cae * cae);
      p.ta = 0.95 * (1 - salida); p.tb = cae < 1 ? 1.3 : 1; return;
    }
    var P0 = paso(seg), P1 = paso(seg + 1);
    p.tx = lerp(P0[0], P1[0], u); p.ty = lerp(P0[1], P1[1], u) - Math.sin(Math.PI * u) * 10; p.ta = 1; p.tb = 1.35;
  }

  // ---- Predicción: tu paciente entre las dos cohortes -------------------
  // Las cohortes (487 LGG y 352 GBM) y una partícula blanca: el perfil del
  // formulario, colocado según la probabilidad de GBM del modelo elegido en
  // la cabecera. La lee de la propia pestaña (el ancho de la barra del
  // mosaico de ese modelo) y viaja cuando cambia; al llegar deja una onda en
  // la cohorte a la que se acerca.
  var PAC = { p: 0.5, desde: 0.5, t0: -10, leidoEn: -1 };
  function leerProbabilidad() {
    var elegido = document.querySelector("#contenido-pestana .segmentado-oscuro label:has(input:checked)");
    var nombre = elegido ? elegido.textContent.trim() : null;
    var mosaicos = document.querySelectorAll("#contenido-pestana .prob-mosaico-nombre");
    for (var i = 0; i < mosaicos.length; i++) {
      if (nombre && mosaicos[i].textContent.trim() !== nombre) continue;
      var barra = mosaicos[i].parentElement && mosaicos[i].parentElement.querySelector(".prob-barra");
      var v = barra ? parseFloat(barra.style.width) : NaN;
      if (isFinite(v)) return v / 100;
    }
    return null;
  }
  function pasoPaciente(c, t) {
    if (t - PAC.leidoEn < 0.25) return;
    PAC.leidoEn = t;
    var v = leerProbabilidad();
    if (v === null || Math.abs(v - PAC.p) < 0.002) return;
    var k = curvaViaje((t - PAC.t0) / 1.1);
    PAC.desde = lerp(PAC.desde, PAC.p, k); PAC.p = v; PAC.t0 = reducir ? -10 : t;
  }
  function posPaciente(c, t) {
    // El paciente recorre el hueco entre los dos discos, sin entrar en ellos:
    // en un extremo, igual que la cohorte LGG; en el otro, igual que la GBM
    var rmax = Math.min(c.h * 0.4, c.w * 0.13), rG = rmax * Math.sqrt(N_GBM / N_LGG);
    var cxL = c.x + rmax + 4, cxG = c.x + c.w - rG - 4;
    var k = reducir ? 1 : curvaViaje((t - PAC.t0) / 1.1), pp = lerp(PAC.desde, PAC.p, k);
    return { pp: pp, x: lerp(cxL + rmax + 42, cxG - rG - 42, pp), y: c.y + c.h * 0.5 - (k < 1 ? Math.sin(Math.PI * k) : 0) * c.h * 0.25,
             cxL: cxL, cxG: cxG, rmax: rmax, hueco: cxG - rG - cxL - rmax, llega: PAC.t0 + 1.1 };
  }
  function figPaciente(p, c, t) {
    if (p.i >= 900) { apagada(p, c, t); return; }
    var P = posPaciente(c, t), cy = c.y + c.h * 0.5;
    if (p.i === 0) { p.tx = P.x; p.ty = P.y; p.te = "b"; p.ta = 1; p.tz = 1; p.tb = 2.2; return; }
    if (p.i < 40) {
      var a = p.r3 * 6.283 + (reducir ? 0 : t * 1.5);
      p.tx = P.x + Math.cos(a) * (10 + p.r4 * 8); p.ty = P.y + Math.sin(a) * (10 + p.r4 * 8); p.te = "b"; p.ta = 0.4; p.tz = 0.7; return;
    }
    var j = p.i - 40;
    if (j >= 839) { polvo(p, c, t); return; }
    var lgg = j < N_LGG, jj = lgg ? j : j - N_LGG, n = lgg ? N_LGG : N_GBM;
    var radio = P.rmax * (lgg ? 1 : Math.sqrt(N_GBM / N_LGG)), cx = lgg ? P.cxL : P.cxG;
    var r = radio * Math.sqrt((jj + 0.5) / n), ang = jj * 2.39996323 + (reducir ? 0 : t * 0.04 * (lgg ? 1 : -1));
    var cercana = P.pp >= 0.5 ? !lgg : lgg, dt = t - P.llega, onda = 0;
    if (cercana && dt > 0 && dt < 1.6) onda = Math.exp(-Math.pow((r / radio - dt * 0.9) / 0.12, 2)) * (1 - dt / 1.6);
    p.tx = cx + Math.cos(ang) * r * (1 + onda * 0.08); p.ty = cy + Math.sin(ang) * r * (1 + onda * 0.08);
    p.te = lgg ? "g" : "l"; p.ta = 0.85; p.tz = 0.8; p.tb = 1 + onda * 1.2;
  }

  // ---- Limitaciones: rebotes en la frontera -----------------------------
  // Los pacientes se mueven dentro de lo que esta cohorte permite concluir y
  // rebotan contra la frontera, que se ilumina donde chocan; de vez en cuando
  // uno la atraviesa y se apaga fuera. Ilustrativa.
  (function () {
    for (var i = 0; i < 600; i++) {
      var p = parts[i], a = p.r3 * 6.283, r = Math.sqrt(p.r4) * 0.85, va = azar() * 6.283, vv = 0.08 + azar() * 0.18;
      p.fu = Math.cos(a) * r; p.fv = Math.sin(a) * r; p.fvu = Math.cos(va) * vv; p.fvv = Math.sin(va) * vv; p.ffuera = -1;
    }
  })();
  EF.choques = [];
  function pasoRebotes(c, t, dt) {
    EF.choques = EF.choques.filter(function (ch) { return t - ch[1] < 0.9; });
    EF.dtRebote = reducir ? 0 : dt;
  }
  function figRebotes(p, c, t) {
    if (p.i >= 1000) { apagada(p, c, t); return; }
    var cx = c.x + c.w * 0.5, cy = c.y + c.h * 0.5, Rx = Math.min(c.w * 0.3, c.h * 0.9), Ry = c.h * 0.42;
    if (p.i < 600) {
      if (p.ffuera >= 0) {
        var k = (t - p.ffuera) / 1.8;
        p.tx = cx + p.fu * Rx * (1 + k * 0.7); p.ty = cy + p.fv * Ry * (1 + k * 0.7); p.ta = Math.max(0, 1 - k); p.te = "c"; p.tz = 0.5;
        if (k >= 1) { p.ffuera = -1; p.fu = (p.r1 - 0.5) * 0.4; p.fv = (p.r2 - 0.5) * 0.4; }
        return;
      }
      p.fu += p.fvu * (EF.dtRebote || 0); p.fv += p.fvv * (EF.dtRebote || 0);
      var d = p.fu * p.fu + p.fv * p.fv;
      if (d > 0.94) {
        var ang = Math.atan2(p.fv, p.fu);
        if (Math.random() < 0.02) p.ffuera = t;
        else {
          var nu = p.fu / Math.sqrt(d), nv = p.fv / Math.sqrt(d), dot = p.fvu * nu + p.fvv * nv;
          p.fvu -= 2 * dot * nu; p.fvv -= 2 * dot * nv; p.fu = nu * 0.96; p.fv = nv * 0.96;
          if (EF.choques.length < 14) EF.choques.push([ang, t]);
        }
      }
      p.tx = cx + p.fu * Rx; p.ty = cy + p.fv * Ry; p.te = p.r2 < 0.58 ? "g" : "l"; p.ta = 0.9; p.tz = 0.8; return;
    }
    if (p.i < 940) {
      var b = p.r3 * 6.2831853, luz = 0;
      for (var q = 0; q < EF.choques.length; q++) {
        var da = Math.abs(((b - EF.choques[q][0] + 9.42478) % 6.28318) - 3.14159);
        luz += Math.exp(-da * da / 0.02) * (1 - (t - EF.choques[q][1]) / 0.9);
      }
      p.tx = cx + Math.cos(b) * (Rx + 6 + p.g1); p.ty = cy + Math.sin(b) * (Ry + 6 + p.g1);
      p.te = "c"; p.ta = 0.35 + Math.min(0.65, luz * 0.9); p.tz = 0.8; p.tb = 1 + Math.min(1, luz) * 0.5; return;
    }
    polvo(p, c, t);
  }

  // ---- Conclusiones: de 23 variables a 4 --------------------------------
  // Las 19 que no hacen falta se deshacen y caen; IDH1, edad, TP53 y PTEN
  // crecen hasta el tamaño de su importancia en XGBoost (43,0 · 13,1 · 12,2
  // · 5,3) y el AUC apenas baja: 0,923 con las 23, 0,915 con las 4.
  var VARS23 = ["Género", "Edad", "Raza", "IDH1", "TP53", "ATRX", "PTEN", "EGFR", "CIC", "MUC16", "PIK3CA", "NF1",
                "PIK3R1", "FUBP1", "RB1", "NOTCH1", "BCOR", "CSMD3", "SMARCA4", "GRIN2A", "IDH2", "FAT4", "PDGFRA"];
  var QUEDAN = { "IDH1": [42.98, "g", 0], "Edad": [13.11, "l", 1], "TP53": [12.21, "g", 2], "PTEN": [5.34, "l", 3] };
  var AUC_PANEL = "AUC 0,915 con 4 · 0,923 con 23";
  // Shiny: el panel y sus importancias salen del capítulo de modelado en R
  if (DATOS_BANDA.panel) {
    QUEDAN = {};
    DATOS_BANDA.panel.quedan.forEach(function (q, i) { QUEDAN[q.nombre] = [q.imp, q.lado, i]; });
    AUC_PANEL = DATOS_BANDA.panel.rotulo;
  }
  var IMP = Object.keys(QUEDAN).map(function (v) { return QUEDAN[v][0]; });
  var IMP_MAX = Math.max.apply(null, IMP), IMP_MIN = Math.min.apply(null, IMP);
  var PASO_Q = IMP.length > 1 ? 0.6 / (IMP.length - 1) : 0;
  var CICLO_C = 11;
  function juntaCuatro(f) { return reducir ? 1 : suave(4.4, 6.4, f) * (1 - suave(CICLO_C - 0.9, CICLO_C, f)); }
  function figVeintitres(p, c, t) {
    if (p.i >= 1200) { apagada(p, c, t); return; }
    var f = (est.tf || 0) % CICLO_C, k = Math.floor(p.r2 * VARS23.length), q = QUEDAN[VARS23[k]];
    var paso = c.w / VARS23.length, x0 = c.x + paso * (k + 0.5), y0 = c.y + c.h * 0.45, r0 = Math.min(paso * 0.38, c.h * 0.1);
    var an = p.r3 * 6.283 + (reducir ? 0 : t * 0.15), rr = Math.sqrt(p.r4), cierra = reducir ? 0 : suave(CICLO_C - 0.9, CICLO_C, f);
    if (!q) {
      var cae = reducir ? 1 : suave(2.2 + (k % 7) * 0.12, 4.2 + (k % 7) * 0.12, f);
      p.tx = x0 + Math.cos(an) * rr * r0 + p.g1 * cae * 14; p.ty = y0 + Math.sin(an) * rr * r0 + cae * cae * c.h * 0.5 * (0.5 + p.r1);
      p.te = "c"; p.ta = 0.7 * (1 - cae) * (1 - cierra) + 0.7 * cierra; p.tz = 0.6; return;
    }
    var junta = juntaCuatro(f), xf = c.x + c.w * (0.2 + q[2] * PASO_Q), rf = Math.min(c.h * 0.36, c.w * 0.1) * Math.sqrt(q[0] / IMP_MAX);
    var R = lerp(r0, rf, junta), X = lerp(x0, xf, junta);
    p.tx = X + Math.cos(an) * rr * R; p.ty = y0 + Math.sin(an) * rr * R;
    // Todas tienen las mismas partículas: en la grande (IDH1) crecen para que
    // no se vea rala
    p.tb = 1 + junta * 0.4 * (Math.sqrt(q[0] / IMP_MIN) - 1);
    p.te = junta > 0.3 ? q[1] : "c"; p.ta = 0.95; p.tz = 0.6 + 0.4 * (1 - p.r4);
  }

  // Lo que algunas figuras calculan una vez por fotograma, antes de colocar
  var PASOS = { edad: pasoEdad, sonda: pasoSonda, paciente: pasoPaciente, rebotes: pasoRebotes };

  function polvo(p, c, t) {
    p.tx = c.x - 20 + p.r3 * (c.w + 40) + (reducir ? 0 : Math.sin(t * 0.2 + p.f) * 6);
    p.ty = c.R.top + p.r4 * c.R.height + (reducir ? 0 : Math.cos(t * 0.17 + p.f) * 4);
    p.te = "c"; p.ta = 0.22; p.tz = 0.5;
  }

  var FUNCIONES = { helice: figHelice, edad: figEdad, secuenciador: figSecuenciador, dardos: figDardos,
                    sonda: figSonda, baraja: figBaraja, lupa: figLupa, barras: figBarrasLectura, plinko: figPlinko,
                    paciente: figPaciente, rebotes: figRebotes, veintitres: figVeintitres, libro: figLibro };

  // Rótulos de las figuras con datos (se dibujan ya colocadas)
  var AREA_EDAD = { g: 0, l: 0 };
  ["g", "l"].forEach(function (k) { for (var i = 1; i < DENS[k].length; i++) AREA_EDAD[k] += (DENS[k][i - 1] + DENS[k][i]) / 2; });
  var MONO = "500 10px 'IBM Plex Mono', ui-monospace, monospace", SANS = "600 11px 'Plus Jakarta Sans', system-ui, sans-serif";
  function rotulos(fig, c, alfa) {
    if (alfa < 0.05) return;
    ctx.globalCompositeOperation = "source-over";
    ctx.textAlign = "center";
    var a3 = alfa.toFixed(3), tinta = "rgba(185, 199, 222," + (0.75 * alfa).toFixed(3) + ")";
    var azul = "rgba(143, 184, 242," + a3 + ")", coral = "rgba(255, 148, 114," + a3 + ")";
    function texto(s, x, y, color, fuente, lado) {
      // Un borde del azul noche: el texto se lee aunque pase sobre partículas
      ctx.font = fuente || MONO; ctx.textAlign = lado || "center";
      ctx.lineWidth = 3; ctx.lineJoin = "round"; ctx.strokeStyle = "rgba(11, 23, 48," + (0.85 * alfa).toFixed(3) + ")";
      ctx.strokeText(s, x, y);
      ctx.fillStyle = color || tinta; ctx.fillText(s, x, y);
    }
    if (fig === "edad") {
      var E = ejeEdad(c), u = EF.uEdad === undefined ? 0.5 : EF.uEdad, x = E.x0 + u * E.an;
      ctx.fillStyle = "rgba(143, 184, 242," + (0.3 * alfa).toFixed(3) + ")";
      ctx.fillRect(E.x0, E.base + 3, E.an, 1);
      [20, 40, 60, 80].forEach(function (a) { texto(String(a), E.x0 + (a - 10) / 80 * E.an, E.base + 16); });
      // El cursor y lo que dice: de los pacientes de esa edad, qué parte es
      // de cada grado (densidades pesadas por el tamaño de cada grupo)
      ctx.fillStyle = "rgba(238, 243, 249," + (0.5 * alfa).toFixed(3) + ")";
      ctx.fillRect(x - 0.5, E.base - E.alto - 4, 1, E.alto + 8);
      var dg = 389 * densidad("g", u) / AREA_EDAD.g, dl = 282 * densidad("l", u) / AREA_EDAD.l, pg = dl / ((dg + dl) || 1);
      var xs = x + 8, lado = "left", y0 = E.base - E.alto + 4;
      texto(Math.round(10 + u * 80) + " años", xs, y0, "rgba(238, 243, 249," + a3 + ")", SANS, lado);
      texto("LGG " + Math.round((1 - pg) * 100) + " %", xs, y0 + 14, azul, MONO, lado);
      texto("GBM " + Math.round(pg * 100) + " %", xs, y0 + 27, coral, MONO, lado);
      if (!c.estrecho) texto("edad al diagnóstico", E.x0 + E.an, E.base + 30, tinta, MONO, "right");
    }
    if (fig === "secuenciador" && !c.estrecho) {
      texto("una muestra cada vez", c.x + c.w * 0.25, c.y + c.h - 2);
      texto("secuencia leída · 20 genes", c.x + c.w * 0.76, c.y + c.h * 0.5 - 18);
    }
    if (fig === "dardos") {
      var D = centroDiana(c), f = (est.tf || 0) % CICLO_DIANA, j = Math.min(TIROS, Math.floor(f / DUR_TIRO) + 1);
      texto("intento " + j + " de " + TIROS, D.cx + D.R + 14, D.cy - 4, tinta, MONO, "left");
      if (j === TIROS && !c.estrecho) texto("panel mínimo", D.cx + D.R + 14, D.cy + 10, coral, MONO, "left");
    }
    if (fig === "sonda" && EF.sonda && EF.sonda.k >= 0) {
      var G = SONDA[EF.sonda.k], col = G[3] < 0 ? "143, 184, 242" : "255, 148, 114", aa = (alfa * EF.sonda.en).toFixed(3);
      texto(G[0], EF.sonda.x, c.y + 8, "rgba(" + col + "," + aa + ")", SANS);
      texto("ρ = " + coma(G[3], 3), EF.sonda.x, c.y + c.h - 2, "rgba(185, 199, 222," + (0.75 * alfa * EF.sonda.en).toFixed(3) + ")");
    }
    if (fig === "baraja") {
      var xr = c.x + c.w * (c.estrecho ? 0.97 : 0.92);
      texto(c.estrecho ? "Entren. · 671" : "Entrenamiento · 671 (389 LGG · 282 GBM)", xr, c.y + c.h * 0.06 - 4, tinta, MONO, "right");
      texto(c.estrecho ? "Prueba · 168" : "Prueba · 168 (98 LGG · 70 GBM)", xr, c.y + c.h * 0.78 - 4, tinta, MONO, "right");
      if (!c.estrecho) texto("839 pacientes", c.x + c.w * 0.14, c.y + c.h - 2);
    }
    if (fig === "barras") {
      var fila = c.h / RHO.length, eje = c.x + c.w * 0.5, fr = (est.tf || 0) % CICLO_R;
      RHO.forEach(function (r, k) {
        var on = lecturaRho(k, fr), yk = c.y + fila * (k + 0.5) + 3.5, neg = r[1] < 0;
        texto(r[0], eje + (neg ? 6 : -6), yk, "rgba(185, 199, 222," + ((0.55 + 0.45 * on) * alfa).toFixed(3) + ")", MONO, neg ? "left" : "right");
        if (on > 0.05) {
          var largo = Math.abs(r[1]) / 0.75 * c.w * 0.44;
          texto(coma(r[1], 3), eje + (neg ? -largo - 6 : largo + 6), yk,
                "rgba(" + (neg ? "143, 184, 242" : "255, 148, 114") + "," + (on * alfa).toFixed(3) + ")", MONO, neg ? "right" : "left");
        }
      });
      ctx.fillStyle = "rgba(143, 184, 242," + (0.35 * alfa).toFixed(3) + ")";
      ctx.fillRect(eje - 0.5, c.y, 1, c.h);
    }
    if (fig === "plinko") {
      var A = arbolPlinko(c), fk = (est.tf || 0) % CICLO_K, n = 0;
      for (var i = 0; i < N_PK; i++) if (reducir || fk - parts[i].pkSale > 1.3) n++;
      var aciertos = PK.okLgg + PK.okGbm, bien = Math.round(n * aciertos / N_PK);
      if (!c.estrecho) texto("regresión logística · prueba", c.x + c.w * 0.5 - A.ancho / 2, A.y0 + 4, tinta, MONO, "left");
      texto(n + " de " + N_PK + " · " + bien + " aciertos" + (n === N_PK ? " (" + coma(aciertos / N_PK * 100, 1) + " %)" : ""), c.x + c.w * 0.5 + A.ancho / 2, A.y0 + 4, tinta, MONO, "right");
    }
    if (fig === "paciente") {
      var P = posPaciente(c, est.tv || 0), cy = c.y + c.h * 0.5;
      texto("LGG · 487", P.cxL, cy + P.rmax + 16, azul, SANS);
      texto("GBM · 352", P.cxG, cy + P.rmax * Math.sqrt(N_GBM / N_LGG) + 16, coral, SANS);
      texto("P(GBM) " + coma(P.pp * 100, 1) + " %", P.x, P.y - 18, "rgba(238, 243, 249," + a3 + ")", MONO);
      if (P.hueco > 150) texto("tu paciente", P.x, P.y + 26, tinta, MONO);
    }
    if (fig === "rebotes") {
      texto("839 pacientes · una sola fuente (TCGA)", c.x + c.w * 0.5, c.y + c.h - 2);
    }
    if (fig === "veintitres") {
      var fv = (est.tf || 0) % CICLO_C, junta = juntaCuatro(fv);
      if (junta > 0.6) {
        Object.keys(QUEDAN).forEach(function (v) {
          var q = QUEDAN[v], xf = c.x + c.w * (0.2 + q[2] * PASO_Q), rf = Math.min(c.h * 0.36, c.w * 0.1) * Math.sqrt(q[0] / IMP_MAX);
          var aq = ((junta - 0.6) / 0.4 * alfa).toFixed(3);
          texto(v, xf, c.y + c.h * 0.45 + rf + 16, "rgba(" + (q[1] === "g" ? "143, 184, 242" : "255, 148, 114") + "," + aq + ")", SANS);
        });
        texto(AUC_PANEL, c.x + c.w * 0.5, c.y + 8, "rgba(185, 199, 222," + (0.75 * alfa * (junta - 0.6) / 0.4).toFixed(3) + ")");
      } else if (!c.estrecho) texto("23 variables", c.x + c.w * 0.5, c.y + 8);
    }
    if (fig === "lupa") {
      var L = centroLupa(c, est.tv), paso = c.w / VARIABLES.length;
      var k = Math.max(0, Math.min(VARIABLES.length - 1, Math.floor((L[0] - c.x) / paso)));
      var RL = Math.min(c.h * 0.36, c.w * 0.2), d = DIRECCION[VARIABLES[k]];
      texto(VARIABLES[k], L[0], L[1] - RL - 10, d === "g" ? azul : (d === "l" ? coral : tinta), "600 11px 'IBM Plex Mono', ui-monospace, monospace");
    }
    if (fig === "libro") {
      var B = paginaLibro(c);
      texto("Jupyter Book · EDA y modelado", B.cx, B.top + B.ph + 18);
    }
    ctx.globalCompositeOperation = "lighter";
  }

  // ------------------------------------------------------------ caja
  // Dónde va la figura dentro de la banda: a la derecha del texto y, si la
  // cabecera tiene un control (Exploración, Predicción), antes de él.
  function cajaDe(R, el, esIntro) {
    var estrecho = R.width < 768, c = { R: R, estrecho: estrecho };
    if (estrecho) {
      c.x = R.left + 16; c.w = R.width - 32; c.h = esIntro ? 60 : 110; c.y = R.bottom - c.h - 12;
      c.hx0 = R.left - 40; c.hx1 = R.right + 60; c.hcy = c.y + c.h / 2; c.amp = Math.min(c.h * 0.36, 24); c.vueltas = 3;
      return c;
    }
    var texto = R.left + R.width * 0.45, limite = R.right - 32;
    if (el) {
      // Se mide el TEXTO (con un Range), no la caja del bloque: los títulos
      // son bloques que ocupan toda su columna aunque el texto sea corto, y la
      // figura de Exploración acababa encogida encima del selector.
      var bloques = el.querySelectorAll(".titular-eyebrow, .titular, .titular-texto, .page-title, .page-subtitle, .brand-subtitle, .brand-title");
      var rango = document.createRange();
      for (var k = 0; k < bloques.length; k++) {
        rango.selectNodeContents(bloques[k]);
        var hijos = rango.getBoundingClientRect();
        if (hijos.width) texto = Math.max(texto, Math.min(hijos.right, R.left + R.width * 0.62) + 40);
      }
      var sel = el.querySelector(".titular-selector, .boton-libro"), rs = sel ? sel.getBoundingClientRect() : null;
    }
    c.y = R.top + 18; c.h = R.height - 36;
    if (rs && rs.width) {
      if (rs.left - 32 - texto >= 260) limite = Math.min(limite, rs.left - 32);
      else c.h = Math.max(80, rs.top - 14 - c.y);   // no cabe al lado: va encima del control
    }
    c.x = texto; c.w = Math.max(200, limite - texto);
    // La hélice se sale por la derecha a propósito: es lo que da la sensación
    // de flujo continuo
    c.hx0 = R.left + R.width * 0.5; c.hx1 = R.right + 60; c.hcy = R.top + R.height * (esIntro ? 0.46 : 0.5);
    c.amp = Math.min(R.height * (esIntro ? 0.3 : 0.31), esIntro ? 52 : 64); c.vueltas = esIntro ? 3.2 : 3.4;
    return c;
  }

  // ---------------------------------------------------------- sprites
  // Copia del sombreador de intro.js: disco smoothstep(1, 0.15, r) y triángulo.
  // La clave es un número (no un texto): se busca ~2.000 veces por fotograma
  // y armar un texto en cada búsqueda llenaba la memoria de basura, que el
  // navegador recogía a ratos con una pausa visible.
  var SPRITES = new Map(), LADO = 32;
  function sprite(col, tri) { return spriteRGB(col[0] | 0, col[1] | 0, col[2] | 0, tri); }
  function spriteRGB(r, gr, b, tri) {
    var clave = ((r * 256 + gr) * 256 + b) * 2 + (tri ? 1 : 0), hecho = SPRITES.get(clave);
    if (hecho) return hecho;
    var col = [r, gr, b];
    var cv = document.createElement("canvas"); cv.width = cv.height = LADO;
    var g = cv.getContext("2d"), img = g.createImageData(LADO, LADO), d = img.data;
    for (var py = 0; py < LADO; py++) for (var px = 0; px < LADO; px++) {
      var qx = (px + 0.5) / LADO * 2 - 1, qy = -((py + 0.5) / LADO * 2 - 1), a;
      if (!tri) a = suave(1.0, 0.15, Math.sqrt(qx * qx + qy * qy));
      else a = suave(0.10, -0.06, Math.max(Math.abs(qx) * 0.866 + qy * 0.5, -qy) - 0.42);
      var o = (py * LADO + px) * 4;
      d[o] = col[0]; d[o + 1] = col[1]; d[o + 2] = col[2]; d[o + 3] = Math.round(a * 255);
    }
    g.putImageData(img, 0, 0);
    // Los colores intermedios de una transición se redondean a pasos de 8
    // para no crear cientos de sprites distintos
    SPRITES.set(clave, cv);
    return cv;
  }

  function dibujar(x, y, tam, col, alfa, tri, halo) {
    dibujarRGB(x, y, tam, col[0] | 0, col[1] | 0, col[2] | 0, alfa, tri, halo);
  }
  // Partícula con halo en UNA sola imagen: el halo (16 % de opacidad, 4,5
  // veces el tamaño) y el núcleo ya sumados en el sprite. Antes eran dos
  // dibujos por partícula; medido en el navegador del autor, las llamadas a
  // drawImage eran casi la mitad del coste del lienzo.
  var HALOS = new Map(), LADO_HALO = 72;
  function spriteHaloRGB(r, g, b, tri) {
    var clave = ((r * 256 + g) * 256 + b) * 2 + (tri ? 1 : 0), hecho = HALOS.get(clave);
    if (hecho) return hecho;
    var cv = document.createElement("canvas"); cv.width = cv.height = LADO_HALO;
    var cg = cv.getContext("2d"), nucleo = LADO_HALO / 4.5;
    cg.globalCompositeOperation = "lighter";
    cg.globalAlpha = 0.16;
    cg.drawImage(spriteRGB(r, g, b, false), 0, 0, LADO_HALO, LADO_HALO);
    cg.globalAlpha = 1;
    cg.drawImage(spriteRGB(r, g, b, tri), (LADO_HALO - nucleo) / 2, (LADO_HALO - nucleo) / 2, nucleo, nucleo);
    HALOS.set(clave, cv);
    return cv;
  }
  function dibujarRGB(x, y, tam, r, g, b, alfa, tri, halo) {
    if (alfa <= 0.01 || tam <= 0.2) return;
    ctx.globalAlpha = Math.min(1, alfa);
    if (halo && alfa > 0.1) {
      var h = tam * 4.5;
      ctx.drawImage(spriteHaloRGB(r, g, b, tri), x - h / 2, y - h / 2, h, h);
      return;
    }
    ctx.drawImage(spriteRGB(r, g, b, tri), x - tam / 2, y - tam / 2, tam, tam);
  }

  // --------------------------------------------------------------- lienzo
  var lienzo = document.createElement("canvas");
  lienzo.className = "banda-particulas";
  lienzo.setAttribute("aria-hidden", "true");
  var ctx = lienzo.getContext("2d");
  var W = 0, H = 0, dpr = 1;
  function medir() {
    dpr = Math.min(window.devicePixelRatio || 1, 2);
    W = window.innerWidth; H = window.innerHeight;
    lienzo.width = Math.round(W * dpr); lienzo.height = Math.round(H * dpr);
    est.forzar = true;   // cambiar el tamaño del lienzo lo borra
    ctx.setTransform(dpr, 0, 0, dpr, 0, 0);
    document.documentElement.style.setProperty("--ancho-util", document.documentElement.clientWidth + "px");
  }

  // ----------------------------------------------------------- lectura
  // La pestaña activa se lee de dbc.Tabs, que cambia en el mismo clic (el
  // "2 de 13" del índice llega tarde del servidor).
  function leerPosicion() {
    var pestanas = document.querySelectorAll("#tabs-principal .nav-link");
    for (var k = 0; k < pestanas.length; k++) if (pestanas[k].classList.contains("active")) return k + 1;
    var el = document.getElementById("indice-posicion");
    var m = el && /(\d+)\s+de\s+(\d+)/.exec(el.textContent || "");
    return m ? parseInt(m[1], 10) : 1;
  }
  function cabecera() {
    return document.querySelector("#contenido-pestana .vista-pestana > .page-header:first-child, " +
                                  "#contenido-pestana .vista-pestana > .titular-fila:first-child");
  }
  function ponerCeja(cab, pos) {
    // React reutiliza el mismo elemento de cabecera entre pestañas y solo le
    // cambia el texto: la ceja se rehace cuando cambia la pestaña, no el nodo.
    if (!cab || !cab.classList.contains("page-header") || huella(cab) === est.huellaVieja) return;
    if (cab.dataset.cejaPos === String(pos)) return;
    var activo = document.querySelectorAll(".indice-item")[pos - 1];
    var bloque = activo && activo.closest(".indice-bloque");
    var titulo = bloque && bloque.querySelector(".indice-bloque-titulo");
    var nombre = activo && activo.querySelector(".indice-item-nombre");
    if (!titulo || !nombre) return;
    var num = titulo.querySelector(".indice-bloque-num");
    var texto = (num ? num.textContent + " " : "") + titulo.textContent.replace(num ? num.textContent : "", "").trim();
    cab.dataset.ceja = texto + " · " + nombre.textContent;
    cab.dataset.cejaPos = String(pos);
  }

  // ------------------------------------------------------------ estado
  // `clave` es dónde está la figura: "portada" (no se dibuja), "intro" (la
  // barra superior) o "p<N>" (la cabecera de la pestaña N).
  var est = { clave: "portada", fig: "helice", pendiente: null, t0: -1e9, dur: 1, rigido: false, hoverPeldano: -1,
              vis: 0, t0Prog: -1e9, visProg: 0, progPendiente: false, fluido: 0 };
  window.__banda = est;   // solo para depurar desde la consola

  function claveActual(enSeccion, pos) {
    var b = document.body.classList;
    if (!b.contains("intro-cerrada") && !b.contains("portada-saliendo")) return "portada";
    if (!enSeccion) return "intro";
    return "p" + pos;
  }

  // ------------------------------------------------------------ ratón
  // Igual que en la portada (intro.js): las partículas cercanas al puntero
  // se apartan y brillan. Radio y empuje equivalen a los del sombreador de la
  // portada (0,16 y 0,035 en coordenadas de pantalla), pasados a píxeles.
  var raton = { x: -1e4, y: -1e4, on: false };
  window.addEventListener("pointermove", function (e) {
    if (e.pointerType === "touch") { raton.on = false; return; }
    raton.x = e.clientX; raton.y = e.clientY; raton.on = true;
  }, { passive: true });
  document.addEventListener("pointerleave", function () { raton.on = false; });
  window.addEventListener("blur", function () { raton.on = false; });
  // En la portada el radio es 0,16 y el empuje 0,035 en coordenadas
  // normalizadas (una unidad = media altura de la ventana): en píxeles,
  // el 8 % y el 1,75 % del alto de la ventana.
  // Devuelve [x, y, factor de tamaño, factor de brillo] en un mismo arreglo
  // reutilizado: se llama una vez por partícula y fotograma, y crear uno
  // nuevo cada vez era basura para el recolector de memoria.
  var REAC = [0, 0, 1, 1];
  function reaccion(x, y, lesion) {
    REAC[0] = x; REAC[1] = y; REAC[2] = 1; REAC[3] = 1;
    if (!raton.on || reducir) return REAC;
    var RADIO_RATON = 0.08 * H, EMPUJE_RATON = 0.0175 * H;
    var dx = x - raton.x, dy = y - raton.y, d2 = dx * dx + dy * dy;
    if (d2 > RADIO_RATON * RADIO_RATON) return REAC;
    var d = Math.sqrt(d2) || 1, inf = suave(RADIO_RATON, 0, d);
    REAC[0] = x + dx / d * inf * EMPUJE_RATON; REAC[1] = y + dy / d * inf * EMPUJE_RATON;
    REAC[2] = 1 + inf * 0.5; REAC[3] = 1 + inf * (lesion ? 0.2 : 0.7);
    return REAC;
  }

  // ---------------------------------------------------- franja fluida
  // Cada cabecera tiene su altura natural. Cuando cambia de una pestaña a
  // otra, el cambio de alto de la franja azul se anima solo con pintado:
  //   crece  -> la cabecera nueva se destapa con clip-path desde el alto viejo
  //   encoge -> una cortina azul noche del alto viejo se recoge con transform
  // Animar la altura real recolocaría la página (y sus gráficos) en cada
  // fotograma, que es justo lo que daba tirones.
  var cortina = document.createElement("div");
  cortina.className = "cortina-banda";
  cortina.setAttribute("aria-hidden", "true");
  var animCortina = null;
  function recogerCortina(desde, hasta) {
    if (reducir || desde - hasta < 6 || !cortina.animate) return;
    if (animCortina) animCortina.cancel();
    cortina.style.height = desde + "px";
    cortina.style.display = "block";
    animCortina = cortina.animate(
      [{ transform: "scaleY(1)" }, { transform: "scaleY(" + (hasta / desde).toFixed(4) + ")" }],
      { duration: 400, easing: "cubic-bezier(0.22, 1, 0.36, 1)", fill: "forwards" });
    animCortina.onfinish = function () { cortina.style.display = "none"; animCortina.cancel(); animCortina = null; };
  }
  function abrirCabecera(cab, falta) {
    if (reducir || falta < 6 || !cab.animate) return;
    cab.animate([{ clipPath: "inset(0 0 " + Math.round(falta) + "px 0)" }, { clipPath: "inset(0 0 0 0)" }],
                { duration: 340, easing: "cubic-bezier(0.22, 1, 0.36, 1)" });
  }
  // Borde inferior de la parte oscura, en coordenadas de página
  function fondoOscuro(enSeccion, cab, nav) {
    var y = window.scrollY || 0;
    // Con cabecera, aunque el índice ya haya vuelto a Introducción: la vieja
    // sigue en pantalla hasta que llega el contenido nuevo
    if (cab) return cab.getBoundingClientRect().bottom + y;
    return nav ? nav.getBoundingClientRect().bottom + y : 0;
  }

  // La caja completa (texto, control, tamaños) se mide como mucho cada medio
  // segundo; entre medias solo se lee dónde está la cabecera y se desplaza.
  // Medir todo en cada fotograma obligaba al navegador a recalcular el diseño
  // mientras montaba la pestaña nueva, y eso costaba fotogramas.
  function cajaCacheada(cab, t) {
    var R = cab.getBoundingClientRect(), h = huella(cab), c0 = est.cajaMem;
    if (!c0 || est.cajaMemHuella !== h || t - est.cajaMemT > 0.5 ||
        Math.abs(R.height - c0.R.height) > 1 || Math.abs(R.width - c0.R.width) > 1) {
      est.cajaMem = cajaDe(R, cab, false); est.cajaMemHuella = h; est.cajaMemT = t;
      return est.cajaMem;
    }
    var dy = R.top - c0.R.top;
    if (!dy) return c0;
    var c = {}; for (var k in c0) c[k] = c0[k];
    c.R = { left: c0.R.left, right: c0.R.right, width: c0.R.width, top: R.top, bottom: R.bottom, height: R.height };
    c.y = c0.y + dy; c.hcy = c0.hcy + dy;
    return c;
  }

  function huella(el) { return el ? (el.textContent || "").slice(0, 160) : ""; }

  function pestanaLista(cab, t, dt) {
    // La pestaña nueva se reconoce por el contenido de su cabecera, no por el
    // elemento: al pasar con anterior/siguiente React reutiliza el mismo nodo
    // y comparar nodos dejaba el viaje esperando para siempre.
    if (!cab) return false;
    var h = huella(cab);
    if (h === est.huellaVieja) return false;
    if (est.listaHuella === h) return true;
    if (est.huellaEl !== h) { est.huellaEl = h; est.tCab = t; est.fluido = 0; }
    est.fluido = dt < 0.026 ? est.fluido + 1 : 0;
    // Los gráficos aplazados (assets/graficos.js, fuera de la vista) no se
    // esperan: se dibujarán después, cuando el viaje ya haya terminado
    var graficos = document.querySelectorAll("#contenido-pestana .js-plotly-plot"), dibujados = true;
    for (var k = 0; k < graficos.length; k++) {
      if (graficos[k].dataset.diferido) continue;
      if (!graficos[k].querySelector(".main-svg")) { dibujados = false; break; }
    }
    var espera = t - est.tCab, ok = (dibujados && espera > 0.12 && est.fluido >= 4) || espera > 0.9;
    if (ok) est.listaHuella = h;
    return ok;
  }

  // Caja mientras se espera a la pestaña nueva
  function cajaEspera() {
    var actual = cabecera();
    if (actual && est.huellaVieja && huella(actual) === est.huellaVieja) return cajaDe(actual.getBoundingClientRect(), actual, false);
    if (est.desdeIntro) {
      // Desde Introducción: la franja oscura de arriba (barra + índice), que
      // se encoge con la barra
      var barra = document.getElementById("barra-superior"), nav = document.getElementById("barra-navegacion");
      if (barra) {
        var rb = barra.getBoundingClientRect(), abajo = nav ? nav.getBoundingClientRect().bottom : rb.bottom;
        return cajaDe({ left: rb.left, right: rb.right, width: rb.width, top: rb.top, bottom: abajo, height: abajo - rb.top }, null, true);
      }
    }
    return est.ultimaCaja;
  }

  function mezclarCaja(a, b, k) {
    if (!a || !b) return b || a;
    function m(x, y) { return x + (y - x) * k; }
    var R = { left: m(a.R.left, b.R.left), right: m(a.R.right, b.R.right), width: m(a.R.width, b.R.width),
              top: m(a.R.top, b.R.top), bottom: m(a.R.bottom, b.R.bottom), height: m(a.R.height, b.R.height) };
    var c = { R: R, estrecho: b.estrecho };
    ["x", "y", "w", "h", "hx0", "hx1", "hcy", "amp", "vueltas"].forEach(function (n) { c[n] = m(a[n], b[n]); });
    return c;
  }

  // ------------------------------------------------------------- progreso
  var progreso = [];
  for (var q = 0; q < 340; q++) {
    var ec = estiloCorteza();
    progreso.push({ tipo: "hebra", hebra: q % 2, u: (q + azar()) / 340, g1: gauss(), g2: gauss(), f: azar() * 6.28, est: ec });
  }
  for (var pp = 0; pp < N_PESTANAS; pp++) for (var kk = 0; kk < 7; kk++) {
    progreso.push({ tipo: "peldano", paso: pp, t: (kk + 0.5) / 7, g1: gauss(), g2: gauss(), f: azar() * 6.28 });
  }
  var luz = []; for (var z0 = 0; z0 < N_PESTANAS; z0++) luz.push(0);

  // Peldaños agrupados por bloque del índice, con un hueco entre bloques: la
  // barra dice en qué etapa estás, no solo cuánto llevas. Los tamaños se leen
  // del propio índice; estos son los de utils/navegacion.py por si aún no está.
  var GRUPOS = [1, 3, 3, 3, 2, 1], HUECO = 1.0, U_PELDANO = [], TRAMOS = [], gruposLeidos = false;
  function calcularPeldanos() {
    var total = N_PESTANAS + (GRUPOS.length - 1) * HUECO, k = 0;
    U_PELDANO = []; TRAMOS = [];
    for (var g = 0; g < GRUPOS.length; g++) {
      // Tramo del bloque: de medio paso antes de su primer peldaño a medio
      // paso después del último. Entre tramos, la hebra se apaga un poco.
      TRAMOS.push([(k + g * HUECO) / total, (k + GRUPOS[g] + g * HUECO) / total]);
      for (var j = 0; j < GRUPOS[g]; j++, k++) U_PELDANO.push((k + 0.5 + g * HUECO) / total);
    }
  }
  function enTramo(u) {
    for (var i = 0; i < TRAMOS.length; i++) if (u >= TRAMOS[i][0] - 0.004 && u <= TRAMOS[i][1] + 0.004) return true;
    return false;
  }
  calcularPeldanos();
  function leerGrupos() {
    if (gruposLeidos) return;
    var bloques = document.querySelectorAll(".indice-bloque");
    if (!bloques.length) return;
    var g = [], suma = 0;
    for (var i = 0; i < bloques.length; i++) { var n = bloques[i].querySelectorAll(".indice-item").length; g.push(n); suma += n; }
    if (suma === N_PESTANAS) { GRUPOS = g; calcularPeldanos(); }
    gruposLeidos = true;
  }
  // Mitad del paso entre peldaños, en unidades de la barra
  function medioPaso() { return 0.5 / (N_PESTANAS + (GRUPOS.length - 1) * HUECO); }

  var VUELO = [0, 0, 1];
  function vueloProg(p, x, y, t) {
    VUELO[0] = x; VUELO[1] = y; VUELO[2] = 1;
    if (reducir || p.ox === undefined) return VUELO;
    var k = (t - est.t0Prog - (p.ret || 0)) / 1.2;
    if (k >= 1) return VUELO;
    if (k <= 0) { VUELO[0] = p.ox; VUELO[1] = p.oy; VUELO[2] = 0.6; return VUELO; }
    var e = curvaViaje(k);
    VUELO[0] = p.ox + (x - p.ox) * e + p.g2 * 30 * Math.sin(Math.PI * e);
    VUELO[1] = p.oy + (y - p.oy) * e - p.g1 * 22 * Math.sin(Math.PI * e);
    VUELO[2] = 0.6 + 0.4 * e;
    return VUELO;
  }

  // ---------------------------------------------------- peldaños clicables
  // Trece botones invisibles encima de los peldaños. Llevan a su pestaña
  // pulsando el mismo botón del índice (así el enrutado no cambia) y dicen su
  // nombre en una etiqueta. Fuera del orden de tabulación: con teclado ya
  // está el índice, que hace lo mismo con nombres.
  var capa = document.createElement("div");
  capa.className = "peldanos";
  capa.setAttribute("aria-hidden", "true");
  capa.hidden = true;
  var etiqueta = document.createElement("div");
  etiqueta.className = "peldano-nombre";
  capa.appendChild(etiqueta);
  var botones = [], capaRect = "", ocultoEn = -1e9, temporizador = null;
  function nombreDe(k) {
    var it = document.querySelectorAll(".indice-item .indice-item-nombre")[k];
    return it ? it.textContent : "";
  }
  function mostrarNombre(k) {
    var bt = botones[k];
    etiqueta.textContent = nombreDe(k);
    etiqueta.style.left = (parseFloat(bt.style.left) + parseFloat(bt.style.width) / 2) + "px";
    etiqueta.classList.add("visible");
  }
  for (var nb = 0; nb < N_PESTANAS; nb++) {
    (function (k) {
      var bt = document.createElement("button");
      bt.type = "button"; bt.tabIndex = -1; bt.className = "peldano";
      bt.addEventListener("pointerenter", function () {
        est.hoverPeldano = k;
        clearTimeout(temporizador);
        // Retardo solo para la primera etiqueta; al pasar a la de al lado,
        // al instante (si no, recorrer la barra se hace lento)
        if (performance.now() - ocultoEn < 400 || etiqueta.classList.contains("visible")) mostrarNombre(k);
        else temporizador = setTimeout(function () { mostrarNombre(k); }, 160);
      });
      bt.addEventListener("pointerleave", function () {
        if (est.hoverPeldano === k) est.hoverPeldano = -1;
        clearTimeout(temporizador);
        temporizador = setTimeout(function () {
          if (est.hoverPeldano === -1) { etiqueta.classList.remove("visible"); ocultoEn = performance.now(); }
        }, 60);
      });
      bt.addEventListener("click", function () {
        if (k === leerPosicion() - 1) return;
        var item = document.querySelectorAll(".indice-item")[k];
        if (item) item.click();
      });
      botones.push(bt); capa.appendChild(bt);
    })(nb);
  }
  function colocarPeldanos(rb, x0, largoX) {
    var clave = [rb.left, rb.top, rb.width, rb.height].map(Math.round).join(",");
    if (clave === capaRect) return;
    capaRect = clave;
    capa.style.left = rb.left + "px"; capa.style.top = rb.top + "px";
    capa.style.width = rb.width + "px"; capa.style.height = rb.height + "px";
    var paso = medioPaso() * 2 * largoX;
    for (var k = 0; k < N_PESTANAS; k++) {
      var cx = x0 - rb.left + U_PELDANO[k] * largoX;
      botones[k].style.left = (cx - paso / 2) + "px";
      botones[k].style.width = paso + "px";
    }
  }

  function dibujarProgreso(rb, t, dt, pos) {
    leerGrupos();
    var cy = rb.top + rb.height / 2, amp = rb.height * 0.4, x0 = rb.left + 4, largoX = rb.width - 8, vueltas = 2.5;
    colocarPeldanos(rb, x0, largoX);
    // Encendido: al cambiar de pestaña el peldaño nuevo crece y brilla una
    // vez (0,3 s) y se queda quieto. Antes latía sin parar en la zona fija de
    // arriba durante toda la lectura.
    if (est.posPintada !== pos) { est.posPintada = pos; est.tEnc = t; }
    var enc = reducir ? 0 : 1 - suave(0, 0.3, t - (est.tEnc || -10));
    var hover = est.hoverPeldano, hasta = U_PELDANO[pos - 1] + medioPaso();
    for (var k = 0; k < N_PESTANAS; k++) {
      var meta = k < pos - 1 ? 0.8 : (k === pos - 1 ? 1 : 0.22);
      luz[k] += (meta - luz[k]) * (reducir ? 1 : Math.min(1, dt * 6));
    }
    for (var i = 0; i < progreso.length; i++) {
      var p = progreso[i], x, y, alfa, col, tam, tri = false, halo = false;
      if (p.tipo === "hebra") {
        var env = suave(0, 0.05, p.u) * suave(1, 0.95, p.u);
        var a = p.u * vueltas * 6.2831853 + p.hebra * Math.PI, z = 0.5 + 0.5 * Math.cos(a);
        x = x0 + p.u * largoX + p.g1 * (1 - env) * 5; y = cy + Math.sin(a) * amp + p.g2 * (1 - env) * 3;
        // Las hebras se iluminan hasta la pestaña actual
        var hecho = p.u < hasta ? 1.5 : 0.75;
        if (p.tramo === undefined || p.tramoGrupos !== TRAMOS) { p.tramo = enTramo(p.u); p.tramoGrupos = TRAMOS; }
        alfa = Math.min(1, p.est.alfa * (0.6 + 0.6 * z) * env * hecho * (p.tramo ? 1 : 0.28)); col = p.est.col; tam = p.est.tam * (1.0 + 0.4 * z); tri = p.est.tri;
        halo = p.est.halo && hecho > 1;
      } else {
        var sk = U_PELDANO[p.paso], ak = sk * vueltas * 6.2831853;
        var ya = cy + Math.sin(ak) * amp, yb = cy + Math.sin(ak + Math.PI) * amp;
        x = x0 + sk * largoX; y = ya + (yb - ya) * p.t;
        var actual = p.paso === pos - 1, encima = p.paso === hover;
        col = actual || encima ? [238, 243, 249] : [143, 184, 242];
        alfa = 0.2 + 0.8 * luz[p.paso]; tam = actual ? 5.2 * (1 + 0.45 * enc) : 3.6;
        halo = actual || p.paso < pos - 1;
        if (encima && !actual) { alfa = Math.max(alfa, 0.9); tam = 4.6; halo = true; }
      }
      var v = vueloProg(p, x, y, t);
      p.sx = v[0]; p.sy = v[1];
      // Sin la reacción al ratón de las figuras: ahora que los peldaños se
      // pulsan, apartar las partículas bajo el puntero deformaba justo el
      // peldaño que querías señalar. La respuesta es el realce del peldaño.
      dibujar(v[0], v[1], tam, col, alfa * v[2] * est.visProg, tri, halo);
    }
    // ¿Queda algo por animar en la barra? (lo usa `quieto` en pintar)
    var asentada = t - est.tEnc > 0.35 && t - est.t0Prog > 1.6 && est.visProg >= 1;
    for (var q2 = 0; q2 < N_PESTANAS && asentada; q2++) {
      var m2 = q2 < pos - 1 ? 0.8 : (q2 === pos - 1 ? 1 : 0.22);
      if (Math.abs(m2 - luz[q2]) > 0.01) asentada = false;
    }
    est.progAsentada = asentada;
    est.hoverPintado = hover;
  }

  // ------------------------------------------------------------ bucle
  var antes = 0;
  function cuadro(ahora) {
    requestAnimationFrame(cuadro);
    if (document.hidden) return;
    var inicioCuadro = performance.now();
    try { pintar(ahora); } finally {
      // Coste medio de un fotograma del lienzo, para depurar (window.__banda.ms)
      est.ms = (est.ms || 0) * 0.9 + (performance.now() - inicioCuadro) * 0.1;
      est.msMax = Math.max(est.msMax || 0, performance.now() - inicioCuadro);
    }
  }

  function pintar(ahora) {
    var dt = Math.min(0.05, (ahora - antes) / 1000 || 0.016);
    antes = ahora;
    // Reloj virtual: tras una congelación sigue donde iba (ver la cabecera)
    est.tv = est.tv === undefined ? ahora / 1000 : est.tv + dt;
    var t = est.tv;

    var nav = document.getElementById("barra-navegacion");
    var enSeccion = document.body.classList.contains("intro-cerrada") && nav && nav.classList.contains("en-seccion");
    document.body.classList.toggle("banda-lista", !!enSeccion);
    var indice = document.getElementById("indice");
    document.body.classList.toggle("indice-abierto", !!(indice && indice.open));

    var pos = enSeccion ? leerPosicion() : 1;
    var clave = claveActual(enSeccion, pos);
    var cab = cabecera();
    var huellaEste = cab ? huella(cab) : null;
    var fondo = fondoOscuro(enSeccion, cab, nav);
    if (est.fondoPrevio !== undefined && huellaEste !== est.huellaPrevia && clave !== "portada") {
      if (est.huellaPrevia && huellaEste) {
        // De una sección a otra
        if (fondo > est.fondoPrevio + 4) abrirCabecera(cab, fondo - est.fondoPrevio);
        else if (fondo < est.fondoPrevio - 4) recogerCortina(est.fondoPrevio, fondo);
      } else if (est.huellaPrevia && !huellaEste) {
        // De una sección a Introducción: la barra superior crece a la vez, así
        // que la cortina se recoge hasta el alto que tendrá Introducción
        recogerCortina(est.fondoPrevio, est.fondoIntro || fondo);
      }
    }
    if (clave === "intro" && !huellaEste && t - (est.tCambio || 0) > 0.6) est.fondoIntro = fondo;
    est.fondoPrevio = fondo;

    // ---- cambio de pestaña: se programa el paso a la figura nueva
    if (clave !== est.clave) {
      var deClave = est.clave;
      var nueva = clave === "intro" || clave === "portada" ? "helice" : FIGURA[pos];
      est.pendiente = { clave: clave, fig: nueva, desdePortada: deClave === "portada",
                        rigido: nueva === est.fig && deClave !== "portada" };
      est.desdeIntro = deClave === "intro";
      // La cabecera "vieja" es la que se vio en el fotograma ANTERIOR: si el
      // navegador se retrasa, el cambio de pestaña y la cabecera nueva pueden
      // llegar en el mismo fotograma, y tomar la de ahora la daba por vieja y
      // dejaba la figura esperando para siempre.
      est.huellaVieja = clave === "intro" ? null : (est.huellaPrevia || null);
      est.huellaEl = null; est.listaHuella = null; est.fluido = 0; est.tCambio = t; est.cajaDesde = est.ultimaCaja;
      if (enSeccion && (deClave === "portada" || deClave === "intro")) { est.progPendiente = true; est.visProg = 0; est.abrirBanda = true; }
      if (!enSeccion && est.visProg > 0) est.progSaliendo = true;
      est.clave = clave;
    }

    // ---- ¿hace falta pintar este fotograma?
    // Leyendo una pestaña, con la cabecera ya bajo el índice fijo, lo único a
    // la vista es la barra de progreso, y quieta no cambia: se deja el lienzo
    // como está en vez de borrarlo y repintar 430 partículas 60 veces por
    // segundo. Vuelve a pintar en cuanto algo cambia (pestaña, peldaño bajo el
    // ratón, cabecera de nuevo a la vista, tamaño de la ventana).
    if (enSeccion) {
      var navR0 = nav ? nav.getBoundingClientRect() : null;
      var cabOculta = !cab || (navR0 && cab.getBoundingClientRect().bottom <= navR0.bottom);
      var quieto = !est.forzar && !est.pendiente && est.figuraOculta && cabOculta &&
                   t > est.t0 + est.dur + 0.7 && est.progAsentada &&
                   est.hoverPeldano === est.hoverPintado && !animCortina && est.posPintada === pos;
      est.quieto = quieto;
      if (quieto) { est.huellaPrevia = huellaEste; return; }
    } else est.quieto = false;
    est.forzar = false;

    ctx.globalCompositeOperation = "source-over"; ctx.globalAlpha = 1;
    ctx.clearRect(0, 0, W, H);
    if (clave === "portada") { est.huellaPrevia = huellaEste; return; }
    ctx.globalCompositeOperation = "lighter";
    if (enSeccion) ponerCeja(cab, pos);
    // Desde Introducción, la cabecera nueva se abre hacia abajo (CSS banda-entra)
    if (est.abrirBanda && cab && huella(cab) !== est.huellaVieja) {
      cab.classList.add("banda-entra"); est.abrirBanda = false;
      setTimeout(function (el) { el.classList.remove("banda-entra"); }, 700, cab);
    }

    // ---- dónde va la figura este fotograma
    var lista = !enSeccion || pestanaLista(cab, t, dt), caja;
    if (clave === "intro") {
      var barra = document.getElementById("barra-superior");
      caja = barra ? cajaDe(barra.getBoundingClientRect(), barra, true) : est.ultimaCaja;
    } else if (lista) {
      caja = cajaCacheada(cab, t);
    } else {
      // Esperando: la figura actual se acomoda en su sitio de espera sin saltos
      var k = Math.min(1, (t - est.tCambio) / 0.45);
      caja = mezclarCaja(est.cajaDesde, cajaEspera(), 0.5 - 0.5 * Math.cos(Math.PI * k));
    }

    if (est.pendiente && lista && caja) {
      // Arranca el viaje desde donde se vio cada partícula por última vez
      var pd = est.pendiente;
      for (var i = 0; i < N; i++) {
        var p = parts[i];
        if (pd.desdePortada || p.sx === undefined) {
          var ancha = W > 900, ox = ancha ? W * 0.68 : W * 0.5, oy = ancha ? H * 0.5 : H * 0.32;
          var a = azar() * 6.2831853, r = Math.sqrt(azar()) * (0.85 + 0.5 * azar());
          p.ox = ox + Math.cos(a) * r * H * 0.36; p.oy = oy + Math.sin(a) * r * H * 0.28;
        } else { p.ox = p.sx; p.oy = p.sy; }
        // Escalonado corto (hasta 0,15 s): uno largo hace que las últimas
        // partículas lleguen tarde y el viaje parezca más lento
        p.ret = pd.rigido ? 0 : azar() * (pd.desdePortada ? 0.45 : 0.15);
      }
      est.fig = pd.fig; est.rigido = pd.rigido;
      // Cambiar de pestaña es lo más frecuente del tablero, y lo que se ve
      // muchas veces tiene que ser corto: 0,75 s entre figuras (antes 1,7) y
      // 0,6 s si la figura es la misma. La salida de la portada, que se ve
      // una vez, conserva sus 2 s.
      est.dur = reducir ? 0 : (pd.desdePortada ? 2.0 : (pd.rigido ? 0.6 : 0.75));
      est.t0 = t; est.pendiente = null;
      if (est.progPendiente && enSeccion) {
        progreso.forEach(function (pq, j) {
          var o = parts[(j * 7) % N];
          pq.ox = o.sx !== undefined ? o.sx : W / 2; pq.oy = o.sy !== undefined ? o.sy : 0; pq.ret = azar() * 0.4;
        });
        est.t0Prog = t; est.visProg = 1; est.progPendiente = false;
      }
    }
    if (caja) est.ultimaCaja = caja;
    est.vis = Math.min(1, est.vis + dt * 6);

    // ---- dibujo de la figura
    if (caja) {
      var viajando = t < est.t0 + est.dur + 0.6 || !lista;
      ctx.save();
      if (!viajando) {
        // Ya colocada, no pisa el índice fijo (ni de Introducción ni de sección)
        var navR = nav ? nav.getBoundingClientRect() : null;
        var arriba = caja.R.top, abajo = caja.R.bottom;
        if (navR) {
          if (clave === "intro") abajo = Math.min(abajo, navR.top);
          else arriba = Math.max(arriba, navR.bottom);
        }
        ctx.beginPath(); ctx.rect(0, arriba, W, Math.max(0, abajo - arriba)); ctx.clip();
      }
      // Con la cabecera fuera de la vista (al leer una pestaña, bajo el
      // índice fijo) no se calcula nada: dejaba al navegador libre para el
      // desplazamiento y los gráficos. Las partículas guardan su sitio.
      var aLaVista = viajando || abajo - arriba > 0;
      est.figuraOculta = !aLaVista;
      // Recogida: en cuanto cambia la pestaña, la figura vieja se encoge un
      // 18 % hacia su centro y se apaga un poco, en 0,3 s. Así el clic tiene
      // respuesta aunque el viaje espere a que la pestaña nueva esté lista.
      var recoge = !lista && est.pendiente && !est.pendiente.rigido && !reducir ? suave(0, 0.3, t - est.tCambio) : 0;
      var rcx = caja.x + caja.w / 2, rcy = caja.y + caja.h / 2;
      var fn = FUNCIONES[est.fig], llegada = 0;
      // Reloj de la figura: sus ciclos empiezan cuando llega
      est.tf = t - est.t0;
      if (aLaVista && PASOS[est.fig]) PASOS[est.fig](caja, t, dt);
      for (var n = 0; n < N && aLaVista; n++) {
        var pa = parts[n];
        pa.tb = 1;
        fn(pa, caja, t);
        if (recoge) {
          var enc = 1 - 0.18 * recoge;
          pa.tx = rcx + (pa.tx - rcx) * enc + Math.sin(t * 1.3 + pa.f) * 5 * recoge;
          pa.ty = rcy + (pa.ty - rcy) * enc + Math.cos(t * 1.1 + pa.f) * 4 * recoge;
          pa.ta *= 1 - 0.2 * recoge;
        }
        var st = pa.est[pa.te];
        // Vuelo: curva suave (o deslizamiento recto si la figura es la misma)
        var x = pa.tx, y = pa.ty, m = 1;
        if (!reducir && pa.ox !== undefined) {
          var kk2 = (t - est.t0 - pa.ret) / est.dur;
          if (kk2 < 1) {
            if (kk2 <= 0) { x = pa.ox; y = pa.oy; m = 0.6; }
            else {
              var ee = curvaViaje(kk2);
              if (est.rigido) { x = pa.ox + (pa.tx - pa.ox) * ee; y = pa.oy + (pa.ty - pa.oy) * ee; }
              else {
                // Arcos más cerrados que con el viaje largo: a esta velocidad,
                // los abiertos se leían como un estallido
                var cx = (pa.ox + pa.tx) / 2 + pa.g2 * 46 * (1 - ee), cy = (pa.oy + pa.ty) / 2 - pa.g1 * 32 * (1 - ee);
                var u1 = (1 - ee) * (1 - ee), u2 = 2 * (1 - ee) * ee, u3 = ee * ee;
                x = u1 * pa.ox + u2 * cx + u3 * pa.tx; y = u1 * pa.oy + u2 * cy + u3 * pa.ty;
              }
              m = 0.6 + 0.4 * ee;
            }
          } else llegada++;
        }
        // Color y tamaño pasan suaves de un estilo a otro
        var vel = reducir ? 1 : Math.min(1, dt * 3.5);
        for (var cc = 0; cc < 3; cc++) pa.col[cc] += (st.col[cc] - pa.col[cc]) * vel;
        pa.tam += (st.tam - pa.tam) * vel;
        if (vel === 1 || Math.abs(pa.tam - st.tam) < 0.6) pa.tri = st.tri;
        var pulso = 1 + (st.les || 0) * (reducir ? 0 : 0.08 * Math.sin(t * 2));
        var alfa = st.alfa * (0.35 + 0.65 * pa.tz) * pa.ta * m * est.vis;
        pa.sx = x; pa.sy = y;
        // Las que la figura no usa no se dibujan (cuando ya se apagaron)
        if (alfa < 0.004 && pa.alfaPrev < 0.004) { pa.alfaPrev = alfa; continue; }
        pa.alfaPrev = alfa;
        var rr = reaccion(x, y, st.les);
        // Los colores intermedios se redondean a pasos de 8 (menos sprites)
        dibujarRGB(rr[0], rr[1], pa.tam * pa.tb * (0.8 + 0.35 * pa.tz) * pulso * rr[2],
                   Math.round(pa.col[0] / 8) * 8, Math.round(pa.col[1] / 8) * 8, Math.round(pa.col[2] / 8) * 8,
                   alfa * rr[3], pa.tri, st.halo);
      }
      ctx.restore();
      if (llegada > N * 0.9 && lista) rotulos(est.fig, caja, Math.min(1, (t - est.t0 - est.dur) * 2));
      // Hueco limpio alrededor de los botones. Mientras espera, la figura
      // vieja sigue en la franja de arriba, que en las secciones es también
      // la fila del índice: sin esto tapaba «anterior / siguiente».
      var libro = document.querySelector("#barra-superior .boton-libro");
      if (libro && (clave === "intro" || !lista)) {
        var rl = libro.getBoundingClientRect();
        ctx.globalCompositeOperation = "source-over";
        ctx.clearRect(rl.left - 10, rl.top - 8, rl.width + 20, rl.height + 16);
        if (!lista) {
          [".nav-indice .indice-vecinos", ".nav-indice .indice-boton"].forEach(function (sel) {
            var el = document.querySelector(sel), rv = el && el.getBoundingClientRect();
            if (rv && rv.width) ctx.clearRect(rv.left - 8, rv.top - 6, rv.width + 16, rv.height + 12);
          });
        }
        ctx.globalCompositeOperation = "lighter";
      }
    }

    // ---- progreso
    if (enSeccion && !est.progPendiente) {
      est.progSaliendo = false;
      est.visProg = Math.min(1, est.visProg + dt * 4);
      var bp = document.querySelector(".navbar-tabs.en-seccion .indice-progreso");
      var rbp = bp ? bp.getBoundingClientRect() : null;
      est.progAncho = rbp ? rbp.width : 0;   // 0 en el móvil, donde la barra no se muestra
      if (rbp && rbp.width > 30) { est.rectProg = rbp; est.posProg = pos; dibujarProgreso(rbp, t, dt, pos); }
      else { est.progAsentada = true; est.posPintada = pos; est.hoverPintado = est.hoverPeldano; }
    } else if (est.progSaliendo && est.rectProg && est.visProg > 0.01) {
      est.visProg = Math.max(0, est.visProg - dt * 3);
      dibujarProgreso(est.rectProg, t, dt, est.posProg || 1);
    } else { est.visProg = 0; est.progSaliendo = false; }
    var conPeldanos = enSeccion && !est.progPendiente && est.visProg > 0.5 && est.progAncho > 30;
    if (capa.hidden === !!conPeldanos) capa.hidden = !conPeldanos;
    est.huellaPrevia = huellaEste;
  }

  function iniciar() {
    document.body.appendChild(cortina);
    document.body.appendChild(lienzo);
    document.body.appendChild(capa);
    medir();
    window.addEventListener("resize", medir);
    requestAnimationFrame(cuadro);
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", iniciar);
  else iniciar();
})();

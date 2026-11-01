# =========================================================================== #
# Formato en español: coma decimal, punto de millar y espacio antes del "%".
# Es el único sitio donde se decide cómo se escribe un número en el tablero.
# Las figuras de plotly lo resuelven con `separators = ",."` (ver tema.R).
# =========================================================================== #

es_nulo <- function(valor) {
  is.null(valor) || length(valor) == 0 || is.na(valor[1])
}

num <- function(valor, decimales = 1, signo = FALSE) {
  if (es_nulo(valor)) return("—")
  formato <- if (signo) paste0("%+.", decimales, "f") else paste0("%.", decimales, "f")
  texto <- sprintf(formato, as.numeric(valor))
  # "-0,0" no existe en un texto: si redondea a cero, va sin signo negativo
  texto <- sub("^-(0(\\.0*)?)$", "\\1", texto)
  gsub(".", ",", texto, fixed = TRUE)
}

miles <- function(valor) {
  if (es_nulo(valor)) return("—")
  formatC(round(as.numeric(valor)), format = "d", big.mark = ".", decimal.mark = ",")
}

pct <- function(valor, decimales = 1, signo = FALSE) {
  if (es_nulo(valor)) return("—")
  paste0(num(valor, decimales, signo), " %")
}

p_valor <- function(valor, umbral = 0.0001) {
  if (es_nulo(valor)) return("—")
  if (valor < umbral) "p < 0,0001" else paste0("p = ", num(valor, 4))
}

# `num` para escribir dentro de una fórmula LaTeX: 0,10 -> 0{,}10. En modo
# matemático la coma es puntuación y TeX deja un espacio detrás; entre llaves
# se compone como coma decimal, sin hueco.
tnum <- function(valor, decimales = 1, signo = FALSE) {
  gsub(",", "{,}", num(valor, decimales, signo), fixed = TRUE)
}

# `p_valor` en LaTeX, para las frases que ya llevan otras fórmulas
p_tex <- function(valor, umbral = 0.0001) {
  if (es_nulo(valor)) return("—")
  if (valor < umbral) paste0("$p < ", tnum(umbral, 4), "$") else paste0("$p = ", tnum(valor, 4), "$")
}

lista_y <- function(elementos) {
  partes <- as.character(unlist(elementos))
  partes <- partes[!is.na(partes) & nzchar(partes)]
  if (length(partes) == 0) return("")
  if (length(partes) == 1) return(partes)
  paste0(paste(partes[-length(partes)], collapse = ", "), " y ", partes[length(partes)])
}

# Vectorizadas, para etiquetas de figuras
num_v <- function(valores, decimales = 1, signo = FALSE) {
  vapply(valores, num, character(1), decimales = decimales, signo = signo,
         USE.NAMES = FALSE)
}

pct_v <- function(valores, decimales = 1, signo = FALSE) {
  vapply(valores, pct, character(1), decimales = decimales, signo = signo,
         USE.NAMES = FALSE)
}

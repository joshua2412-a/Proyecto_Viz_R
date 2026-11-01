# Precalcula el experimento de las 48 combinaciones fuera del libro (opcional).
# El capítulo hace lo mismo al compilar si no encuentra la caché; este script
# solo permite lanzarlo aparte y reanudarlo si se interrumpe:
#   Rscript R/precalcular.R      (desde la carpeta rbook_gliomas)
source("R/cargar.R")
d <- cargar_particion()
correr_experimento(d$X_train, d$y_train, procesos = PROCESOS)

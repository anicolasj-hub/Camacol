# =============================================================================
# ejecutar_todo.R — Reproduce toda la prueba de principio a fin
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Uso: abrir Prueba_CAMACOL.Rproj en RStudio y ejecutar este archivo (Source).
#
# Corre en orden 01, 02, 03, 03b, 04 y 06, cada uno en un entorno limpio, mide el tiempo y
# guarda la información de la sesión (versión de R y de cada paquete) en
# salidas/sesion_R.txt. El 05 (scraping) no se incluye: depende de internet
# y de la oferta publicada el día en que se corra.
#
# Requisitos: R >= 4.1; internet solo la primera vez, para instalar paquetes.
# Tiempo aproximado: 2 a 3 minutos (más la instalación de paquetes, si aplica).
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")

scripts <- c("01_integracion_geih.R",     # GEIH: hogares + personas, módulos y meses
             "02_limpieza_vivienda.R",    # proyectos de vivienda nueva
             "03_mineria.R",              # minería de datos: modelo hedónico + PLN
             "03b_prediccion_precios.R",  # minería de datos: XGBoost, bosque aleatorio, SHAP
             "04_preparar_tablero.R",     # datos del tablero Shiny
             "06_tablero_html.R")         # versión web del tablero (un archivo HTML)

for (s in scripts) {
  message("\n==================== ", s, " ====================")
  t0 <- Sys.time()
  source(here::here("scripts", s), local = new.env(), encoding = "UTF-8")
  message(sprintf("-> %s terminado en %.0f segundos", s,
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

# Paquetes del tablero: se instalan aquí para que runApp() funcione de una vez
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("shiny", "bslib", "dplyr", "tidyr", "purrr", "plotly", "DT"))

# Registro de la sesión (reproducibilidad)
writeLines(capture.output(sessionInfo()), here::here("salidas", "sesion_R.txt"))

message("\nListo. Todos los productos se regeneraron.",
        "\nTablero Shiny: shiny::runApp(\"tablero\")",
        "\nTablero web: abra tablero_web/tablero_construccion_geih2026.html en el navegador")

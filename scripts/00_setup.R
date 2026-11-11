# =============================================================================
# 00_setup.R — Configuración común del proyecto
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Lo invocan todos los scripts con source(). Hace tres cosas:
#   1. Define cargar_paquetes(): instala (si faltan) y carga paquetes.
#   2. Define las rutas del proyecto, relativas a la raíz del .Rproj.
#   3. Fija opciones globales (semilla, notación, mensajes).
# =============================================================================

if (getRversion() < "4.1.0") {
  stop("Se requiere R 4.1 o superior (versión actual: ", getRversion(), ").")
}

# Versiones mínimas de paquetes con funciones recientes que el código usa:
#   dplyr 1.1  -> argumentos relationship/unmatched de los joins
#   purrr 1.0  -> funciones anónimas e imap
#   bslib 0.6  -> value_box_theme() del tablero
VERSIONES_MIN <- c(dplyr = "1.1.0", purrr = "1.0.0", bslib = "0.6.0")

# ---- 1. Gestión de paquetes ---------------------------------------------------
cargar_paquetes <- function(pkgs) {
  faltan <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  viejos <- pkgs[pkgs %in% names(VERSIONES_MIN) & !pkgs %in% faltan]
  viejos <- viejos[vapply(viejos, \(p) packageVersion(p) < VERSIONES_MIN[[p]], logical(1))]
  faltan <- c(faltan, viejos)
  if (length(faltan) > 0) {
    message("Instalando o actualizando paquetes: ", paste(faltan, collapse = ", "))
    install.packages(faltan)
  }
  suppressPackageStartupMessages(
    invisible(lapply(pkgs, library, character.only = TRUE))
  )
}

cargar_paquetes("here")

# ---- 2. Rutas -----------------------------------------------------------------
# here() ubica la raíz del proyecto (donde está Prueba_CAMACOL.Rproj), así los
# scripts corren igual desde RStudio, la consola o Rscript.
rutas <- list(
  raw      = here("datos", "raw"),          # insumos originales (no se modifican)
  geih_raw = here("datos", "raw", "GEIH"),  # microdatos GEIH descargados del DANE
  proc     = here("datos", "procesados"),   # bases limpias / integradas
  salidas  = here("salidas"),               # tablas, gráficos y modelos
  docs     = here("docs")                   # documento de justificación, guion
)
invisible(lapply(rutas, dir.create, recursive = TRUE, showWarnings = FALSE))

# ---- 3. Opciones globales -----------------------------------------------------
options(scipen = 999,                       # evita notación científica en pesos
        dplyr.summarise.inform = FALSE)
set.seed(2026)                              # reproducibilidad

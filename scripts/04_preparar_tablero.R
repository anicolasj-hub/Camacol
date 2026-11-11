# =============================================================================
# 04_preparar_tablero.R
# Prepara los datos del tablero de control (Shiny) a partir de la GEIH integrada
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# El tablero (tablero/app.R) no carga la base completa: usa dos extractos
# livianos con solo las variables que necesita, lo que lo hace rápido y
# permite publicarlo en shinyapps.io sin depender del resto del proyecto.
#
# Insumo:   datos/procesados/geih_2026_integrada.rds (01_integracion_geih.R)
# Producto: tablero/datos_tablero.rds
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("dplyr"))

geih <- readRDS(file.path(rutas$proc, "geih_2026_integrada.rds"))

# Ocupados: insumo de las pestañas de empleo y ciudades
ocupados <- geih |>
  filter(ocupado) |>
  transmute(mes, fex, ciudad, trece = trece_ciudades,
            sexo = as.character(sexo),
            construccion, subsector = subsector_construccion, posicion,
            informal = informal_pension, ingreso = ingreso_laboral,
            horas = horas_semana)

# Hogares (una fila por jefe de hogar): insumo de la pestaña de vivienda
hogares <- geih |>
  filter(jefe_hogar) |>
  transmute(mes, fex, ciudad, trece = trece_ciudades,
            sexo = as.character(sexo), tenencia,
            arriendo = arriendo_mensual, cuota = cuota_amortizacion,
            hacinamiento = hacinamiento_critico)

dir.create(here("tablero"), showWarnings = FALSE)
saveRDS(list(ocupados = ocupados, hogares = hogares,
             meses = sort(unique(geih$mes)), generado = Sys.Date()),
        here("tablero", "datos_tablero.rds"))

message(sprintf("Datos del tablero: %s ocupados y %s hogares (meses %s)",
                format(nrow(ocupados), big.mark = ".", decimal.mark = ","),
                format(nrow(hogares), big.mark = ".", decimal.mark = ","),
                paste(range(geih$mes), collapse = "-")))

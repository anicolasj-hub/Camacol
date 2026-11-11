# =============================================================================
# 07_datos_tablero_excel.R
# Datos del tablero de control en Excel
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Prepara, en tablas planas, todo lo que usa el tablero en Excel:
#   · Los indicadores precalculados para cada combinación de filtros
#     (territorio x sexo x ventana x mes), los mismos del tablero web.
#   · La base de microdatos de los ocupados en construcción (enero-agosto),
#     lista para tablas dinámicas, con su factor de expansión.
# El libro de Excel (tablero_excel/Tablero_construccion_GEIH2026.xlsx) lee
# estas tablas y calcula todo con fórmulas a partir de los filtros elegidos.
#
# Insumos:  tablero_web/indicadores_tablero.rds (06_tablero_html.R)
#           datos/procesados/geih_2026_integrada.rds (01_integracion_geih.R)
# Producto: tablero_excel/datos/*.csv (y, si hay Python, el libro de Excel)
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("dplyr", "tidyr", "purrr", "readr", "stringr"))

ind     <- readRDS(here("tablero_web", "indicadores_tablero.rds"))
dir_out <- here("tablero_excel", "datos")
dir.create(dir_out, recursive = TRUE, showWarnings = FALSE)

MESES_ABR <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
MESES_NOM <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
               "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")

# Etiquetas legibles tal como aparecen en las listas desplegables del Excel
etiqueta_sexo    <- function(s) recode(s, Todos = "Todos", Hombre = "Hombres", Mujer = "Mujeres")
etiqueta_ventana <- function(v) recode(v, mes = "Mensual", trimestre = "Trimestre móvil")
periodo <- function(v, m) if_else(v == "trimestre", paste0(MESES_ABR[pmax(m - 2, 1)], "–", MESES_ABR[m]), MESES_ABR[m])

# Columnas de una lista (subsectores, posiciones, tenencias) -> columnas planas
expandir <- function(df, col, nombres, prefijo, escala = 1) {
  m <- do.call(rbind, df[[col]]) / escala
  colnames(m) <- paste0(prefijo, nombres)
  bind_cols(select(df, -all_of(col)), as_tibble(m))
}
# Nombres de columna sin tildes; stringi no depende del idioma del sistema (iconv sí)
slug <- function(x) str_replace_all(stringi::stri_trans_general(x, "Latin-ASCII"), "[^A-Za-z0-9]+", "_")

# ---- 1. Empleo ----------------------------------------------------------------
empleo <- ind$empleo |>
  mutate(sexo = etiqueta_sexo(s), ventana = etiqueta_ventana(v), periodo = periodo(v, m),
         clave = paste(t, sexo, ventana, m, sep = "|")) |>
  expandir("sub", slug(ind$meta$subsectores), "sub_") |>
  expandir("posc", slug(ind$meta$posiciones), "posc_", 100) |>
  expandir("post", slug(ind$meta$posiciones), "post_", 100) |>
  transmute(clave, territorio = t, sexo, ventana, mes = m, periodo,
            ocupados_miles = ocu, construccion_miles = con,
            participacion = par / 100, informal_construccion = ic / 100, informal_total = it / 100,
            ingreso_med_construccion = yc, ingreso_med_total = yt,
            bajo_smmlv_construccion = bc / 100, bajo_smmlv_total = bt / 100,
            encuestados_construccion = n,
            across(starts_with("sub_")), across(starts_with("posc_")), across(starts_with("post_")))

# ---- 2. Ciudades (trimestre móvil) -------------------------------------------
ciudades <- ind$ciudades |>
  mutate(sexo = etiqueta_sexo(s)) |>
  transmute(clave = paste(c, sexo, m, sep = "|"), ciudad = c, sexo, mes = m,
            periodo = periodo(rep("trimestre", length(m)), m), participacion = par / 100,
            informal_construccion = inf / 100, encuestados_construccion = n)

# ---- 3. Hogares ---------------------------------------------------------------
hogares <- ind$hogares |>
  mutate(sexo = etiqueta_sexo(s), ventana = etiqueta_ventana(v), periodo = periodo(v, m),
         clave = paste(t, sexo, ventana, m, sep = "|")) |>
  expandir("ten", slug(ind$meta$tenencias), "ten_", 100) |>
  transmute(clave, territorio = t, sexo, ventana, mes = m, periodo,
            hogares_miles = hog, arriendo = arr / 100, pagando = pag / 100, propia = pro / 100,
            arriendo_mediano = arrmed, hacinamiento = hac / 100, encuestados_hogares = n,
            across(starts_with("ten_")))

hogares_ciudad <- ind$hogares_ciudad |>
  mutate(sexo = etiqueta_sexo(s)) |>
  transmute(clave = paste(c, sexo, m, sep = "|"), ciudad = c, sexo, mes = m,
            arriendo = arr / 100, arriendo_mediano = arrmed)

# ---- 4. Microdatos de los ocupados en construcción ----------------------------
geih <- readRDS(file.path(rutas$proc, "geih_2026_integrada.rds"))
base_construccion <- geih |>
  filter(construccion) |>
  transmute(mes, mes_nombre = MESES_NOM[mes],
            ciudad = coalesce(ciudad, "Resto del país"), departamento, zona,
            sexo = as.character(sexo), edad, grupo_edad = as.character(grupo_edad),
            nivel_educativo = as.character(nivel_educativo),
            subsector = subsector_construccion, posicion,
            tamano_empresa = as.character(tamano_empresa),
            informal = as.integer(informal_pension), afiliado_arl = as.integer(afiliado_arl),
            contrato_escrito = as.integer(contrato_escrito),
            horas_semana, ingreso_laboral,
            fex = round(fex, 4)) |>
  arrange(mes, ciudad)

# ---- 5. Listas para los filtros -----------------------------------------------
listas <- list(
  territorios = ind$meta$territorios,
  meses       = MESES_NOM[ind$meta$meses],
  ventanas    = c("Mensual", "Trimestre móvil"),
  sexos       = c("Todos", "Hombres", "Mujeres"),
  ciudades23  = ind$meta$ciudades23,
  subsectores = ind$meta$subsectores,
  posiciones  = ind$meta$posiciones,
  tenencias   = ind$meta$tenencias
)

# ---- 6. Exportación -----------------------------------------------------------
write_csv(empleo,            file.path(dir_out, "empleo.csv"), na = "")
write_csv(ciudades,          file.path(dir_out, "ciudades.csv"), na = "")
write_csv(hogares,           file.path(dir_out, "hogares.csv"), na = "")
write_csv(hogares_ciudad,    file.path(dir_out, "hogares_ciudad.csv"), na = "")
write_csv(base_construccion, file.path(dir_out, "base_construccion.csv"), na = "")
walk2(listas, names(listas), \(x, n) write_csv(tibble(valor = x), file.path(dir_out, paste0("lista_", n, ".csv"))))

stopifnot(round(empleo$construccion_miles[empleo$clave == "Total nacional|Todos|Mensual|8"]) == 1790)
message(sprintf("Datos del tablero en Excel: %d filas de empleo, %d de hogares, %d ocupados en construcción",
                nrow(empleo), nrow(hogares), nrow(base_construccion)))

# ---- 7. Libro de Excel (opcional) ---------------------------------------------
# El libro ya viene armado en tablero_excel/. Si se cambian los datos, se vuelve
# a armar con un script corto de Python (openpyxl): los paquetes de R para Excel
# no escriben gráficos nativos ligados a celdas, que son los que hacen que el
# tablero responda a los filtros. Si no hay Python, este paso se omite y basta
# con correr el script después: python3 tablero_excel/construir_tablero_excel.py
py <- Sys.which(c("python3", "python"))
py <- py[nzchar(py)][1]
constructor <- here("tablero_excel", "construir_tablero_excel.py")
if (!is.na(py) && system2(py, c("-c", shQuote("import openpyxl, pandas")),
                          stdout = FALSE, stderr = FALSE) == 0) {
  if (system2(py, shQuote(constructor)) == 0) {
    message("Libro de Excel regenerado: tablero_excel/Tablero_construccion_GEIH2026.xlsx")
  } else {
    warning("No se pudo regenerar el libro de Excel; revise el mensaje de Python.")
  }
} else {
  message("Python con openpyxl y pandas no está disponible: se conserva el libro de Excel entregado.")
}

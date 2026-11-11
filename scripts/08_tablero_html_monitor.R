# =============================================================================
# 08_tablero_html_monitor.R
# Versión web del tablero en Excel: misma estructura, en un solo archivo HTML
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Replica en una página web el tablero de Excel (07): filtros de territorio,
# mes, ventana y sexo; franjas de lectura que se reescriben con cada filtro;
# tarjetas de indicadores; empleo, ciudades, hogares y la serie del
# territorio; el resumen de la base de ocupados en construcción y la
# metodología. Usa los mismos indicadores que el tablero web (06) y agrega el
# perfil del trabajador por mes, calculado aquí desde los microdatos.
#
# Insumos:  tablero_web/indicadores_tablero.rds (06_tablero_html.R)
#           datos/procesados/geih_2026_integrada.rds (01_integracion_geih.R)
#           tablero_web/plantilla_monitor.html (diseño, estilos y lógica)
# Producto: tablero_web/monitor_construccion_geih2026.html
#           (se abre con doble clic; necesita internet para la librería de gráficos)
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("dplyr", "purrr", "jsonlite"))

ind  <- readRDS(here("tablero_web", "indicadores_tablero.rds"))
geih <- readRDS(file.path(rutas$proc, "geih_2026_integrada.rds"))

SMMLV_2026 <- 1750905   # Decreto 1469 de 2025


# ---- 1. Perfil de los ocupados en construcción, por mes ----------------------
# Las mismas categorías y definiciones de la hoja Resumen_base del Excel
oc <- geih |>
  filter(construccion) |>
  transmute(m = mes, w = fex,
            sexo = as.character(sexo), nivel = as.character(nivel_educativo),
            posicion = as.character(posicion), tamano = as.character(tamano_empresa),
            informal = as.numeric(informal_pension), arl = as.numeric(afiliado_arl),
            contrato = as.numeric(contrato_escrito), horas = horas_semana)

GRUPOS <- list(
  list(g = "Sexo", var = "sexo", cats = c("Hombre", "Mujer")),
  list(g = "Nivel educativo", var = "nivel",
       cats = c("Primaria o menos", "Secundaria y media", "Técnica o tecnológica",
                "Universitaria", "Posgrado")),
  list(g = "Posición ocupacional", var = "posicion",
       cats = c("Asalariado", "Cuenta propia", "Patrón o empleador", "Trabajador sin remuneración")),
  list(g = "Tamaño de la empresa", var = "tamano",
       cats = c("Trabaja solo", "2 a 10", "11 a 50", "51 a 200", "201 o más"))
)

perfil <- map_dfr(GRUPOS, function(gr) {
  oc |>
    mutate(c = .data[[gr$var]]) |>
    group_by(m) |>
    mutate(total = sum(w)) |>
    filter(c %in% gr$cats) |>
    group_by(m, c) |>
    summarise(par = 100 * sum(w) / first(total),
              inf = 100 * weighted.mean(informal, w, na.rm = TRUE),   # sin pensionados (NA)
              .groups = "drop") |>
    arrange(m, factor(c, levels = gr$cats)) |>   # el orden de las categorías, no el alfabético
    mutate(g = gr$g, .before = 1)
})

condiciones <- oc |>
  group_by(m) |>
  summarise(arl      = 100 * weighted.mean(arl, w, na.rm = TRUE),
            contrato = 100 * weighted.mean(contrato, w, na.rm = TRUE),  # entre quienes tienen contrato
            horas    = weighted.mean(horas, w, na.rm = TRUE),
            n        = n(), .groups = "drop")

# Control: agosto debe coincidir con la hoja Resumen_base del Excel
ago <- filter(perfil, m == 8, c == "Trabaja solo")
stopifnot(round(ago$inf, 1) == 92.6, round(condiciones$arl[condiciones$m == 8], 1) == 37.7)


# ---- 2. Página HTML -----------------------------------------------------------
redondear <- function(df) mutate(df, across(where(is.double), \(x) signif(x, 8)))

json <- toJSON(
  list(meta = c(ind$meta, list(smmlv = SMMLV_2026, generado = format(Sys.Date(), "%d/%m/%Y"))),
       empleo = redondear(ind$empleo), ciudades = redondear(ind$ciudades),
       hogares = redondear(ind$hogares), hogares_ciudad = redondear(ind$hogares_ciudad),
       perfil = redondear(perfil), condiciones = redondear(condiciones)),
  dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA
)
json <- gsub("</", "<\\/", json, fixed = TRUE)   # seguridad dentro de <script>

plantilla <- paste(readLines(here("tablero_web", "plantilla_monitor.html"), encoding = "UTF-8",
                             warn = FALSE), collapse = "\n")
plantilla <- sub("<!-- cuerpo -->", "</head>\n<body>", plantilla, fixed = TRUE)
partes <- strsplit(plantilla, "__DATOS__", fixed = TRUE)[[1]]
stopifnot(length(partes) == 2)

pagina <- paste0("<!doctype html>\n<html lang=\"es\">\n<head>\n<meta charset=\"utf-8\">\n",
                 "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, viewport-fit=cover\">\n",
                 partes[1], json, partes[2], "\n</body>\n</html>\n")
salida <- here("tablero_web", "monitor_construccion_geih2026.html")
writeLines(enc2utf8(pagina), salida, useBytes = TRUE)

message(sprintf("Monitor web (réplica del Excel): %s (%.1f MB). Ábralo con doble clic en el navegador.",
                salida, file.size(salida) / 1e6))

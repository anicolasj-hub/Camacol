# =============================================================================
# 05_scraping.R  (opcional)
# Web scraping de la oferta actual de proyectos de vivienda nueva en Fincaraíz,
# la misma fuente de las bases de 2021 de la prueba
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Cómo funciona: el portal está hecho en Next.js y cada página de resultados
# trae los avisos en un bloque JSON (<script id="__NEXT_DATA__">) que el
# servidor envía ya armado. En lugar de leer el HTML visual, que cambia con
# cada rediseño, el script lee ese JSON. Es más estable y trae más campos:
# coordenadas, estrato, estado de obra y constructora.
#
# Buenas prácticas:
#   · Se lee el robots.txt y se verifica que cada URL esté permitida. Para
#     agentes genéricos permite /proyectos-vivienda/ y solo excluye las
#     combinaciones de filtros con "-y-", que aquí no se usan.
#   · Pausa aleatoria de 2 a 4 segundos entre páginas y reintentos con espera.
#   · User-agent descriptivo; sin credenciales ni cookies; solo páginas públicas.
#   · Volumen pequeño: unas 20 páginas (en nov-2026 eran ~390 avisos).
#
# Productos:
#   datos/raw/scraping/fincaraiz_paginas_AAAAMMDD.rds   respuesta cruda (trazabilidad)
#   datos/procesados/scraping_fincaraiz_proyectos.csv   base limpia, 1 fila por aviso
#   salidas/scraping_comparacion_2021_2026.csv          precio/m² por municipio, 2021 vs. hoy
# =============================================================================


# ---- 0. Configuración --------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("httr2", "rvest", "jsonlite", "dplyr", "purrr", "stringr",
                  "stringi", "tibble", "readr"))

BASE_URL   <- "https://www.fincaraiz.com.co"
RUTA       <- "/proyectos-vivienda"
USER_AGENT <- "Mozilla/5.0 (compatible; PruebaTecnicaCamacol/1.0; investigacion economica)"
PAUSA_SEG  <- c(2, 4)
HOY        <- Sys.Date()

dir_scraping <- file.path(rutas$raw, "scraping")
dir.create(dir_scraping, showWarnings = FALSE, recursive = TRUE)


# ---- 1. Funciones ------------------------------------------------------------

# Valor por defecto para campos ausentes (JSON null -> NULL en R)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Reglas "Disallow" del robots.txt que aplican a cualquier agente ("*")
reglas_robots <- function() {
  txt <- request(paste0(BASE_URL, "/robots.txt")) |>
    req_user_agent(USER_AGENT) |>
    req_perform() |>
    resp_body_string()
  bloque <- str_split(txt, fixed("User-agent: *"))[[1]]
  if (length(bloque) < 2) return(character(0))
  bloque <- str_split(bloque[2], "User-agent:")[[1]][1]        # hasta el siguiente agente
  str_match_all(bloque, "Disallow:\\s*(\\S+)")[[1]][, 2]
}

# TRUE si la ruta no coincide con ninguna regla (el "*" del robots = comodín)
ruta_permitida <- function(ruta, reglas) {
  if (length(reglas) == 0) return(TRUE)
  patrones <- reglas |>
    str_replace_all("([.?+^$(){}|\\[\\]\\\\])", "\\\\\\1") |>
    str_replace_all("\\*", ".*")
  !any(str_detect(ruta, paste0("^", patrones)))
}

# Extrae el bloque de resultados del JSON embebido en el HTML de una página
leer_resultados <- function(html) {
  json <- read_html(html) |> html_element("script#__NEXT_DATA__") |> html_text()
  if (is.na(json)) stop("La página no trae el bloque __NEXT_DATA__ (¿cambió el sitio?)")
  fromJSON(json, simplifyVector = FALSE)$props$pageProps$fetchResult$searchFast
}

# Descarga la página n del listado de proyectos
descargar_pagina <- function(n, reglas) {
  ruta <- if (n == 1) RUTA else paste0(RUTA, "/pagina", n)
  if (!ruta_permitida(ruta, reglas)) stop("robots.txt no permite ", ruta)
  request(paste0(BASE_URL, ruta)) |>
    req_user_agent(USER_AGENT) |>
    req_timeout(30) |>
    req_retry(max_tries = 3, backoff = \(intento) 5 * intento) |>
    req_perform() |>
    resp_body_string() |>
    leer_resultados()
}

# Valor de un campo de la "ficha técnica" del aviso (lista de field/value)
ficha <- function(aviso, campo) {
  item <- keep(aviso$technicalSheet %||% list(), \(t) identical(t$field, campo))
  if (length(item) == 0) return(NA_character_)
  valor <- item[[1]]$value %||% NA_character_
  if (identical(valor, "")) NA_character_ else valor
}

# Nombre del primer elemento de una lista de ubicaciones
nombre <- function(lista) {
  if (length(lista) == 0) NA_character_ else lista[[1]]$name %||% NA_character_
}

# Un aviso (lista JSON) -> una fila
extraer_aviso <- function(r) {
  tibble(
    id                  = as.character(r$id %||% NA),
    titulo              = r$title %||% NA_character_,
    proyecto            = str_trim(str_extract(r$title %||% NA_character_, "^[^,]+")),
    tipo_inmueble       = r$property_type$name %||% NA_character_,
    estado_obra         = ficha(r, "construction_state_name"),
    precio              = as.numeric(r$price$amount %||% NA),
    precio_oculto       = isTRUE(r$price$hidePrice),
    area_construida_m2  = as.numeric(r$m2Built %||% NA),
    area_privada_m2     = as.numeric(r$m2apto %||% NA),
    habitaciones        = ficha(r, "bedrooms"),
    banos               = ficha(r, "bathrooms"),
    parqueaderos        = ficha(r, "garage"),
    estrato             = ficha(r, "stratum"),
    ciudad              = nombre(r$locations$city),
    departamento        = nombre(r$locations$state),
    zona                = nombre(r$locations$zone),
    barrio              = r$locations$location_main$name %||% NA_character_,
    latitud             = as.numeric(r$latitude %||% NA),
    longitud            = as.numeric(r$longitude %||% NA),
    constructora        = r$owner$name %||% NA_character_,
    tipo_anunciante     = r$owner$type %||% NA_character_,
    fecha_publicacion   = r$created_at %||% NA_character_,
    fecha_actualizacion = r$updated_at %||% NA_character_,
    url                 = paste0(BASE_URL, r$link %||% "")
  )
}

# Limpieza: tipos, códigos de "sin dato" y precio por m²
limpiar_avisos <- function(avisos, fecha_consulta) {
  avisos |>
    distinct(id, .keep_all = TRUE) |>
    mutate(
      across(c(habitaciones, banos, parqueaderos), \(x) suppressWarnings(as.integer(x))),
      estrato = suppressWarnings(as.integer(estrato)),
      estrato = if_else(estrato %in% 1:6, estrato, NA_integer_),  # el portal usa 110 = sin dato
      precio  = if_else(precio_oculto | is.na(precio) | precio <= 0, NA_real_, precio),
      area_construida_m2 = if_else(area_construida_m2 > 0, area_construida_m2, NA_real_),
      area_privada_m2    = if_else(area_privada_m2 > 0, area_privada_m2, NA_real_),
      precio_m2 = precio / coalesce(area_construida_m2, area_privada_m2),
      across(c(fecha_publicacion, fecha_actualizacion), \(x) as.Date(x, optional = TRUE)),
      fecha_consulta = fecha_consulta
    )
}

clave <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> str_to_upper() |>
    str_remove("\\s*D\\.?\\s*C\\.?$") |> str_squish()
}


# ---- 2. Recorrido de las páginas ---------------------------------------------
# (el bloque solo corre al ejecutar el script completo, no al cargar funciones)
if (sys.nframe() == 0L || interactive()) {

  reglas <- reglas_robots()
  message("robots.txt: reglas para '*' sobre proyectos -> ",
          paste(reglas[str_detect(reglas, "proyectos")], collapse = " | "))

  primera <- descargar_pagina(1, reglas)
  info    <- primera$paginatorInfo
  message(sprintf("El portal reporta %d avisos de proyectos en %d páginas",
                  info$total, info$lastPage))

  paginas <- list(primera)
  for (n in seq_len(info$lastPage)[-1]) {
    Sys.sleep(runif(1, PAUSA_SEG[1], PAUSA_SEG[2]))      # pausa entre solicitudes
    message("  página ", n, " de ", info$lastPage)
    paginas[[n]] <- tryCatch(descargar_pagina(n, reglas), error = function(e) {
      warning("Página ", n, ": ", conditionMessage(e)); NULL
    })
  }
  saveRDS(paginas, file.path(dir_scraping,
                             sprintf("fincaraiz_paginas_%s.rds", format(HOY, "%Y%m%d"))))


  # ---- 3. Base limpia ----------------------------------------------------------
  avisos <- paginas |>
    compact() |>
    map("data") |>
    unlist(recursive = FALSE) |>
    map_dfr(extraer_aviso) |>
    limpiar_avisos(HOY)

  write_csv(avisos, file.path(rutas$proc, "scraping_fincaraiz_proyectos.csv"))
  saveRDS(avisos, file.path(rutas$proc, "scraping_fincaraiz_proyectos.rds"))


  # ---- 4. Comparación con la oferta de 2021 -------------------------------------
  # Mediana del precio por m² por municipio en ambos cortes (pesos corrientes).
  # Es una comparación descriptiva: la composición de la oferta cambia entre cortes.
  v2021 <- readRDS(file.path(rutas$proc, "vivienda_limpia.rds"))$tipologias |>
    filter(apta_modelo) |>
    group_by(municipio_clave = clave(municipio)) |>
    summarise(municipio = first(municipio), tipologias_2021 = n(),
              precio_m2_2021 = median(precio_m2), .groups = "drop")

  comparacion <- avisos |>
    filter(!is.na(precio_m2)) |>
    group_by(municipio_clave = clave(ciudad)) |>
    summarise(avisos_hoy = n(), precio_m2_hoy = median(precio_m2), .groups = "drop") |>
    inner_join(v2021, by = "municipio_clave") |>
    filter(avisos_hoy >= 5, tipologias_2021 >= 5) |>
    mutate(variacion_nominal_pct = round(100 * (precio_m2_hoy / precio_m2_2021 - 1), 1)) |>
    select(municipio, tipologias_2021, precio_m2_2021, avisos_hoy, precio_m2_hoy,
           variacion_nominal_pct) |>
    arrange(desc(avisos_hoy))

  write_csv(comparacion, file.path(rutas$salidas, "scraping_comparacion_2021_2026.csv"))


  # ---- 5. Resumen ----------------------------------------------------------------
  message(sprintf("\nAvisos únicos: %d | con precio/m²: %d | municipios: %d | constructoras: %d",
                  nrow(avisos), sum(!is.na(avisos$precio_m2)),
                  n_distinct(avisos$ciudad), n_distinct(avisos$constructora)))
  print(count(avisos, ciudad, sort = TRUE), n = 10)
  print(comparacion, n = Inf)
}

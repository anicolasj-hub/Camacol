# =============================================================================
# 06_tablero_html.R
# Versión web del tablero: un solo archivo HTML autocontenido, sin servidor
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# El tablero Shiny (tablero/app.R) calcula los indicadores en vivo desde los
# microdatos, por eso necesita R corriendo. Esta versión precalcula en R todos
# los indicadores para cada combinación de filtros (territorio x sexo x
# ventana x mes) y los incrusta como JSON en una página HTML con gráficos
# interactivos (Plotly). El resultado se abre con doble clic en cualquier
# navegador y se puede publicar en la web como página estática.
#
# Insumos:  tablero/datos_tablero.rds (04_preparar_tablero.R)
#           tablero_web/plantilla.html (diseño, estilos y lógica de la página)
# Producto: tablero_web/tablero_construccion_geih2026.html
# =============================================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("dplyr", "purrr", "jsonlite"))

datos <- readRDS(here("tablero", "datos_tablero.rds"))
oc    <- datos$ocupados
hg    <- datos$hogares
meses <- datos$meses

SMMLV_2026  <- 1750905   # Decreto 1469 de 2025
CIUDADES_23 <- c("Bogotá D.C.", "Medellín A.M.", "Cali A.M.", "Barranquilla A.M.",
                 "Bucaramanga A.M.", "Manizales A.M.", "Pereira A.M.", "Cúcuta A.M.",
                 "Pasto", "Ibagué", "Montería", "Cartagena", "Villavicencio", "Tunja",
                 "Florencia", "Popayán", "Valledupar", "Quibdó", "Neiva", "Riohacha",
                 "Santa Marta", "Armenia", "Sincelejo")
SUBSECTORES <- c("Construcción de edificios", "Obras de ingeniería civil",
                 "Actividades especializadas")
POSICIONES  <- c("Asalariado", "Cuenta propia", "Patrón o empleador",
                 "Trabajador sin remuneración", "Empleado doméstico",
                 "Jornalero o peón", "Otro")
TENENCIAS   <- c("En arriendo o subarriendo", "Propia, totalmente pagada",
                 "En usufructo", "Posesión sin título, colectiva u otra",
                 "Propia, la están pagando")
territorios <- c("Total nacional", "13 ciudades y A.M.", sort(unique(na.omit(oc$ciudad))))


# ---- Funciones (las mismas definiciones del tablero Shiny) -------------------
mediana_pond <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NA_real_)
  x <- x[ok]; w <- w[ok]; o <- order(x)
  x[o][which(cumsum(w[o]) / sum(w) >= 0.5)[1]]
}

en_territorio <- function(df, t) {
  switch(t,
         "Total nacional"     = df,
         "13 ciudades y A.M." = df[df$trece, ],
         df[!is.na(df$ciudad) & df$ciudad == t, ])
}

# Duplica cada registro para "Todos" y para su propio sexo
con_sexo <- function(d) bind_rows(mutate(d, s = "Todos"), mutate(d, s = sexo))

# Cada registro cuenta en su mes (factor completo) y en los tres trimestres
# móviles que lo incluyen (factor / 3); m es el mes de referencia
con_ventanas <- function(d) {
  bind_rows(
    mutate(d, v = "mes", m = mes, w = fex),
    map_dfr(0:2, \(k) mutate(d, v = "trimestre", m = mes + k, w = fex / 3)) |>
      filter(m >= min(meses) + 2, m <= max(meses))
  )
}

participaciones <- function(categoria, w, niveles) {
  tot <- sum(w[!is.na(categoria)])
  round(100 * vapply(niveles, \(n) sum(w[categoria %in% n]), numeric(1)) / tot, 2)
}


# ---- 1. Empleo por territorio, sexo, ventana y mes ---------------------------
empleo <- map_dfr(territorios, function(t) {
  en_territorio(oc, t) |>
    con_sexo() |>
    con_ventanas() |>
    mutate(pos = !is.na(ingreso) & ingreso > 0) |>
    group_by(s, v, m) |>
    summarise(
      ocu  = sum(w) / 1e3,
      con  = sum(w[construccion]) / 1e3,
      par  = 100 * sum(w[construccion]) / sum(w),
      ic   = 100 * weighted.mean(informal[construccion], w[construccion], na.rm = TRUE),
      it   = 100 * weighted.mean(informal, w, na.rm = TRUE),
      yc   = mediana_pond(ingreso[construccion & pos], w[construccion & pos]),
      yt   = mediana_pond(ingreso[pos], w[pos]),
      # 0,99 SMMLV: muchos reportan el mínimo redondeado ($1.750.000)
      bc   = 100 * sum(w[construccion & pos & ingreso < 0.99 * SMMLV_2026]) / sum(w[construccion & pos]),
      bt   = 100 * sum(w[pos & ingreso < 0.99 * SMMLV_2026]) / sum(w[pos]),
      n    = sum(construccion),
      sub  = list(round(vapply(SUBSECTORES, \(x) sum(w[subsector %in% x]), numeric(1)) / 1e3, 2)),
      posc = list(participaciones(posicion[construccion], w[construccion], POSICIONES)),
      post = list(participaciones(posicion, w, POSICIONES)),
      .groups = "drop") |>
    mutate(t = t, .before = 1)
})


# ---- 2. Ranking de las 23 ciudades (trimestre móvil) -------------------------
ciudades <- oc |>
  filter(ciudad %in% CIUDADES_23) |>
  con_sexo() |>
  con_ventanas() |>
  filter(v == "trimestre") |>
  group_by(c = ciudad, s, m) |>
  summarise(par = 100 * sum(w[construccion]) / sum(w),
            inf = 100 * weighted.mean(informal[construccion], w[construccion], na.rm = TRUE),
            n   = sum(construccion), .groups = "drop")


# ---- 3. Hogares y vivienda ---------------------------------------------------
hogares <- map_dfr(territorios, function(t) {
  en_territorio(hg, t) |>
    con_sexo() |>
    con_ventanas() |>
    group_by(s, v, m) |>
    summarise(
      hog = sum(w) / 1e3,
      arr = 100 * sum(w[tenencia %in% "En arriendo o subarriendo"]) / sum(w),
      pag = 100 * sum(w[tenencia %in% "Propia, la están pagando"]) / sum(w),
      pro = 100 * sum(w[tenencia %in% "Propia, totalmente pagada"]) / sum(w),
      arrmed = mediana_pond(arriendo, w),
      hac = 100 * weighted.mean(hacinamiento, w, na.rm = TRUE),
      n   = n(),
      ten = list(participaciones(tenencia, w, TENENCIAS)),
      .groups = "drop") |>
    mutate(t = t, .before = 1)
})

hogares_ciudad <- hg |>
  filter(ciudad %in% CIUDADES_23) |>
  con_sexo() |>
  con_ventanas() |>
  filter(v == "trimestre") |>
  group_by(c = ciudad, s, m) |>
  summarise(arr = 100 * sum(w[tenencia %in% "En arriendo o subarriendo"]) / sum(w),
            arrmed = mediana_pond(arriendo, w), .groups = "drop")


# ---- 4. Control: debe coincidir con el boletín del DANE (agosto, nacional) ---
control <- empleo |> filter(t == "Total nacional", s == "Todos", v == "mes", m == 8)
stopifnot(round(control$ocu) == 23685, round(control$con) == 1790)


# ---- 5. Página HTML ----------------------------------------------------------
# 8 cifras significativas: aligera el archivo sin alterar el redondeo final
# (redondear antes a 2 decimales causaba doble redondeo, p. ej. 40,9499 -> 40,95 -> 41,0)
redondear <- function(df) mutate(df, across(where(is.double), \(x) signif(x, 8)))

json <- toJSON(
  list(
    meta = list(meses = meses, territorios = territorios, ciudades23 = CIUDADES_23,
                subsectores = SUBSECTORES, posiciones = POSICIONES,
                tenencias = TENENCIAS, smmlv = SMMLV_2026,
                generado = format(Sys.Date(), "%d/%m/%Y")),
    empleo = redondear(empleo), ciudades = redondear(ciudades),
    hogares = redondear(hogares), hogares_ciudad = redondear(hogares_ciudad)),
  dataframe = "rows", auto_unbox = TRUE, na = "null", digits = NA
)
json <- gsub("</", "<\\/", json, fixed = TRUE)   # seguridad dentro de <script>

plantilla <- paste(readLines(here("tablero_web", "plantilla.html"), encoding = "UTF-8",
                             warn = FALSE), collapse = "\n")
plantilla <- sub("<!-- cuerpo -->", "</head>\n<body>", plantilla, fixed = TRUE)
partes <- strsplit(plantilla, "__DATOS__", fixed = TRUE)[[1]]
stopifnot(length(partes) == 2)

pagina <- paste0("<!doctype html>\n<html lang=\"es\">\n<head>\n<meta charset=\"utf-8\">\n",
                 "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, viewport-fit=cover\">\n",
                 partes[1], json, partes[2], "\n</body>\n</html>\n")
salida <- here("tablero_web", "tablero_construccion_geih2026.html")
writeLines(pagina, salida, useBytes = TRUE)

message(sprintf("Tablero web: %s (%.1f MB). Ábralo con doble clic en el navegador.",
                salida, file.size(salida) / 1e6))

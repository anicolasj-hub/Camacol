# =============================================================================
# 02_limpieza_vivienda.R
# Preprocesamiento y limpieza de la oferta de proyectos de vivienda nueva
# (extracción de un portal inmobiliario; fecha de consulta: 05-nov-2021)
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Insumos (datos/raw/):
#   ws_fr_vn_ver2_cs.xlsx        hoja "Base":  tarjetas del portal (listados)
#   ws_fr_vn_tipos_ver2_cs.xlsx  hoja "Tipos": tipologías de cada proyecto
#   Llave común: Link (URL del proyecto; su número final es el id del proyecto)
#
# Productos (datos/procesados/):
#   vivienda_proyectos   1 fila por proyecto: atributos + resumen de su oferta
#   vivienda_tipologias  1 fila por tipología: base analítica para la minería
#   vivienda_listados    tarjetas depuradas, con el campo "Área" desagregado
#   vivienda_limpia.xlsx las tres tablas + diccionario + bitácora de limpieza
#   vivienda_limpia.rds  las tres tablas como lista de R
#
# Problemas de calidad detectados en el diagnóstico y su tratamiento:
#   1. Filas duplicadas exactas en ambas hojas ................ se eliminan
#   2. Un proyecto de prueba ("CONSTRUCTORA TEST") ............ se elimina
#   3. "Estrato" corrido en 8 proyectos: trae el precio
#      "desde" del proyecto en vez del estrato ................ NA + bandera
#   4. "Área" mezcla m², habitaciones, baños y parqueaderos .... se separa
#   5. Formato numérico colombiano ("2.432 m²" = 2 432 m²;
#      "46,65 m²" = 46,65 m²) ................................. parser propio
#   6. Faltantes codificados como "--" y "0 m²" ............... NA
#   7. "Más de 10" parqueaderos por unidad (no creíble) ....... NA + bandera
#   8. Fecha de publicación relativa ("hace 3 meses") ......... fecha aprox.
#   9. Ciudades con espacios y corregimientos, sin código ..... DIVIPOLA
#  10. Áreas de lote (> 1.000 m²) y precios/m² extremos ........ banderas
# =============================================================================


# ---- 0. Configuración --------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("readxl", "dplyr", "tidyr", "stringr", "stringi",
                  "lubridate", "readr", "tibble", "writexl"))

# Parámetros normativos vigentes en 2021 (año de la extracción)
SMMLV_2021     <- 908526  # Salario mínimo 2021 (Decreto 1785 de 2020)
TOPE_VIP       <- 90      # SMMLV, vivienda de interés prioritario
TOPE_VIS       <- 135     # SMMLV, tope general VIS (Ley 1955 de 2019, art. 85)
TOPE_VIS_AMP   <- 150     # SMMLV, tope en aglomeraciones > 1 millón de hab.
                          # (Decreto 1467 de 2019)
UMBRAL_LOTE_M2 <- 1000    # áreas mayores se asumen de lote, no de la unidad

# Bitácora: deja constancia de cada paso de limpieza (trazabilidad)
bitacora <- tibble(tabla = character(), paso = character(),
                   filas_antes = integer(), filas_despues = integer(),
                   detalle = character())
registrar <- function(tabla, paso, antes, despues, detalle = "") {
  bitacora <<- add_row(bitacora, tabla = tabla, paso = paso,
                       filas_antes = as.integer(antes),
                       filas_despues = as.integer(despues), detalle = detalle)
}


# ---- 1. Funciones auxiliares -------------------------------------------------

# Números en formato colombiano -> numérico.
# El punto es separador de miles solo si lo siguen exactamente 3 dígitos
# ("2.432" -> 2432); la coma es el separador decimal ("46,65" -> 46.65).
num_co <- function(x) {
  x <- str_remove_all(x, "\\.(?=\\d{3}(\\D|$))")
  x <- str_replace(x, ",", ".")
  suppressWarnings(as.numeric(x))
}

# Valor en pesos a partir de textos como "$ 256.563.000 COP"
pesos <- function(x) suppressWarnings(as.numeric(str_remove_all(x, "[^0-9]")))

# Primer número que antecede a "m²" o "m2" (área en metros cuadrados)
extraer_m2 <- function(x) num_co(str_extract(x, "[0-9][0-9.,]*(?=\\s*m(\u00b2|2))"))

# Primer entero que antecede a un sufijo dado (p. ej. "3ha." -> 3)
extraer_entero <- function(x, sufijo) {
  as.integer(str_match(x, paste0("(\\d+)\\s*", sufijo))[, 2])
}

# Texto libre: unifica guiones largos, quita espacios sobrantes; "" -> NA
limpiar_txt <- function(x) {
  x |> str_replace_all("[\u2013\u2014]", "-") |> str_squish() |> na_if("")
}

# Clave de comparación: sin tildes, mayúsculas, sin signos de puntuación
clave <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> str_to_upper() |>
    str_replace_all("[^A-Z0-9 ]", " ") |> str_squish()
}

# Valor más frecuente (para homogeneizar un atributo dentro de un proyecto)
moda <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_character_)
  names(sort(table(x), decreasing = TRUE))[1]
}

# Direcciones: estandariza abreviaturas frecuentes (facilita geocodificar)
estandarizar_direccion <- function(x) {
  x |>
    str_replace_all(regex("\\b(CLL|CL|CALL)\\b\\.?", ignore_case = TRUE), "Calle") |>
    str_replace_all(regex("\\b(KRA|KR|CRA|CR|CARRERA)\\b\\.?", ignore_case = TRUE), "Carrera") |>
    str_replace_all(regex("\\b(AVDA|AV)\\b\\.?", ignore_case = TRUE), "Avenida") |>
    str_replace_all(regex("\\b(TRANSV|TV|TR)\\b\\.?", ignore_case = TRUE), "Transversal") |>
    str_replace_all(regex("\\b(DG)\\b\\.?", ignore_case = TRUE), "Diagonal") |>
    str_replace_all(regex("\\b(No|Nro|N)\\b\\.?(?=\\s*\\d)|\u00b0", ignore_case = TRUE), "#") |>
    str_squish()
}


# ---- 2. Lectura --------------------------------------------------------------
base_raw  <- read_excel(file.path(rutas$raw, "ws_fr_vn_ver2_cs.xlsx"),
                        sheet = "Base", .name_repair = "unique_quiet")
tipos_raw <- read_excel(file.path(rutas$raw, "ws_fr_vn_tipos_ver2_cs.xlsx"),
                        sheet = "Tipos", .name_repair = "unique_quiet")

# Se renombra por posición (evita problemas de codificación con tildes en
# nombres como "Descipción" —sic— o "Área"). Se verifica la estructura antes.
stopifnot(ncol(base_raw) == 14, ncol(tipos_raw) == 8)

base <- base_raw |>
  select(-1) |>                       # columna 1: índice heredado de pandas
  setNames(c("ciudad", "zona", "constructora", "destacado", "proyecto",
             "precio_txt", "area_txt", "link", "fecha_consulta",
             "descripcion", "estrato_txt", "direccion", "publicacion_txt"))

tipos <- tipos_raw |>
  select(-1) |>
  setNames(c("link", "area_construida_txt", "area_privada_txt",
             "alcobas_txt", "banos_txt", "parqueaderos_txt", "precio_txt"))

registrar("listados",   "Lectura", nrow(base_raw), nrow(base), "Hoja Base")
registrar("tipologias", "Lectura", nrow(tipos_raw), nrow(tipos), "Hoja Tipos")


# ---- 3. Listados (hoja "Base") -----------------------------------------------

# 3.1 Texto: espacios, guiones y vacíos
base <- base |> mutate(across(where(is.character), limpiar_txt))

# 3.2 Duplicados exactos (el índice de pandas los ocultaba)
n0 <- nrow(base); base <- distinct(base)
registrar("listados", "Elimina duplicados exactos", n0, nrow(base),
          "Filas idénticas en todas las columnas")

# 3.3 Proyecto de prueba del portal
links_prueba <- base |>
  filter(str_detect(clave(constructora), "\\bTEST\\b") |
           str_detect(clave(proyecto), "^TEST\\b")) |>
  distinct(link) |> pull(link)
n0 <- nrow(base); base <- filter(base, !link %in% links_prueba)
registrar("listados", "Elimina proyecto de prueba", n0, nrow(base),
          paste("Constructora 'TEST':", length(links_prueba), "proyecto(s)"))

# 3.4 Variables numéricas y categóricas
base <- base |>
  mutate(
    id_proyecto = str_extract(link, "\\d+$"),
    destacado   = str_detect(destacado, regex("destacado", ignore_case = TRUE)),
    precio      = pesos(precio_txt),

    # Estrato: en 8 proyectos la columna trae un precio ("$ ..."), que
    # corresponde al precio mínimo ("desde") del proyecto -> se deja NA
    estrato_corrido        = str_detect(estrato_txt, "\\$"),
    estrato                = if_else(estrato_corrido, NA_integer_,
                                     suppressWarnings(as.integer(estrato_txt))),
    precio_desde_en_estrato = if_else(estrato_corrido, pesos(estrato_txt), NA_real_),

    # "Área" compuesta: "84,18m² • 2ha. • 2ba. • 1pa." / "Áreas desde 46,65m2"
    area_desde     = str_detect(area_txt, regex("^.reas desde", ignore_case = TRUE)),
    area_m2        = extraer_m2(area_txt),
    habitaciones   = extraer_entero(area_txt, "ha\\."),
    banos          = extraer_entero(area_txt, "ba\\."),
    parq_mas_de_10 = str_detect(area_txt, regex("m.s de \\d+\\s*pa", ignore_case = TRUE)),
    parqueaderos   = if_else(parq_mas_de_10, NA_integer_,
                             extraer_entero(area_txt, "pa\\.")),

    # Publicación relativa -> días transcurridos y fecha aproximada
    pub_n      = as.numeric(str_match(clave(publicacion_txt), "HACE (\\d+)")[, 2]),
    pub_unidad = case_when(
      str_detect(clave(publicacion_txt), "HORA") ~ 1 / 24,
      str_detect(clave(publicacion_txt), "DIA")  ~ 1,
      str_detect(clave(publicacion_txt), "MES")  ~ 365.25 / 12,
      str_detect(clave(publicacion_txt), "ANO")  ~ 365.25),
    dias_publicado          = round(pub_n * pub_unidad, 1),
    fecha_consulta          = as_date(fecha_consulta),
    fecha_publicacion_aprox = fecha_consulta - days(floor(dias_publicado)),

    # Textos
    zona         = str_to_title(zona),
    constructora = str_to_upper(constructora),
    direccion    = estandarizar_direccion(direccion),
    descripcion  = str_replace(descripcion, "\\.{2,}$", "."),
    n_caracteres_desc = nchar(descripcion)
  ) |>
  select(-pub_n, -pub_unidad)

stopifnot(all(base$estrato %in% c(1:6, NA)),
          all(!is.na(base$precio)), all(!is.na(base$dias_publicado)))

# 3.5 Geografía: municipio, código DIVIPOLA, departamento y mercado.
# Llano Grande y San Antonio de Pereira son corregimientos de Rionegro;
# San Antonio de Prado, de Medellín. "Mercado" agrupa por aglomeración urbana.
geo <- tribble(
  ~ciudad_clave,            ~municipio,        ~cod_mpio, ~departamento,     ~mercado,
  "ARMENIA",                "Armenia",         "63001",   "Quindío",         "Eje Cafetero",
  "BARRANQUILLA",           "Barranquilla",    "08001",   "Atlántico",       "AM Barranquilla",
  "BELLO",                  "Bello",           "05088",   "Antioquia",       "Valle de Aburrá",
  "BOGOTA",                 "Bogotá D.C.",     "11001",   "Bogotá D.C.",     "Bogotá D.C.",
  "BUCARAMANGA",            "Bucaramanga",     "68001",   "Santander",       "AM Bucaramanga",
  "CAJICA",                 "Cajicá",          "25126",   "Cundinamarca",    "Sabana de Bogotá",
  "CALARCA",                "Calarcá",         "63130",   "Quindío",         "Eje Cafetero",
  "CALI",                   "Cali",            "76001",   "Valle del Cauca", "Cali y alrededores",
  "CARTAGENA",              "Cartagena",       "13001",   "Bolívar",         "Cartagena",
  "CHINCHINA",              "Chinchiná",       "17174",   "Caldas",          "Eje Cafetero",
  "CHIA",                   "Chía",            "25175",   "Cundinamarca",    "Sabana de Bogotá",
  "COPACABANA",             "Copacabana",      "05212",   "Antioquia",       "Valle de Aburrá",
  "COTA",                   "Cota",            "25214",   "Cundinamarca",    "Sabana de Bogotá",
  "DOSQUEBRADAS",           "Dosquebradas",    "66170",   "Risaralda",       "Eje Cafetero",
  "EL RETIRO",              "El Retiro",       "05607",   "Antioquia",       "Oriente antioqueño",
  "ENVIGADO",               "Envigado",        "05266",   "Antioquia",       "Valle de Aburrá",
  "FLANDES",                "Flandes",         "73275",   "Tolima",          "Sumapaz - Alto Magdalena",
  "FUSAGASUGA",             "Fusagasugá",      "25290",   "Cundinamarca",    "Sumapaz - Alto Magdalena",
  "GIRARDOT",               "Girardot",        "25307",   "Cundinamarca",    "Sumapaz - Alto Magdalena",
  "GIRON",                  "Girón",           "68307",   "Santander",       "AM Bucaramanga",
  "GUASCA",                 "Guasca",          "25322",   "Cundinamarca",    "Sabana de Bogotá",
  "IBAGUE",                 "Ibagué",          "73001",   "Tolima",          "Otras ciudades",
  "ITAGUI",                 "Itagüí",          "05360",   "Antioquia",       "Valle de Aburrá",
  "JAMUNDI",                "Jamundí",         "76364",   "Valle del Cauca", "Cali y alrededores",
  "LA CALERA",              "La Calera",       "25377",   "Cundinamarca",    "Sabana de Bogotá",
  "LA ESTRELLA",            "La Estrella",     "05380",   "Antioquia",       "Valle de Aburrá",
  "LA TEBAIDA",             "La Tebaida",      "63401",   "Quindío",         "Eje Cafetero",
  "LLANO GRANDE",           "Rionegro",        "05615",   "Antioquia",       "Oriente antioqueño",
  "MADRID",                 "Madrid",          "25430",   "Cundinamarca",    "Sabana de Bogotá",
  "MEDELLIN",               "Medellín",        "05001",   "Antioquia",       "Valle de Aburrá",
  "PALMIRA",                "Palmira",         "76520",   "Valle del Cauca", "Cali y alrededores",
  "PEREIRA",                "Pereira",         "66001",   "Risaralda",       "Eje Cafetero",
  "PIEDECUESTA",            "Piedecuesta",     "68547",   "Santander",       "AM Bucaramanga",
  "PUERTO COLOMBIA",        "Puerto Colombia", "08573",   "Atlántico",       "AM Barranquilla",
  "RICAURTE",               "Ricaurte",        "25612",   "Cundinamarca",    "Sumapaz - Alto Magdalena",
  "RIONEGRO",               "Rionegro",        "05615",   "Antioquia",       "Oriente antioqueño",
  "SABANETA",               "Sabaneta",        "05631",   "Antioquia",       "Valle de Aburrá",
  "SAN ANTONIO DE PEREIRA", "Rionegro",        "05615",   "Antioquia",       "Oriente antioqueño",
  "SAN ANTONIO DE PRADO",   "Medellín",        "05001",   "Antioquia",       "Valle de Aburrá",
  "SANTA MARTA",            "Santa Marta",     "47001",   "Magdalena",       "Santa Marta",
  "SOLEDAD",                "Soledad",         "08758",   "Atlántico",       "AM Barranquilla",
  "TUNJA",                  "Tunja",           "15001",   "Boyacá",          "Otras ciudades",
  "VILLAVICENCIO",          "Villavicencio",   "50001",   "Meta",            "Otras ciudades",
  "YUMBO",                  "Yumbo",           "76892",   "Valle del Cauca", "Cali y alrededores"
)

base <- base |>
  mutate(ciudad_clave = clave(ciudad)) |>
  left_join(geo, by = "ciudad_clave") |>
  rename(localidad = ciudad) |>        # nombre tal como aparece en el portal
  select(-ciudad_clave)

sin_codigo <- unique(base$localidad[is.na(base$cod_mpio)])
if (length(sin_codigo) > 0) stop("Ciudades sin DIVIPOLA: ", paste(sin_codigo, collapse = ", "))

# 3.6 Zona: 11 proyectos traen la misma zona escrita de formas distintas;
#     se asigna la forma más frecuente dentro de cada proyecto
base <- base |> group_by(link) |> mutate(zona = moda(zona)) |> ungroup()

listados <- base |>
  select(id_proyecto, link, proyecto, localidad, municipio, cod_mpio,
         departamento, mercado, zona, constructora, destacado, estrato,
         precio, area_desde, area_m2, habitaciones, banos, parqueaderos,
         parq_mas_de_10, area_txt, precio_txt, fecha_consulta,
         dias_publicado, fecha_publicacion_aprox)


# ---- 4. Proyectos (1 fila por Link) ------------------------------------------
proyectos <- base |>
  group_by(link) |>
  summarise(
    across(c(id_proyecto, proyecto, localidad, municipio, cod_mpio,
             departamento, mercado, zona, constructora, destacado, estrato,
             estrato_corrido, precio_desde_en_estrato, direccion,
             descripcion, n_caracteres_desc, fecha_consulta, dias_publicado,
             fecha_publicacion_aprox), first),
    n_listados = n()
  ) |>
  relocate(id_proyecto)

stopifnot(!anyDuplicated(proyectos$link))


# ---- 5. Tipologías (hoja "Tipos") --------------------------------------------

# 5.1 Texto, duplicados y proyecto de prueba
tipos <- tipos |> mutate(across(everything(), limpiar_txt))

n0 <- nrow(tipos); tipos <- distinct(tipos)
registrar("tipologias", "Elimina duplicados exactos", n0, nrow(tipos),
          "Filas idénticas en todas las columnas")

n0 <- nrow(tipos); tipos <- filter(tipos, !link %in% links_prueba)
registrar("tipologias", "Elimina proyecto de prueba", n0, nrow(tipos))

# 5.2 Conversión de variables
tipos <- tipos |>
  mutate(
    area_construida_m2 = extraer_m2(area_construida_txt),
    area_privada_m2    = na_if(extraer_m2(area_privada_txt), 0),  # "0 m²" = no reportada
    alcobas            = suppressWarnings(as.integer(na_if(alcobas_txt, "--"))),
    banos              = suppressWarnings(as.integer(na_if(banos_txt, "--"))),
    parq_mas_de_10     = str_detect(parqueaderos_txt, regex("m.s de", ignore_case = TRUE)),
    parqueaderos       = if_else(parq_mas_de_10, NA_integer_,
                                 suppressWarnings(as.integer(na_if(parqueaderos_txt, "--")))),
    precio             = pesos(precio_txt)
  )

# 5.3 Validación cruzada con los listados: una tarjeta con el mismo proyecto
#     y el mismo precio es la misma unidad. Con las coincidencias únicas
#     (link + precio sin ambigüedad) se (a) mide la concordancia entre ambas
#     hojas y (b) se completan alcobas/baños/parqueaderos si faltan en Tipos.
ref_listados <- listados |>
  filter(!area_desde) |>
  distinct(link, precio, area_m2, habitaciones, banos, parqueaderos) |>
  group_by(link, precio) |> filter(n() == 1) |> ungroup() |>
  rename(area_l = area_m2, hab_l = habitaciones, ban_l = banos, parq_l = parqueaderos)

tipos <- tipos |> left_join(ref_listados, by = c("link", "precio"))

concordancia <- tipos |>
  filter(!is.na(hab_l)) |>
  summarise(n = n(),
            area    = mean(abs(area_construida_m2 - area_l) < 0.5, na.rm = TRUE),
            alcobas = mean(alcobas == hab_l, na.rm = TRUE),
            banos   = mean(banos == ban_l, na.rm = TRUE))

tipos <- tipos |>
  mutate(
    imputado_listado = (is.na(alcobas) & !is.na(hab_l)) |
                       (is.na(banos) & !is.na(ban_l)) |
                       (is.na(parqueaderos) & !parq_mas_de_10 & !is.na(parq_l)),
    alcobas      = coalesce(alcobas, hab_l),
    banos        = coalesce(banos, ban_l),
    parqueaderos = if_else(parq_mas_de_10, NA_integer_, coalesce(parqueaderos, parq_l))
  ) |>
  select(-area_l, -hab_l, -ban_l, -parq_l)

registrar("tipologias", "Validación cruzada Tipos vs. Listados", nrow(tipos), nrow(tipos),
          sprintf(paste("%d tipologías emparejadas; concordancia área %.1f%%, alcobas %.1f%%,",
                        "baños %.1f%%; %d faltantes completados"),
                  concordancia$n, 100 * concordancia$area, 100 * concordancia$alcobas,
                  100 * concordancia$banos, sum(tipos$imputado_listado)))

# 5.4 Variables derivadas y banderas de calidad
tipos <- tipos |>
  mutate(
    flag_area_lote     = area_construida_m2 > UMBRAL_LOTE_M2,  # área de terreno
    # Área privada mayor que la construida es inconsistente (p. ej. "4.173 m²"
    # frente a 46,9 m² construidos: error de separador) -> se deja NA
    flag_privada_mayor = !is.na(area_privada_m2) & area_privada_m2 > area_construida_m2,
    area_privada_m2    = if_else(flag_privada_mayor, NA_real_, area_privada_m2),
    precio_m2          = if_else(flag_area_lote, NA_real_, precio / area_construida_m2),
    precio_smmlv       = precio / SMMLV_2021,
    segmento_precio    = cut(precio_smmlv,
                             breaks = c(0, TOPE_VIP, TOPE_VIS, TOPE_VIS_AMP, Inf),
                             labels = c("VIP (<=90 SMMLV)", "VIS (90-135 SMMLV)",
                                        "VIS tope ampliado (135-150 SMMLV)",
                                        "No VIS (>150 SMMLV)"),
                             right = TRUE)
  )

# Precio por m² atípico: regla de Tukey (3 RIC) sobre el logaritmo
lp  <- log(tipos$precio_m2)
q   <- quantile(lp, c(.25, .75), na.rm = TRUE)
ric <- diff(q)
tipos <- tipos |>
  mutate(flag_precio_m2_atipico = !is.na(precio_m2) &
           (lp < q[1] - 3 * ric | lp > q[2] + 3 * ric),
         apta_modelo = !is.na(precio_m2) & !flag_precio_m2_atipico)

registrar("tipologias", "Banderas de calidad", nrow(tipos), nrow(tipos),
          sprintf("Lote: %d | Privada > construida: %d | Precio/m2 atípico: %d | Aptas para modelo: %d",
                  sum(tipos$flag_area_lote), sum(tipos$flag_privada_mayor),
                  sum(tipos$flag_precio_m2_atipico), sum(tipos$apta_modelo)))

# 5.5 Base analítica: tipología + atributos del proyecto
tipologias <- tipos |>
  inner_join(proyectos |>
               select(link, id_proyecto, proyecto, localidad, municipio, cod_mpio,
                      departamento, mercado, zona, constructora, destacado,
                      estrato, dias_publicado, descripcion),
             by = "link") |>
  group_by(id_proyecto) |>
  arrange(area_construida_m2, precio, .by_group = TRUE) |>
  mutate(id_tipologia = sprintf("%s_%02d", id_proyecto, row_number())) |>
  ungroup() |>
  select(id_tipologia, id_proyecto, proyecto, municipio, cod_mpio, departamento,
         mercado, localidad, zona, constructora, destacado, estrato,
         precio, precio_smmlv, segmento_precio, area_construida_m2,
         area_privada_m2, precio_m2, alcobas, banos, parqueaderos,
         parq_mas_de_10, dias_publicado, imputado_listado, flag_area_lote,
         flag_privada_mayor, flag_precio_m2_atipico, apta_modelo,
         descripcion, link)

stopifnot(all(tipologias$precio > 0),
          nrow(tipologias) == nrow(tipos),          # toda tipología tiene proyecto
          !anyDuplicated(tipologias$id_tipologia))


# ---- 6. Resumen de la oferta por proyecto ------------------------------------
# min/max que devuelven NA (sin advertencias) si el proyecto solo tiene lotes
min_na <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
max_na <- function(x) if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)

resumen_oferta <- tipologias |>
  group_by(link) |>
  summarise(
    n_tipologias       = n(),
    precio_min         = min(precio),
    precio_max         = max(precio),
    precio_mediana     = median(precio),
    area_min_m2        = min_na(area_construida_m2[!flag_area_lote]),
    area_max_m2        = max_na(area_construida_m2[!flag_area_lote]),
    precio_m2_mediana  = median(precio_m2[apta_modelo], na.rm = TRUE),
    pct_tipologias_vis = mean(precio_smmlv <= TOPE_VIS)
  )

proyectos <- proyectos |> left_join(resumen_oferta, by = "link")

# Verificación del diagnóstico: el valor corrido en "Estrato" es el precio
# mínimo del proyecto (confirma que es un desplazamiento de columna)
chequeo_estrato <- proyectos |>
  filter(estrato_corrido) |>
  summarise(n = n(), coincide_con_precio_min = sum(precio_desde_en_estrato == precio_min))
registrar("proyectos", "Estrato corrido = precio mínimo del proyecto",
          nrow(proyectos), nrow(proyectos),
          sprintf("%d de %d proyectos coinciden", chequeo_estrato$coincide_con_precio_min,
                  chequeo_estrato$n))


# ---- 7. Diccionario de variables ---------------------------------------------
diccionario <- tribble(
  ~variable,                 ~tabla,               ~descripcion,
  "id_proyecto",             "todas",              "Id del proyecto (número final del Link)",
  "id_tipologia",            "tipologias",         "Id de la tipología: id_proyecto + consecutivo por área",
  "link",                    "todas",              "URL del proyecto en el portal (llave de unión)",
  "localidad",               "proyectos/listados", "Ciudad tal como aparece en el portal",
  "municipio / cod_mpio",    "todas",              "Municipio y código DIVIPOLA (corregimientos asignados a su municipio)",
  "mercado",                 "todas",              "Aglomeración urbana a la que pertenece el municipio",
  "zona",                    "todas",              "Barrio o sector reportado (forma más frecuente en el proyecto)",
  "constructora",            "todas",              "Constructora o gerente del proyecto (mayúsculas)",
  "destacado",               "todas",              "TRUE si el portal marca el proyecto como destacado (pauta)",
  "estrato",                 "todas",              "Estrato socioeconómico 1-6; NA si el dato venía corrido",
  "estrato_corrido",         "proyectos",          "TRUE si la columna Estrato traía un precio",
  "precio",                  "todas",              "Precio de venta en pesos corrientes de 2021",
  "precio_smmlv",            "tipologias",         "Precio expresado en salarios mínimos de 2021",
  "segmento_precio",         "tipologias",         "VIP / VIS / VIS tope ampliado / No VIS según topes en SMMLV",
  "area_construida_m2",      "tipologias",         "Área construida en m²",
  "area_privada_m2",         "tipologias",         "Área privada en m²; NA si el portal reportaba 0 o si superaba la construida",
  "precio_m2",               "tipologias",         "Precio por m² construido; NA en áreas de lote",
  "alcobas / banos",         "tipologias",         "Número de alcobas y baños de la tipología",
  "parqueaderos",            "tipologias",         "Parqueaderos de la unidad; NA si no reportado o 'Más de 10'",
  "parq_mas_de_10",          "tipologias/listados","TRUE si el portal reporta 'Más de 10' parqueaderos",
  "imputado_listado",        "tipologias",         "TRUE si alcobas/baños/parqueaderos se completaron desde los listados",
  "flag_area_lote",          "tipologias",         "TRUE si el área supera 1.000 m² (probable área de lote)",
  "flag_privada_mayor",      "tipologias",         "TRUE si el área privada supera la construida (inconsistencia)",
  "flag_precio_m2_atipico",  "tipologias",         "TRUE si log(precio/m²) está fuera de 3 RIC (regla de Tukey)",
  "apta_modelo",             "tipologias",         "TRUE si la tipología es apta para modelar precio/m²",
  "area_desde",              "listados",           "TRUE si la tarjeta muestra 'Áreas desde' (proyecto destacado)",
  "area_m2 / habitaciones",  "listados",           "Campos extraídos del texto compuesto 'Área'",
  "dias_publicado",          "todas",              "Días desde la publicación (a partir de 'hace N días/meses')",
  "fecha_publicacion_aprox", "proyectos/listados", "Fecha de consulta menos dias_publicado",
  "n_tipologias / n_listados","proyectos",         "Número de tipologías y de tarjetas del proyecto",
  "precio_min / max / mediana","proyectos",        "Resumen de precios de las tipologías del proyecto",
  "precio_m2_mediana",       "proyectos",          "Mediana del precio/m² de tipologías aptas",
  "pct_tipologias_vis",      "proyectos",          "Proporción de tipologías con precio <= 135 SMMLV",
  "descripcion",             "proyectos/tipologias","Texto descriptivo del proyecto (insumo para PLN)"
)


# ---- 8. Exportación ----------------------------------------------------------
write_csv(proyectos,  file.path(rutas$proc, "vivienda_proyectos.csv"))
write_csv(tipologias, file.path(rutas$proc, "vivienda_tipologias.csv"))
write_csv(listados,   file.path(rutas$proc, "vivienda_listados.csv"))

saveRDS(list(proyectos = proyectos, tipologias = tipologias, listados = listados),
        file.path(rutas$proc, "vivienda_limpia.rds"))

write_xlsx(list(proyectos = proyectos, tipologias = tipologias,
                listados = listados, diccionario = diccionario,
                bitacora = bitacora),
           file.path(rutas$proc, "vivienda_limpia.xlsx"))


# ---- 9. Resumen en consola ---------------------------------------------------
message("\n== Limpieza de vivienda nueva: resumen ==")
print(bitacora, n = Inf, width = Inf)
message(sprintf("\nProyectos: %d | Tipologías: %d (aptas para modelo: %d) | Municipios: %d",
                nrow(proyectos), nrow(tipologias), sum(tipologias$apta_modelo),
                n_distinct(tipologias$cod_mpio)))
print(count(tipologias, segmento_precio))

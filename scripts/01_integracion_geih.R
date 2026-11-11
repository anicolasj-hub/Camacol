# =============================================================================
# 01_integracion_geih.R
# Integración de los microdatos de la GEIH 2026 (DANE): hogares y personas,
# varias temáticas (módulos) y varios meses en una sola base analítica
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Insumo: datos/raw/GEIH/ con un .zip por mes, tal como se descarga del DANE
#
# Estructura de la GEIH (marco 2018):
#   Vivienda -> DIRECTORIO
#   Hogar    -> DIRECTORIO + SECUENCIA_P
#   Persona  -> DIRECTORIO + SECUENCIA_P + ORDEN
# Cada mes trae 8 módulos temáticos en CSV. Se integran 5:
#   · Características generales, seguridad social y educación (todas las personas)
#   · Datos del hogar y la vivienda                              (hogares)
#   · Fuerza de trabajo                                          (población en edad de trabajar)
#   · Ocupados                                                   (ocupados)
#   · No ocupados                                                (desocupados y fuera de la FT)
#
# Estrategia: dentro de cada mes, la base de personas es el eje y los demás
# módulos se pegan con left_join, validando la cardinalidad (1:1 persona,
# n:1 hogar) y que ningún registro quede sin pareja. Luego se apilan los meses.
#
# Particularidades de los archivos 2026 que el script resuelve:
# Productos:
#   datos/procesados/geih_2026_integrada.rds  base persona-mes integrada
#   salidas/geih_chequeos_integracion.csv     control de cada unión por mes
#   salidas/geih_validacion_dane.csv          indicadores vs. cifras oficiales


# ---- 0. Configuración --------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("data.table", "dplyr", "tidyr", "purrr", "stringr",
                  "lubridate", "readr", "tibble"))

SMMLV_2026 <- 1750905   # Salario mínimo 2026 (Decreto 1469 de 2025)

LLAVE_HOGAR   <- c("directorio", "secuencia_p")
LLAVE_PERSONA <- c("directorio", "secuencia_p", "orden")


# ---- 1. Módulos y variables a integrar ---------------------------------------
# patron: expresión regular (sin distinguir mayúsculas) que ubica el CSV.
modulos <- list(
  personas = list(
    patron = "aracter",                  # "...características generales..."
    nivel  = "persona",
    vars   = c("periodo", "mes", "hogar", "area", "clase", "dpto", "fex_c18",
               "p3271",                  # sexo al nacer
               "p6040",                  # edad
               "p6050",                  # parentesco con el jefe del hogar
               "p6090",                  # afiliado a salud
               "p6100",                  # régimen de salud
               "p3042",                  # nivel educativo más alto alcanzado
               "p6170")),                # asiste a institución educativa
  hogares = list(
    patron = "vivienda",
    nivel  = "hogar",
    vars   = c("p5090",                  # tenencia de la vivienda
               "p5100",                  # cuota mensual de amortización
               "p5140",                  # arriendo mensual
               "p5130",                  # arriendo estimado si tuviera que pagar
               "p5000",                  # cuartos del hogar (incluye sala-comedor)
               "p5010",                  # cuartos donde duermen
               "p6008",                  # personas en el hogar
               "p70")),                  # hogares en la vivienda
  fuerza = list(
    patron = "fuerza",
    nivel  = "persona",
    vars   = c("pet", "ft", "fft")),     # en edad de trabajar / en FT / fuera FT
  ocupados = list(
    patron = "/ocupados\\.csv$",         # el "/" lo distingue de "No ocupados"
    nivel  = "persona",
    vars   = c("oci", "inglabo", "rama2d_r4", "rama4d_r4", "oficio_c8",
               "p6430",                  # posición ocupacional
               "p6800",                  # horas semanales trabajadas normalmente
               "p6920",                  # cotiza a pensión
               "p6990",                  # afiliado a ARL
               "p6450",                  # contrato verbal o escrito
               "p3069",                  # personas en la empresa (tamaño)
               "p6426")),                # meses trabajando en la empresa
  no_ocupados = list(
    patron = "no.{1,4}ocupados",         # espacio normal o no separable
    nivel  = "persona",
    vars   = c("dsi"))                   # desocupado
)


# ---- 2. Funciones de lectura -------------------------------------------------

# Devuelve la carpeta con los CSV de un mes. Si la fuente es un .zip, extrae
# solo los CSV a una carpeta temporal; si ya es una carpeta, la usa tal cual.
preparar_fuente <- function(fuente) {
  if (dir.exists(fuente)) return(fuente)
  destino <- file.path(tempdir(), tools::file_path_sans_ext(basename(fuente)))
  dir.create(destino, showWarnings = FALSE)
  contenido <- unzip(fuente, list = TRUE)$Name
  csv <- contenido[grepl("\\.csv$", contenido, ignore.case = TRUE, useBytes = TRUE)]
  unzip(fuente, files = csv, exdir = destino)
  destino
}

# Lee un módulo: ubica el archivo, selecciona llaves + variables y valida que
# la llave sea única.
leer_modulo <- function(carpeta, modulo, nombre) {
  archivos <- list.files(carpeta, pattern = "\\.csv$", ignore.case = TRUE,
                         recursive = TRUE, full.names = TRUE)
  ruta <- archivos[grepl(modulo$patron, archivos, ignore.case = TRUE, useBytes = TRUE)]
  if (length(ruta) != 1) {
    stop(sprintf("Módulo '%s': se esperaba 1 archivo y se encontraron %d en %s",
                 nombre, length(ruta), carpeta))
  }

  llave      <- if (modulo$nivel == "persona") LLAVE_PERSONA else LLAVE_HOGAR
  encabezado <- tolower(names(fread(ruta, sep = ";", nrows = 0)))
  faltantes  <- setdiff(c(llave, modulo$vars), encabezado)
  if (length(faltantes) > 0) {
    warning(sprintf("Módulo '%s' (%s): no trae %s", nombre, basename(carpeta),
                    paste(faltantes, collapse = ", ")))
  }

  dt <- fread(ruta, sep = ";", colClasses = "character", na.strings = c("", "."),
              select = which(encabezado %in% c(llave, modulo$vars)),
              showProgress = FALSE)
  setnames(dt, tolower(names(dt)))

  if (anyDuplicated(dt, by = llave) > 0) stop("Llave duplicada en el módulo ", nombre)
  as_tibble(dt)
}

# Integra los 5 módulos de un mes. Los left_join validan la cardinalidad
# (relationship) y fallan si algún registro del módulo no encuentra a su
# persona u hogar en la base de personas (unmatched = "error").
integrar_mes <- function(fuente) {
  message("  · ", basename(fuente))
  carpeta <- preparar_fuente(fuente)
  m <- imap(modulos, \(modulo, nombre) leer_modulo(carpeta, modulo, nombre))

  base <- m$personas |>
    left_join(m$hogares,     by = LLAVE_HOGAR,
              relationship = "many-to-one", unmatched = "error") |>
    left_join(m$fuerza,      by = LLAVE_PERSONA,
              relationship = "one-to-one",  unmatched = "error") |>
    left_join(m$ocupados,    by = LLAVE_PERSONA,
              relationship = "one-to-one",  unmatched = "error") |>
    left_join(m$no_ocupados, by = LLAVE_PERSONA,
              relationship = "one-to-one",  unmatched = "error")

  chequeo <- tibble(
    archivo      = basename(fuente),
    mes          = as.integer(unique(m$personas$mes)),
    personas     = nrow(m$personas),
    hogares      = nrow(m$hogares),
    pet          = nrow(m$fuerza),
    ocupados     = nrow(m$ocupados),
    no_ocupados  = nrow(m$no_ocupados),
    hogares_sin_personas = nrow(anti_join(m$hogares, m$personas, by = LLAVE_HOGAR)),
    pet_cuadra   = nrow(m$fuerza) == nrow(m$ocupados) + nrow(m$no_ocupados),
    filas_base   = nrow(base)
  )
  stopifnot(length(chequeo$mes) == 1, chequeo$filas_base == chequeo$personas)

  if (!identical(carpeta, fuente)) unlink(carpeta, recursive = TRUE)
  list(base = base, chequeo = chequeo)
}


# ---- 3. Integración de todos los meses ---------------------------------------
fuentes <- list.files(rutas$geih_raw, pattern = "\\.zip$", full.names = TRUE)
if (length(fuentes) == 0) fuentes <- list.dirs(rutas$geih_raw, recursive = FALSE)
if (length(fuentes) == 0) stop("No hay meses de la GEIH en ", rutas$geih_raw)

message("Integrando ", length(fuentes), " meses de la GEIH 2026:")
resultados <- map(fuentes, integrar_mes)

chequeos <- map(resultados, "chequeo") |> bind_rows() |> arrange(mes)
if (anyDuplicated(chequeos$mes)) stop("Hay meses repetidos entre los archivos")

geih <- map(resultados, "base") |> bind_rows()
rm(resultados); invisible(gc())


# ---- 4. Tipos de datos -------------------------------------------------------
# Coma decimal -> punto, por si algún mes viene con otra configuración regional
num <- function(x) as.numeric(str_replace(x, ",", "."))

geih <- geih |>
  mutate(
    across(c(mes, clase, p3271, p6040, p6050, p6090, p6100, p3042, p6170,
             p5090, p5000, p5010, p6008, p70, pet, ft, fft, oci, dsi,
             p6430, p6920, p6990, p6450, p3069), as.integer),
    across(c(fex_c18, inglabo, p6800, p6426, p5100, p5140, p5130), num)
  )


# ---- 5. Variables analíticas -------------------------------------------------

# Ciudades y áreas metropolitanas (variable AREA, código del departamento)
ciudades <- c(
  "05" = "Medellín A.M.",    "08" = "Barranquilla A.M.", "11" = "Bogotá D.C.",
  "13" = "Cartagena",        "15" = "Tunja",             "17" = "Manizales A.M.",
  "18" = "Florencia",        "19" = "Popayán",           "20" = "Valledupar",
  "23" = "Montería",         "27" = "Quibdó",            "41" = "Neiva",
  "44" = "Riohacha",         "47" = "Santa Marta",       "50" = "Villavicencio",
  "52" = "Pasto",            "54" = "Cúcuta A.M.",       "63" = "Armenia",
  "66" = "Pereira A.M.",     "68" = "Bucaramanga A.M.",  "70" = "Sincelejo",
  "73" = "Ibagué",           "76" = "Cali A.M.",         "81" = "Arauca",
  "85" = "Yopal",            "86" = "Mocoa",             "88" = "San Andrés",
  "91" = "Leticia",          "94" = "Inírida",           "95" = "San José del Guaviare",
  "97" = "Mitú",             "99" = "Puerto Carreño")
TRECE_CIUDADES <- c("11", "05", "76", "08", "68", "17", "52", "66", "73", "54", "50", "23", "13")

departamentos <- c(
  "05" = "Antioquia", "08" = "Atlántico", "11" = "Bogotá D.C.", "13" = "Bolívar",
  "15" = "Boyacá", "17" = "Caldas", "18" = "Caquetá", "19" = "Cauca", "20" = "Cesar",
  "23" = "Córdoba", "25" = "Cundinamarca", "27" = "Chocó", "41" = "Huila",
  "44" = "La Guajira", "47" = "Magdalena", "50" = "Meta", "52" = "Nariño",
  "54" = "Norte de Santander", "63" = "Quindío", "66" = "Risaralda",
  "68" = "Santander", "70" = "Sucre", "73" = "Tolima", "76" = "Valle del Cauca",
  "81" = "Arauca", "85" = "Casanare", "86" = "Putumayo", "88" = "San Andrés",
  "91" = "Amazonas", "94" = "Guainía", "95" = "Guaviare", "97" = "Vaupés",
  "99" = "Vichada")

# Las 13 ramas de actividad que publica el DANE (agregación de CIIU Rev. 4 A.C.)
rama_dane <- function(d) {
  case_when(
    d %in% 1:3   ~ "Agricultura, ganadería, caza, silvicultura y pesca",
    d %in% 5:9   ~ "Explotación de minas y canteras",
    d %in% 10:33 ~ "Industrias manufactureras",
    d %in% 35:39 ~ "Electricidad, gas, agua y gestión de desechos",
    d %in% 41:43 ~ "Construcción",
    d %in% 45:47 ~ "Comercio y reparación de vehículos",
    d %in% 49:53 ~ "Transporte y almacenamiento",
    d %in% 55:56 ~ "Alojamiento y servicios de comida",
    d %in% 58:63 ~ "Información y comunicaciones",
    d %in% 64:66 ~ "Actividades financieras y de seguros",
    d == 68      ~ "Actividades inmobiliarias",
    d %in% 69:82 ~ "Actividades profesionales, científicas, técnicas y administrativas",
    d %in% 84:88 ~ "Administración pública, educación y salud",
    d %in% 90:99 ~ "Actividades artísticas, de entretenimiento y otros servicios"
  )
}

geih <- geih |>
  mutate(
    # Tiempo y factor de expansión
    anio  = 2026L,
    fecha = make_date(anio, mes, 1),
    fex   = fex_c18,

    # Geografía
    ciudad         = unname(ciudades[area]),
    trece_ciudades = coalesce(area %in% TRECE_CIUDADES, FALSE),
    departamento   = unname(departamentos[dpto]),
    zona           = if_else(clase == 1, "Cabecera", "Centros poblados y rural disperso"),

    # Persona
    sexo       = factor(p3271, 1:2, c("Hombre", "Mujer")),
    edad       = p6040,
    grupo_edad = cut(edad, c(-Inf, 14, 24, 34, 44, 54, 64, Inf),
                     labels = c("0-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+")),
    jefe_hogar = p6050 == 1,
    nivel_educativo = factor(
      case_when(p3042 %in% 1:3   ~ "Primaria o menos",
                p3042 %in% 4:7   ~ "Secundaria y media",
                p3042 %in% 8:9   ~ "Técnica o tecnológica",
                p3042 == 10      ~ "Universitaria",
                p3042 %in% 11:13 ~ "Posgrado"),
      levels = c("Primaria o menos", "Secundaria y media", "Técnica o tecnológica",
                 "Universitaria", "Posgrado")),
    afiliado_salud = case_when(p6090 == 1 ~ TRUE, p6090 == 2 ~ FALSE),  # 9 = no sabe
    regimen_salud  = case_when(p6100 == 1 ~ "Contributivo", p6100 == 2 ~ "Especial",
                               p6100 == 3 ~ "Subsidiado"),

    # Condición de actividad (PET = ocupados + desocupados + fuera de la FT)
    pet        = coalesce(pet == 1, FALSE),
    ocupado    = coalesce(oci == 1, FALSE),
    desocupado = coalesce(dsi == 1, FALSE),
    fuera_ft   = coalesce(fft == 1, FALSE),
    condicion  = case_when(ocupado    ~ "Ocupado",
                           desocupado ~ "Desocupado",
                           fuera_ft   ~ "Fuera de la fuerza de trabajo",
                           TRUE       ~ "Fuera de la edad de trabajar"),

    # Actividad económica: construcción = divisiones CIIU 41, 42 y 43
    rama2d       = as.integer(rama2d_r4),
    rama         = rama_dane(rama2d),
    construccion = ocupado & coalesce(rama2d %in% 41:43, FALSE),
    subsector_construccion = case_when(
      construccion & rama2d == 41 ~ "Construcción de edificios",
      construccion & rama2d == 42 ~ "Obras de ingeniería civil",
      construccion & rama2d == 43 ~ "Actividades especializadas"),

    # Características del empleo
    posicion = case_when(
      p6430 %in% 1:2 ~ "Asalariado",
      p6430 == 3     ~ "Empleado doméstico",
      p6430 == 4     ~ "Cuenta propia",
      p6430 == 5     ~ "Patrón o empleador",
      p6430 %in% 6:7 ~ "Trabajador sin remuneración",
      p6430 == 8     ~ "Jornalero o peón",
      p6430 == 9     ~ "Otro"),
    tamano_empresa = factor(
      case_when(p3069 == 1      ~ "Trabaja solo",
                p3069 %in% 2:4  ~ "2 a 10",
                p3069 %in% 5:7  ~ "11 a 50",
                p3069 %in% 8:9  ~ "51 a 200",
                p3069 == 10     ~ "201 o más"),
      levels = c("Trabaja solo", "2 a 10", "11 a 50", "51 a 200", "201 o más")),
    cotiza_pension   = case_when(p6920 == 1 ~ TRUE, p6920 == 2 ~ FALSE),  # 3 = pensionado
    # Informalidad por seguridad social: ocupado que no cotiza a pensión
    # (aproximación; la medición oficial del DANE combina tamaño, registro y
    # contabilidad de la unidad económica)
    informal_pension = if_else(ocupado, !cotiza_pension, NA),
    afiliado_arl     = case_when(p6990 == 1 ~ TRUE, p6990 == 2 ~ FALSE),
    contrato_escrito = case_when(p6450 == 2 ~ TRUE, p6450 == 1 ~ FALSE),
    horas_semana     = p6800,
    ingreso_laboral  = inglabo,
    # Umbral 0,99 SMMLV: muchos ocupados reportan el mínimo redondeado ($1.750.000)
    ingreso_menor_smmlv = if_else(ocupado & !is.na(inglabo), inglabo < 0.99 * SMMLV_2026, NA),

    # Hogar y vivienda
    tenencia = case_when(
      p5090 == 1     ~ "Propia, totalmente pagada",
      p5090 == 2     ~ "Propia, la están pagando",
      p5090 == 3     ~ "En arriendo o subarriendo",
      p5090 == 4     ~ "En usufructo",
      p5090 %in% 5:7 ~ "Posesión sin título, colectiva u otra"),  # el diccionario
      # 2026 trae 6 categorías y los datos 7: se agrupan las de menor peso
    arriendo_mensual   = if_else(p5090 == 3, p5140, NA_real_),
    cuota_amortizacion = if_else(p5090 == 2, p5100, NA_real_),
    personas_hogar     = p6008,
    personas_por_cuarto = p6008 / p5000,
    hacinamiento_critico = personas_por_cuarto > 3   # criterio NBI del DANE
  )


# ---- 6. Base final -----------------------------------------------------------
geih <- geih |>
  select(
    # Identificación y diseño muestral
    directorio, secuencia_p, orden, hogar, periodo, anio, mes, fecha, fex,
    area, ciudad, trece_ciudades, dpto, departamento, clase, zona,
    # Persona
    sexo, edad, grupo_edad, jefe_hogar, nivel_educativo, afiliado_salud, regimen_salud,
    # Mercado laboral
    pet, condicion, ocupado, desocupado, fuera_ft,
    rama2d, rama4d_r4, oficio_c8, rama, construccion, subsector_construccion,
    posicion, tamano_empresa, cotiza_pension, informal_pension, afiliado_arl,
    contrato_escrito, horas_semana, ingreso_laboral, ingreso_menor_smmlv,
    # Hogar y vivienda
    tenencia, arriendo_mensual, cuota_amortizacion, personas_hogar,
    personas_por_cuarto, hacinamiento_critico,
    # Variables originales de referencia
    p3042, p6430, p5090
  )

stopifnot(!anyDuplicated(geih[c("mes", LLAVE_PERSONA)]),
          all(geih$fex > 0))


# ---- 7. Validación contra las cifras oficiales del DANE ----------------------
indicadores <- geih |>
  group_by(mes) |>
  summarise(
    pet_miles          = sum(fex[pet]) / 1e3,
    ocupados_miles     = sum(fex[ocupado]) / 1e3,
    desocupados_miles  = sum(fex[desocupado]) / 1e3,
    construccion_miles = sum(fex[construccion]) / 1e3,
    informalidad_total = 100 * weighted.mean(informal_pension[ocupado],
                                             fex[ocupado], na.rm = TRUE),
    informalidad_construccion = 100 * weighted.mean(informal_pension[construccion],
                                                    fex[construccion], na.rm = TRUE)
  ) |>
  mutate(tgp = 100 * (ocupados_miles + desocupados_miles) / pet_miles,
         to  = 100 * ocupados_miles / pet_miles,
         td  = 100 * desocupados_miles / (ocupados_miles + desocupados_miles),
         part_construccion = 100 * construccion_miles / ocupados_miles)

# Boletín técnico GEIH agosto 2026 (DANE), total nacional
oficial <- tibble(mes = 8L, tgp_dane = 63.3, to_dane = 57.4, td_dane = 9.4,
                  ocupados_dane = 23685, construccion_dane = 1790)

validacion <- indicadores |>
  left_join(oficial, by = "mes") |>
  mutate(across(where(is.double), \(x) round(x, 1)))


# ---- 8. Exportación ----------------------------------------------------------
saveRDS(geih, file.path(rutas$proc, "geih_2026_integrada.rds"))
write_csv(chequeos,   file.path(rutas$salidas, "geih_chequeos_integracion.csv"))
write_csv(validacion, file.path(rutas$salidas, "geih_validacion_dane.csv"))
# Opcional, para usar la base fuera de R:
# fwrite(geih, file.path(rutas$proc, "geih_2026_integrada.csv.gz"))

message(sprintf("\nBase integrada: %s personas-mes, %d variables, meses %s",
                format(nrow(geih), big.mark = ".", decimal.mark = ","), ncol(geih),
                paste(range(geih$mes), collapse = "-")))
print(chequeos, width = Inf)
print(select(validacion, mes, tgp, to, td, ocupados_miles, construccion_miles,
             part_construccion, informalidad_construccion, ends_with("_dane")),
      width = Inf)

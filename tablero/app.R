# =============================================================================
# tablero/app.R — Monitor de empleo en construcción y vivienda · GEIH 2026
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Tablero en Shiny con cuatro pestañas:
#   1. Empleo en construcción: indicadores clave, evolución, subsectores,
#      informalidad y posición ocupacional
#   2. Ciudades: comparación de las 23 ciudades y áreas metropolitanas
#   3. Hogares y vivienda: tenencia, arriendo y hacinamiento
#   4. Datos y metodología: tabla descargable y notas técnicas
#
# Filtros globales: territorio, mes de referencia, ventana (mes o trimestre
# móvil) y sexo. Todas las cifras son estimaciones expandidas con FEX_C18
# (para el trimestre móvil se usa FEX_C18 / 3).
#
# Cómo correrlo:  shiny::runApp("tablero")  (desde la raíz del proyecto)
# Cómo publicarlo (opcional, cuenta gratuita en shinyapps.io):
#   install.packages("rsconnect")
#   rsconnect::setAccountInfo(name = "<cuenta>", token = "<token>", secret = "<secreto>")
#   rsconnect::deployApp("tablero", appName = "monitor-construccion-geih")
# Requiere haber corrido scripts/04_preparar_tablero.R
# =============================================================================

library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(purrr)
library(plotly)
library(DT)

# ---- Datos -------------------------------------------------------------------
datos <- readRDS("datos_tablero.rds")
ocup  <- datos$ocupados
hog   <- datos$hogares

MESES_ABR  <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
MESES_NOM  <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", "Julio",
                "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
meses_disp <- datos$meses
ciudades   <- sort(unique(na.omit(ocup$ciudad)))
territorios <- c("Total nacional", "13 ciudades y A.M.", ciudades)
CIUDADES_23 <- c("Bogotá D.C.", "Medellín A.M.", "Cali A.M.", "Barranquilla A.M.",
                 "Bucaramanga A.M.", "Manizales A.M.", "Pereira A.M.", "Cúcuta A.M.",
                 "Pasto", "Ibagué", "Montería", "Cartagena", "Villavicencio", "Tunja",
                 "Florencia", "Popayán", "Valledupar", "Quibdó", "Neiva", "Riohacha",
                 "Santa Marta", "Armenia", "Sincelejo")
SMMLV_2026  <- 1750905   # Decreto 1469 de 2025
MIN_MUESTRA <- 150   # por debajo, la estimación se marca como de baja precisión

# ---- Paleta (validada para daltonismo; ver skill de visualización) -----------
COL <- c(azul = "#2a78d6", naranja = "#eb6834", aqua = "#1baf7a",
         gris = "#a8a7a1", azul_claro = "#b7d3f6", texto = "#0b0b0b",
         texto2 = "#52514e", fondo = "#fcfcfb", grilla = "#e6e5e0")
COL_SUBSECTOR <- c("Construcción de edificios" = COL[["azul"]],
                   "Obras de ingeniería civil" = COL[["naranja"]],
                   "Actividades especializadas" = COL[["aqua"]])

# ---- Funciones de cálculo ----------------------------------------------------
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

# Ventana: el mes m o el trimestre móvil que termina en m (factor / 3)
ventana <- function(df, m, tipo) {
  if (tipo == "trimestre") {
    if (m < min(meses_disp) + 2) return(NULL)
    d <- df[df$mes %in% (m - 2):m, ]
    d$w <- d$fex / 3
  } else {
    d <- df[df$mes == m, ]
    d$w <- d$fex
  }
  d
}

indicadores_empleo <- function(d) {
  c   <- d$construccion
  pos <- !is.na(d$ingreso) & d$ingreso > 0
  tibble(
    ocupados_miles     = sum(d$w) / 1e3,
    construccion_miles = sum(d$w[c]) / 1e3,
    participacion      = 100 * sum(d$w[c]) / sum(d$w),
    informal_constr    = 100 * weighted.mean(d$informal[c], d$w[c], na.rm = TRUE),
    informal_total     = 100 * weighted.mean(d$informal, d$w, na.rm = TRUE),
    ingreso_constr     = mediana_pond(d$ingreso[c & pos], d$w[c & pos]),
    ingreso_total      = mediana_pond(d$ingreso[pos], d$w[pos]),
    horas_constr       = weighted.mean(d$horas[c], d$w[c], na.rm = TRUE),
    # Umbral 0,99 SMMLV: muchos reportan el mínimo redondeado ($1.750.000)
    bajo_smmlv_constr  = 100 * sum(d$w[c & pos & d$ingreso < 0.99 * SMMLV_2026]) / sum(d$w[c & pos]),
    bajo_smmlv_total   = 100 * sum(d$w[pos & d$ingreso < 0.99 * SMMLV_2026]) / sum(d$w[pos]),
    muestra_constr     = sum(c)
  )
}

indicadores_hogar <- function(d) {
  tibble(
    hogares_miles   = sum(d$w) / 1e3,
    pct_arriendo    = 100 * sum(d$w[d$tenencia %in% "En arriendo o subarriendo"]) / sum(d$w),
    pct_pagando     = 100 * sum(d$w[d$tenencia %in% "Propia, la están pagando"]) / sum(d$w),
    pct_propia      = 100 * sum(d$w[d$tenencia %in% "Propia, totalmente pagada"]) / sum(d$w),
    arriendo_med    = mediana_pond(d$arriendo, d$w),
    pct_hacinamiento = 100 * weighted.mean(d$hacinamiento, d$w, na.rm = TRUE),
    muestra         = nrow(d)
  )
}

# ---- Formatos (convención colombiana) ----------------------------------------
num  <- function(x, d = 0) formatC(x, format = "f", digits = d, big.mark = ".", decimal.mark = ",")
pct  <- function(x, d = 1) paste0(num(x, d), "\u00a0%")
pesos <- function(x) paste0("$", num(x))
pp   <- function(x) paste0(ifelse(x >= 0, "+", "−"), num(abs(x), 1), " p.p.")
etiqueta_mes <- function(m, tipo) {
  if (tipo == "trimestre") paste0(MESES_ABR[m - 2], "–", MESES_ABR[m]) else MESES_ABR[m]
}

# ---- Estilo de los gráficos --------------------------------------------------
estilo <- function(p, y_titulo = NULL, x_titulo = NULL, leyenda = TRUE) {
  p |>
    layout(
      font = list(family = "system-ui, -apple-system, Segoe UI, Roboto, sans-serif",
                  color = COL[["texto2"]], size = 12),
      paper_bgcolor = "rgba(0,0,0,0)", plot_bgcolor = "rgba(0,0,0,0)",
      separators = ",.",
      xaxis = list(title = list(text = if (is.null(x_titulo)) "" else x_titulo), gridcolor = COL[["grilla"]], zeroline = FALSE,
                   linecolor = COL[["grilla"]], fixedrange = TRUE),
      yaxis = list(title = list(text = if (is.null(y_titulo)) "" else y_titulo), gridcolor = COL[["grilla"]], zeroline = FALSE,
                   fixedrange = TRUE, rangemode = "tozero"),
      showlegend = leyenda,
      legend = list(orientation = "h", x = 0, y = 1.12, font = list(size = 11)),
      hoverlabel = list(bgcolor = "white", bordercolor = COL[["grilla"]],
                        font = list(color = COL[["texto"]])),
      margin = list(l = 10, r = 10, t = 30, b = 10)
    ) |>
    config(displayModeBar = FALSE, locale = "es")
}

tarjeta <- function(titulo, salida, nota = NULL, alto = "330px") {
  card(full_screen = TRUE,
       card_header(titulo),
       card_body(salida, min_height = alto, padding = 8),
       if (!is.null(nota)) card_footer(class = "small text-muted", nota))
}

caja <- function(titulo, id) {
  value_box(title = titulo, value = textOutput(paste0(id, "_v")),
            p(class = "small mb-0", textOutput(paste0(id, "_s"), inline = TRUE)),
            theme = value_box_theme(bg = "#ffffff", fg = COL[["texto"]]),
            class = "border")
}


# ---- Interfaz ----------------------------------------------------------------
tema <- bs_theme(
  version = 5, bg = COL[["fondo"]], fg = COL[["texto"]], primary = COL[["azul"]],
  base_font = font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
  "card-border-color" = COL[["grilla"]]
)

ui <- page_navbar(
  title = "Monitor de empleo en construcción · GEIH 2026",
  theme = tema,
  fillable = FALSE,
  sidebar = sidebar(
    width = 270,
    selectInput("territorio", "Territorio", territorios, selected = "Total nacional"),
    selectInput("mes", "Mes de referencia",
                setNames(meses_disp, MESES_NOM[meses_disp]), selected = max(meses_disp)),
    radioButtons("ventana", "Ventana de cálculo",
                 c("Mensual" = "mes", "Trimestre móvil" = "trimestre"), selected = "mes"),
    radioButtons("sexo", "Sexo", c("Todos", "Hombre", "Mujer"), inline = TRUE),
    hr(),
    p(class = "small text-muted",
      "Ciudades: use el trimestre móvil. Con menos de ", MIN_MUESTRA,
      " encuestados en construcción la cifra se marca como de baja precisión."),
    p(class = "small text-muted", "Fuente: DANE, GEIH 2026 (ene–ago). Cálculos propios.")
  ),

  # 1. Empleo en construcción ---------------------------------------------------
  nav_panel(
    "Empleo en construcción",
    uiOutput("aviso_precision"),
    layout_columns(
      col_widths = c(3, 3, 3, 3), fill = FALSE,
      caja("Ocupados en construcción", "vb1"),
      caja("Participación en el empleo total", "vb2"),
      caja("Informalidad en construcción", "vb3"),
      caja("Ingreso laboral mediano en construcción", "vb4")
    ),
    layout_columns(
      col_widths = c(6, 6),
      tarjeta("Ocupados en construcción (miles)", plotlyOutput("g_serie", height = "300px")),
      tarjeta("Ocupados por subsector (miles)", plotlyOutput("g_subsector", height = "300px"),
              "Divisiones CIIU Rev. 4: 41 edificios, 42 obras civiles, 43 actividades especializadas.")
    ),
    layout_columns(
      col_widths = c(6, 6),
      tarjeta("Informalidad: construcción frente al total de ocupados (%)",
              plotlyOutput("g_informal", height = "300px"),
              "Informalidad aproximada como ocupados que no cotizan a pensión (excluye pensionados)."),
      tarjeta("Posición ocupacional (% de los ocupados)", plotlyOutput("g_posicion", height = "300px"))
    )
  ),

  # 2. Ciudades -----------------------------------------------------------------
  nav_panel(
    "Ciudades",
    layout_columns(
      col_widths = c(6, 6),
      tarjeta("Participación de la construcción en el empleo (%)",
              plotlyOutput("g_ciudades_part", height = "620px"),
              "23 ciudades y A.M., trimestre móvil que termina en el mes de referencia. Resaltado: territorio seleccionado.",
              alto = "640px"),
      tarjeta("Informalidad en construcción (%)",
              plotlyOutput("g_ciudades_inf", height = "620px"),
              "23 ciudades y A.M., trimestre móvil. Ocupados en construcción que no cotizan a pensión.",
              alto = "640px")
    )
  ),

  # 3. Hogares y vivienda -------------------------------------------------------
  nav_panel(
    "Hogares y vivienda",
    layout_columns(
      col_widths = c(3, 3, 3, 3), fill = FALSE,
      caja("Hogares que viven en arriendo", "vh1"),
      caja("Hogares pagando su vivienda", "vh2"),
      caja("Arriendo mensual mediano", "vh3"),
      caja("Hogares en hacinamiento crítico", "vh4")
    ),
    layout_columns(
      col_widths = c(5, 7),
      tarjeta("Tenencia de la vivienda (% de hogares)", plotlyOutput("g_tenencia", height = "340px"),
              "El filtro de sexo se aplica al jefe o jefa del hogar."),
      tarjeta("Hogares en arriendo por ciudad (%)", plotlyOutput("g_arriendo_ciudad", height = "520px"),
              "23 ciudades y A.M., trimestre móvil. Los hogares arrendatarios son la demanda potencial de vivienda propia.")
    )
  ),

  # 4. Datos y metodología ------------------------------------------------------
  nav_panel(
    "Datos y metodología",
    layout_columns(
      col_widths = c(8, 4),
      card(card_header("Serie mensual del territorio seleccionado"),
           DTOutput("tabla"),
           card_footer(downloadButton("descargar", "Descargar CSV", class = "btn-sm"))),
      card(card_header("Notas metodológicas"),
           card_body(class = "small",
             tags$ul(
               tags$li("Fuente: microdatos GEIH 2026 (DANE), enero a agosto, integrados en ",
                       tags$code("01_integracion_geih.R"), "."),
               tags$li("Construcción: ocupados en las divisiones 41, 42 y 43 de la CIIU Rev. 4 A.C."),
               tags$li("Expansión con FEX_C18; en el trimestre móvil se divide por 3."),
               tags$li("Informalidad: ocupados que no cotizan a pensión. Es una aproximación por seguridad social; la cifra oficial del DANE usa otra definición."),
               tags$li("Ingreso laboral: mediana de INGLABO entre ocupados con ingreso positivo."),
               tags$li("Hacinamiento crítico: más de 3 personas por cuarto (criterio NBI)."),
               tags$li("Validación: en agosto, 23.685 mil ocupados y 1.790 mil en construcción, igual al boletín del DANE.")
             )))
    )
  ),
  nav_spacer(),
  nav_item(tags$span(class = "small text-muted", "Prueba técnica · Camacol"))
)


# ---- Servidor ----------------------------------------------------------------
server <- function(input, output, session) {

  mes_ref <- reactive(as.integer(input$mes))

  # Ocupados filtrados por territorio y sexo
  base_ocup <- reactive({
    d <- en_territorio(ocup, input$territorio)
    if (input$sexo != "Todos") d <- d[d$sexo == input$sexo, ]
    d
  })

  # Serie de indicadores para todos los meses disponibles
  serie <- reactive({
    d <- base_ocup()
    map_dfr(meses_disp, function(m) {
      v <- ventana(d, m, input$ventana)
      if (is.null(v) || nrow(v) == 0) return(NULL)
      bind_cols(tibble(mes = m, periodo = etiqueta_mes(m, input$ventana)),
                indicadores_empleo(v))
    })
  })

  actual <- reactive({
    s <- serie()
    validate(need(mes_ref() %in% s$mes,
                  "El trimestre móvil empieza en marzo (enero–marzo). Elija un mes posterior."))
    s[s$mes == mes_ref(), ]
  })
  previo <- reactive({ s <- serie(); s[s$mes == mes_ref() - 1, ] })

  output$aviso_precision <- renderUI({
    a <- actual()
    if (a$muestra_constr < MIN_MUESTRA) {
      div(class = "alert alert-warning py-2 small",
          sprintf("Baja precisión: solo %d encuestados en construcción en este periodo y territorio. Use el trimestre móvil o un territorio mayor.",
                  a$muestra_constr))
    }
  })

  # Indicadores clave
  output$vb1_v <- renderText(paste(num(actual()$construccion_miles), "mil"))
  output$vb1_s <- renderText({
    p <- previo()
    if (nrow(p) == 0) return(actual()$periodo)
    v <- 100 * (actual()$construccion_miles / p$construccion_miles - 1)
    sprintf("%s · %s%s %% frente a %s", actual()$periodo, ifelse(v >= 0, "+", "−"),
            num(abs(v), 1), p$periodo)
  })
  output$vb2_v <- renderText(pct(actual()$participacion))
  output$vb2_s <- renderText(sprintf("de %s mil ocupados", num(actual()$ocupados_miles)))
  output$vb3_v <- renderText(pct(actual()$informal_constr))
  output$vb3_s <- renderText(sprintf("Total ocupados: %s (%s)", pct(actual()$informal_total),
                                     pp(actual()$informal_constr - actual()$informal_total)))
  output$vb4_v <- renderText(pesos(actual()$ingreso_constr))
  output$vb4_s <- renderText(sprintf("Ganan menos de 1 SMMLV: %s (total ocupados: %s)",
                                     pct(actual()$bajo_smmlv_constr), pct(actual()$bajo_smmlv_total)))

  # Evolución de los ocupados en construcción
  output$g_serie <- renderPlotly({
    s <- serie() |> mutate(periodo = factor(periodo, levels = periodo),
                           ref = mes == mes_ref())
    plot_ly(s, x = ~periodo, y = ~construccion_miles, type = "scatter", mode = "lines+markers",
            line = list(color = COL[["azul"]], width = 2),
            marker = list(color = ifelse(s$ref, COL[["azul"]], "white"), size = 9,
                          line = list(color = COL[["azul"]], width = 2)),
            customdata = ~participacion,
            hovertemplate = "<b>%{x}</b><br>%{y:,.0f} mil ocupados<br>%{customdata:.1f} % del empleo<extra></extra>") |>
      estilo(leyenda = FALSE)
  })

  # Subsectores
  output$g_subsector <- renderPlotly({
    d <- base_ocup()[base_ocup()$construccion, ]
    s <- map_dfr(meses_disp, function(m) {
      v <- ventana(d, m, input$ventana)
      if (is.null(v) || nrow(v) == 0) return(NULL)
      v |> group_by(subsector) |> summarise(miles = sum(w) / 1e3, .groups = "drop") |>
        mutate(periodo = etiqueta_mes(m, input$ventana), mes = m)
    })
    s <- s |> mutate(periodo = factor(periodo, levels = unique(periodo[order(mes)])),
                     subsector = factor(subsector, levels = names(COL_SUBSECTOR)))
    plot_ly(s, x = ~periodo, y = ~miles, color = ~subsector, colors = COL_SUBSECTOR,
            type = "bar", marker = list(line = list(color = COL[["fondo"]], width = 1.5)),
            hovertemplate = "<b>%{x}</b><br>%{fullData.name}: %{y:,.0f} mil<extra></extra>") |>
      layout(barmode = "stack", bargap = 0.35) |>
      estilo()
  })

  # Informalidad: construcción vs. total
  output$g_informal <- renderPlotly({
    s <- serie() |> mutate(periodo = factor(periodo, levels = periodo))
    plot_ly(s, x = ~periodo) |>
      add_trace(y = ~informal_constr, name = "Construcción", type = "scatter", mode = "lines+markers",
                line = list(color = COL[["azul"]], width = 2), marker = list(color = COL[["azul"]], size = 8),
                hovertemplate = "<b>%{x}</b><br>Construcción: %{y:.1f} %<extra></extra>") |>
      add_trace(y = ~informal_total, name = "Total ocupados", type = "scatter", mode = "lines+markers",
                line = list(color = COL[["gris"]], width = 2, dash = "dot"),
                marker = list(color = COL[["gris"]], size = 8),
                hovertemplate = "<b>%{x}</b><br>Total ocupados: %{y:.1f} %<extra></extra>") |>
      estilo() |>
      layout(yaxis = list(rangemode = "normal", ticksuffix = " %"))
  })

  # Posición ocupacional
  output$g_posicion <- renderPlotly({
    v <- ventana(base_ocup(), mes_ref(), input$ventana)
    req(v)
    comp <- bind_rows(
      v[v$construccion, ] |> mutate(grupo = "Construcción"),
      v |> mutate(grupo = "Total ocupados")
    ) |>
      filter(!is.na(posicion)) |>
      group_by(grupo, posicion) |> summarise(w = sum(w), .groups = "drop_last") |>
      mutate(pct = 100 * w / sum(w)) |> ungroup()
    orden <- comp |> filter(grupo == "Total ocupados") |> arrange(pct) |> pull(posicion)
    comp <- comp |> mutate(posicion = factor(posicion, levels = orden),
                           grupo = factor(grupo, levels = c("Total ocupados", "Construcción")))
    plot_ly(comp, y = ~posicion, x = ~pct, color = ~grupo,
            colors = c("Total ocupados" = COL[["gris"]], "Construcción" = COL[["azul"]]),
            type = "bar", orientation = "h",
            hovertemplate = "<b>%{y}</b><br>%{fullData.name}: %{x:.1f} %<extra></extra>") |>
      layout(barmode = "group", bargap = 0.3, legend = list(traceorder = "reversed")) |>
      estilo() |>
      layout(xaxis = list(ticksuffix = " %"))
  })

  # Ciudades (siempre trimestre móvil por tamaño de muestra)
  ranking_ciudades <- reactive({
    d <- ocup
    if (input$sexo != "Todos") d <- d[d$sexo == input$sexo, ]
    v <- ventana(d[d$ciudad %in% CIUDADES_23, ], mes_ref(), "trimestre")
    validate(need(!is.null(v), "El ranking de ciudades usa el trimestre móvil: elija marzo o un mes posterior."))
    v |> group_by(ciudad) |>
      summarise(participacion = 100 * sum(w[construccion]) / sum(w),
                informal = 100 * weighted.mean(informal[construccion], w[construccion], na.rm = TRUE),
                muestra = sum(construccion), .groups = "drop") |>
      mutate(resaltar = ciudad == input$territorio |
               (input$territorio == "13 ciudades y A.M." & ciudad %in% unique(ocup$ciudad[ocup$trece])))
  })

  grafico_ranking <- function(r, var, sufijo, titulo_hover) {
    r <- r |> arrange(.data[[var]]) |> mutate(ciudad = factor(ciudad, levels = ciudad))
    plot_ly(r, y = ~ciudad, x = r[[var]], type = "bar", orientation = "h",
            marker = list(color = ifelse(r$resaltar, COL[["azul"]], COL[["azul_claro"]])),
            customdata = ~muestra,
            hovertemplate = paste0("<b>%{y}</b><br>", titulo_hover, ": %{x:.1f}", sufijo,
                                   "<br>Encuestados en construcción: %{customdata}<extra></extra>")) |>
      estilo(leyenda = FALSE) |>
      layout(xaxis = list(ticksuffix = sufijo), bargap = 0.25)
  }
  output$g_ciudades_part <- renderPlotly(grafico_ranking(ranking_ciudades(), "participacion", " %", "Participación"))
  output$g_ciudades_inf  <- renderPlotly(grafico_ranking(ranking_ciudades(), "informal", " %", "Informalidad"))

  # Hogares y vivienda
  base_hog <- reactive({
    d <- en_territorio(hog, input$territorio)
    if (input$sexo != "Todos") d <- d[d$sexo == input$sexo, ]
    v <- ventana(d, mes_ref(), input$ventana)
    validate(need(!is.null(v), "El trimestre móvil empieza en marzo. Elija un mes posterior."))
    v
  })
  ind_hog <- reactive(indicadores_hogar(base_hog()))

  output$vh1_v <- renderText(pct(ind_hog()$pct_arriendo))
  output$vh1_s <- renderText(sprintf("de %s mil hogares", num(ind_hog()$hogares_miles)))
  output$vh2_v <- renderText(pct(ind_hog()$pct_pagando))
  output$vh2_s <- renderText(sprintf("Propia totalmente pagada: %s", pct(ind_hog()$pct_propia)))
  output$vh3_v <- renderText(pesos(ind_hog()$arriendo_med))
  output$vh3_s <- renderText("Hogares arrendatarios")
  output$vh4_v <- renderText(pct(ind_hog()$pct_hacinamiento))
  output$vh4_s <- renderText("Más de 3 personas por cuarto")

  output$g_tenencia <- renderPlotly({
    t <- base_hog() |> filter(!is.na(tenencia)) |>
      group_by(tenencia) |> summarise(w = sum(w)) |> mutate(pct = 100 * w / sum(w)) |>
      arrange(pct) |> mutate(tenencia = factor(tenencia, levels = tenencia))
    plot_ly(t, y = ~tenencia, x = ~pct, type = "bar", orientation = "h",
            marker = list(color = COL[["azul"]]),
            hovertemplate = "<b>%{y}</b><br>%{x:.1f} % de los hogares<extra></extra>") |>
      estilo(leyenda = FALSE) |>
      layout(xaxis = list(ticksuffix = " %"), bargap = 0.3)
  })

  output$g_arriendo_ciudad <- renderPlotly({
    d <- hog
    if (input$sexo != "Todos") d <- d[d$sexo == input$sexo, ]
    v <- ventana(d[d$ciudad %in% CIUDADES_23, ], mes_ref(), "trimestre")
    validate(need(!is.null(v), "Elija marzo o un mes posterior."))
    r <- v |> group_by(ciudad) |>
      summarise(pct = 100 * sum(w[tenencia %in% "En arriendo o subarriendo"]) / sum(w),
                arriendo = mediana_pond(arriendo, w), .groups = "drop") |>
      arrange(desc(pct)) |>
      mutate(ciudad = factor(ciudad, levels = ciudad),
             resaltar = ciudad == input$territorio)
    r <- r |> arrange(pct) |> mutate(ciudad = factor(as.character(ciudad), levels = as.character(ciudad)))
    plot_ly(r, y = ~ciudad, x = ~pct, type = "bar", orientation = "h",
            marker = list(color = ifelse(r$resaltar, COL[["azul"]], COL[["azul_claro"]])),
            customdata = ~arriendo,
            hovertemplate = "<b>%{y}</b><br>%{x:.1f} % en arriendo<br>Arriendo mediano: $%{customdata:,.0f}<extra></extra>") |>
      estilo(leyenda = FALSE) |>
      layout(xaxis = list(ticksuffix = " %"), bargap = 0.25)
  })

  # Tabla y descarga
  tabla_serie <- reactive({
    serie() |>
      transmute(Periodo = periodo,
                `Ocupados (miles)` = round(ocupados_miles, 1),
                `Construcción (miles)` = round(construccion_miles, 1),
                `Participación (%)` = round(participacion, 2),
                `Informalidad construcción (%)` = round(informal_constr, 1),
                `Informalidad total (%)` = round(informal_total, 1),
                `Ingreso mediano construcción` = round(ingreso_constr),
                `Ingreso mediano total` = round(ingreso_total),
                `Encuestados en construcción` = muestra_constr)
  })
  output$tabla <- renderDT(
    datatable(tabla_serie(), rownames = FALSE,
              options = list(dom = "t", pageLength = 12, ordering = FALSE, scrollX = TRUE,
                             language = list(decimal = ",", thousands = "."))) |>
      formatRound(c(2, 3), digits = 1, mark = ".", dec.mark = ",") |>
      formatRound(c(4, 5, 6), digits = 1, mark = ".", dec.mark = ",") |>
      formatCurrency(c(7, 8), currency = "$", digits = 0, mark = ".", dec.mark = ",")
  )
  output$descargar <- downloadHandler(
    filename = function() paste0("serie_construccion_", gsub("[^A-Za-z0-9]+", "_", input$territorio), ".csv"),
    content  = function(file) write.csv(tabla_serie(), file, row.names = FALSE, fileEncoding = "UTF-8")
  )
}

shinyApp(ui, server)

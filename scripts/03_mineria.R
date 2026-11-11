# =============================================================================
# 03_mineria.R
# Minería de datos: ¿qué explica el precio por m² de la vivienda nueva?
# Modelo hedónico + procesamiento de lenguaje natural (PLN) sobre la
# descripción comercial + comparación de modelos predictivos
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Pregunta: ¿cuánto del precio por m² se explica por los atributos físicos,
# la localización y el estrato, y cuánto agregan las amenidades y el
# posicionamiento que el proyecto comunica en su descripción?
#
# Técnicas:
#   · Regresión: MCO hedónico de log(precio/m²), errores agrupados por proyecto
#   · PLN: (a) diccionario de amenidades guiado por teoría
#          (b) bolsa de palabras (unigramas y bigramas) para descubrir términos
#   · Regularización: LASSO que penaliza solo el texto (los fundamentales
#     entran siempre), así cada palabra se mide "después" de los fundamentales
#   · Predicción: bosque aleatorio (ranger)
#   · Validación cruzada agrupada por proyecto (5 grupos x 5 repeticiones):
#     las tipologías de un mismo proyecto nunca están a la vez en
#     entrenamiento y prueba, lo que evita sobrestimar el desempeño
#
# Insumo:    datos/procesados/vivienda_limpia.rds (02_limpieza_vivienda.R)
# Productos: salidas/mineria/ (tablas .csv y gráficos .png)
# =============================================================================


# ---- 0. Configuración --------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("dplyr", "tidyr", "stringr", "stringi", "readr", "purrr",
                  "forcats", "tibble", "ggplot2", "scales", "tidytext",
                  "glmnet", "Matrix", "ranger", "sandwich", "lmtest"))

dir_min <- file.path(rutas$salidas, "mineria")
dir.create(dir_min, showWarnings = FALSE, recursive = TRUE)

K_FOLDS      <- 5      # grupos de validación cruzada
REPETICIONES <- 5      # repeticiones con distinta partición
MIN_DOCS     <- 8      # un término debe aparecer en al menos 8 proyectos


# ---- 1. Datos ----------------------------------------------------------------
# Tipologías aptas: sin área de lote ni precio/m² atípico (ver 02_limpieza)
tip <- readRDS(file.path(rutas$proc, "vivienda_limpia.rds"))$tipologias |>
  filter(apta_modelo)

message(sprintf("Muestra: %d tipologías de %d proyectos en %d mercados",
                nrow(tip), n_distinct(tip$id_proyecto), n_distinct(tip$mercado)))


# ---- 2. PLN (a): diccionario de amenidades y posicionamiento ------------------
# La descripción es del proyecto, así que el texto se procesa una vez por
# proyecto y luego se asigna a sus tipologías.
normalizar <- function(x) {
  x |> stri_trans_general("Latin-ASCII") |> str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |> str_squish()
}

textos <- tip |>
  distinct(id_proyecto, descripcion) |>
  mutate(texto = normalizar(descripcion))

# Expresiones con límites de palabra (\\b) para evitar falsos positivos
# como "vis" dentro de "vista" o "spa" dentro de "espacio"
diccionario_pln <- c(
  piscina       = "\\bpiscinas?\\b",
  gimnasio      = "\\bgimnasio|\\bgym\\b",
  zona_humeda   = "\\bspa\\b|\\bsauna|\\bturco|\\bjacuzzi|zona humeda",
  bbq           = "\\bbbq\\b|zona de asados|\\basador",
  club_house    = "club ?house|\\bclub\\b",
  coworking     = "\\bco ?working|teletrabajo",
  canchas       = "\\bcanchas?\\b|\\bsquash|polideportiv",
  zona_infantil = "juegos infantiles|zona infantil|parque infantil|\\bninos\\b",
  mascotas      = "\\bmascotas?\\b|\\bpet\\b",
  vista         = "\\bvistas?\\b",
  mar_playa     = "\\bmar\\b|\\bplayas?\\b|\\bcaribe\\b",
  naturaleza    = "naturaleza|\\bbosques?\\b|\\blagos?\\b|senderos|zonas verdes",
  exclusividad  = "exclusiv|\\blujo|premium|alto nivel|privilegiad",
  transporte    = "transmilenio|\\bmetro\\b|\\bestacion|autopista",
  inversion     = "inversion|valorizacion|rentabilidad|\\brenta\\b|turistic",
  vis_subsidio  = "\\bvis\\b|\\bvip\\b|subsidio|mi casa ya|interes social",
  obra_gris     = "obra (gris|negra)"        # entrega sin acabados
)
vars_amen <- names(diccionario_pln)

amenidades <- textos |>
  select(id_proyecto) |>
  bind_cols(map_dfc(diccionario_pln, \(p) as.integer(str_detect(textos$texto, p))))

prevalencia <- amenidades |>
  summarise(across(all_of(vars_amen), mean)) |>
  pivot_longer(everything(), names_to = "amenidad", values_to = "pct_proyectos") |>
  arrange(desc(pct_proyectos))


# ---- 3. PLN (b): bolsa de palabras (unigramas y bigramas) --------------------
# Palabras vacías en español (lista propia, sin dependencias externas)
stop_es <- c(
  "de", "la", "el", "en", "y", "a", "los", "las", "del", "con", "por", "para",
  "un", "una", "unos", "unas", "que", "se", "su", "sus", "al", "es", "lo", "como",
  "mas", "o", "u", "e", "este", "esta", "estos", "estas", "ese", "esa", "son",
  "sin", "sobre", "entre", "tu", "tus", "te", "muy", "desde", "hasta", "cada",
  "todo", "toda", "todos", "todas", "nuestro", "nuestra", "nuestros", "nuestras",
  "usted", "ya", "le", "les", "ha", "han", "hay", "ser", "sera", "seran", "tiene",
  "tienen", "donde", "cual", "cuales", "tambien", "asi", "nos", "mi", "mis",
  "ademas", "solo", "otros", "otras", "otro", "otra", "porque", "pero", "si",
  "no", "ni", "cuenta", "cuentan", "contara", "podras", "puedes", "vive", "vivir")

es_valida <- function(p) !(p %in% stop_es) & !str_detect(p, "\\d") & nchar(p) >= 3

unigramas <- textos |>
  unnest_tokens(termino, texto) |>
  filter(es_valida(termino))

bigramas <- textos |>
  unnest_tokens(termino, texto, token = "ngrams", n = 2) |>
  filter(!is.na(termino)) |>
  mutate(p1 = word(termino, 1), p2 = word(termino, 2)) |>
  filter(es_valida(p1), es_valida(p2)) |>
  select(-p1, -p2)

n_docs <- nrow(textos)
tokens <- bind_rows(unigramas, bigramas) |> distinct(id_proyecto, termino)

# Vocabulario: términos en al menos MIN_DOCS proyectos y en no más de la mitad
vocabulario <- tokens |>
  count(termino, name = "n_proyectos") |>
  filter(n_proyectos >= MIN_DOCS, n_proyectos <= 0.5 * n_docs)

# Matriz proyecto x término (presencia/ausencia)
dtm <- tokens |>
  semi_join(vocabulario, by = "termino") |>
  mutate(termino = paste0("w_", str_replace_all(termino, " ", "_")), valor = 1L) |>
  pivot_wider(names_from = termino, values_from = valor, values_fill = 0L)

message(sprintf("PLN: %d amenidades de diccionario y %d términos en la bolsa de palabras",
                length(vars_amen), nrow(vocabulario)))


# ---- 4. Variables del modelo -------------------------------------------------
recodificar <- function(x, tope) {
  case_when(is.na(x) ~ "NR", x >= tope ~ paste0(tope, "+"), TRUE ~ as.character(x))
}

base <- tip |>
  left_join(amenidades, by = "id_proyecto") |>
  left_join(dtm, by = "id_proyecto") |>
  mutate(
    across(starts_with("w_"), \(x) coalesce(x, 0L)),
    y         = log(precio_m2),
    log_area  = log(area_construida_m2),
    alcobas_c = fct_relevel(factor(recodificar(alcobas, 4)), "2"),
    banos_c   = fct_relevel(factor(recodificar(banos, 4)), "2"),
    parq_c    = factor(case_when(is.na(parqueaderos) | parqueaderos == 0 ~ "No reportado",
                                 parqueaderos == 1 ~ "1", TRUE ~ "2 o más"),
                       levels = c("No reportado", "1", "2 o más")),
    estrato_c = fct_relevel(factor(if_else(is.na(estrato), "Sin dato",
                                           paste("Estrato", estrato))), "Estrato 3"),
    mercado   = fct_relevel(factor(mercado), "Bogotá D.C."),
    destacado = as.integer(destacado),
    log_dias  = log1p(dias_publicado)
  )

terminos <- grep("^w_", names(base), value = TRUE)

# Especificaciones
f_fund <- y ~ log_area + alcobas_c + banos_c + parq_c + estrato_c + mercado +
  destacado + log_dias
f_hed  <- update(f_fund, paste(". ~ . +", paste(vars_amen, collapse = " + ")))

# Matrices de diseño construidas una sola vez sobre toda la muestra: así las
# columnas son las mismas en todos los grupos de validación aunque un mercado
# o estrato no aparezca en el entrenamiento.
X_fund <- model.matrix(f_fund, base)
X_hed  <- model.matrix(f_hed, base)
W      <- Matrix(as.matrix(base[terminos]), sparse = TRUE)
X_lasso <- cbind(Matrix(X_hed[, -1], sparse = TRUE), W)
penal   <- c(rep(0, ncol(X_fund) - 1),                 # fundamentales: sin penalidad
             rep(1, ncol(X_hed) - ncol(X_fund)),       # amenidades: penalizadas
             rep(1, ncol(W)))                          # palabras: penalizadas

X_rf <- base |>
  transmute(area_construida_m2,
            alcobas      = coalesce(alcobas, -1L),
            banos        = coalesce(banos, -1L),
            parqueaderos = coalesce(parqueaderos, -1L),
            estrato      = coalesce(estrato, -1L),
            destacado, dias_publicado,
            across(all_of(vars_amen))) |>
  bind_cols(as_tibble(model.matrix(~ mercado - 1, base))) |>
  rename_with(make.names)

y <- base$y
grupo <- base$id_proyecto


# ---- 5. Modelos (cada uno entrena en `tr` y predice en `te`) -----------------

# MCO sobre una matriz de diseño fija: columnas sin variación en el
# entrenamiento quedan con coeficiente 0
mco <- function(X, tr, te) {
  b <- coef(lm.fit(X[tr, , drop = FALSE], y[tr]))
  b[is.na(b)] <- 0
  drop(X[te, , drop = FALSE] %*% b)
}

# Grupos internos (por proyecto) para elegir lambda dentro del entrenamiento
folds_por_proyecto <- function(g, k) {
  proy <- unique(g)
  asignacion <- setNames(sample(rep(seq_len(k), length.out = length(proy))), proy)
  unname(asignacion[g])
}

modelos <- list(
  "Línea base: mediana por mercado y estrato" = function(tr, te) {
    global <- median(y[tr])      # respaldo si el mercado no está en el entrenamiento
    m1 <- base[tr, ] |> group_by(mercado, estrato_c) |> summarise(p1 = median(y), .groups = "drop")
    m2 <- base[tr, ] |> group_by(mercado) |> summarise(p2 = median(y))
    base[te, ] |> left_join(m1, by = c("mercado", "estrato_c")) |>
      left_join(m2, by = "mercado") |>
      mutate(p = coalesce(p1, p2, global)) |> pull(p)
  },
  "MCO: atributos y localización" = function(tr, te) mco(X_fund, tr, te),
  "MCO hedónico + amenidades (PLN)" = function(tr, te) mco(X_hed, tr, te),
  "LASSO + bolsa de palabras (PLN)" = function(tr, te) {
    cv <- cv.glmnet(X_lasso[tr, ], y[tr], alpha = 1, penalty.factor = penal,
                    foldid = folds_por_proyecto(grupo[tr], 5))
    drop(predict(cv, X_lasso[te, ], s = "lambda.min"))
  },
  "Bosque aleatorio" = function(tr, te) {
    rf <- ranger(x = X_rf[tr, ], y = y[tr], num.trees = 500, seed = 2026)
    predict(rf, X_rf[te, ])$predictions
  }
)


# ---- 6. Validación cruzada agrupada por proyecto -----------------------------
message("Validación cruzada agrupada (", K_FOLDS, " grupos x ", REPETICIONES, " repeticiones)...")

cv <- map_dfr(seq_len(REPETICIONES), function(r) {
  set.seed(2026 + r)
  fold <- folds_por_proyecto(grupo, K_FOLDS)
  map_dfr(seq_len(K_FOLDS), function(k) {
    tr <- fold != k
    te <- fold == k
    imap_dfr(modelos, \(f, nombre) tibble(repeticion = r, grupo_cv = k,
                                          modelo = nombre, fila = which(te),
                                          y = y[te], pred = f(tr, te)))
  })
})

metricas <- cv |>
  group_by(modelo, repeticion) |>
  summarise(rmse = sqrt(mean((y - pred)^2)),
            r2   = 1 - sum((y - pred)^2) / sum((y - mean(y))^2),
            mape = 100 * mean(abs(exp(pred) / exp(y) - 1)),
            .groups = "drop") |>
  group_by(modelo) |>
  summarise(across(c(r2, rmse, mape), list(media = mean, de = sd))) |>
  mutate(modelo = factor(modelo, levels = names(modelos))) |>
  arrange(modelo)

print(metricas, width = Inf)


# ---- 7. Modelo hedónico final: primas con errores agrupados por proyecto ------
mco_hed <- lm(f_hed, data = base)
vc_cl   <- vcovCL(mco_hed, cluster = ~ id_proyecto)
coefs   <- coeftest(mco_hed, vcov. = vc_cl)

primas <- tibble(termino = rownames(coefs),
                 coef = coefs[, 1], ee_agrupado = coefs[, 2], p_valor = coefs[, 4]) |>
  mutate(prima_pct = 100 * (exp(coef) - 1),
         ic_inf    = 100 * (exp(coef - 1.96 * ee_agrupado) - 1),
         ic_sup    = 100 * (exp(coef + 1.96 * ee_agrupado) - 1),
         grupo = case_when(
           termino %in% vars_amen          ~ "Amenidad / posicionamiento (PLN)",
           str_detect(termino, "^estrato") ~ "Estrato",
           str_detect(termino, "^mercado") ~ "Mercado",
           TRUE                            ~ "Atributos físicos y del aviso"))

r2_hed <- summary(mco_hed)$r.squared
message(sprintf("MCO hedónico: R² = %.3f (muestra completa)", r2_hed))


# ---- 8. LASSO final: términos que agregan información al precio --------------
set.seed(2026)
lasso <- cv.glmnet(X_lasso, y, alpha = 1, penalty.factor = penal,
                   foldid = folds_por_proyecto(grupo, 5))
b_lasso <- coef(lasso, s = "lambda.min")

palabras <- tibble(termino = rownames(b_lasso), coef = as.numeric(b_lasso)) |>
  filter(coef != 0, termino %in% c(vars_amen, terminos)) |>
  mutate(etiqueta  = str_replace_all(str_remove(termino, "^w_"), "_", " "),
         tipo      = if_else(termino %in% vars_amen, "Diccionario", "Bolsa de palabras"),
         prima_pct = 100 * (exp(coef) - 1)) |>
  arrange(desc(prima_pct))


# ---- 9. Bosque aleatorio final: importancia por permutación -------------------
rf <- ranger(x = X_rf, y = y, num.trees = 1000, importance = "permutation",
             seed = 2026)
importancia <- tibble(variable = names(rf$variable.importance),
                      importancia = rf$variable.importance) |>
  mutate(variable = str_replace(variable, "^mercado", "Mercado: ") |>
           str_replace_all("\\.", " ")) |>
  arrange(desc(importancia))


# ---- 10. Aplicación: precio por m² comparable entre mercados -----------------
# Mismo inmueble en todos los mercados (70 m², 3 alcobas, 2 baños,
# 1 parqueadero, estrato 4, sin amenidades declaradas): aísla el efecto de
# la localización de las diferencias en composición de la oferta.
smearing <- mean(exp(residuals(mco_hed)))   # corrección de Duan al volver a niveles

unidad_estandar <- tibble(
  mercado   = factor(levels(base$mercado), levels = levels(base$mercado)),
  log_area  = log(70),
  alcobas_c = factor("3", levels = levels(base$alcobas_c)),
  banos_c   = factor("2", levels = levels(base$banos_c)),
  parq_c    = factor("1", levels = levels(base$parq_c)),
  estrato_c = factor("Estrato 4", levels = levels(base$estrato_c)),
  destacado = 0L,
  log_dias  = median(base$log_dias)) |>
  bind_cols(as_tibble(setNames(as.list(rep(0L, length(vars_amen))), vars_amen)))

precio_comparable <- unidad_estandar |>
  mutate(precio_m2_ajustado = exp(predict(mco_hed, newdata = unidad_estandar)) * smearing) |>
  select(mercado, precio_m2_ajustado) |>
  left_join(base |> group_by(mercado) |>
              summarise(precio_m2_mediana = median(precio_m2),
                        tipologias = n(), proyectos = n_distinct(id_proyecto)),
            by = "mercado") |>
  arrange(desc(precio_m2_ajustado))


# ---- 11. Aplicación: proyectos por encima / por debajo de su valor hedónico --
# Residuo promedio del proyecto: cuánto más (o menos) cobra frente a lo que
# predicen sus atributos. Útil para inteligencia de mercado.
residuos_proyecto <- base |>
  mutate(residuo = residuals(mco_hed)) |>
  group_by(id_proyecto, proyecto, municipio, mercado, estrato) |>
  summarise(tipologias = n(),
            precio_m2_mediana = median(precio_m2),
            desvio_pct = 100 * (exp(mean(residuo)) - 1),
            .groups = "drop") |>
  arrange(desc(desvio_pct))


# ---- 12. Tablas --------------------------------------------------------------
write_csv(metricas,          file.path(dir_min, "cv_metricas_modelos.csv"))
write_csv(primas,            file.path(dir_min, "mco_primas_hedonicas.csv"))
write_csv(palabras,          file.path(dir_min, "lasso_terminos_seleccionados.csv"))
write_csv(importancia,       file.path(dir_min, "rf_importancia_variables.csv"))
write_csv(precio_comparable, file.path(dir_min, "precio_m2_comparable_mercado.csv"))
write_csv(residuos_proyecto, file.path(dir_min, "proyectos_desvio_hedonico.csv"))
write_csv(prevalencia,       file.path(dir_min, "pln_prevalencia_amenidades.csv"))

# Base del modelo (tipologías + variables de PLN): insumo de 03b_prediccion_precios.R
saveRDS(list(base = base, vars_amen = vars_amen),
        file.path(rutas$proc, "vivienda_base_modelo.rds"))


# ---- 13. Gráficos ------------------------------------------------------------
# Paleta: azul para la serie principal, gris para lo no significativo,
# azul/rojo para primas positivas/negativas (polaridad)
col_azul  <- "#2a78d6"; col_rojo <- "#e34948"; col_gris <- "#a8a7a1"
col_texto <- "#0b0b0b"; col_texto2 <- "#52514e"; col_fondo <- "#fcfcfb"

tema <- theme_minimal(base_size = 11) +
  theme(plot.background    = element_rect(fill = col_fondo, colour = NA),
        panel.grid.minor   = element_blank(),
        panel.grid.major   = element_line(colour = "#e6e5e0", linewidth = 0.3),
        axis.text          = element_text(colour = col_texto2),
        axis.title         = element_text(colour = col_texto2, size = 10),
        plot.title         = element_text(colour = col_texto, face = "bold", size = 13),
        plot.subtitle      = element_text(colour = col_texto2, size = 10),
        plot.caption       = element_text(colour = col_texto2, size = 8, hjust = 0),
        plot.title.position = "plot",
        legend.position    = "top", legend.justification = "left",
        legend.title       = element_blank(),
        legend.text        = element_text(colour = col_texto2))
guardar <- function(p, nombre, ancho = 8, alto = 5) {
  ggsave(file.path(dir_min, nombre), p, width = ancho, height = alto, dpi = 200, bg = col_fondo)
}
fuente <- "Fuente: oferta de vivienda nueva en portal inmobiliario (nov-2021). Cálculos propios."

# 13.1 Desempeño fuera de muestra
g1 <- metricas |>
  mutate(modelo = fct_rev(modelo)) |>
  ggplot(aes(x = r2_media, y = modelo)) +
  geom_errorbarh(aes(xmin = r2_media - r2_de, xmax = r2_media + r2_de),
                 height = 0, colour = col_gris, linewidth = 0.6) +
  geom_point(colour = col_azul, size = 3) +
  geom_text(aes(label = sprintf("R² %.2f · error medio %.0f%%", r2_media, mape_media)),
            nudge_y = 0.3, size = 3.2, colour = col_texto2, hjust = 0.5) +
  scale_x_continuous(limits = c(0, 1), labels = label_number(accuracy = 0.1)) +
  labs(title = "Desempeño fuera de muestra de cada modelo",
       subtitle = "R² en log(precio/m²); validación cruzada agrupada por proyecto (5 grupos × 5 repeticiones)",
       x = "R² fuera de muestra (media ± desviación estándar)", y = NULL, caption = fuente) +
  tema
guardar(g1, "fig1_desempeno_modelos.png", alto = 4.2)

# 13.2 Primas de amenidades (MCO con IC 95 % agrupado por proyecto)
g2 <- primas |>
  filter(grupo == "Amenidad / posicionamiento (PLN)") |>
  mutate(significativa = if_else(p_valor < 0.05, "Significativa al 5 %", "No significativa"),
         termino = fct_reorder(str_replace_all(termino, "_", " "), prima_pct)) |>
  ggplot(aes(x = prima_pct, y = termino, colour = significativa)) +
  geom_vline(xintercept = 0, colour = col_texto2, linewidth = 0.3) +
  geom_errorbarh(aes(xmin = ic_inf, xmax = ic_sup), height = 0, linewidth = 0.6) +
  geom_point(size = 2.6) +
  scale_colour_manual(values = c("Significativa al 5 %" = col_azul, "No significativa" = col_gris)) +
  scale_x_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Prima en el precio por m² asociada a lo que dice la descripción",
       subtitle = "MCO hedónico controlando por área, alcobas, baños, parqueaderos, estrato y mercado; IC 95 %",
       x = "Prima sobre el precio por m²", y = NULL, caption = fuente) +
  tema
guardar(g2, "fig2_primas_amenidades.png", alto = 5.5)

# 13.3 Precio comparable vs. mediana observada por mercado
g3 <- precio_comparable |>
  mutate(mercado = fct_reorder(mercado, precio_m2_ajustado)) |>
  pivot_longer(c(precio_m2_mediana, precio_m2_ajustado), names_to = "medida", values_to = "valor") |>
  mutate(medida = recode(medida,
                         precio_m2_mediana  = "Mediana observada",
                         precio_m2_ajustado = "Ajustado: misma vivienda en todos los mercados")) |>
  ggplot(aes(x = valor / 1e6, y = mercado)) +
  geom_line(aes(group = mercado), colour = "#d6d5d0", linewidth = 1.2) +
  geom_point(aes(colour = medida), size = 3) +
  scale_colour_manual(values = c("Mediana observada" = col_gris,
                                 "Ajustado: misma vivienda en todos los mercados" = col_azul)) +
  scale_x_continuous(labels = label_number(accuracy = 0.1, suffix = " M")) +
  labs(title = "Precio por m² comparable entre mercados",
       subtitle = "Vivienda estándar: 70 m², 3 alcobas, 2 baños, 1 parqueadero, estrato 4, sin amenidades",
       x = "Millones de pesos por m² (2021)", y = NULL, caption = fuente) +
  tema
guardar(g3, "fig3_precio_comparable_mercado.png", alto = 5.5)

# 13.4 Términos seleccionados por el LASSO
if (nrow(palabras) > 0) {
  g4 <- palabras |>
    filter(abs(prima_pct) >= 1) |>             # se omiten primas menores a 1 %
    slice_max(abs(prima_pct), n = 20) |>
    mutate(signo = if_else(prima_pct > 0, "Prima positiva", "Prima negativa"),
           etiqueta = fct_reorder(etiqueta, prima_pct)) |>
    ggplot(aes(x = prima_pct, y = etiqueta, fill = signo)) +
    geom_col(width = 0.65) +
    geom_vline(xintercept = 0, colour = col_texto2, linewidth = 0.3) +
    scale_fill_manual(values = c("Prima positiva" = col_azul, "Prima negativa" = col_rojo)) +
    scale_x_continuous(labels = label_percent(scale = 1)) +
    labs(title = "Términos de la descripción que el LASSO conserva",
         subtitle = "Prima sobre el precio por m² después de controlar por atributos, estrato y mercado",
         x = "Prima sobre el precio por m²", y = NULL, caption = fuente) +
    tema
  guardar(g4, "fig4_terminos_lasso.png", alto = 5.5)
}

# 13.5 Importancia de variables en el bosque aleatorio
g5 <- importancia |>
  slice_max(importancia, n = 15) |>
  mutate(variable = fct_reorder(str_replace_all(variable, "_", " "), importancia)) |>
  ggplot(aes(x = importancia, y = variable)) +
  geom_col(fill = col_azul, width = 0.65) +
  labs(title = "Qué variables usa más el bosque aleatorio",
       subtitle = "Importancia por permutación (aumento del error al desordenar la variable)",
       x = "Importancia", y = NULL, caption = fuente) +
  tema
guardar(g5, "fig5_importancia_rf.png", alto = 5)

# 13.6 Observado vs. predicho fuera de muestra (bosque aleatorio, repetición 1)
g6 <- cv |>
  filter(modelo == "Bosque aleatorio", repeticion == 1) |>
  ggplot(aes(x = exp(pred) / 1e6, y = exp(y) / 1e6)) +
  geom_abline(slope = 1, intercept = 0, colour = col_texto2, linewidth = 0.3, linetype = 2) +
  geom_point(colour = col_azul, alpha = 0.55, size = 1.8) +
  scale_x_continuous(labels = label_number(suffix = " M")) +
  scale_y_continuous(labels = label_number(suffix = " M")) +
  coord_equal() +
  labs(title = "Precio por m²: observado vs. predicho fuera de muestra",
       subtitle = "Bosque aleatorio; cada punto es una tipología predicha sin ver su proyecto",
       x = "Predicho (millones por m²)", y = "Observado (millones por m²)", caption = fuente) +
  tema
guardar(g6, "fig6_observado_vs_predicho.png", ancho = 6.5, alto = 6.5)


# ---- 14. Resumen en consola --------------------------------------------------
message("\n== Primas significativas (MCO hedónico, EE agrupados por proyecto) ==")
primas |> filter(p_valor < 0.05, termino != "(Intercept)") |>
  select(grupo, termino, prima_pct, p_valor) |>
  mutate(across(where(is.numeric), \(x) round(x, 3))) |> print(n = Inf)
message("\n== Términos LASSO ==")
print(select(palabras, etiqueta, tipo, prima_pct), n = Inf)
message("\n== Precio por m² comparable ==")
print(precio_comparable, n = Inf)

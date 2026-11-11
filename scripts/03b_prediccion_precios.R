# =============================================================================
# 03b_prediccion_precios.R
# Predicción del precio por m² de vivienda nueva con aprendizaje automático:
# bosque aleatorio y XGBoost ajustados, ensamble con el modelo hedónico,
# variables más relevantes (SHAP) y un ejercicio de predicción
# -----------------------------------------------------------------------------
# Prueba técnica · Coordinación Económica · Camacol
# Autor: Andrés Nicolás Jiménez Díaz
#
# Pregunta: si llega una tipología de un proyecto que el modelo nunca vio,
# ¿con qué error se puede estimar su precio por m²? ¿Qué variables pesan más?
#
# Diseño de la evaluación:
#   1. Se aparta el 20 % de los PROYECTOS como conjunto de prueba. No se usa
#      para nada hasta el final: es la medida honesta del error de predicción.
#   2. En el 80 % restante se ajustan los hiperparámetros con validación
#      cruzada agrupada por proyecto (5 grupos), como en 03_mineria.R.
#   3. Se comparan cinco modelos en el conjunto de prueba.
#   4. Con el modelo final se interpretan las variables (valores SHAP) y se
#      predicen cuatro viviendas hipotéticas, con intervalo del 80 %.
#
# Por qué estos modelos:
#   · Bosque aleatorio (ranger): promedia cientos de árboles independientes;
#     robusto y con pocos hiperparámetros.
#   · XGBoost: árboles secuenciales, cada uno corrige el error del anterior;
#     suele ser el más preciso en datos tabulares. Se fija subsample = 1 y
#     colsample_bytree = 1: con ~800 observaciones el submuestreo agrega
#     ruido, y así el resultado es determinístico (igual en cualquier equipo).
#   · Ensamble: promedio ponderado del MCO hedónico y XGBoost. Combina la
#     forma log-lineal (que extrapola bien) con las no linealidades del árbol.
#
# Insumo:    datos/procesados/vivienda_base_modelo.rds (03_mineria.R)
# Productos: salidas/prediccion/ (tablas .csv y gráficos .png)
# Tiempo:    alrededor de un minuto.
# =============================================================================


# ---- 0. Configuración --------------------------------------------------------
if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
source(here::here("scripts", "00_setup.R"))
cargar_paquetes(c("dplyr", "tidyr", "purrr", "stringr", "stringi", "readr",
                  "tibble", "forcats", "ggplot2", "scales", "ranger", "xgboost"))

dir_pred <- file.path(rutas$salidas, "prediccion")
dir.create(dir_pred, showWarnings = FALSE, recursive = TRUE)

K_FOLDS    <- 5      # grupos de validación cruzada (por proyecto)
PCT_PRUEBA <- 0.20   # proporción de proyectos en el conjunto de prueba
INTERVALO  <- 0.80   # cobertura del intervalo de predicción


# ---- 1. Datos y variables del proyecto ---------------------------------------
mb        <- readRDS(file.path(rutas$proc, "vivienda_base_modelo.rds"))
vars_amen <- mb$vars_amen

# Variables del proyecto que NO usan el precio (usarlo filtraría el objetivo):
# cuántas tipologías ofrece, qué tan grande es la unidad frente a las demás
# del mismo proyecto y cuántos proyectos tiene la constructora en la muestra
base <- mb$base |>
  group_by(id_proyecto) |>
  mutate(n_tipologias  = n(),
         area_relativa = area_construida_m2 / median(area_construida_m2)) |>
  ungroup() |>
  group_by(constructora) |>
  mutate(proyectos_constructora = n_distinct(id_proyecto)) |>
  ungroup()

y <- base$y   # log(precio por m²)


# ---- 2. Matriz de variables para los modelos de árboles ----------------------
MERCADOS <- levels(base$mercado)
slug     <- function(x) str_replace_all(stri_trans_general(x, "Latin-ASCII"), "[^A-Za-z0-9]+", "_")
COL_MERCADO <- setNames(paste0("mercado_", slug(MERCADOS)), MERCADOS)

# La misma función arma la matriz de la muestra y la de los casos hipotéticos,
# así las columnas coinciden siempre
construir_X <- function(d) {
  numericas <- d |>
    transmute(area_construida_m2, area_privada_m2, area_relativa,
              alcobas, banos, parqueaderos, estrato,
              destacado = as.integer(destacado), dias_publicado,
              n_tipologias, proyectos_constructora,
              across(all_of(vars_amen), as.integer))
  dummies <- matrix(vapply(MERCADOS, \(m) as.integer(as.character(d$mercado) == m),
                           integer(nrow(d))),
                    nrow = nrow(d), dimnames = list(NULL, COL_MERCADO))
  X <- cbind(as.matrix(numericas), dummies)
  storage.mode(X) <- "double"
  X
}

X    <- construir_X(base)        # XGBoost acepta faltantes (NA) de forma nativa
X_rf <- X; X_rf[is.na(X_rf)] <- -1   # el bosque aleatorio no: se codifican como -1

# Nombres legibles y familias de variables para los gráficos
etiqueta <- function(v) {
  mercado <- names(COL_MERCADO)[match(v, COL_MERCADO)]
  case_when(
    v == "area_construida_m2"     ~ "Área construida",
    v == "area_privada_m2"        ~ "Área privada",
    v == "area_relativa"          ~ "Área frente al resto del proyecto",
    v == "alcobas"                ~ "Alcobas",
    v == "banos"                  ~ "Baños",
    v == "parqueaderos"           ~ "Parqueaderos",
    v == "estrato"                ~ "Estrato",
    v == "destacado"              ~ "Aviso destacado",
    v == "dias_publicado"         ~ "Días publicado",
    v == "n_tipologias"           ~ "Tipologías del proyecto",
    v == "proyectos_constructora" ~ "Proyectos de la constructora",
    !is.na(mercado)               ~ paste("Mercado:", mercado),
    TRUE                          ~ paste("Texto:", str_replace_all(v, "_", " "))
  )
}
familia <- function(v) {
  case_when(
    v %in% c("area_construida_m2", "area_privada_m2", "area_relativa",
             "alcobas", "banos", "parqueaderos") ~ "Tamaño y distribución",
    v == "estrato"                               ~ "Estrato",
    v %in% COL_MERCADO                           ~ "Mercado (localización)",
    v %in% vars_amen                             ~ "Amenidades y texto (PLN)",
    TRUE                                         ~ "Proyecto y aviso"
  )
}


# ---- 3. Conjunto de prueba y grupos de validación ----------------------------
folds_por_proyecto <- function(g, k) {
  proy <- unique(g)
  asignacion <- setNames(sample(rep(seq_len(k), length.out = length(proy))), proy)
  unname(asignacion[g])
}

set.seed(2026)
proyectos   <- unique(base$id_proyecto)
proy_prueba <- sample(proyectos, round(PCT_PRUEBA * length(proyectos)))
te <- which(base$id_proyecto %in% proy_prueba)    # prueba
tr <- which(!base$id_proyecto %in% proy_prueba)   # entrenamiento

set.seed(2026)
grupo_tr <- folds_por_proyecto(base$id_proyecto[tr], K_FOLDS)
folds    <- unname(split(seq_along(tr), grupo_tr))   # posiciones dentro de tr

message(sprintf("Entrenamiento: %d tipologías de %d proyectos | Prueba: %d tipologías de %d proyectos",
                length(tr), n_distinct(base$id_proyecto[tr]),
                length(te), n_distinct(base$id_proyecto[te])))


# ---- 4. Modelos: funciones que entrenan en `ent` y predicen en `pru` ---------
# (ent y pru son posiciones de fila en la base completa)

# Línea base: mediana por mercado y estrato
pred_base <- function(ent, pru) {
  global <- median(y[ent])     # respaldo si el mercado no aparece en el entrenamiento
  m1 <- base[ent, ] |> group_by(mercado, estrato_c) |> summarise(p1 = median(y), .groups = "drop")
  m2 <- base[ent, ] |> group_by(mercado) |> summarise(p2 = median(y))
  base[pru, ] |> left_join(m1, by = c("mercado", "estrato_c")) |>
    left_join(m2, by = "mercado") |>
    mutate(p = coalesce(p1, p2, global)) |> pull(p)
}

# MCO hedónico: la especificación de 03_mineria.R (atributos + PLN)
f_hed <- reformulate(c("log_area", "alcobas_c", "banos_c", "parq_c", "estrato_c",
                       "mercado", "destacado", "log_dias", vars_amen), response = "y")
X_mco <- model.matrix(f_hed, base)
coef_mco <- function(ent) {
  b <- coef(lm.fit(X_mco[ent, , drop = FALSE], y[ent]))
  b[is.na(b)] <- 0     # columnas sin variación en el entrenamiento
  b
}
pred_mco <- function(ent, pru) drop(X_mco[pru, , drop = FALSE] %*% coef_mco(ent))

# Bosque aleatorio
ajustar_rf <- function(ent, hp, importancia = "none") {
  ranger(x = X_rf[ent, , drop = FALSE], y = y[ent], num.trees = 500,
         mtry = hp$mtry, min.node.size = hp$min.node.size,
         importance = importancia, seed = 2026)
}
pred_rf <- function(ent, pru, hp) predict(ajustar_rf(ent, hp), X_rf[pru, , drop = FALSE])$predictions

# XGBoost
PARAMS_XGB <- list(objective = "reg:squarederror", eval_metric = "rmse",
                   subsample = 1, colsample_bytree = 1, nthread = 2)
ajustar_xgb <- function(ent, hp) {
  xgb.train(params = c(PARAMS_XGB, hp$params), data = xgb.DMatrix(X[ent, , drop = FALSE], label = y[ent]),
            nrounds = hp$nrounds, verbose = 0)
}
pred_xgb <- function(ent, pru, hp) predict(ajustar_xgb(ent, hp), xgb.DMatrix(X[pru, , drop = FALSE]))

# Predicción fuera de grupo (out-of-fold) dentro del entrenamiento
oof <- function(f) {
  p <- numeric(length(tr))
  for (k in seq_along(folds)) {
    pru <- folds[[k]]
    p[pru] <- f(tr[-pru], tr[pru])
  }
  p
}

metricas <- function(obs, pred) {
  error_pct <- abs(exp(pred - obs) - 1)          # error relativo en pesos por m²
  tibble(r2        = 1 - sum((obs - pred)^2) / sum((obs - mean(obs))^2),
         rmse_log  = sqrt(mean((obs - pred)^2)),
         mape      = 100 * mean(error_pct),
         mdape     = 100 * median(error_pct),
         dentro_20 = 100 * mean(error_pct <= 0.20))
}


# ---- 5. Ajuste de hiperparámetros: bosque aleatorio --------------------------
message("Ajustando el bosque aleatorio...")
grilla_rf <- expand_grid(mtry = c(3, 6, 10, 15, 20), min.node.size = c(2, 5, 10))
grilla_rf$rmse_cv <- pmap_dbl(grilla_rf, function(mtry, min.node.size) {
  hp <- list(mtry = mtry, min.node.size = min.node.size)
  p  <- oof(\(ent, pru) pred_rf(ent, pru, hp))
  sqrt(mean((y[tr] - p)^2))
})
mejor_rf <- grilla_rf |> slice_min(rmse_cv, n = 1, with_ties = FALSE)
hp_rf    <- list(mtry = mejor_rf$mtry, min.node.size = mejor_rf$min.node.size)


# ---- 6. Ajuste de hiperparámetros: XGBoost -----------------------------------
# Para cada combinación, xgb.cv entrena hasta 3.000 árboles y se detiene cuando
# el error de validación deja de bajar en 50 rondas (parada temprana); el
# número óptimo de árboles es el de menor error de validación.
message("Ajustando XGBoost...")
d_tr <- xgb.DMatrix(X[tr, ], label = y[tr])
grilla_xgb <- expand_grid(learning_rate = c(0.03, 0.08), max_depth = c(2, 3, 4, 6),
                          min_child_weight = c(1, 4), lambda = c(1, 10))
res_xgb <- pmap_dfr(grilla_xgb, function(learning_rate, max_depth, min_child_weight, lambda) {
  p <- c(PARAMS_XGB, list(learning_rate = learning_rate, max_depth = max_depth,
                          min_child_weight = min_child_weight, lambda = lambda))
  cv <- xgb.cv(params = p, data = d_tr, nrounds = 3000, folds = folds,
               early_stopping_rounds = 50, verbose = FALSE)
  registro <- as.data.frame(cv$evaluation_log)
  tibble(rmse_cv = min(registro$test_rmse_mean),
         nrounds = registro$iter[which.min(registro$test_rmse_mean)])
})
grilla_xgb <- bind_cols(grilla_xgb, res_xgb)
mejor_xgb  <- grilla_xgb |> slice_min(rmse_cv, n = 1, with_ties = FALSE)
hp_xgb <- list(params = list(learning_rate = mejor_xgb$learning_rate, max_depth = mejor_xgb$max_depth,
                             min_child_weight = mejor_xgb$min_child_weight, lambda = mejor_xgb$lambda),
               nrounds = mejor_xgb$nrounds)


# ---- 7. Ensamble: peso óptimo entre MCO y XGBoost ----------------------------
# Se elige con las predicciones fuera de grupo del entrenamiento, nunca con la prueba
oof_base <- oof(pred_base)
oof_mco  <- oof(pred_mco)
oof_rf   <- oof(\(ent, pru) pred_rf(ent, pru, hp_rf))
oof_xgb  <- oof(\(ent, pru) pred_xgb(ent, pru, hp_xgb))

pesos <- tibble(w_xgb = seq(0, 1, by = 0.05)) |>
  mutate(rmse_cv = map_dbl(w_xgb, \(w) sqrt(mean((y[tr] - (w * oof_xgb + (1 - w) * oof_mco))^2))))
W_XGB <- pesos$w_xgb[which.min(pesos$rmse_cv)]
oof_ens <- W_XGB * oof_xgb + (1 - W_XGB) * oof_mco


# ---- 8. Evaluación final en el conjunto de prueba ----------------------------
pred_prueba <- tibble(
  fila = te,
  `Línea base: mediana por mercado y estrato` = pred_base(tr, te),
  `MCO hedónico + PLN`                        = pred_mco(tr, te),
  `Bosque aleatorio ajustado`                 = pred_rf(tr, te, hp_rf),
  `XGBoost ajustado`                          = pred_xgb(tr, te, hp_xgb)
) |>
  mutate(`Ensamble MCO + XGBoost` = W_XGB * `XGBoost ajustado` + (1 - W_XGB) * `MCO hedónico + PLN`)

MODELOS <- setdiff(names(pred_prueba), "fila")
oof_lista <- list(oof_base, oof_mco, oof_rf, oof_xgb, oof_ens) |> setNames(MODELOS)

desempeno <- map_dfr(MODELOS, \(m) bind_cols(
  tibble(modelo = m),
  metricas(y[tr], oof_lista[[m]]) |> select(r2_cv = r2, mdape_cv = mdape),
  metricas(y[te], pred_prueba[[m]]))) |>
  mutate(modelo = factor(modelo, levels = MODELOS))

print(desempeno, width = Inf)

# Intervalo de predicción: cuantiles de los errores fuera de grupo del ensamble
# en el entrenamiento; su cobertura real se verifica en la prueba
resid_oof <- y[tr] - oof_ens
q_int     <- quantile(resid_oof, c((1 - INTERVALO) / 2, 1 - (1 - INTERVALO) / 2))
resid_te  <- y[te] - pred_prueba$`Ensamble MCO + XGBoost`
cobertura <- mean(resid_te >= q_int[1] & resid_te <= q_int[2])
message(sprintf("Intervalo del %d %%: [%+.1f %%, %+.1f %%]; cobertura real en la prueba: %.1f %%",
                100 * INTERVALO, 100 * (exp(q_int[1]) - 1), 100 * (exp(q_int[2]) - 1), 100 * cobertura))


# ---- 9. Modelos finales con toda la muestra e interpretación -----------------
todas    <- seq_len(nrow(base))
xgb_fin  <- ajustar_xgb(todas, hp_xgb)
b_mco    <- coef_mco(todas)
rf_fin   <- ajustar_rf(todas, hp_rf, importancia = "permutation")

# Valores SHAP (TreeSHAP exacto de XGBoost): cuánto mueve cada variable la
# predicción de cada tipología frente al promedio, en log puntos (≈ %)
shap_todo <- predict(xgb_fin, xgb.DMatrix(X), predcontrib = TRUE)
shap      <- shap_todo[, colnames(X), drop = FALSE]   # sin la columna del intercepto

importancia <- tibble(variable = colnames(X),
                      shap_medio = colMeans(abs(shap))) |>
  left_join(tibble(variable = names(rf_fin$variable.importance),
                   permutacion_rf = rf_fin$variable.importance), by = "variable") |>
  mutate(etiqueta = etiqueta(variable), familia = familia(variable),
         efecto_tipico_pct = 100 * (exp(shap_medio) - 1)) |>
  arrange(desc(shap_medio))

importancia_familia <- tibble(familia = familia(colnames(X)), idx = seq_len(ncol(X))) |>
  group_by(familia) |>
  summarise(shap_medio = mean(abs(rowSums(shap[, idx, drop = FALSE]))), .groups = "drop") |>
  mutate(participacion = 100 * shap_medio / sum(shap_medio)) |>
  arrange(desc(shap_medio))

print(importancia_familia)


# ---- 10. Ejercicio de predicción: cuatro viviendas hipotéticas ---------------
recodificar <- function(x, tope) {
  case_when(is.na(x) ~ "NR", x >= tope ~ paste0(tope, "+"), TRUE ~ as.character(x))
}
casos <- tribble(
  ~caso, ~descripcion,                                                       ~mercado,          ~estrato, ~area_construida_m2, ~alcobas, ~banos, ~parqueaderos, ~amenidades,
  "A",   "Bogotá, estrato 3: 50 m², 2 alcobas, 1 baño; proyecto VIS en obra gris", "Bogotá D.C.",     3L,  50, 2L, 1L, NA_integer_, list(c("vis_subsidio", "obra_gris")),
  "B",   "Valle de Aburrá, estrato 4: 70 m², 3 alcobas, 2 baños, piscina y gimnasio", "Valle de Aburrá", 4L, 70, 3L, 2L, 1L, list(c("piscina", "gimnasio")),
  "C",   "Cartagena, estrato 5: 85 m², 2 alcobas, frente al mar con piscina",  "Cartagena",       5L,  85, 2L, 2L, 1L, list(c("piscina", "mar_playa", "vista")),
  "D",   "Bogotá, estrato 6: 130 m², 3 alcobas, 4 baños, 2 parqueaderos, lujo", "Bogotá D.C.",     6L, 130, 3L, 4L, 2L, list(c("gimnasio", "exclusividad"))
) |>
  mutate(amenidades = map(amenidades, unlist))

# Valores típicos para lo que no describe el caso (mediana de la muestra)
casos_df <- casos |>
  mutate(area_privada_m2 = NA_real_, area_relativa = 1, destacado = 0L,
         dias_publicado = median(base$dias_publicado),
         n_tipologias = median(base$n_tipologias),
         proyectos_constructora = median(base$proyectos_constructora))
for (a in vars_amen) casos_df[[a]] <- as.integer(map_lgl(casos$amenidades, \(v) a %in% v))
casos_df <- casos_df |>
  mutate(y = NA_real_,
         log_area  = log(area_construida_m2),
         alcobas_c = factor(recodificar(alcobas, 4), levels = levels(base$alcobas_c)),
         banos_c   = factor(recodificar(banos, 4), levels = levels(base$banos_c)),
         parq_c    = factor(case_when(is.na(parqueaderos) | parqueaderos == 0 ~ "No reportado",
                                      parqueaderos == 1 ~ "1", TRUE ~ "2 o más"),
                            levels = levels(base$parq_c)),
         estrato_c = factor(paste("Estrato", estrato), levels = levels(base$estrato_c)),
         mercado   = factor(mercado, levels = MERCADOS),
         log_dias  = log1p(dias_publicado))

X_casos  <- construir_X(casos_df)
Xm_casos <- model.matrix(update(f_hed, NULL ~ .), casos_df)
p_xgb    <- predict(xgb_fin, xgb.DMatrix(X_casos))
p_mco    <- drop(Xm_casos[, colnames(X_mco)] %*% b_mco)
p_ens    <- W_XGB * p_xgb + (1 - W_XGB) * p_mco

ejercicio <- casos |>
  select(caso, descripcion, area_construida_m2) |>
  mutate(precio_m2_xgb  = exp(p_xgb),
         precio_m2_mco  = exp(p_mco),
         precio_m2      = exp(p_ens),
         precio_m2_inf  = exp(p_ens + q_int[1]),
         precio_m2_sup  = exp(p_ens + q_int[2]),
         precio_total   = precio_m2 * area_construida_m2,
         precio_tot_inf = precio_m2_inf * area_construida_m2,
         precio_tot_sup = precio_m2_sup * area_construida_m2)

# Descomposición SHAP de cada caso por familia de variables (log puntos)
shap_casos <- predict(xgb_fin, xgb.DMatrix(X_casos), predcontrib = TRUE)
base_xgb   <- shap_casos[1, ncol(shap_casos)]          # valor esperado del modelo
shap_casos <- shap_casos[, colnames(X), drop = FALSE]
descomposicion <- map_dfr(seq_len(nrow(casos)), function(i) {
  tibble(caso = casos$caso[i], variable = colnames(X), shap = shap_casos[i, ]) |>
    mutate(familia = familia(variable)) |>
    group_by(caso, familia) |> summarise(shap = sum(shap), .groups = "drop")
})


# ---- 11. Tablas --------------------------------------------------------------
write_csv(grilla_rf,           file.path(dir_pred, "ajuste_bosque_aleatorio.csv"))
write_csv(grilla_xgb,          file.path(dir_pred, "ajuste_xgboost.csv"))
write_csv(pesos,               file.path(dir_pred, "ajuste_peso_ensamble.csv"))
write_csv(desempeno,           file.path(dir_pred, "desempeno_modelos.csv"))
write_csv(importancia,         file.path(dir_pred, "importancia_variables.csv"))
write_csv(importancia_familia, file.path(dir_pred, "importancia_familias.csv"))
write_csv(ejercicio,           file.path(dir_pred, "ejercicio_prediccion.csv"))
write_csv(descomposicion,      file.path(dir_pred, "ejercicio_descomposicion_shap.csv"))
write_csv(tibble(hiperparametro = c("rf_mtry", "rf_min_node_size", "xgb_learning_rate", "xgb_max_depth",
                                    "xgb_min_child_weight", "xgb_lambda", "xgb_nrounds", "peso_xgb_ensamble",
                                    "intervalo_inf_pct", "intervalo_sup_pct", "cobertura_prueba_pct"),
                 valor = c(hp_rf$mtry, hp_rf$min.node.size, hp_xgb$params$learning_rate,
                           hp_xgb$params$max_depth, hp_xgb$params$min_child_weight,
                           hp_xgb$params$lambda, hp_xgb$nrounds, W_XGB,
                           100 * (exp(q_int[1]) - 1), 100 * (exp(q_int[2]) - 1), 100 * cobertura)),
          file.path(dir_pred, "hiperparametros_elegidos.csv"))


# ---- 12. Gráficos ------------------------------------------------------------
col_azul <- "#2a78d6"; col_rojo <- "#e34948"; col_verde <- "#1baf7a"; col_gris <- "#a8a7a1"
col_texto <- "#0b0b0b"; col_texto2 <- "#52514e"; col_fondo <- "#fcfcfb"
tema <- theme_minimal(base_size = 11) +
  theme(plot.background = element_rect(fill = col_fondo, colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = "#e6e5e0", linewidth = 0.3),
        axis.text = element_text(colour = col_texto2), axis.title = element_text(colour = col_texto2, size = 10),
        plot.title = element_text(colour = col_texto, face = "bold", size = 13),
        plot.subtitle = element_text(colour = col_texto2, size = 10),
        plot.caption = element_text(colour = col_texto2, size = 8, hjust = 0),
        plot.title.position = "plot", legend.position = "top", legend.justification = "left",
        legend.title = element_blank(), legend.text = element_text(colour = col_texto2))
guardar <- function(p, nombre, ancho = 8, alto = 5) {
  ggsave(file.path(dir_pred, nombre), p, width = ancho, height = alto, dpi = 200, bg = col_fondo)
}
fuente <- "Fuente: oferta de vivienda nueva en portal inmobiliario (nov-2021). Cálculos propios."
millones <- label_number(accuracy = 0.1, suffix = " M", scale = 1e-6, big.mark = ".", decimal.mark = ",")

# 12.1 Desempeño en la prueba
g7 <- desempeno |>
  mutate(modelo = fct_rev(modelo), destacar = modelo %in% c("Ensamble MCO + XGBoost", "XGBoost ajustado")) |>
  ggplot(aes(x = r2, y = modelo)) +
  geom_segment(aes(x = 0, xend = r2, yend = modelo), colour = "#d6d5d0", linewidth = 1) +
  geom_point(aes(colour = destacar), size = 3.2, show.legend = FALSE) +
  geom_text(aes(label = sprintf("R² %.2f · error mediano %.0f %%", r2, mdape)),
            nudge_y = 0.32, size = 3.2, colour = col_texto2, hjust = 1, x = 0.98) +
  scale_colour_manual(values = c(`TRUE` = col_azul, `FALSE` = col_gris)) +
  scale_x_continuous(limits = c(0, 1), labels = label_number(accuracy = 0.1, decimal.mark = ",")) +
  labs(title = "Error de predicción en proyectos que los modelos no vieron",
       subtitle = sprintf("Conjunto de prueba: %d tipologías de %d proyectos (20 %%), apartado antes de ajustar",
                          length(te), length(proy_prueba)),
       x = "R² en log(precio/m²)", y = NULL, caption = fuente) +
  tema
guardar(g7, "fig7_desempeno_prueba.png", alto = 4.2)

# 12.2 Variables más relevantes (SHAP medio)
g8 <- importancia |>
  slice_max(shap_medio, n = 15) |>
  mutate(etiqueta = fct_reorder(etiqueta, shap_medio)) |>
  ggplot(aes(x = efecto_tipico_pct, y = etiqueta)) +
  geom_col(fill = col_azul, width = 0.65) +
  geom_text(aes(label = sprintf("%.1f %%", efecto_tipico_pct)), hjust = -0.15, size = 3.1, colour = col_texto2) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = label_percent(scale = 1)) +
  labs(title = "Qué variables mueven más la predicción de XGBoost",
       subtitle = "Efecto típico de cada variable sobre el precio por m² (promedio del valor SHAP absoluto)",
       x = "Efecto típico sobre el precio por m²", y = NULL, caption = fuente) +
  tema
guardar(g8, "fig8_importancia_shap.png", alto = 5.5)

# 12.3 Dependencia: cómo cambia el aporte del área según el tamaño
g9 <- tibble(area = base$area_construida_m2, aporte = 100 * (exp(shap[, "area_construida_m2"]) - 1)) |>
  ggplot(aes(x = area, y = aporte)) +
  geom_hline(yintercept = 0, colour = col_texto2, linewidth = 0.3) +
  geom_point(colour = col_azul, alpha = 0.35, size = 1.6) +
  geom_smooth(method = "loess", formula = y ~ x, se = FALSE, colour = col_texto, linewidth = 0.8, span = 0.5) +
  scale_x_log10(breaks = c(30, 50, 75, 100, 150, 250, 400)) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(title = "Aporte del área construida al precio por m²",
       subtitle = "Valor SHAP de cada tipología; la línea es la tendencia suavizada",
       x = "Área construida (m², escala logarítmica)", y = "Aporte al precio por m²", caption = fuente) +
  tema
guardar(g9, "fig9_dependencia_area.png", alto = 4.8)

# 12.4 Observado vs. predicho en la prueba (ensamble)
g10 <- tibble(obs = exp(y[te]), pred = exp(pred_prueba$`Ensamble MCO + XGBoost`)) |>
  ggplot(aes(x = pred, y = obs)) +
  geom_abline(slope = 1, intercept = 0, colour = col_texto2, linewidth = 0.3, linetype = 2) +
  geom_point(colour = col_azul, alpha = 0.6, size = 1.9) +
  scale_x_continuous(labels = millones) + scale_y_continuous(labels = millones) +
  coord_equal() +
  labs(title = "Precio por m²: observado vs. predicho en la prueba",
       subtitle = "Ensamble MCO + XGBoost; cada punto es una tipología de un proyecto no visto",
       x = "Predicho (pesos por m²)", y = "Observado (pesos por m²)", caption = fuente) +
  tema
guardar(g10, "fig10_observado_predicho_prueba.png", ancho = 6.5, alto = 6.5)

# 12.5 Ejercicio de predicción con intervalo
g11 <- ejercicio |>
  mutate(etiqueta = fct_rev(factor(paste0(caso, ". ", str_wrap(descripcion, 42))))) |>
  ggplot(aes(y = etiqueta)) +
  geom_segment(aes(x = precio_m2_inf, xend = precio_m2_sup, yend = etiqueta), colour = col_gris, linewidth = 1.2) +
  geom_point(aes(x = precio_m2), colour = col_azul, size = 3.4) +
  geom_text(aes(x = precio_m2_sup, label = sprintf("%s/m² · total %s", millones(precio_m2), millones(precio_total))),
            hjust = -0.06, size = 3.1, colour = col_texto) +
  scale_x_continuous(labels = millones, expand = expansion(mult = c(0.03, 0.45))) +
  labs(title = "Ejercicio: precio por m² predicho para cuatro viviendas",
       subtitle = sprintf("Ensamble MCO + XGBoost; la barra gris es el intervalo del %d %% (pesos de 2021)", 100 * INTERVALO),
       x = "Precio por m²", y = NULL, caption = fuente) +
  tema
guardar(g11, "fig11_ejercicio_prediccion.png", alto = 5)

# 12.6 Cascadas SHAP: del precio promedio del modelo al predicho, casos A y D
cascada_caso <- function(caso_w) {
  cascada <- descomposicion |>
    filter(caso == caso_w) |>
    arrange(desc(abs(shap))) |>
    mutate(fin = base_xgb + cumsum(shap), inicio = lag(fin, default = base_xgb))
  pasos <- bind_rows(
    tibble(paso = "Promedio del modelo", ymin = 0, ymax = exp(base_xgb), tipo = "Total",
           etiqueta = millones(exp(base_xgb))),
    cascada |> transmute(paso = familia, ymin = exp(pmin(inicio, fin)), ymax = exp(pmax(inicio, fin)),
                         tipo = if_else(shap >= 0, "Sube el precio", "Baja el precio"),
                         etiqueta = sprintf("%+.0f %%", 100 * (exp(shap) - 1))),
    tibble(paso = "Predicción XGBoost", ymin = 0, ymax = exp(base_xgb + sum(cascada$shap)), tipo = "Total",
           etiqueta = millones(exp(base_xgb + sum(cascada$shap))))
  ) |>
    mutate(x = row_number())
  g <- ggplot(pasos) +
    geom_rect(aes(xmin = x - 0.3, xmax = x + 0.3, ymin = ymin, ymax = ymax, fill = tipo)) +
    geom_text(aes(x = x, y = ymax, label = etiqueta), vjust = -0.5, size = 3.1, colour = col_texto) +
    scale_fill_manual(values = c("Total" = col_gris, "Sube el precio" = col_verde, "Baja el precio" = col_rojo),
                      breaks = c("Sube el precio", "Baja el precio")) +
    scale_x_continuous(breaks = pasos$x, labels = str_wrap(pasos$paso, 14)) +
    scale_y_continuous(labels = millones, expand = expansion(mult = c(0, 0.12))) +
    labs(title = sprintf("Caso %s: de dónde sale el precio predicho", caso_w),
         subtitle = str_wrap(casos$descripcion[casos$caso == caso_w], 90),
         x = NULL, y = "Precio por m²",
         caption = paste(fuente, "Aportes SHAP de XGBoost por familia.")) +
    tema + theme(panel.grid.major.x = element_blank())
  guardar(g, sprintf("fig12_cascada_caso_%s.png", caso_w), alto = 5)
}
walk(c("A", "D"), cascada_caso)


# ---- 13. Resumen -------------------------------------------------------------
message("\n== Hiperparámetros elegidos ==")
message(sprintf("Bosque aleatorio: mtry = %d, min.node.size = %d", hp_rf$mtry, hp_rf$min.node.size))
message(sprintf("XGBoost: learning_rate = %.2f, max_depth = %d, min_child_weight = %d, lambda = %d, árboles = %d",
                hp_xgb$params$learning_rate, hp_xgb$params$max_depth, hp_xgb$params$min_child_weight,
                hp_xgb$params$lambda, hp_xgb$nrounds))
message(sprintf("Peso de XGBoost en el ensamble: %.2f", W_XGB))
message("\n== Ejercicio de predicción ==")
print(ejercicio |> transmute(caso, precio_m2 = round(precio_m2 / 1e6, 2),
                             intervalo = sprintf("%.2f–%.2f", precio_m2_inf / 1e6, precio_m2_sup / 1e6),
                             precio_total_millones = round(precio_total / 1e6)))

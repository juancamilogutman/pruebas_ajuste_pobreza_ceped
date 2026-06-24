# prepara los parquets que consume app/app.R: ingresos pre/post ajuste por
# persona-pasada y los umbrales en $ donde la brecha cruza 1, por trimestre.

# para que no llenen la consola/log de shiny
suppressMessages({
  library(arrow); library(dplyr); library(tidyr); library(readxl); library(Hmisc)
})

cols_necesarias <- c(
  "CODUSU", "NRO_HOGAR", "COMPONENTE", "ANO4", "TRIMESTRE", "AGLOMERADO",
  "CAT_OCUP", "PP07H", "PP04A", "PP04B_COD", "PP07E", "V2_M",
  "P21", "PP08J1", "PP08J2", "PP08J3",
  "PONDERA", "PONDIIO", "PONDII"
)

base <- open_dataset("bases/eph_combinada_2003_2025") |>
  select(all_of(cols_necesarias)) |>
  filter(ANO4 %in% 2003:2025) |>
  # mismas exclusiones de cobertura INDEC (tierra del fuego en un momento de la pandemia, gran resistencia en cierto semestre)
  filter(
    !(ANO4 == 2019 & TRIMESTRE %in% c(3, 4) & AGLOMERADO == 8),
    !(ANO4 == 2020 & TRIMESTRE %in% c(3, 4) & AGLOMERADO == 31)
  ) |>
  collect()

# fallback de pesos de ingreso a PONDERA sólo donde la columna falta entera (NA,
# intervención ~2007-2015). NO tocamos los ceros: PONDIIO==0 / PONDII==0 son la
# marca de INDEC para excluir de las estadísticas de ingreso (no respuesta).
base <- base |> mutate(
  PONDIIO = if_else(is.na(PONDIIO), as.numeric(PONDERA), PONDIIO),
  PONDII  = if_else(is.na(PONDII),  as.numeric(PONDERA), PONDII)
)

canastas_q <- read_parquet("bases/canastas_combinadas.parquet") |>
  distinct(periodo) |>
  transmute(ANO4 = as.integer(substr(periodo, 1, 4)),
            TRIMESTRE = as.integer(substr(periodo, 6, 6)))

base <- base |> semi_join(canastas_q, by = c("ANO4", "TRIMESTRE"))

# armamos el grupo laboral de 10 niveles que mostramos en la app
base <- base |>
  mutate(across(c(P21, PP08J1, PP08J2, PP08J3, V2_M),
                ~ if_else(.x < 0, NA_real_, as.numeric(.x))),
         ingreso_laboral_total = rowSums(
           across(c(P21, PP08J1, PP08J2, PP08J3)), na.rm = TRUE),
         es_serv_dom    = !is.na(PP04B_COD) & PP04B_COD == "9700",
         es_sd_amplio   = es_serv_dom |
                          (CAT_OCUP == 3 & !is.na(PP04A) & PP04A == 3),
         es_plan_empleo = !is.na(PP07E) & PP07E == 1,
         grupo_lab = case_when(
           es_plan_empleo                                                    ~ NA_character_,
           CAT_OCUP == 3 & es_sd_amplio & PP07H == 1                         ~ "Serv. dom. registrado",
           CAT_OCUP == 3 & es_sd_amplio & PP07H == 2                         ~ "Serv. dom. no registrado",
           CAT_OCUP == 3 & !es_sd_amplio & PP04A == 2 & PP07H == 1           ~ "Asal. priv. registrado",
           CAT_OCUP == 3 & !es_sd_amplio & PP04A == 2 & PP07H == 2           ~ "Asal. priv. no registrado",
           CAT_OCUP == 3 & !es_sd_amplio & PP04A == 1 & PP07H == 1           ~ "Asal. pub. registrado",
           CAT_OCUP == 3 & !es_sd_amplio & PP04A == 1 & PP07H == 2           ~ "Asal. pub. no registrado",
           CAT_OCUP == 2                                                     ~ "Cuentapropistas",
           CAT_OCUP == 1                                                     ~ "Patrones",
           CAT_OCUP == 3                                                     ~ "Otros",  # asalariado con PP04A/PP07H NA, pero no me convence, tal vez los TRUE también debería poner acá...
           TRUE                                                              ~ NA_character_
         ))

# data viene en formato largo: una fila por persona × pasada de ajuste (laboral o jubilatoria),
# con ingreso pre y post ajuste
pasadas <- read_parquet("bases/pasadas_ajuste.parquet") |>
  select(CODUSU, NRO_HOGAR, COMPONENTE, ANO4, TRIMESTRE, pasada, delta_ing)

# hogares que salieron de la pobreza,
# la solapa "Tasa de pobreza" descompone tanto el delta ITF total como el exclusivo a estos hogares.
hogares_lp <- read_parquet("bases/hogares_que_mejoraron.parquet") |>
  filter(situacion_ajustada == "no_pobre") |>
  distinct(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE) |>
  mutate(hogar_left_poverty = TRUE)

keys <- c("CODUSU", "NRO_HOGAR", "COMPONENTE", "ANO4", "TRIMESTRE")
keys_hog <- c("CODUSU", "NRO_HOGAR", "ANO4", "TRIMESTRE")

laboral <- base |>
  filter(!is.na(grupo_lab),
         !is.na(ingreso_laboral_total), ingreso_laboral_total > 0) |>
  left_join(pasadas |> filter(pasada == "laboral") |> select(-pasada),
            by = keys) |>
  left_join(hogares_lp, by = keys_hog) |>
  mutate(hogar_left_poverty = coalesce(hogar_left_poverty, FALSE)) |>
  transmute(ANO4, TRIMESTRE, grupo = grupo_lab,
            ingreso     = ingreso_laboral_total,
            ingreso_post = ingreso_laboral_total + coalesce(delta_ing, 0),
            PONDERA, PONDIIO, PONDII,
            peso_ingreso = PONDIIO,
            hogar_left_poverty)

jubilatorio <- base |>
  filter(!is.na(V2_M), V2_M > 0) |>
  left_join(pasadas |> filter(pasada == "jubilatoria") |> select(-pasada),
            by = keys) |>
  left_join(hogares_lp, by = keys_hog) |>
  mutate(hogar_left_poverty = coalesce(hogar_left_poverty, FALSE)) |>
  transmute(ANO4, TRIMESTRE, grupo = "Jubilados",
            ingreso     = V2_M,
            ingreso_post = V2_M + coalesce(delta_ing, 0),
            PONDERA, PONDIIO, PONDII,
            peso_ingreso = PONDII,
            hogar_left_poverty)

datos_app <- bind_rows(laboral, jubilatorio)

cat("Filas dataset app:", nrow(datos_app), "\n")
cat("\nDistribución por grupo:\n")
print(datos_app |> count(grupo, sort = TRUE))

write_parquet(datos_app, "bases/datos_app_densidades.parquet")
cat("\nGuardado bases/datos_app_densidades.parquet\n")

# umbral en $ donde la brecha cruza 1, por trimestre.
# para sombrear la zona donde se aplica el ajuste.
brecha_long <- read_excel("bases/Estimación brecha ANR_20260416.xlsx", #cortesía de Nacho
                          sheet = "Brecha_AR_nom_percentil") |>
  rename(percentil = 1) |>
  # filas con percentil NA = se metían calculos auxiliares debajo de la tabla propiamente dicha
  filter(!is.na(percentil)) |>
  # 2019_mler_c venía como character y generaba problemas
  # todas las columnas numéricas pivot_longer
  mutate(across(matches("_mler_[pc]$"), as.numeric)) |>
  pivot_longer(-percentil, names_to = "col", values_to = "brecha") |>
  mutate(ANO4 = as.integer(substr(col, 1, 4)),
         tipo_eph = substr(col, 11, 11)) |> # el caracter 11 es p o c segun puntual o continua
  filter(tipo_eph == "c", ANO4 %in% 2003:2025)

pct_min <- brecha_long |>
  filter(brecha > 1) |>
  group_by(ANO4) |>
  summarise(pct_min = min(percentil), .groups = "drop")

# para cada trimestre, el umbral en $ es el percentil mínimo con brecha mayor o igual a 1
# (sino no ajustamos por hipótesis de salario en gris)
# (tengo que revisar si la brecha se hace 1 y vuelve a ser menor a uno al menos algunas veces, como me dijo wati,
# pero igual solo ajusta si la brecha es mayor a 1)
# evaluado sobre asal. registrados privados sin serv dom (pero pienso que nacho quiza metía a los públicos para calcular la brecha)
ref <- base |>
  filter(CAT_OCUP == 3, PP07H == 1, PP04A == 2, !es_sd_amplio,
         !is.na(ingreso_laboral_total), ingreso_laboral_total > 0,
         !is.na(PONDIIO))

umbrales <- ref |>
  group_by(ANO4, TRIMESTRE) |>
  group_modify(~ {
    p <- pct_min$pct_min[pct_min$ANO4 == .y$ANO4]
    if (length(p) == 0 || is.na(p)) return(tibble(umbral = NA_real_))
    tibble(umbral = as.numeric(
      wtd.quantile(.x$ingreso_laboral_total, weights = .x$PONDIIO,
                   probs = p / 100, normwt = TRUE)))
  }) |>
  ungroup() |>
  left_join(pct_min, by = "ANO4") |>
  mutate(periodo_lbl = sprintf("%d-T%d", as.integer(ANO4), TRIMESTRE)) |>
  select(ANO4, TRIMESTRE, periodo_lbl, pct_min, umbral)

write_parquet(umbrales, "bases/umbrales_brecha_por_trimestre.parquet")

# base para la pestaña "Pobreza: ITF / CBT" del shiny: una fila por persona con
# el ingreso total familiar por adulto equivalente expresado en múltiplos de la
# Canasta Básica Total del hogar (la línea de pobreza), antes y después del
# ajuste. Normalizamos por CBT_hogar porque la CBT por adulto equivalente es
# regional (6 valores por trimestre), así una sola línea vertical en 1 separa
# pobres de no pobres en todas las regiones. Ponderado por PONDIH (persona).
ecdf_pobreza <- read_parquet("bases/eph_ajustada_v2.parquet") |>
  filter(!is.na(ITF), !is.na(ITF_ajustado), CBT_hogar > 0, PONDIH > 0) |>
  transmute(
    ANO4        = as.integer(ANO4),
    TRIMESTRE   = as.integer(TRIMESTRE),
    periodo_lbl = sprintf("%d-T%d", as.integer(ANO4), TRIMESTRE),
    ratio_antes   = ITF / CBT_hogar,
    ratio_despues = ITF_ajustado / CBT_hogar,
    PONDIH
  )

write_parquet(ecdf_pobreza, "bases/datos_app_ecdf_pobreza.parquet")
cat("Guardado bases/datos_app_ecdf_pobreza.parquet (", nrow(ecdf_pobreza), "filas)\n")

app_data <- "app/data"

if (dir.exists(app_data)) {
  archivos_app <- c("datos_app_densidades.parquet",
                    "umbrales_brecha_por_trimestre.parquet",
                    "hogares_que_mejoraron.parquet",
                    "pobreza_serie.parquet",
                    "datos_app_ecdf_pobreza.parquet",
                    "sensibilidad_lineas_serie.parquet")  # la genera el 08
  for (f in archivos_app) {
    src <- file.path("bases", f)
    dst <- file.path(app_data, f)
    if (file.exists(src)) {
      file.copy(src, dst, overwrite = TRUE)
    }
  }
}
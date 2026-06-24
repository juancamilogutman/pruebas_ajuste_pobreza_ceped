# aplica la brecha MLER/EPH al ingreso laboral total (P21 + PP08J*) y a las
# jubilaciones (V2_M); recalcula pobreza y guarda los parquets

library(arrow)
library(dplyr)
library(tidyr)
library(readxl)
library(Hmisc)
library(eph)


cols_necesarias <- c(
  "CODUSU", "NRO_HOGAR", "COMPONENTE", "ANO4", "TRIMESTRE", "REGION",
  "AGLOMERADO",
  "CH04", "CH06",
  "CAT_OCUP", "PP07H", "PP07E", "PP04A", "PP04B_COD",
  "P21", "PP08J1", "PP08J2", "PP08J3", "V2_M",
  "P47T", "ITF", "IPCF", "IX_TOT",
  "PONDERA", "PONDIIO", "PONDIH"
)

# abrimos todos los parquets del directorio
base <- open_dataset("bases/eph_combinada_2003_2025") |>
  select(all_of(cols_necesarias)) |>
  filter(ANO4 %in% 2003:2025) |>
  collect()

# replicamos las exclusiones que hace INDEC, es decir,
# del Gran Resistencia en S2-2019 y de Ushuaia-Río Grande en S2-2020
filas_pre <- nrow(base)
base <- base |> filter(
  !(ANO4 == 2019 & TRIMESTRE %in% c(3, 4) & AGLOMERADO == 8),
  !(ANO4 == 2020 & TRIMESTRE %in% c(3, 4) & AGLOMERADO == 31)
)

# fallback de pesos: en los años de la intervención (~2007-2015) la EPH continua
# no trae PONDIH/PONDIIO (la columna viene NA entera), sólo PONDERA. Sólo donde
# falta (NA) usamos PONDERA. OJO: NO tocamos los ceros: PONDIH==0 es la marca de
# INDEC para "excluir de pobreza por ingresos" (no respuesta de ingresos) y
# calculate_poverty la usa para poner situacion=NA. Si reemplazáramos esos ceros
# por PONDERA volveríamos a contar a ~25% de la gente (más pobre en promedio) y
# la tasa salta ~15pp por encima de INDEC. (decisión: serie continua con PONDERA)
base <- base |> mutate(
  PONDIH  = if_else(is.na(PONDIH),  as.numeric(PONDERA), PONDIH),
  PONDIIO = if_else(is.na(PONDIIO), as.numeric(PONDERA), PONDIIO)
)

# canasta combinada: CEPED (Canastas.xlsx) 2003-2016 + INDEC/eph 2017-2025
canastas <- read_parquet("bases/canastas_combinadas.parquet")

canastas_q <- canastas |>
  distinct(periodo) |>
  transmute(ANO4 = as.integer(substr(periodo, 1, 4)),
            TRIMESTRE = as.integer(substr(periodo, 6, 6)))

filas_pre_canasta <- nrow(base)
base <- base |> semi_join(canastas_q, by = c("ANO4", "TRIMESTRE")) #porque pueden llegar a haber trimestres sin canasta regional (pero la agregué manualmente)
message(sprintf("Filas descartadas por falta de canasta: %d",
                filas_pre_canasta - nrow(base)))

# pobreza original
base_pov <- calculate_poverty(base, basket = canastas, window = "quarter",
                              print_summary = FALSE)

base <- base_pov |>
  select(all_of(cols_necesarias), situacion, adequi_hogar, CBA_hogar, CBT_hogar)
rm(base_pov); gc(verbose = FALSE)

# armamos ingreso laboral, semestre y marcamos a quienes entran al ajuste
base <- base |>
  mutate(across(c(P21, PP08J1, PP08J2, PP08J3, V2_M),
                ~ if_else(.x < 0, NA_real_, as.numeric(.x))),
         ingreso_laboral_total = rowSums(across(c(P21, PP08J1, PP08J2, PP08J3)),
                                         na.rm = TRUE)) |>
  mutate(SEMESTRE = if_else(TRIMESTRE %in% c(1L, 2L), 1L, 2L),
         es_plan_empleo = !is.na(PP07E) & PP07E == 1,
         es_serv_dom    = !is.na(PP04B_COD) & PP04B_COD == "9700",
         es_patron_afect         = CAT_OCUP == 1 & !es_plan_empleo,
         es_asalariado_afect     = CAT_OCUP == 3 & !es_plan_empleo,
         es_cuentapropista_afect = CAT_OCUP == 2 & !es_plan_empleo,
         es_jubilado_afect       = !is.na(V2_M) & V2_M > 0,
         afectado = es_patron_afect | es_asalariado_afect |
                    es_cuentapropista_afect | es_jubilado_afect)

hog_flag <- base |>
  group_by(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE) |>
  summarise(hogar_con_afectado = any(afectado, na.rm = TRUE),
            .groups = "drop")
base <- base |>
  left_join(hog_flag, by = c("CODUSU", "NRO_HOGAR", "ANO4", "TRIMESTRE"))

# hábitos que hay que incorporar...
rm(hog_flag); gc(verbose = FALSE)

tasa <- function(df, col = "situacion") {
  df |> summarise(
    n = n(),
    poblacion = sum(PONDIH, na.rm = TRUE),
    tasa = sum(PONDIH[.data[[col]] %in% c("pobre", "indigente")],
               na.rm = TRUE) / sum(PONDIH, na.rm = TRUE),
    .groups = "drop"
  )
}

# esto tengo que revisarlo bastante, debe estar flojo.
# brecha de pobreza ponderada por PONDIH, usando CBT_hogar como línea
brecha_pob <- function(df, itf_col) {
  df |> summarise(
    fgt1 = sum(
      PONDIH * pmax(0, (CBT_hogar - .data[[itf_col]]) / CBT_hogar),
      na.rm = TRUE
    ) / sum(PONDIH, na.rm = TRUE),
    .groups = "drop"
  )
}

afect_base <- base |> filter(afectado) |>
  group_by(ANO4, SEMESTRE) |> tasa() |>
  rename(n_afectados = n, pobreza_afectados_base = tasa) |>
  select(ANO4, SEMESTRE, n_afectados, pobreza_afectados_base)

hog_base <- base |> filter(hogar_con_afectado) |>
  group_by(ANO4, SEMESTRE) |> tasa() |>
  rename(n_hog_con_afectado = n, pobreza_hog_base = tasa) |>
  select(ANO4, SEMESTRE, n_hog_con_afectado, pobreza_hog_base)

tot_base <- base |>
  group_by(ANO4, SEMESTRE) |> tasa() |>
  rename(n_total = n, pobreza_total_base = tasa) |>
  select(ANO4, SEMESTRE, n_total, pobreza_total_base)

# brecha del MLER/EPH en formato largo. nos quedamos con el las continuas x las dudas
# no se baja ningún ingreso
brecha_long <- read_excel("bases/Estimación brecha ANR_20260416.xlsx", #cortesía de nacho
                          sheet = "Brecha_AR_nom_percentil") |>
  rename(percentil = 1) |>
  filter(!is.na(percentil)) |>
  # por problemas de tipo por encabezados que eran character hacemos esto:
  mutate(across(matches("_mler_[pc]$"), as.numeric)) |>
  pivot_longer(-percentil, names_to = "col", values_to = "brecha") |>
  mutate(ANO4 = as.integer(substr(col, 1, 4)),
         metodo = substr(col, 11, 11)) |>
  filter(metodo == "c", ANO4 %in% 2003:2025) |>
  transmute(ANO4, percentil,
            brecha_aplicada = if_else(brecha > 1, brecha, 1))

# los cortes se calculan con  los percentiles de asalariados registrados privados
# sin serv. dom. (pero no se si nacho aca metia tanto privados como publicos),
# y después se aplican a todos los que entran al ajuste
cortes_q <- base |>
  filter(CAT_OCUP == 3, PP07H == 1, PP04A == 2,
         !es_serv_dom,
         !is.na(ingreso_laboral_total), ingreso_laboral_total > 0,
         !is.na(PONDIIO)) |>
  group_by(ANO4, TRIMESTRE) |>
  summarise(
    cortes = list(Hmisc::wtd.quantile(ingreso_laboral_total, weights = PONDIIO,
                                      probs = seq(0.01, 0.99, 0.01),
                                      normwt = TRUE)),
    .groups = "drop"
  )

asignar_pct <- function(df, key) {
  cv <- cortes_q$cortes[[which(cortes_q$ANO4 == key$ANO4 &
                               cortes_q$TRIMESTRE == key$TRIMESTRE)]]
  df$percentil <- pmax(1L, pmin(99L, findInterval(df$ingreso, cv) + 1L))
  df
}

# dos pasadas: laboral (patrón + asalariado + cuentapropista) y jubilatoria
pasada_laboral <- base |>
  filter((es_patron_afect | es_asalariado_afect | es_cuentapropista_afect) &
         !is.na(ingreso_laboral_total) & ingreso_laboral_total > 0) |>
  select(CODUSU, NRO_HOGAR, COMPONENTE, ANO4, TRIMESTRE,
         ingreso = ingreso_laboral_total) |>
  group_by(ANO4, TRIMESTRE) |>
  group_modify(asignar_pct) |>
  ungroup() |>
  left_join(brecha_long, by = c("ANO4", "percentil")) |>
  mutate(brecha_aplicada = coalesce(brecha_aplicada, 1),
         delta_ing = ingreso * (brecha_aplicada - 1))

pasada_jub <- base |>
  filter(es_jubilado_afect) |>
  select(CODUSU, NRO_HOGAR, COMPONENTE, ANO4, TRIMESTRE, ingreso = V2_M) |>
  group_by(ANO4, TRIMESTRE) |>
  group_modify(asignar_pct) |>
  ungroup() |>
  left_join(brecha_long, by = c("ANO4", "percentil")) |>
  mutate(brecha_aplicada = coalesce(brecha_aplicada, 1),
         delta_ing = ingreso * (brecha_aplicada - 1))

# guardamos las pasadas a nivel persona para el shiny
write_parquet(
  bind_rows(
    pasada_laboral |> mutate(pasada = "laboral"),
    pasada_jub     |> mutate(pasada = "jubilatoria")
  ),
  "bases/pasadas_ajuste.parquet"
)

# delta_ITF por hogar = suma de las dos pasadas, después ITF_ajustado = ITF + delta
delta_hogar <- bind_rows(
    pasada_laboral |> select(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE, delta_ing),
    pasada_jub     |> select(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE, delta_ing)
  ) |>
  group_by(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE) |>
  summarise(delta_ITF = sum(delta_ing, na.rm = TRUE), .groups = "drop")

base <- base |>
  left_join(delta_hogar, by = c("CODUSU", "NRO_HOGAR", "ANO4", "TRIMESTRE")) |>
  mutate(delta_ITF = coalesce(delta_ITF, 0),
         ITF_ajustado = ITF + delta_ITF)

rm(pasada_laboral, pasada_jub, delta_hogar); gc(verbose = FALSE)

cat("\n=== Sanity checks ===\n")
sin <- base |> filter(!hogar_con_afectado)
cat("Hogares sin afectados (filas-persona):", nrow(sin), "\n")
cat("  ...con delta_ITF == 0 exacto:", sum(sin$delta_ITF == 0), "\n")
cat("Filas con ITF_ajustado < ITF:",
    sum(base$ITF_ajustado < base$ITF, na.rm = TRUE), "\n")
rm(sin); gc(verbose = FALSE)

# situación ajustada
base <- base |>
  mutate(situacion_aj = case_when(
    PONDIH == 0 ~ NA_character_,
    ITF_ajustado < CBA_hogar ~ "indigente",
    ITF_ajustado < CBT_hogar ~ "pobre",
    TRUE ~ "no_pobre"
  ))

afect_aj <- base |> filter(afectado) |>
  group_by(ANO4, SEMESTRE) |> tasa("situacion_aj") |>
  rename(pobreza_afectados_aj = tasa) |> select(ANO4, SEMESTRE, pobreza_afectados_aj)

hog_aj <- base |> filter(hogar_con_afectado) |>
  group_by(ANO4, SEMESTRE) |> tasa("situacion_aj") |>
  rename(pobreza_hog_aj = tasa) |> select(ANO4, SEMESTRE, pobreza_hog_aj)

tot_aj <- base |>
  group_by(ANO4, SEMESTRE) |> tasa("situacion_aj") |>
  rename(pobreza_total_aj = tasa) |> select(ANO4, SEMESTRE, pobreza_total_aj)

brecha_tot_base <- base |>
  group_by(ANO4, SEMESTRE) |> brecha_pob("ITF") |>
  rename(brecha_pob_total_base = fgt1)

brecha_tot_aj <- base |>
  group_by(ANO4, SEMESTRE) |> brecha_pob("ITF_ajustado") |>
  rename(brecha_pob_total_aj = fgt1)

resultados <- afect_base |>
  left_join(afect_aj, by = c("ANO4", "SEMESTRE")) |>
  left_join(hog_base, by = c("ANO4", "SEMESTRE")) |>
  left_join(hog_aj, by = c("ANO4", "SEMESTRE")) |>
  left_join(tot_base, by = c("ANO4", "SEMESTRE")) |>
  left_join(tot_aj, by = c("ANO4", "SEMESTRE")) |>
  left_join(brecha_tot_base, by = c("ANO4", "SEMESTRE")) |>
  left_join(brecha_tot_aj, by = c("ANO4", "SEMESTRE")) |>
  mutate(delta_pp_afect = (pobreza_afectados_aj - pobreza_afectados_base) * 100,
         delta_pp_hog   = (pobreza_hog_aj - pobreza_hog_base) * 100,
         delta_pp_total = (pobreza_total_aj - pobreza_total_base) * 100,
         delta_pp_brecha_total = (brecha_pob_total_aj - brecha_pob_total_base) * 100)

write_parquet(resultados, "bases/resultados_pobreza_v2.parquet")
cat("\nGuardado en bases/resultados_pobreza_v2.parquet\n")

# base liviana a nivel persona para los gráficos del 05 y la app
base_liviana <- base |>
  select(CODUSU, NRO_HOGAR, COMPONENTE, ANO4, TRIMESTRE, SEMESTRE, REGION,
         CH04, CH06, IPCF, PONDIH, ITF, ITF_ajustado,
         CBA_hogar, CBT_hogar, adequi_hogar, IX_TOT,
         situacion, situacion_aj,
         afectado, hogar_con_afectado,
         es_patron_afect, es_asalariado_afect,
         es_cuentapropista_afect, es_jubilado_afect)

write_parquet(base_liviana, "bases/eph_ajustada_v2.parquet")
cat("Guardado en bases/eph_ajustada_v2.parquet\n")

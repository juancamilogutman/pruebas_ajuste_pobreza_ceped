# gráficos del efecto del ajuste sobre la pobreza. todo semestral

library(arrow)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(scales)
library(Hmisc)
library(ggrepel)

dir.create("outputs")

resultados <- read_parquet("bases/resultados_pobreza_v2.parquet")
base_aj    <- read_parquet("bases/eph_ajustada_v2.parquet")

# tasas oficiales INDEC (Cuadro 1, fila 4: Pobreza > Personas)
indec_raw <- read_excel("bases/cuadros_informe_pobreza_03_26.xls",
                        sheet = "Cuadro 1", skip = 2)

fila_personas <- indec_raw |> slice(4)
cols_periodos <- setdiff(names(indec_raw), "Indicador")

indec_long <- fila_personas |>
  select(all_of(cols_periodos)) |>
  pivot_longer(everything(), names_to = "etiqueta", values_to = "tasa_pct") |>
  mutate(
    SEMESTRE = as.integer(sub("^(\\d)(?:do|er)\\.?\\s+semestre.*", "\\1", etiqueta)),
    ANO4     = as.integer(sub(".*?(\\d{4}).*", "\\1", etiqueta)),
    tasa_indec = tasa_pct / 100
  ) |>
  filter(!is.na(tasa_indec), !is.na(ANO4), !is.na(SEMESTRE)) |>
  select(ANO4, SEMESTRE, tasa_indec)

# no voy a mentir esto no lo entiendo bien
sem_periodo <- function(ano, sem) ano + (sem - 1) * 0.5
sem_label_fn <- function(x) {
  ano <- floor(x); sem <- round((x - ano) * 2) + 1
  sprintf("S%d %d", sem, ano)
}

# gráfico 1: total ARG semestral, original vs ajustada vs INDEC
tot_s <- resultados |>
  transmute(ANO4, SEMESTRE,
            pobreza_original = pobreza_total_base,
            pobreza_ajustada = pobreza_total_aj) |>
  left_join(indec_long, by = c("ANO4", "SEMESTRE")) |>
  mutate(periodo  = sem_periodo(ANO4, SEMESTRE),
         delta_pp = (pobreza_original - pobreza_ajustada) * 100)

# parquet para que la app shiny replique este gráfico en la webapp sin necesitar el excel de INDEC
write_parquet(tot_s, "bases/pobreza_serie.parquet")

max_tasa     <- max(c(tot_s$pobreza_original, tot_s$tasa_indec), na.rm = TRUE)
max_delta    <- max(tot_s$delta_pp)
scale_factor <- (max_tasa * 0.18) / max_delta

df_long <- tot_s |>
  select(periodo, pobreza_original, pobreza_ajustada, tasa_indec) |>
  pivot_longer(c(pobreza_original, pobreza_ajustada, tasa_indec),
               names_to = "tipo", values_to = "tasa") |>
  mutate(tipo = recode(tipo,
                       pobreza_original = "Original",
                       pobreza_ajustada = "Ajustada",
                       tasa_indec       = "INDEC oficial"),
         tipo = factor(tipo, levels = c("Original", "Ajustada", "INDEC oficial"))) |>
  filter(!is.na(tasa))

color_barras <- "#41b6c4"
col_orig <- "#1f78b4"; col_aj <- "#e31a1c"; col_indec <- "#33a02c"

g1 <- ggplot() +
  geom_col(data = tot_s,
           aes(periodo, delta_pp * scale_factor),
           fill = color_barras, color = NA, width = 0.10, alpha = 0.85) +
  geom_text_repel(
    data = tot_s,
    aes(periodo, delta_pp * scale_factor,
        label = sprintf("%.2f", delta_pp)),
    color = color_barras, size = 3, fontface = "bold",
    nudge_y = 0.008, direction = "y", min.segment.length = Inf, seed = 3
  ) +
  geom_line(data = df_long,
            aes(periodo, tasa, color = tipo, linetype = tipo),
            linewidth = 0.7) +
  geom_point(data = df_long, aes(periodo, tasa, color = tipo), size = 1.9) +
  geom_text_repel(
    data = df_long |> filter(tipo == "Original"),
    aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
    color = col_orig, size = 3,
    nudge_y = 0.020, direction = "y", min.segment.length = 0,
    segment.size = 0.2, segment.alpha = 0.4, seed = 1
  ) +
  geom_text_repel(
    data = df_long |> filter(tipo == "Ajustada"),
    aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
    color = col_aj, size = 3,
    nudge_y = -0.020, direction = "y", min.segment.length = 0,
    segment.size = 0.2, segment.alpha = 0.4, seed = 2
  ) +
  geom_text_repel(
    data = df_long |> filter(tipo == "INDEC oficial"),
    aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
    color = col_indec, size = 2.6, fontface = "italic",
    nudge_y = 0.038, direction = "y", min.segment.length = 0,
    segment.size = 0.2, segment.alpha = 0.4, seed = 4
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    sec.axis = sec_axis(~ . / scale_factor,
                        name = "Δ pp (Original − Ajustada)",
                        labels = label_number(accuracy = 0.1))
  ) +
  scale_x_continuous(
    breaks = sort(unique(tot_s$periodo)),
    labels = sem_label_fn
  ) +
  scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj,
                                "INDEC oficial" = col_indec)) +
  scale_linetype_manual(values = c(Original = "solid", Ajustada = "dashed",
                                   "INDEC oficial" = "dotdash")) +
  labs(title = "Tasa de pobreza semestral (Total ARG): original vs ajustada por subdeclaración",
       subtitle = "Ajuste a asalariados + cuentapropistas + jubilados (sin servicio doméstico ni planes de empleo). Línea verde = INDEC oficial. Barras = Δ pp (eje derecho).",
       x = NULL, y = "Tasa de pobreza",
       color = NULL, linetype = NULL,
       caption = "Fuentes: EPH continua + brecha MLER/EPH por percentil + Cuadro 1 INDEC. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/pobreza_lineas_por_anio.png", g1,
       width = 14, height = 6.5, dpi = 150)
cat("Guardado outputs/pobreza_lineas_por_anio.png\n")

# gráfico 2: diferencia en pp por subgrupo y semestre, barras agrupadas
df_delta <- resultados |>
  mutate(periodo_lbl = sprintf("S%d %d", SEMESTRE, ANO4),
         periodo_num = sem_periodo(ANO4, SEMESTRE)) |>
  select(periodo_lbl, periodo_num,
         "Total ARG"             = delta_pp_total,
         "Hogares con afectado"  = delta_pp_hog,
         "Asalariados afectados" = delta_pp_afect) |>
  pivot_longer(c(-periodo_lbl, -periodo_num),
               names_to = "subgrupo", values_to = "delta") |>
  mutate(subgrupo = factor(subgrupo, levels = c("Asalariados afectados",
                                                "Hogares con afectado",
                                                "Total ARG")),
         periodo_lbl = factor(periodo_lbl,
           levels = unique(periodo_lbl[order(periodo_num)])))

g2 <- ggplot(df_delta, aes(periodo_lbl, delta, fill = subgrupo)) +
  geom_col(position = position_dodge(width = 0.85), width = 0.78) +
  geom_text(aes(label = sprintf("%.2f", delta)),
            position = position_dodge(width = 0.85),
            vjust = 1.25, size = 2.7, color = "grey20") +
  geom_hline(yintercept = 0, color = "grey30") +
  scale_fill_brewer(palette = "Set2") +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(title = "Cambio en pobreza por ajuste de subdeclaración (semestral)",
       subtitle = "Δ pp = tasa ajustada − tasa original (negativo = baja pobreza)",
       x = NULL, y = "Δ puntos porcentuales",
       fill = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1))

ggsave("outputs/delta_pp_subgrupo_anio.png", g2,
       width = 13, height = 5.5, dpi = 150)

# este tengo que revisarlo bien y repasar la metodología de brecha de pobreza
# gráfico 3: brecha de pobreza FGT(1) total ARG, original vs ajustada
brecha_s <- resultados |>
  transmute(ANO4, SEMESTRE,
            brecha_original = brecha_pob_total_base,
            brecha_ajustada = brecha_pob_total_aj) |>
  mutate(periodo  = sem_periodo(ANO4, SEMESTRE),
         delta_pp_brecha = (brecha_original - brecha_ajustada) * 100)

max_br        <- max(brecha_s$brecha_original)
max_db        <- max(brecha_s$delta_pp_brecha)
factor_escala_g3 <- (max_br * 0.18) / max_db

brecha_long_g3 <- brecha_s |>
  select(periodo, brecha_original, brecha_ajustada) |>
  pivot_longer(c(brecha_original, brecha_ajustada),
               names_to = "tipo", values_to = "fgt1") |>
  mutate(tipo = recode(tipo,
                       brecha_original = "Original",
                       brecha_ajustada = "Ajustada"),
         tipo = factor(tipo, levels = c("Original", "Ajustada")))

g3 <- ggplot() +
  geom_col(data = brecha_s,
           aes(periodo, delta_pp_brecha * factor_escala_g3),
           fill = color_barras, color = NA, width = 0.10, alpha = 0.85) +
  geom_text_repel(
    data = brecha_s,
    aes(periodo, delta_pp_brecha * factor_escala_g3,
        label = sprintf("%.2f", delta_pp_brecha)),
    color = color_barras, size = 3, fontface = "bold",
    nudge_y = 0.004, direction = "y", min.segment.length = Inf, seed = 5
  ) +
  geom_line(data = brecha_long_g3,
            aes(periodo, fgt1, color = tipo, linetype = tipo),
            linewidth = 0.7) +
  geom_point(data = brecha_long_g3, aes(periodo, fgt1, color = tipo),
             size = 1.9) +
  geom_text_repel(
    data = brecha_long_g3 |> filter(tipo == "Original"),
    aes(periodo, fgt1, label = label_percent(accuracy = 0.1)(fgt1)),
    color = col_orig, size = 3,
    nudge_y = 0.010, direction = "y", min.segment.length = 0,
    segment.size = 0.2, segment.alpha = 0.4, seed = 6
  ) +
  geom_text_repel(
    data = brecha_long_g3 |> filter(tipo == "Ajustada"),
    aes(periodo, fgt1, label = label_percent(accuracy = 0.1)(fgt1)),
    color = col_aj, size = 3,
    nudge_y = -0.010, direction = "y", min.segment.length = 0,
    segment.size = 0.2, segment.alpha = 0.4, seed = 7
  ) +
  scale_y_continuous(
    labels = label_percent(accuracy = 1),
    sec.axis = sec_axis(~ . / factor_escala_g3,
                        name = "Δ pp (Original − Ajustada)",
                        labels = label_number(accuracy = 0.1))
  ) +
  scale_x_continuous(
    breaks = sort(unique(brecha_s$periodo)),
    labels = sem_label_fn
  ) +
  scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
  scale_linetype_manual(values = c(Original = "solid", Ajustada = "dashed")) +
  labs(title = "Brecha de pobreza (FGT₁) semestral: original vs ajustada por subdeclaración",
       subtitle = "Intensidad de pobreza = (CBT_hogar − ITF) / CBT_hogar promedio sobre toda la población (no sólo pobres). Ajuste a patrones + asalariados + cuentapropistas + jubilados (con serv. doméstico).",
       x = NULL, y = "Brecha de pobreza (FGT₁)",
       color = NULL, linetype = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/brecha_pobreza_lineas.png", g3,
       width = 14, height = 6.5, dpi = 150)

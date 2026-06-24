# efecto del ajuste por subdeclaración sobre la DISTRIBUCIÓN del ingreso.
# todo a nivel persona y semestral, original vs ajustado:
#   - Gini del ingreso per cápita familiar (IPCF)
#   - cambio en la media y la mediana del IPCF
#   - cambio en el ingreso medio de cada categoría ocupacional
#   - brecha interdecílica (decil 10 / decil 1) del IPCF
#   - curva de Lorenz y participación por decil (último semestre)
#
# nota de concepto: para la distribución personal del ingreso uso el ingreso per
# cápita familiar (IPCF) ponderado por PONDIH, que es lo que mira INDEC en
# "Evolución de la distribución del ingreso". Lo calculo como ITF / IX_TOT en
# lugar de usar la columna IPCF publicada: coinciden en casi todas las filas
# (mediana de la diferencia = 0) pero la IPCF publicada viene escalada ×100 en
# 2020-T3 (ITF está bien, es lo que usa todo el pipeline de pobreza). Así el
# "antes" es ITF/IX_TOT y el "después" ITF_ajustado/IX_TOT, y lo único que
# cambia entre escenarios es la corrección por subdeclaración.

library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(Hmisc)
library(ggrepel)

dir.create("outputs", showWarnings = FALSE)

# ---- helpers ---------------------------------------------------------------

# misma convención semestral que el 03 para que los ejes queden igual
sem_periodo  <- function(ano, sem) ano + (sem - 1) * 0.5
sem_label_fn <- function(x) {
  ano <- floor(x); sem <- round((x - ano) * 2) + 1
  sprintf("S%d %d", sem, ano)
}

# Gini ponderado por el área bajo la curva de Lorenz (regla del trapecio).
# asume x >= 0 (vale para IPCF). devuelve NA si no hay datos útiles.
gini_w <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  if (length(x) < 2 || sum(x) == 0) return(NA_real_)
  o <- order(x); x <- x[o]; w <- w[o]
  F <- c(0, cumsum(w) / sum(w))             # share de población acumulada
  L <- c(0, cumsum(w * x) / sum(w * x))     # share de ingreso acumulada (Lorenz)
  1 - sum((F[-1] - F[-length(F)]) * (L[-1] + L[-length(L)]))
}

# asigna decil ponderado (1..10) re-rankeando dentro de cada distribución
asignar_decil <- function(x, w) {
  cortes <- Hmisc::wtd.quantile(x, weights = w, probs = seq(.1, .9, .1),
                                normwt = TRUE)
  pmin(10L, pmax(1L, findInterval(x, cortes) + 1L))
}

# resumen distributivo de un (x, w): Gini, media, mediana y brecha D10/D1
resumen_dist <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  x <- x[ok]; w <- w[ok]
  dec <- asignar_decil(x, w)
  medias_dec <- tapply(seq_along(x), dec, function(i) weighted.mean(x[i], w[i]))
  m <- rep(NA_real_, 10L); m[as.integer(names(medias_dec))] <- medias_dec
  tibble(
    gini       = gini_w(x, w),
    media      = weighted.mean(x, w),
    mediana    = as.numeric(Hmisc::wtd.quantile(x, weights = w, probs = .5,
                                                normwt = TRUE)),
    gap_d10_d1 = m[10] / m[1]
  )
}

# ---- base persona: IPCF antes y después ------------------------------------

base <- read_parquet(
  "bases/eph_ajustada_v2.parquet",
  col_select = c("CODUSU", "NRO_HOGAR", "ANO4", "TRIMESTRE", "SEMESTRE",
                 "ITF", "ITF_ajustado", "IX_TOT", "PONDIH")
) |>
  filter(PONDIH > 0, !is.na(ITF), !is.na(ITF_ajustado)) |>
  # IX_TOT (cantidad de miembros) no existe en la EPH continua vieja (pre-2016).
  # Donde falte lo reconstruimos contando las personas del hogar en la base.
  # Para 2016+ usamos IX_TOT, que evita el bug del IPCF ×100 de 2020-T3.
  group_by(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE) |>
  mutate(miembros = if_else(!is.na(IX_TOT) & IX_TOT > 0,
                            IX_TOT, as.numeric(n()))) |>
  ungroup() |>
  filter(miembros > 0) |>
  mutate(ipcf_antes   = ITF / miembros,
         ipcf_despues = ITF_ajustado / miembros)

cat("Filas persona (PONDIH>0, ITF válido):", nrow(base), "\n")

# serie semestral original vs ajustada
serie <- bind_rows(
  base |> group_by(ANO4, SEMESTRE) |>
    group_modify(~ resumen_dist(.x$ipcf_antes, .x$PONDIH)) |>
    mutate(escenario = "Original"),
  base |> group_by(ANO4, SEMESTRE) |>
    group_modify(~ resumen_dist(.x$ipcf_despues, .x$PONDIH)) |>
    mutate(escenario = "Ajustada")
) |>
  ungroup() |>
  mutate(periodo = sem_periodo(ANO4, SEMESTRE))

# versión ancha con deltas para graficar y para la app
serie_w <- serie |>
  pivot_wider(names_from = escenario,
              values_from = c(gini, media, mediana, gap_d10_d1)) |>
  mutate(
    delta_gini      = gini_Ajustada - gini_Original,
    delta_gap       = gap_d10_d1_Ajustada - gap_d10_d1_Original,
    cambio_media    = media_Ajustada   / media_Original   - 1,
    cambio_mediana  = mediana_Ajustada / mediana_Original - 1
  ) |>
  arrange(periodo)

write_parquet(serie_w, "bases/distribucion_ingreso_serie.parquet")
cat("Guardado bases/distribucion_ingreso_serie.parquet\n")

# paleta compartida con el resto del repo
col_orig     <- "#1f78b4"
col_aj       <- "#e31a1c"
color_barras <- "#41b6c4"

# =============================================================================
# gráfico 1: Gini del IPCF, original vs ajustado + Δ
# =============================================================================
max_g  <- max(serie$gini, na.rm = TRUE)
max_dg <- max(abs(serie_w$delta_gini), na.rm = TRUE)
fac_g  <- (max_g * 0.18) / max_dg

gini_long <- serie |>
  mutate(escenario = factor(escenario, levels = c("Original", "Ajustada")))

g1 <- ggplot() +
  geom_col(data = serie_w,
           aes(periodo, delta_gini * fac_g),
           fill = color_barras, color = NA, width = 0.10, alpha = 0.85) +
  geom_text_repel(data = serie_w,
                  aes(periodo, delta_gini * fac_g,
                      label = sprintf("%+.3f", delta_gini)),
                  color = color_barras, size = 2.8, fontface = "bold",
                  nudge_y = max_g * 0.02, direction = "y",
                  min.segment.length = Inf, seed = 1) +
  geom_line(data = gini_long,
            aes(periodo, gini, color = escenario, linetype = escenario),
            linewidth = 0.7) +
  geom_point(data = gini_long, aes(periodo, gini, color = escenario),
             size = 1.9) +
  geom_text_repel(data = gini_long |> filter(escenario == "Original"),
                  aes(periodo, gini, label = sprintf("%.3f", gini)),
                  color = col_orig, size = 2.8, nudge_y = 0.012,
                  direction = "y", min.segment.length = 0,
                  segment.alpha = 0.4, seed = 2) +
  geom_text_repel(data = gini_long |> filter(escenario == "Ajustada"),
                  aes(periodo, gini, label = sprintf("%.3f", gini)),
                  color = col_aj, size = 2.8, nudge_y = -0.012,
                  direction = "y", min.segment.length = 0,
                  segment.alpha = 0.4, seed = 3) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / fac_g, name = "Δ Gini (Ajustada − Original)",
                        labels = label_number(accuracy = 0.001))) +
  scale_x_continuous(breaks = sort(unique(serie$periodo)), labels = sem_label_fn) +
  scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
  scale_linetype_manual(values = c(Original = "solid", Ajustada = "dashed")) +
  labs(title = "Gini del ingreso per cápita familiar: original vs ajustado por subdeclaración",
       subtitle = "IPCF por persona ponderado por PONDIH. Barras = Δ Gini (eje derecho). Δ>0 ⇒ el ajuste aumenta la desigualdad.",
       x = NULL, y = "Coeficiente de Gini", color = NULL, linetype = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/dist_gini_ipcf.png", g1, width = 14, height = 6.5, dpi = 150)
cat("Guardado outputs/dist_gini_ipcf.png\n")

# =============================================================================
# gráfico 2: cambio % en media y mediana del IPCF por el ajuste
# =============================================================================
cambio_long <- serie_w |>
  select(periodo, Media = cambio_media, Mediana = cambio_mediana) |>
  pivot_longer(c(Media, Mediana), names_to = "estadistico",
               values_to = "cambio") |>
  mutate(estadistico = factor(estadistico, levels = c("Media", "Mediana")))

g2 <- ggplot(cambio_long, aes(periodo, cambio,
                              color = estadistico, linetype = estadistico)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2) +
  geom_text_repel(aes(label = label_percent(accuracy = 0.1)(cambio)),
                  size = 2.7, show.legend = FALSE, direction = "y",
                  min.segment.length = 0, segment.alpha = 0.35, seed = 4) +
  scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
  scale_x_continuous(breaks = sort(unique(serie$periodo)), labels = sem_label_fn) +
  scale_color_manual(values = c(Media = col_aj, Mediana = "#6a3d9a")) +
  scale_linetype_manual(values = c(Media = "solid", Mediana = "dashed")) +
  labs(title = "Cuánto sube el ingreso per cápita familiar con el ajuste por subdeclaración",
       subtitle = "Variación % entre IPCF ajustado y original, por semestre. Media > mediana ⇒ el ajuste se concentra arriba.",
       x = NULL, y = "Variación %", color = NULL, linetype = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/dist_media_mediana_cambio.png", g2,
       width = 14, height = 6, dpi = 150)
cat("Guardado outputs/dist_media_mediana_cambio.png\n")

# =============================================================================
# gráfico 3: brecha interdecílica (D10/D1) del IPCF, original vs ajustado + Δ
# =============================================================================
max_b  <- max(serie$gap_d10_d1, na.rm = TRUE)
max_db <- max(abs(serie_w$delta_gap), na.rm = TRUE)
fac_b  <- (max_b * 0.18) / max_db

gap_long <- serie |>
  mutate(escenario = factor(escenario, levels = c("Original", "Ajustada")))

g3 <- ggplot() +
  geom_col(data = serie_w,
           aes(periodo, delta_gap * fac_b),
           fill = color_barras, color = NA, width = 0.10, alpha = 0.85) +
  geom_text_repel(data = serie_w,
                  aes(periodo, delta_gap * fac_b,
                      label = sprintf("%+.1f", delta_gap)),
                  color = color_barras, size = 2.8, fontface = "bold",
                  nudge_y = max_b * 0.02, direction = "y",
                  min.segment.length = Inf, seed = 5) +
  geom_line(data = gap_long,
            aes(periodo, gap_d10_d1, color = escenario, linetype = escenario),
            linewidth = 0.7) +
  geom_point(data = gap_long, aes(periodo, gap_d10_d1, color = escenario),
             size = 1.9) +
  geom_text_repel(data = gap_long |> filter(escenario == "Original"),
                  aes(periodo, gap_d10_d1, label = sprintf("%.1f", gap_d10_d1)),
                  color = col_orig, size = 2.8, nudge_y = max_b * 0.03,
                  direction = "y", min.segment.length = 0,
                  segment.alpha = 0.4, seed = 6) +
  geom_text_repel(data = gap_long |> filter(escenario == "Ajustada"),
                  aes(periodo, gap_d10_d1, label = sprintf("%.1f", gap_d10_d1)),
                  color = col_aj, size = 2.8, nudge_y = -max_b * 0.03,
                  direction = "y", min.segment.length = 0,
                  segment.alpha = 0.4, seed = 7) +
  scale_y_continuous(
    sec.axis = sec_axis(~ . / fac_b, name = "Δ brecha (Ajustada − Original)",
                        labels = label_number(accuracy = 0.1))) +
  scale_x_continuous(breaks = sort(unique(serie$periodo)), labels = sem_label_fn) +
  scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
  scale_linetype_manual(values = c(Original = "solid", Ajustada = "dashed")) +
  labs(title = "Brecha interdecílica del IPCF (decil 10 / decil 1): original vs ajustado",
       subtitle = "Cociente entre el IPCF medio del decil más rico y el más pobre. Deciles re-rankeados en cada escenario.",
       x = NULL, y = "Ingreso medio D10 / D1 (veces)", color = NULL, linetype = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/dist_brecha_deciles.png", g3, width = 14, height = 6.5, dpi = 150)
cat("Guardado outputs/dist_brecha_deciles.png\n")

# =============================================================================
# gráfico 4: cambio % en el ingreso medio de cada categoría ocupacional
# heatmap categoría × semestre (ingreso laboral / jubilatorio individual)
# =============================================================================
cat_occ <- read_parquet("bases/datos_app_densidades.parquet") |>
  filter(!is.na(ingreso), ingreso > 0, !is.na(peso_ingreso), peso_ingreso > 0) |>
  mutate(SEMESTRE = if_else(TRIMESTRE %in% c(1L, 2L), 1L, 2L)) |>
  group_by(ANO4, SEMESTRE, grupo) |>
  summarise(media_antes   = weighted.mean(ingreso,      peso_ingreso),
            media_despues = weighted.mean(ingreso_post, peso_ingreso),
            .groups = "drop") |>
  mutate(cambio_pct  = media_despues / media_antes - 1,
         periodo     = sem_periodo(ANO4, SEMESTRE),
         periodo_lbl = sem_label_fn(periodo))

# ordenamos las categorías por cuánto las toca el ajuste en promedio
orden_grupos <- cat_occ |>
  group_by(grupo) |>
  summarise(m = mean(cambio_pct, na.rm = TRUE), .groups = "drop") |>
  arrange(m) |> pull(grupo)

cat_occ <- cat_occ |>
  mutate(grupo = factor(grupo, levels = orden_grupos),
         periodo_lbl = factor(periodo_lbl,
                              levels = sem_label_fn(sort(unique(periodo)))))

g4 <- ggplot(cat_occ, aes(periodo_lbl, grupo, fill = cambio_pct)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = label_percent(accuracy = 1)(cambio_pct)),
            size = 2.4, color = "grey15") +
  scale_fill_gradient(low = "#f7fbff", high = "#08519c",
                      labels = label_percent(accuracy = 1)) +
  labs(title = "Cambio en el ingreso medio por categoría ocupacional por el ajuste",
       subtitle = "Variación % del ingreso individual (laboral o jubilatorio) entre ajustado y original. Más oscuro = el ajuste lo sube más.",
       x = NULL, y = NULL, fill = "Cambio %",
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ingreso laboral ponderado por PONDIIO; jubilaciones por PONDII.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

ggsave("outputs/dist_cambio_por_categoria.png", g4,
       width = 14, height = 6, dpi = 150)
cat("Guardado outputs/dist_cambio_por_categoria.png\n")

# =============================================================================
# último semestre disponible: curva de Lorenz y participación por decil
# =============================================================================
periodo_max <- max(sem_periodo(base$ANO4, base$SEMESTRE))
ult <- base |> filter(sem_periodo(ANO4, SEMESTRE) == periodo_max)
ult_lbl <- sem_label_fn(periodo_max)
cat("Lorenz / deciles del último semestre:", ult_lbl,
    "(", nrow(ult), "personas)\n")

# ---- gráfico 5: curva de Lorenz original vs ajustada ----
lorenz_pts <- function(x, w, etiqueta) {
  ok <- !is.na(x) & w > 0; x <- x[ok]; w <- w[ok]
  o <- order(x); x <- x[o]; w <- w[o]
  pop <- cumsum(w) / sum(w)            # población acumulada
  inc <- cumsum(w * x) / sum(w * x)    # ingreso acumulado (Lorenz)
  grid <- seq(0, 1, 0.02)
  Lg <- approx(c(0, pop), c(0, inc), xout = grid, ties = "ordered")$y
  tibble(escenario = etiqueta, p = grid, L = Lg)
}

lorenz <- bind_rows(
  lorenz_pts(ult$ipcf_antes,   ult$PONDIH, "Original"),
  lorenz_pts(ult$ipcf_despues, ult$PONDIH, "Ajustada")
) |>
  mutate(escenario = factor(escenario, levels = c("Original", "Ajustada")))

g5 <- ggplot(lorenz, aes(p, L, color = escenario)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
  geom_line(linewidth = 0.9) +
  scale_x_continuous(labels = label_percent(accuracy = 1)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
  coord_equal() +
  labs(title = paste0("Curva de Lorenz del IPCF — ", ult_lbl),
       subtitle = "Más lejos de la diagonal = más desigual.",
       x = "Proporción acumulada de población",
       y = "Proporción acumulada del ingreso", color = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave("outputs/dist_lorenz.png", g5, width = 7.5, height = 7.5, dpi = 150)
cat("Guardado outputs/dist_lorenz.png\n")

# ---- gráfico 6: participación en el ingreso por decil ----
share_dec <- function(x, w, etiqueta) {
  ok <- !is.na(x) & w > 0; x <- x[ok]; w <- w[ok]
  dec <- asignar_decil(x, w)
  tibble(decil = dec, inc = x * w) |>
    group_by(decil) |>
    summarise(s = sum(inc), .groups = "drop") |>
    mutate(escenario = etiqueta, share = s / sum(s)) |>
    select(escenario, decil, share)
}

shares <- bind_rows(
  share_dec(ult$ipcf_antes,   ult$PONDIH, "Original"),
  share_dec(ult$ipcf_despues, ult$PONDIH, "Ajustada")
) |>
  mutate(escenario = factor(escenario, levels = c("Original", "Ajustada")))

g6 <- ggplot(shares, aes(factor(decil), share, fill = escenario)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_text(aes(label = label_percent(accuracy = 0.1)(share)),
            position = position_dodge(width = 0.8), vjust = -0.4, size = 2.5,
            color = "grey25") +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = expansion(mult = c(0, 0.08))) +
  scale_fill_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
  labs(title = paste0("Participación en el ingreso total por decil de IPCF — ", ult_lbl),
       subtitle = "Qué porción del ingreso total se lleva cada decil, antes y después del ajuste.",
       x = "Decil de IPCF", y = "Participación en el ingreso total", fill = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())

ggsave("outputs/dist_share_deciles.png", g6, width = 12, height = 5.5, dpi = 150)
cat("Guardado outputs/dist_share_deciles.png\n")

# =============================================================================
# series completas para la app: Lorenz y participación por decil en cada
# semestre (los gráficos 5 y 6 son sólo el snapshot del último semestre)
# =============================================================================
lorenz_serie <- bind_rows(
  base |> group_by(ANO4, SEMESTRE) |>
    group_modify(~ lorenz_pts(.x$ipcf_antes,   .x$PONDIH, "Original")),
  base |> group_by(ANO4, SEMESTRE) |>
    group_modify(~ lorenz_pts(.x$ipcf_despues, .x$PONDIH, "Ajustada"))
) |>
  ungroup() |>
  mutate(periodo = sem_periodo(ANO4, SEMESTRE))

write_parquet(lorenz_serie, "bases/distribucion_ingreso_lorenz.parquet")
cat("Guardado bases/distribucion_ingreso_lorenz.parquet\n")

deciles_serie <- bind_rows(
  base |> group_by(ANO4, SEMESTRE) |>
    group_modify(~ share_dec(.x$ipcf_antes,   .x$PONDIH, "Original")),
  base |> group_by(ANO4, SEMESTRE) |>
    group_modify(~ share_dec(.x$ipcf_despues, .x$PONDIH, "Ajustada"))
) |>
  ungroup() |>
  mutate(periodo = sem_periodo(ANO4, SEMESTRE))

write_parquet(deciles_serie, "bases/distribucion_ingreso_deciles.parquet")
cat("Guardado bases/distribucion_ingreso_deciles.parquet\n")

# copiamos a app/data/ lo que consume el shiny (mismo patrón que el 05)
app_data <- "app/data"
if (dir.exists(app_data)) {
  for (f in c("distribucion_ingreso_serie.parquet",
              "distribucion_ingreso_lorenz.parquet",
              "distribucion_ingreso_deciles.parquet")) {
    src <- file.path("bases", f)
    if (file.exists(src)) file.copy(src, file.path(app_data, f), overwrite = TRUE)
  }
}

cat("\nListo. 6 gráficos en outputs/ y las series en bases/distribucion_ingreso_*.parquet\n")

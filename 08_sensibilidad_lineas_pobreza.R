# análisis de SENSIBILIDAD de la pobreza al valor de la línea. la pregunta es:
# la conclusión "el ajuste por subdeclaración baja la pobreza" ¿aguanta si movemos
# la línea de pobreza? para eso contamos no sólo a la gente por debajo de 1 línea
# (la CBT, el corte oficial) sino también por debajo de 0,5 / 0,75 / 1,25 / 1,5 / 2
# líneas. una persona es "pobre al umbral k" si su ITF del hogar < k * CBT_hogar.
#
# todo a nivel persona, semestral y original vs ajustado, como el resto del repo.
# tres gráficos:
#   1. % bajo cada umbral en el tiempo, facetado por umbral (original vs ajustado)
#   2. heatmap del Δpp del ajuste por umbral x semestre (¿es robusto el efecto?)
#   3. curvas de incidencia (CDF de ITF/CBT): headcount vs línea, para algunos
#      semestres. si la curva ajustada queda SIEMPRE por debajo de la original
#      (dominancia de primer orden) la baja de pobreza no depende de dónde pongas
#      la línea.

library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(openxlsx)

dir.create("outputs", showWarnings = FALSE)

# ---- helpers (mismas convenciones que el 03 y el 06) -----------------------
sem_periodo  <- function(ano, sem) ano + (sem - 1) * 0.5
sem_label_fn <- function(x) {
  ano <- floor(x); sem <- round((x - ano) * 2) + 1
  sprintf("S%d %d", sem, ano)
}
# etiqueta del umbral: "0,5 LP", "1 LP", "1,5 LP", ... (LP = línea de pobreza)
fmt_k <- function(k) sub("\\.", ",", sprintf("%g LP", k))

col_orig     <- "#1f78b4"
col_aj       <- "#e31a1c"
color_barras <- "#41b6c4"

# grilla de múltiplos de la línea de pobreza para la serie y el heatmap
multiplicadores <- c(0.5, 0.75, 1.0, 1.25, 1.5, 2.0)
# grilla fina (continua) para las curvas de incidencia
kgrid <- seq(0, 2.5, by = 0.05)

# ---- base persona: ITF/CBT antes y después ---------------------------------
# r = ITF / CBT_hogar es el ingreso del hogar medido en "líneas de pobreza".
# pobre al umbral k  <=>  r < k. con k = 1 reproduce la pobreza oficial.
base <- read_parquet(
  "bases/eph_ajustada_v2.parquet",
  col_select = c("ANO4", "TRIMESTRE", "SEMESTRE",
                 "ITF", "ITF_ajustado", "CBT_hogar", "PONDIH")
) |>
  filter(PONDIH > 0, !is.na(ITF), !is.na(ITF_ajustado),
         !is.na(CBT_hogar), CBT_hogar > 0) |>
  mutate(r_orig = ITF / CBT_hogar,
         r_aj   = ITF_ajustado / CBT_hogar)

cat("Filas persona (PONDIH>0, ITF y CBT válidos):", nrow(base), "\n")

# headcount ponderado bajo cada umbral k, para un grupo (semestre)
hc_por_k <- function(df, ks) {
  W <- sum(df$PONDIH)
  do.call(rbind, lapply(ks, function(k) data.frame(
    k       = k,
    hc_orig = sum(df$PONDIH[df$r_orig < k]) / W,
    hc_aj   = sum(df$PONDIH[df$r_aj   < k]) / W
  )))
}

serie <- base |>
  group_by(ANO4, SEMESTRE) |>
  group_modify(~ hc_por_k(.x, multiplicadores)) |>
  ungroup() |>
  mutate(periodo  = sem_periodo(ANO4, SEMESTRE),
         delta_pp = (hc_aj - hc_orig) * 100,           # <0 = el ajuste baja pobreza
         k_lbl    = factor(fmt_k(k), levels = fmt_k(multiplicadores)))

write_parquet(serie, "bases/sensibilidad_lineas_serie.parquet")
cat("Guardado bases/sensibilidad_lineas_serie.parquet\n")

# copia para la app shiny (mismo patrón que el final de 05): la pestaña
# "Sensibilidad" consume esta serie tal cual, sin recalcular nada.
if (dir.exists("app/data")) {
  file.copy("bases/sensibilidad_lineas_serie.parquet",
            "app/data/sensibilidad_lineas_serie.parquet", overwrite = TRUE)
  cat("Copiado a app/data/sensibilidad_lineas_serie.parquet\n")
}

# sanity check: a k=1 el headcount original tiene que pegar con la pobreza_total
# del 02 (ITF < CBT = pobre+indigente). si no, algo se desalineó.
res <- read_parquet("bases/resultados_pobreza_v2.parquet") |>
  select(ANO4, SEMESTRE, pobreza_total_base)
chk <- serie |> filter(k == 1) |>
  left_join(res, by = c("ANO4", "SEMESTRE"))
cat(sprintf("Sanity k=1 vs pobreza_total_base: desvío máx %.3f pp\n",
            max(abs(chk$hc_orig - chk$pobreza_total_base), na.rm = TRUE) * 100))

# =============================================================================
# gráfico 1: % bajo cada umbral en el tiempo, facetado por umbral
# =============================================================================
serie_long <- serie |>
  select(periodo, k_lbl, Original = hc_orig, Ajustada = hc_aj) |>
  pivot_longer(c(Original, Ajustada), names_to = "escenario", values_to = "hc") |>
  mutate(escenario = factor(escenario, levels = c("Original", "Ajustada")))

# etiqueta sólo el último punto de cada línea, así no se satura
lab_ult <- serie_long |>
  group_by(k_lbl, escenario) |>
  filter(periodo == max(periodo)) |>
  ungroup()

g1 <- ggplot(serie_long, aes(periodo, hc, color = escenario, linetype = escenario)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  geom_text_repel(data = lab_ult,
                  aes(label = label_percent(accuracy = 0.1)(hc)),
                  size = 2.6, direction = "y", nudge_x = 0.6,
                  min.segment.length = 0, segment.alpha = 0.3,
                  show.legend = FALSE, seed = 1) +
  facet_wrap(~ k_lbl, scales = "free_y") +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = expansion(mult = c(0.04, 0.12))) +
  scale_x_continuous(breaks = seq(2004, 2024, 4)) +
  scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
  scale_linetype_manual(values = c(Original = "solid", Ajustada = "dashed")) +
  labs(title = "Sensibilidad de la pobreza al valor de la línea: original vs ajustada",
       subtitle = paste0("Gente con ingreso del hogar (ITF) por debajo de k líneas de pobreza (k·CBT), por semestre. ",
                         "1 LP = línea oficial. Ejes Y independientes por panel."),
       x = NULL, y = "Población por debajo del umbral",
       color = NULL, linetype = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave("outputs/sensibilidad_lineas_por_umbral.png", g1,
       width = 14, height = 8, dpi = 150)
cat("Guardado outputs/sensibilidad_lineas_por_umbral.png\n")

# =============================================================================
# gráfico 2: heatmap del Δpp del ajuste por umbral x semestre
# cuánto baja (en pp) el headcount al pasar de original a ajustado, en cada
# umbral. si todas las celdas son del mismo signo, el efecto es robusto a la línea.
# =============================================================================
heat <- serie |>
  mutate(periodo_lbl = factor(sem_label_fn(periodo),
                              levels = sem_label_fn(sort(unique(periodo)))))

lim <- max(abs(heat$delta_pp), na.rm = TRUE)

g2 <- ggplot(heat, aes(periodo_lbl, k_lbl, fill = delta_pp)) +
  geom_tile(color = "white", linewidth = 0.3) +
  geom_text(aes(label = sprintf("%.1f", delta_pp)), size = 1.9, color = "grey15") +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-lim, lim),
                       labels = label_number(accuracy = 0.1)) +
  labs(title = "Efecto del ajuste por subdeclaración según dónde se ponga la línea de pobreza",
       subtitle = "Δ pp = headcount ajustado − original, por umbral (k·CBT) y semestre. Azul = el ajuste baja la pobreza; rojo = la sube.",
       x = NULL, y = "Umbral (líneas de pobreza)", fill = "Δ pp",
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = "right")

ggsave("outputs/sensibilidad_heatmap_delta.png", g2,
       width = 16, height = 5, dpi = 150)
cat("Guardado outputs/sensibilidad_heatmap_delta.png\n")

# =============================================================================
# gráfico 3: curvas de incidencia de pobreza (CDF de ITF/CBT)
# para cada línea posible (eje x, en múltiplos de CBT) qué % de la población
# queda por debajo (eje y). la curva ajustada por debajo de la original en TODO
# el rango = el ajuste baja la pobreza sea cual sea la línea (dominancia 1er orden).
# tomamos algunos semestres equiespaciados para ver la robustez a lo largo de la serie.
# =============================================================================
periodos_disp <- sort(unique(sem_periodo(base$ANO4, base$SEMESTRE)))
sel <- periodos_disp[round(seq(1, length(periodos_disp), length.out = 6))]

curva <- function(df, kg) {
  W <- sum(df$PONDIH)
  bind_rows(
    data.frame(escenario = "Original", k = kg,
               hc = vapply(kg, function(kk) sum(df$PONDIH[df$r_orig < kk]), numeric(1)) / W),
    data.frame(escenario = "Ajustada", k = kg,
               hc = vapply(kg, function(kk) sum(df$PONDIH[df$r_aj   < kk]), numeric(1)) / W)
  )
}

curvas <- base |>
  mutate(periodo = sem_periodo(ANO4, SEMESTRE)) |>
  filter(periodo %in% sel) |>
  group_by(periodo) |>
  group_modify(~ curva(.x, kgrid)) |>
  ungroup() |>
  mutate(periodo_lbl = factor(sem_label_fn(periodo), levels = sem_label_fn(sel)),
         escenario   = factor(escenario, levels = c("Original", "Ajustada")))

g3 <- ggplot(curvas, aes(k, hc, color = escenario)) +
  geom_vline(xintercept = c(1, 1.5, 2), linetype = "dotted",
             color = "grey60", linewidth = 0.4) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ periodo_lbl) +
  scale_x_continuous(breaks = c(0, 0.5, 1, 1.5, 2, 2.5)) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
  labs(title = "Curvas de incidencia de pobreza: ¿la baja depende de dónde se ponga la línea?",
       subtitle = paste0("% de la población con ITF por debajo de k líneas de pobreza, para cada k (eje x). ",
                         "Líneas punteadas en 1 / 1,5 / 2 LP. Ajustada por debajo de Original en todo el rango = baja robusta."),
       x = "Línea de pobreza (múltiplos de la CBT)", y = "Población por debajo",
       color = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold"))

ggsave("outputs/sensibilidad_curvas_incidencia.png", g3,
       width = 13, height = 7.5, dpi = 150)
cat("Guardado outputs/sensibilidad_curvas_incidencia.png\n")

# =============================================================================
# gráfico 4: todos los umbrales superpuestos en un solo panel
# como el gráfico principal de la app (solapa "tasa de pobreza"), pero con varios
# umbrales: color = umbral, original = línea sólida, ajustada = punteada. sin las
# barras de Δ, sin la referencia del INDEC. mantenemos la banda del apagón y el eje
# semestral para que se lea igual que el resto.
# =============================================================================
gap_xmin <- 2015.25; gap_xmax <- 2016.25
max_tasa4 <- max(serie_long$hc, na.rm = TRUE)

g4 <- ggplot(serie_long, aes(periodo, hc, color = k_lbl, linetype = escenario)) +
  annotate("rect", xmin = gap_xmin, xmax = gap_xmax, ymin = -Inf, ymax = Inf,
           fill = "grey88", alpha = 0.6) +
  annotate("text", x = (gap_xmin + gap_xmax) / 2, y = max_tasa4 * 1.01,
           label = "apagón\nestadístico\n2015–2016", size = 2.4,
           color = "grey45", fontface = "italic", lineheight = 0.9) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  geom_text_repel(data = lab_ult,
                  aes(label = label_percent(accuracy = 0.1)(hc)),
                  size = 2.2, direction = "y", nudge_x = 0.5,
                  min.segment.length = 0, segment.alpha = 0.3,
                  show.legend = FALSE, seed = 4) +
  scale_y_continuous(labels = label_percent(accuracy = 1),
                     expand = expansion(mult = c(0.02, 0.08))) +
  scale_x_continuous(breaks = sort(unique(serie_long$periodo)), labels = sem_label_fn,
                     expand = expansion(mult = c(0.01, 0.05))) +
  scale_color_viridis_d(option = "C", begin = 0.08, end = 0.86) +
  scale_linetype_manual(values = c(Original = "solid", Ajustada = "dashed")) +
  labs(title = "Tasa de pobreza por umbral de línea: original (sólida) vs ajustada (punteada)",
       subtitle = "Población con ITF del hogar por debajo de cada múltiplo de la línea de pobreza (k·CBT), por semestre. Color = umbral.",
       x = NULL, y = "Población por debajo del umbral",
       color = "Umbral", linetype = NULL,
       caption = "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH.") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  guides(color = guide_legend(override.aes = list(linetype = "solid", linewidth = 1)))

ggsave("outputs/sensibilidad_lineas_overlay.png", g4, width = 16, height = 8, dpi = 150)
cat("Guardado outputs/sensibilidad_lineas_overlay.png\n")

# =============================================================================
# export a Excel legible: % de población bajo cada múltiplo de la línea
# son las mismas cifras que alimentan los gráficos de arriba (la serie); no se
# calcula nada nuevo. la idea es que se pueda mirar y graficar directo en Excel.
# =============================================================================
orden_periodo <- sem_label_fn(sort(unique(serie$periodo)))

# formato largo (tidy): una fila por semestre × umbral. cómodo para filtrar y
# para tablas dinámicas.
datos_largo <- serie |>
  arrange(periodo, k) |>
  transmute(
    Periodo                      = sem_label_fn(periodo),
    Umbral                       = as.character(k_lbl),
    `Umbral (k×CBT)`             = k,
    `% Original`                 = hc_orig,
    `% Ajustada`                 = hc_aj,
    `Δ pp (ajustada − original)` = delta_pp
  )

# formato ancho (un valor por celda): filas = semestre, columnas = umbral. listo
# para seleccionar el bloque e insertar un gráfico de líneas en Excel.
ancho <- function(col) {
  serie |>
    mutate(Periodo = factor(sem_label_fn(periodo), levels = orden_periodo)) |>
    select(Periodo, k_lbl, valor = {{ col }}) |>
    pivot_wider(names_from = k_lbl, values_from = valor) |>
    arrange(Periodo) |>
    mutate(Periodo = as.character(Periodo))
}
ancho_orig  <- ancho(hc_orig)
ancho_aj    <- ancho(hc_aj)
ancho_delta <- ancho(delta_pp)

wb <- createWorkbook()
est_header <- createStyle(textDecoration = "bold", fgFill = "#E8E8E8",
                          halign = "center", border = "bottom")
est_pct <- createStyle(numFmt = "0.0%")  # hc están como proporción (0–1)
est_pp  <- createStyle(numFmt = "0.0")   # Δ ya viene en puntos porcentuales

# hoja léeme: qué es cada cosa, para que el coworker no tenga que adivinar
addWorksheet(wb, "Léeme")
texto <- c(
  "Sensibilidad de la pobreza al valor de la línea",
  "",
  "Qué hay acá: % de personas con ingreso total familiar (ITF) por debajo de",
  "distintos múltiplos de la línea de pobreza (k × CBT del hogar), por semestre.",
  "k = 1 es la pobreza oficial (CBT). Todo ponderado por PONDIH (nivel persona).",
  "",
  "Original  = ITF sin ajustar.",
  "Ajustada  = ITF corregido por subdeclaración (brecha MLER/EPH por percentil).",
  "Δ pp      = % ajustada − % original, en puntos porcentuales.",
  "            negativo = el ajuste baja la pobreza en ese umbral.",
  "",
  "Hojas:",
  "  - Datos (largo): una fila por semestre y umbral (para filtrar / tablas dinámicas).",
  "  - Original (ancho), Ajustada (ancho), Delta pp (ancho): filas = semestre,",
  "    columnas = umbral. Seleccioná el bloque e insertá un gráfico de líneas.",
  "",
  "Los % están guardados como proporción (0 a 1) con formato de porcentaje.",
  "Fuente: EPH continua + brecha MLER/EPH por percentil."
)
writeData(wb, "Léeme", texto, colNames = FALSE)
addStyle(wb, "Léeme", createStyle(fontSize = 13, textDecoration = "bold"),
         rows = 1, cols = 1)
setColWidths(wb, "Léeme", cols = 1, widths = 82)

# hoja largo
addWorksheet(wb, "Datos (largo)")
writeData(wb, "Datos (largo)", datos_largo)
addStyle(wb, "Datos (largo)", est_header, rows = 1, cols = 1:ncol(datos_largo))
addStyle(wb, "Datos (largo)", est_pct,
         rows = 2:(nrow(datos_largo) + 1), cols = c(4, 5), gridExpand = TRUE)
addStyle(wb, "Datos (largo)", est_pp,
         rows = 2:(nrow(datos_largo) + 1), cols = 6, gridExpand = TRUE)
freezePane(wb, "Datos (largo)", firstActiveRow = 2)
setColWidths(wb, "Datos (largo)", cols = 1:ncol(datos_largo),
             widths = c(10, 10, 14, 12, 12, 26))

# hojas anchas, una por escenario y otra para el Δ
escribir_ancho <- function(hoja, df, fmt) {
  addWorksheet(wb, hoja)
  writeData(wb, hoja, df)
  addStyle(wb, hoja, est_header, rows = 1, cols = 1:ncol(df))
  addStyle(wb, hoja, fmt, rows = 2:(nrow(df) + 1), cols = 2:ncol(df),
           gridExpand = TRUE)
  freezePane(wb, hoja, firstActiveRow = 2, firstActiveCol = 2)
  setColWidths(wb, hoja, cols = 1:ncol(df), widths = c(10, rep(9, ncol(df) - 1)))
}
escribir_ancho("Original (ancho)", ancho_orig,  est_pct)
escribir_ancho("Ajustada (ancho)", ancho_aj,    est_pct)
escribir_ancho("Delta pp (ancho)", ancho_delta, est_pp)

saveWorkbook(wb, "outputs/sensibilidad_lineas_pobreza.xlsx", overwrite = TRUE)
cat("Guardado outputs/sensibilidad_lineas_pobreza.xlsx\n")

cat("\nListo. 4 gráficos + Excel en outputs/ y la serie en bases/sensibilidad_lineas_serie.parquet\n")

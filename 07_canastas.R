# compara las dos fuentes de canastas regionales que usa el pipeline:
#   - EPH (paquete eph): get_poverty_lines(regional = TRUE) + extensión INDEC,
#     guardado en bases/canastas_regionales.parquet. Cubre 2015.4 -> 2025.4.
#   - CEPED (bases/Canastas.xlsx): CBA/CBT por adulto equivalente "NM-EMP",
#     mensual 2003 -> 2016, que promediamos a trimestral.
# El pipeline empalma CEPED para 2003-2016 y EPH para 2017+. Acá miramos que en
# el solapamiento (2015.4 - 2016.4) las dos coincidan y qué agrega cada una.

library(arrow)
library(dplyr)
library(tidyr)
library(readxl)
library(ggplot2)
library(scales)

dir.create("outputs", showWarnings = FALSE)

# ---- fuente A: canastas del paquete eph ------------------------------------
eph_q <- read_parquet("bases/canastas_regionales.parquet") |>
  transmute(region,
            ano  = as.integer(substr(periodo, 1, 4)),
            trim = as.integer(substr(periodo, 6, 6)),
            CBA, CBT)

# ---- fuente B: canastas del Excel del CEPED (mismo reshape que el 01) -------
regiones_xlsx <- tibble::tribble(
  ~suf,    ~region,      ~codigo,
  "GBA",   "GBA",        1,
  "Cuyo",  "Cuyo",      42,
  "NEA",   "Noreste",   41,
  "NOA",   "Noroeste",  40,
  "Pamp",  "Pampeana",  43,
  "Patag", "Patagonia", 44
)

cx <- suppressMessages(
  read_excel("bases/Canastas.xlsx", sheet = "Hoja1", .name_repair = "minimal")
)
names(cx) <- c("ano", "mes", "trim",
               paste0("CBA_", regiones_xlsx$suf),
               paste0("CBT_", regiones_xlsx$suf))
cx <- cx |> mutate(across(everything(), as.numeric))

xlsx_q <- cx |>
  pivot_longer(cols = -c(ano, mes, trim),
               names_to = c(".value", "suf"), names_sep = "_") |>
  group_by(ano, trim, suf) |>
  summarise(CBA = mean(CBA, na.rm = TRUE),
            CBT = mean(CBT, na.rm = TRUE), .groups = "drop") |>
  inner_join(regiones_xlsx, by = "suf") |>
  transmute(region, ano, trim, CBA, CBT)

# ---- combinamos en formato largo para graficar -----------------------------
orden_reg <- c("GBA", "Pampeana", "Cuyo", "Noroeste", "Noreste", "Patagonia")

comp <- bind_rows(
  eph_q  |> mutate(fuente = "EPH (paquete)"),
  xlsx_q |> mutate(fuente = "CEPED (Canastas.xlsx)")
) |>
  mutate(t = ano + (trim - 1) / 4,
         region = factor(region, levels = orden_reg),
         fuente = factor(fuente, levels = c("CEPED (Canastas.xlsx)", "EPH (paquete)"))) |>
  pivot_longer(c(CBA, CBT), names_to = "canasta", values_to = "valor")

col_ceped <- "#e31a1c"   # rojo, igual que la línea "Ajustada" del resto
col_eph   <- "#1f78b4"   # azul, igual que la línea "Original"

# ventana donde existen las dos fuentes (2015.4 - 2016.4)
ov_min <- 2015 + 3 / 4
ov_max <- 2016 + 3 / 4

# =============================================================================
# gráfico 1: CBA y CBT por región, ambas fuentes, escala log (inflación de 22 años)
# =============================================================================
g1 <- ggplot(comp, aes(t, valor, color = fuente, linetype = canasta)) +
  annotate("rect", xmin = ov_min, xmax = ov_max, ymin = -Inf, ymax = Inf,
           fill = "grey70", alpha = 0.25) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ region) +
  scale_y_log10(labels = label_number(scale_cut = cut_short_scale())) +
  scale_x_continuous(breaks = seq(2004, 2024, 4)) +
  scale_color_manual(values = c("CEPED (Canastas.xlsx)" = col_ceped,
                                "EPH (paquete)" = col_eph)) +
  scale_linetype_manual(values = c(CBA = "dashed", CBT = "solid")) +
  labs(title = "Canastas regionales por adulto equivalente: CEPED (Canastas.xlsx) vs paquete eph",
       subtitle = paste0("CBT (línea sólida) y CBA (punteada) por región, escala log. ",
                         "Franja gris = solapamiento (2015.4-2016.4), donde las dos fuentes coinciden."),
       x = NULL, y = "Pesos por adulto equivalente (log)",
       color = "Fuente", linetype = "Canasta",
       caption = "CEPED: Canastas.xlsx (NM-EMP), 2003-2016. EPH: get_poverty_lines(regional) + extensión INDEC, 2015.4-2025.4.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

ggsave("outputs/canastas_comparacion.png", g1, width = 13, height = 7, dpi = 150)
cat("Guardado outputs/canastas_comparacion.png\n")

# =============================================================================
# gráfico 2: validación del empalme. En el solapamiento, cociente EPH / CEPED
# (debería pegarse a 1 en todas las regiones y trimestres).
# =============================================================================
overlap <- inner_join(
    eph_q  |> rename(CBA_eph = CBA, CBT_eph = CBT),
    xlsx_q |> rename(CBA_ceped = CBA, CBT_ceped = CBT),
    by = c("region", "ano", "trim")
  ) |>
  mutate(t = ano + (trim - 1) / 4,
         CBA = CBA_eph / CBA_ceped,
         CBT = CBT_eph / CBT_ceped,
         region = factor(region, levels = orden_reg)) |>
  pivot_longer(c(CBA, CBT), names_to = "canasta", values_to = "ratio")

cat(sprintf("Solapamiento: %d trimestres (%.2f - %.2f). Desvío máx. respecto de 1: %.1f%%\n",
            n_distinct(overlap$t), min(overlap$t), max(overlap$t),
            max(abs(overlap$ratio - 1)) * 100))

g2 <- ggplot(overlap, aes(t, ratio, color = region)) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_line(linewidth = 0.6) +
  geom_point(size = 1.8) +
  facet_wrap(~ canasta) +
  scale_y_continuous(labels = label_percent(accuracy = 1)) +
  scale_x_continuous(breaks = seq(2015.75, 2016.75, 0.25),
                     labels = function(x) {
                       ano <- floor(x); tr <- round((x - ano) * 4) + 1
                       sprintf("%d-T%d", ano, tr)
                     }) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Validación del empalme: cociente EPH / CEPED en el solapamiento",
       subtitle = "Valor por adulto equivalente del paquete eph sobre el del Excel del CEPED, por región y trimestre. 100% = coinciden exactamente.",
       x = NULL, y = "EPH / CEPED", color = NULL,
       caption = "Solapamiento 2015-T4 a 2016-T4. CBA y CBT por adulto equivalente.") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "top", panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("outputs/canastas_overlap_ratio.png", g2, width = 11, height = 5.5, dpi = 150)
cat("Guardado outputs/canastas_overlap_ratio.png\n")

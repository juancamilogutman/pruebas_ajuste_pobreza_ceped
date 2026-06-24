library(shiny)
library(bslib)
library(arrow)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggrepel)
library(ggiraph)
library(patchwork)
library(DT)
library(gt)

# Carga de datos ####

# cuando corre en shinyapps.io el cwd es el propio app/ y los parquets viven
# en app/data/ (los copiamos antes del deploy). en local el cwd suele ser la
# raíz del repo, así que también probamos "bases" y "../bases".
ruta_bases <- {
  if (dir.exists("data")) "data"
  else if (dir.exists("bases")) "bases"
  else "../bases"
}

datos    <- read_parquet(file.path(ruta_bases, "datos_app_densidades.parquet"))
umbrales <- read_parquet(file.path(ruta_bases, "umbrales_brecha_por_trimestre.parquet"))
hogares_mejor <- read_parquet(file.path(ruta_bases, "hogares_que_mejoraron.parquet"))
pobreza_serie <- read_parquet(file.path(ruta_bases, "pobreza_serie.parquet"))
ecdf_pob      <- read_parquet(file.path(ruta_bases, "datos_app_ecdf_pobreza.parquet"))
sensib        <- read_parquet(file.path(ruta_bases, "sensibilidad_lineas_serie.parquet"))
dist_serie    <- read_parquet(file.path(ruta_bases, "distribucion_ingreso_serie.parquet"))
dist_lorenz   <- read_parquet(file.path(ruta_bases, "distribucion_ingreso_lorenz.parquet"))
dist_deciles  <- read_parquet(file.path(ruta_bases, "distribucion_ingreso_deciles.parquet"))

# para los gráficos de semestrales (lo mismo de 03_graficos_pobreza_ajustada,R)
sem_label_fn <- function(x) {
  ano <- floor(x); sem <- round((x - ano) * 2) + 1
  sprintf("S%d %d", sem, ano)
}

# un tick por año en el eje x (centrado en los semestres presentes de ese año),
# para etiquetar los paneles de la pestaña "Tasa de pobreza" con el año en vez
# del semestre, que amontonaba demasiado
anio_breaks_labels <- function(periodos) {
  data.frame(periodo = sort(unique(periodos))) |>
    mutate(anio = floor(periodo)) |>
    group_by(anio) |>
    summarise(at = mean(periodo), .groups = "drop")
}

# regla de Scott con sd ponderada y tamaño muestral para el tamaño de los bins, igual no entiendo tanto
scott_bw <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  x <- x[ok]; w <- w[ok]
  n <- length(x)
  if (n < 2) return(NA_real_)
  m <- sum(w * x) / sum(w)
  s <- sqrt(sum(w * (x - m)^2) / sum(w))
  if (!is.finite(s) || s <= 0) return(NA_real_)
  3.5 * s / n^(1/3)
}

periodos_sem <- hogares_mejor |>
  distinct(ANO4, SEMESTRE, periodo_lbl) |>
  arrange(ANO4, SEMESTRE) |>
  pull(periodo_lbl)

niveles_grupo <- c(
  "Asal. priv. registrado", "Asal. priv. no registrado",
  "Asal. pub. registrado",  "Asal. pub. no registrado",
  "Serv. dom. registrado",  "Serv. dom. no registrado",
  "Cuentapropistas", "Patrones", "Jubilados", "Otros"
)

paleta_10 <- c(
  "Asal. priv. registrado"     = "#1f78b4",
  "Asal. priv. no registrado"  = "#a6cee3",
  "Asal. pub. registrado"      = "#33a02c",
  "Asal. pub. no registrado"   = "#b2df8a",
  "Serv. dom. registrado"      = "#6a3d9a",
  "Serv. dom. no registrado"   = "#cab2d6",
  "Cuentapropistas"            = "#e31a1c",
  "Patrones"                   = "#fb9a99",
  "Jubilados"                  = "#ff7f00",
  "Otros"                      = "#b15928"
)

periodos <- umbrales |>
  arrange(ANO4, TRIMESTRE) |>
  pull(periodo_lbl)

# composición del delta ITF por grupo y semestre (para la pestaña "Tasa de pobreza")
# delta ponderado por peso_ingreso (PONDIIO laboral / PONDII jubilatorio) pero no se si va bien

delta_grupo_sem <- datos |>
  mutate(SEMESTRE = if_else(TRIMESTRE %in% c(1L, 2L), 1L, 2L),
         delta_ing = ingreso_post - ingreso) |>
  group_by(ANO4, SEMESTRE, grupo) |>
  summarise(delta_w = sum(peso_ingreso * delta_ing, na.rm = TRUE),
            .groups = "drop") |>
  group_by(ANO4, SEMESTRE) |>
  mutate(share   = delta_w / sum(delta_w),
         periodo = ANO4 + (SEMESTRE - 1) * 0.5) |>
  ungroup()

# el grupo más grande en el primer semestre va abajo, el más chico va arriba
# ggplot apila el primer nivel del factor en la parte alta de la barra
orden_grupos_s1 <- delta_grupo_sem |>
  filter(periodo == min(periodo)) |>
  arrange(share) |>
  pull(grupo)
orden_grupos <- c(setdiff(niveles_grupo, orden_grupos_s1), orden_grupos_s1)

delta_grupo_sem <- delta_grupo_sem |>
  mutate(grupo = factor(grupo, levels = orden_grupos))

# ymin/ymax pre-computados por barra, para apilar y labels, porque salía todo mezclado
delta_bars <- delta_grupo_sem |>
  arrange(periodo, desc(as.integer(grupo))) |>
  group_by(periodo) |>
  mutate(ymax = cumsum(share),
         ymin = ymax - share,
         ymid = (ymin + ymax) / 2) |>
  ungroup() |>
  mutate(xmin = periodo - 0.275, xmax = periodo + 0.275)

delta_inside_lbl <- delta_bars |> filter(share >= 0.05)

# misma descomposición pero solo contando los hogares que salieron de la pobreza
# lo cual es mucho más interesante
delta_grupo_sem_lp <- datos |>
  filter(hogar_left_poverty) |>
  mutate(SEMESTRE = if_else(TRIMESTRE %in% c(1L, 2L), 1L, 2L),
         delta_ing = ingreso_post - ingreso) |>
  group_by(ANO4, SEMESTRE, grupo) |>
  summarise(delta_w = sum(peso_ingreso * delta_ing, na.rm = TRUE),
            .groups = "drop") |>
  group_by(ANO4, SEMESTRE) |>
  mutate(share   = delta_w / sum(delta_w),
         periodo = ANO4 + (SEMESTRE - 1) * 0.5) |>
  ungroup() |>
  mutate(grupo = factor(grupo, levels = orden_grupos))

delta_bars_lp <- delta_grupo_sem_lp |>
  arrange(periodo, desc(as.integer(grupo))) |>
  group_by(periodo) |>
  mutate(ymax = cumsum(share),
         ymin = ymax - share,
         ymid = (ymin + ymax) / 2) |>
  ungroup() |>
  mutate(xmin = periodo - 0.275, xmax = periodo + 0.275)

delta_inside_lbl_lp <- delta_bars_lp |> filter(share >= 0.05)

# pestaña "Distribución del ingreso" ####

# colores compartidos con el resto del repo
col_orig     <- "#1f78b4"
col_aj       <- "#e31a1c"
color_barras <- "#41b6c4"

# cambio % del ingreso medio por categoría ocupacional y semestre (heatmap).
# se calcula acá, igual que el 06, a partir de datos_app_densidades.
cat_occ <- datos |>
  filter(!is.na(ingreso), ingreso > 0, !is.na(peso_ingreso), peso_ingreso > 0) |>
  mutate(SEMESTRE = if_else(TRIMESTRE %in% c(1L, 2L), 1L, 2L)) |>
  group_by(ANO4, SEMESTRE, grupo) |>
  summarise(media_antes   = weighted.mean(ingreso,      peso_ingreso),
            media_despues = weighted.mean(ingreso_post, peso_ingreso),
            .groups = "drop") |>
  mutate(cambio_pct  = media_despues / media_antes - 1,
         periodo     = ANO4 + (SEMESTRE - 1) * 0.5,
         periodo_lbl = sem_label_fn(periodo))

orden_grupos_occ <- cat_occ |>
  group_by(grupo) |>
  summarise(m = mean(cambio_pct, na.rm = TRUE), .groups = "drop") |>
  arrange(m) |> pull(grupo)

cat_occ <- cat_occ |>
  mutate(grupo = factor(grupo, levels = orden_grupos_occ),
         periodo_lbl = factor(periodo_lbl,
                              levels = sem_label_fn(sort(unique(periodo)))))

# semestres disponibles para el selector de Lorenz / deciles
periodos_dist <- dist_lorenz |>
  distinct(periodo) |> arrange(periodo) |>
  mutate(lbl = sem_label_fn(periodo)) |> pull(lbl)

# interfaz usuaria pero acá estoy flojo de papeles

ui <- page_navbar(
  title = "Pobreza con ajuste por subdeclaración",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  header = tags$head(tags$style(HTML("
    .container, .container-fluid,
    .container-sm, .container-md, .container-lg, .container-xl, .container-xxl {
      max-width: 100% !important;
      padding-left: 0.75rem;
      padding-right: 0.75rem;
    }
    .bslib-page-navbar .navbar > .container { max-width: 100% !important; }
  "))),

  nav_panel(
    "Tasa de pobreza",
    card(
      full_screen = TRUE,
      card_header("Tasa de pobreza semestral (Total ARG): original vs ajustada vs INDEC"),
      girafeOutput("plot_pobreza_total", height = "1100px", width = "100%")
    )
  ),

  nav_panel(
    "Densidades",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,

        selectInput("periodo", "Trimestre:",
                    choices  = periodos,
                    selected = periodos[length(periodos)]),

        checkboxGroupInput("grupos", "Grupos a mostrar:",
                           choices  = niveles_grupo,
                           selected = niveles_grupo),

        radioButtons("tipo_plot", "Tipo de gráfico:",
                     choices = c("Acumulada (ECDF)" = "ecdf",
                                 "Histograma (Scott)" = "hist"),
                     selected = "ecdf")
      ),

      card(
        full_screen = TRUE,
        card_header("Distribución de ingresos"),
        plotOutput("plot", height = "560px")
      ),

      card(
        card_header("% de cada grupo por encima del umbral (recibe ajuste por brecha > 1)"),
        plotOutput("plot_pct", height = "360px")
      ),

      card(
        card_header("Información del trimestre"),
        textOutput("info_periodo"),
        textOutput("info_umbral"),
        textOutput("info_n")
      )
    )
  ),

  nav_panel(
    "Hogares que mejoraron",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,

        selectInput("periodo_hog", "Semestre:",
                    choices  = periodos_sem,
                    selected = periodos_sem[length(periodos_sem)]),

        helpText(tags$small(
          "Hogares que pasaron a una situación mejor al aplicar el ajuste. ",
          "Una fila por hogar-semestre."
        ))
      ),

      card(
        card_header("Resumen del semestre"),
        textOutput("info_hog_resumen")
      ),

      card(
        full_screen = TRUE,
        card_header("Detalle de hogares"),
        DTOutput("tabla_hogares")
      )
    )
  ),

  nav_panel(
    "Antes vs. después",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,

        selectInput("grupo_cmp", "Grupo:",
                    choices  = niveles_grupo,
                    selected = "Asal. priv. registrado"),

        selectInput("periodo_cmp", "Trimestre:",
                    choices  = periodos,
                    selected = periodos[length(periodos)]),

        radioButtons("tipo_plot_cmp", "Tipo de gráfico:",
                     choices = c("Acumulada (ECDF)" = "ecdf",
                                 "Histograma (Scott)" = "hist"),
                     selected = "ecdf")
      ),

      card(
        full_screen = TRUE,
        card_header("Distribución de ingresos individuales — antes vs. después del ajuste"),
        plotOutput("plot_cmp", height = "600px")
      )
    )
  ),

  nav_panel(
    "Pobreza: ITF / CBT",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,

        selectInput("periodo_pob", "Trimestre:",
                    choices  = periodos,
                    selected = periodos[length(periodos)]),

        helpText(tags$small(
          "ECDF del ingreso total familiar por adulto equivalente (ITF / adequí ",
          "del hogar), expresado en múltiplos de la Canasta Básica Total del hogar, ",
          "antes y después del ajuste por subdeclaración. Una fila por persona, ",
          "ponderada por PONDIH. La línea roja punteada en 1 es la CBT: a su ",
          "izquierda quedan las personas pobres, a su derecha las no pobres."
        ))
      ),

      card(
        full_screen = TRUE,
        card_header("ECDF de ITF por adulto equivalente / CBT del hogar — antes vs. después"),
        plotOutput("plot_ecdf_pob", height = "620px")
      ),

      card(
        card_header("Personas bajo la línea de pobreza"),
        textOutput("info_pob_ecdf")
      )
    )
  ),

  nav_panel(
    "Sensibilidad",
    layout_sidebar(
      sidebar = sidebar(
        width = 320,

        radioButtons("escenario_sensib", "Escenario:",
          choices = c("% Ajustada (post-subdeclaración)" = "hc_aj",
                      "% Original"                        = "hc_orig",
                      "Δ pp (ajustada − original)"        = "delta_pp"),
          selected = "hc_aj"),

        helpText(tags$small(
          "% de personas con ingreso del hogar (ITF) por debajo de cada múltiplo ",
          "de la línea de pobreza (k·CBT del hogar), por semestre. 1 LP = línea ",
          "oficial (resaltada). Δ pp negativo = el ajuste baja la pobreza en ese umbral."
        ))
      ),

      card(
        full_screen = TRUE,
        card_header("Pobreza bajo distintos múltiplos de la línea — original vs ajustada"),
        gt_output("tabla_sensib")
      )
    )
  ),

  nav_panel(
    "Distribución del ingreso",
    navset_card_tab(
      full_screen = TRUE,
      title = "Efecto del ajuste sobre la distribución del ingreso (IPCF, ponderado por PONDIH)",

      nav_panel(
        "Gini",
        plotOutput("dist_gini", height = "620px"),
        helpText(tags$small(
          "Coeficiente de Gini del ingreso per cápita familiar, original vs ajustado. ",
          "Las barras son el Δ Gini (eje derecho): Δ > 0 ⇒ el ajuste aumenta la desigualdad medida."
        ))
      ),

      nav_panel(
        "Media y mediana",
        plotOutput("dist_media_mediana", height = "620px"),
        helpText(tags$small(
          "Variación % del IPCF entre el escenario ajustado y el original, por semestre. ",
          "Media > mediana ⇒ el ajuste se concentra en la parte alta de la distribución."
        ))
      ),

      nav_panel(
        "Brecha D10/D1",
        plotOutput("dist_brecha", height = "620px"),
        helpText(tags$small(
          "Cociente entre el IPCF medio del decil más rico y el del más pobre, ",
          "original vs ajustado. Deciles re-rankeados dentro de cada escenario."
        ))
      ),

      nav_panel(
        "Por categoría",
        plotOutput("dist_categoria", height = "620px"),
        helpText(tags$small(
          "Cambio % del ingreso individual medio (laboral o jubilatorio) por categoría ",
          "ocupacional y semestre. Más oscuro = el ajuste lo sube más."
        ))
      ),

      nav_panel(
        "Lorenz y deciles",
        layout_sidebar(
          sidebar = sidebar(
            width = 300,
            selectInput("periodo_dist", "Semestre:",
                        choices  = periodos_dist,
                        selected = periodos_dist[length(periodos_dist)]),
            helpText(tags$small(
              "Curva de Lorenz y participación en el ingreso total por decil de IPCF, ",
              "antes y después del ajuste, para el semestre elegido."
            ))
          ),
          layout_columns(
            col_widths = c(6, 6),
            card(card_header("Curva de Lorenz"),
                 plotOutput("dist_lorenz", height = "560px")),
            card(card_header("Participación por decil"),
                 plotOutput("dist_deciles", height = "560px"))
          )
        )
      )
    )
  )
)

# Server

server <- function(input, output, session) {

  # Pestaña: Tasa de pobreza

  output$plot_pobreza_total <- renderGirafe({
    tot_s <- pobreza_serie

    color_barras <- "#41b6c4"
    col_orig <- "#1f78b4"; col_aj <- "#e31a1c"
    col_indec <- "#33a02c"; col_indec_hist <- "#6a3d9a"

    max_tasa     <- max(c(tot_s$pobreza_original, tot_s$tasa_indec), na.rm = TRUE)
    max_delta    <- max(tot_s$delta_pp)
    scale_factor <- (max_tasa * 0.16) / max_delta

    # serie oficial partida en hist (2003-06, otra canasta) y vigente (2016+): son
    # tipos distintos para que geom_line no los una cruzando la intervención, que
    # queda excluida (indec_grupo == "intervencion", valores subdeclarados).
    df_lines <- bind_rows(
      tot_s |> transmute(ANO4, SEMESTRE, periodo, tasa = pobreza_original, tipo = "Original"),
      tot_s |> transmute(ANO4, SEMESTRE, periodo, tasa = pobreza_ajustada, tipo = "Ajustada"),
      tot_s |> filter(indec_grupo == "hist")   |> transmute(ANO4, SEMESTRE, periodo, tasa = tasa_indec, tipo = "INDEC 2003-06"),
      tot_s |> filter(indec_grupo == "actual") |> transmute(ANO4, SEMESTRE, periodo, tasa = tasa_indec, tipo = "INDEC oficial")
    ) |>
      mutate(tipo = factor(tipo, levels = c("Original", "Ajustada", "INDEC oficial", "INDEC 2003-06"))) |>
      filter(!is.na(tasa))

    gap_xmin <- 2015.25; gap_xmax <- 2016.25

    # mismos breaks de año para los tres paneles (comparten los 42 semestres)
    brks_anio <- anio_breaks_labels(tot_s$periodo)

    g <- ggplot() +
      annotate("rect", xmin = gap_xmin, xmax = gap_xmax, ymin = -Inf, ymax = Inf,
               fill = "grey88", alpha = 0.6) +
      annotate("text", x = (gap_xmin + gap_xmax) / 2, y = max_tasa * 0.55,
               label = "apagón\nestadístico\n2015–2016", size = 2.6,
               color = "grey45", fontface = "italic", lineheight = 0.9) +
      annotate("text", x = 2010.5, y = max_tasa * 0.9,
               label = "Intervención del INDEC: serie oficial\nintervenida (subdeclarada), no se grafica",
               size = 3, color = "grey45", fontface = "italic", lineheight = 0.95) +
      geom_col_interactive(
        data = tot_s,
        aes(periodo, delta_pp * scale_factor,
            tooltip = sprintf("S%d %d · Δ ajuste = %.2f pp",
                              SEMESTRE, ANO4, delta_pp),
            data_id = as.character(periodo)),
        fill = color_barras, color = NA, width = 0.16, alpha = 0.85
      ) +
      geom_text_repel(
        data = tot_s,
        aes(periodo, delta_pp * scale_factor,
            label = sprintf("%.2f", delta_pp)),
        color = color_barras, size = 3.2, fontface = "bold",
        nudge_y = 0.008, direction = "y", min.segment.length = Inf, seed = 3
      ) +
      geom_line(data = df_lines,
                aes(periodo, tasa, color = tipo, linetype = tipo),
                linewidth = 0.7) +
      geom_point_interactive(
        data = df_lines,
        aes(periodo, tasa, color = tipo,
            tooltip = sprintf("S%d %d · %s: %s",
                              SEMESTRE, ANO4, tipo,
                              label_percent(accuracy = 0.1)(tasa)),
            data_id = as.character(periodo)),
        size = 1.9
      ) +
      geom_text_repel(
        data = df_lines |> filter(tipo == "Original"),
        aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
        color = col_orig, size = 3.2,
        nudge_y = 0.020, direction = "y", min.segment.length = 0,
        segment.size = 0.2, segment.alpha = 0.4, seed = 1
      ) +
      geom_text_repel(
        data = df_lines |> filter(tipo == "Ajustada"),
        aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
        color = col_aj, size = 3.2,
        nudge_y = -0.020, direction = "y", min.segment.length = 0,
        segment.size = 0.2, segment.alpha = 0.4, seed = 2
      ) +
      geom_text_repel(
        data = df_lines |> filter(tipo == "INDEC oficial"),
        aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
        color = col_indec, size = 2.8, fontface = "italic",
        nudge_y = 0.038, direction = "y", min.segment.length = 0,
        segment.size = 0.2, segment.alpha = 0.4, seed = 4
      ) +
      geom_text_repel(
        data = df_lines |> filter(tipo == "INDEC 2003-06"),
        aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
        color = col_indec_hist, size = 2.8, fontface = "italic",
        nudge_y = -0.030, direction = "y", min.segment.length = 0,
        segment.size = 0.2, segment.alpha = 0.4, seed = 5
      ) +
      scale_y_continuous(
        labels = label_percent(accuracy = 1),
        sec.axis = sec_axis(~ . / scale_factor,
                            name = "Δ pp (Original − Ajustada)",
                            labels = label_number(accuracy = 0.1))
      ) +
      scale_x_continuous(
        breaks = brks_anio$at,
        labels = as.character(brks_anio$anio)
      ) +
      scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj,
                                    "INDEC oficial" = col_indec,
                                    "INDEC 2003-06" = col_indec_hist)) +
      scale_linetype_manual(values = c(Original = "solid", Ajustada = "dashed",
                                       "INDEC oficial" = "dotdash",
                                       "INDEC 2003-06" = "dotdash")) +
      labs(x = NULL, y = "Tasa de pobreza",
           color = NULL, linetype = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            panel.grid.minor = element_blank(),
            axis.title.x = element_blank())

    g_share <- ggplot() +
      geom_rect_interactive(
        data = delta_bars,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
            fill = grupo,
            tooltip = sprintf("S%d %d · %s: %s del Δ ITF",
                              SEMESTRE, ANO4, grupo,
                              label_percent(accuracy = 0.1)(share)),
            data_id = as.character(periodo)),
        color = "white", linewidth = 0.15
      ) +
      geom_text(
        data = delta_inside_lbl,
        aes(x = periodo, y = ymid,
            label = label_percent(accuracy = 1)(share)),
        inherit.aes = FALSE,
        color = "white", size = 3, fontface = "bold"
      ) +
      scale_y_continuous(labels = label_percent(accuracy = 1),
                         expand = expansion(mult = c(0, 0.02))) +
      scale_x_continuous(
        breaks = brks_anio$at,
        labels = as.character(brks_anio$anio)
      ) +
      scale_fill_manual(values = paleta_10, drop = FALSE, guide = "none") +
      scale_color_manual(values = paleta_10, drop = FALSE, guide = "none") +
      labs(x = NULL, y = "Composición del Δ ITF") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.title.x = element_blank())

    g_left_poverty <- ggplot() +
      geom_rect_interactive(
        data = delta_bars_lp,
        aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
            fill = grupo,
            tooltip = sprintf("S%d %d · %s: %s del Δ ITF (hogares que salieron de la pobreza)",
                              SEMESTRE, ANO4, grupo,
                              label_percent(accuracy = 0.1)(share)),
            data_id = as.character(periodo)),
        color = "white", linewidth = 0.15
      ) +
      geom_text(
        data = delta_inside_lbl_lp,
        aes(x = periodo, y = ymid,
            label = label_percent(accuracy = 1)(share)),
        inherit.aes = FALSE,
        color = "white", size = 3, fontface = "bold"
      ) +
      scale_y_continuous(labels = label_percent(accuracy = 1),
                         expand = expansion(mult = c(0, 0.02))) +
      scale_x_continuous(
        breaks = brks_anio$at,
        labels = as.character(brks_anio$anio)
      ) +
      # leyenda clásica abajo (en vez de las etiquetas ggrepel sobre las barras):
      # documenta los grupos de los dos paneles apilados sin tapar el diseño
      scale_fill_manual(values = paleta_10, drop = FALSE,
                        guide = guide_legend(nrow = 2, byrow = TRUE)) +
      labs(x = NULL, y = "Composición del Δ ITF — hogares que salieron de la pobreza",
           fill = NULL,
           caption = "Fuentes: EPH continua + brecha MLER/EPH por percentil + Cuadro 1 INDEC. Ponderado por PONDIH (líneas) y peso de ingreso (composición).") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom",
            legend.title = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())

    combined <- g / g_share / g_left_poverty + plot_layout(heights = c(2.2, 1, 1))

    girafe(
      ggobj      = combined,
      width_svg  = 18,
      height_svg = 12,
      options = list(
        opts_sizing(rescale = TRUE, width = 1),
        opts_hover(css = "stroke-width:2.5;opacity:1;"),
        opts_hover_inv(css = "opacity:0.25;"),
        opts_selection(type = "none"),
        opts_tooltip(css = "background:#222;color:#fff;padding:6px 8px;border-radius:4px;font-family:sans-serif;font-size:12px;"),
        opts_toolbar(saveaspng = FALSE)
      )
    )
  })

  periodo_actual <- reactive({
    p <- strsplit(input$periodo, "-T")[[1]]
    list(ano = as.integer(p[1]), tri = as.integer(p[2]))
  })

  datos_filtrados <- reactive({
    p <- periodo_actual()
    datos |>
      filter(ANO4 == p$ano, TRIMESTRE == p$tri,
             grupo %in% input$grupos) |>
      mutate(grupo = factor(grupo, levels = niveles_grupo))
  })

  umbral_actual <- reactive({
    p <- periodo_actual()
    umbrales |> filter(ANO4 == p$ano, TRIMESTRE == p$tri)
  })

  output$plot <- renderPlot({
    d <- datos_filtrados()
    u <- umbral_actual()
    umbral_x <- u$umbral[1]

    if (input$tipo_plot == "hist") {
      d_plot <- d |> mutate(x_plot = log10(ingreso))
      umbral_plot <- if (!is.na(umbral_x)) log10(umbral_x) else NA_real_

      bw <- scott_bw(d_plot$x_plot, d_plot$peso_ingreso)
      if (!is.finite(bw) || bw <= 0) bw <- 0.1

      p <- ggplot(d_plot, aes(x = x_plot, weight = peso_ingreso,
                              color = grupo, fill = grupo)) +
        geom_histogram(binwidth = bw, position = "identity",
                       alpha = 0.25, linewidth = 0.25) +
        scale_y_continuous(
          labels = label_number(big.mark = ".", decimal.mark = ",")
        ) +
        labs(y = "Frecuencia ponderada")
    } else {
      ecdf_df <- d |>
        arrange(grupo, ingreso) |>
        group_by(grupo) |>
        mutate(cum_w = cumsum(peso_ingreso) / sum(peso_ingreso)) |>
        ungroup() |>
        mutate(x_plot = ingreso)
      umbral_plot <- umbral_x

      p <- ggplot(ecdf_df, aes(x = x_plot, y = cum_w, color = grupo)) +
        geom_step(linewidth = 0.7) +
        scale_y_continuous(labels = label_percent(),
                           breaks = seq(0, 1, 0.25)) +
        labs(y = "Acumulada (% del grupo ≤ x)")
    }

    p <- p +
      scale_color_manual(values = paleta_10, drop = FALSE) +
      scale_fill_manual(values  = paleta_10, drop = FALSE) +
      labs(x = "Ingreso (pesos corrientes)",
           color = NULL, fill = NULL,
           subtitle = sprintf(
             "Asalariados, cuentapropistas, patrones y jubilados — %s",
             input$periodo)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.title = element_blank())

    if (!is.na(umbral_plot)) {
      p <- p +
        annotate("rect",
                 xmin = umbral_plot, xmax = Inf,
                 ymin = -Inf, ymax = Inf,
                 fill = "#E15759", alpha = 0.10) +
        geom_vline(xintercept = umbral_plot,
                   linetype = "dashed", color = "#E15759", linewidth = 0.6)
    }

    if (input$tipo_plot == "hist") {
      breaks_pesos <- c(100, 1000, 10000, 100000, 1000000)
      p <- p + scale_x_continuous(
        breaks = log10(breaks_pesos),
        labels = formatC(breaks_pesos, big.mark = ".",
                         decimal.mark = ",", format = "d")
      )
    } else {
      p <- p + scale_x_log10(
        labels = label_number(big.mark = ".", decimal.mark = ",")
      )
    }

    p
  })

  porc_derecha <- reactive({
    d <- datos_filtrados()
    u <- umbral_actual()
    umbral_x <- u$umbral[1]
    if (is.na(umbral_x) || nrow(d) == 0) return(NULL)

    d |>
      group_by(grupo, .drop = FALSE) |>
      summarise(
        total        = sum(peso_ingreso, na.rm = TRUE),
        sobre_umbral = sum(peso_ingreso[ingreso > umbral_x], na.rm = TRUE),
        .groups      = "drop"
      ) |>
      mutate(pct = if_else(total > 0, sobre_umbral / total, NA_real_)) |>
      filter(!is.na(pct))
  })

  output$plot_pct <- renderPlot({
    d <- porc_derecha()
    if (is.null(d) || nrow(d) == 0) return(NULL)

    ggplot(d, aes(x = pct, y = reorder(grupo, pct), fill = grupo)) +
      geom_col(width = 0.7, alpha = 0.85) +
      geom_text(aes(label = label_percent(accuracy = 0.1)(pct)),
                hjust = -0.12, size = 4.2, color = "grey25") +
      scale_x_continuous(labels = label_percent(),
                         expand = expansion(mult = c(0, 0.18))) +
      scale_fill_manual(values = paleta_10, drop = FALSE) +
      labs(x = "% de la masa del grupo por encima del umbral",
           y = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor   = element_blank())
  })

  output$info_periodo <- renderText({
    p <- periodo_actual()
    sprintf("Período: %d trimestre %d", p$tri, p$ano)
  })

  output$info_umbral <- renderText({
    u <- umbral_actual()
    if (nrow(u) == 0 || is.na(u$umbral[1])) {
      return("Umbral de brecha > 1: no disponible para este período.")
    }
    sprintf(
      "Umbral de brecha > 1: $%s (corresponde al percentil %d de la distribución de asalariados privados registrados sin serv. doméstico).",
      format(round(u$umbral[1]), big.mark = ".", decimal.mark = ","),
      u$pct_min[1]
    )
  })

  output$info_n <- renderText({
    d <- datos_filtrados()
    sprintf("Observaciones en el panel: %s (suma de personas-pasada por todos los grupos visibles).",
            format(nrow(d), big.mark = ".", decimal.mark = ","))
  })

  # Pestaña: Hogares que mejoraron

  hogares_periodo <- reactive({
    hogares_mejor |> filter(periodo_lbl == input$periodo_hog)
  })

  output$info_hog_resumen <- renderText({
    d <- hogares_periodo()
    if (nrow(d) == 0) return("Sin hogares para este semestre.")
    conteo <- d |> count(tipo_movimiento)
    desglose <- paste(
      sprintf("%s: %s",
              conteo$tipo_movimiento,
              format(conteo$n, big.mark = ".", decimal.mark = ",")),
      collapse = " · "
    )
    sprintf("Total: %s hogares que mejoraron. %s",
            format(nrow(d), big.mark = ".", decimal.mark = ","),
            desglose)
  })

  output$tabla_hogares <- renderDT({
    d <- hogares_periodo() |>
      select(
        Aglomerado         = aglomerado_nombre,
        CODUSU             = CODUSU,
        NRO_HOGAR          = NRO_HOGAR,
        Miembros           = miembros,
        Adequi             = adequi_hogar,
        `ITF orig`         = ITF,
        `ITF ajust`        = ITF_ajustado,
        Aumento            = delta_ITF,
        `% aumento`        = pct_aumento,
        `CBT hogar`        = CBT_hogar,
        `Margen orig`      = margen_orig,
        `Margen ajust`     = margen_aj,
        `Sit. original`    = situacion_original,
        `Sit. ajustada`    = situacion_ajustada,
        `Tipo movimiento`  = tipo_movimiento
      )

    datatable(
      d,
      filter   = "top",
      rownames = FALSE,
      class    = "compact stripe hover",
      options  = list(
        pageLength = 25,
        scrollX    = TRUE,
        order      = list(list(7, "desc"))
      )
    ) |>
      formatCurrency(
        c("ITF orig", "ITF ajust", "Aumento",
          "CBT hogar", "Margen orig", "Margen ajust"),
        currency = "$ ", mark = ".", dec.mark = ",", digits = 0
      ) |>
      formatPercentage("% aumento", 1) |>
      formatRound("Adequi", 2) |>
      formatStyle(
        c("Sit. original", "Sit. ajustada"),
        backgroundColor = styleEqual(
          c("indigente", "pobre", "no_pobre"),
          c("#E15759", "#F28E2B", "#59A14F")
        ),
        color = "white", fontWeight = "bold"
      )
  })

  # Pestaña: Antes vs. después

  paleta_cmp <- c("Antes" = "#1f78b4", "Después" = "#e31a1c")

  datos_cmp <- reactive({
    p <- strsplit(input$periodo_cmp, "-T")[[1]]
    ano <- as.integer(p[1]); tri <- as.integer(p[2])
    datos |>
      filter(ANO4 == ano, TRIMESTRE == tri,
             grupo == input$grupo_cmp) |>
      select(ingreso, ingreso_post, peso_ingreso) |>
      pivot_longer(c(ingreso, ingreso_post),
                   names_to = "momento", values_to = "x") |>
      mutate(momento = factor(
        if_else(momento == "ingreso", "Antes", "Después"),
        levels = c("Antes", "Después")
      ))
  })

  output$plot_cmp <- renderPlot({
    d <- datos_cmp()
    if (nrow(d) == 0) return(NULL)

    if (input$tipo_plot_cmp == "hist") {
      d_plot <- d |> mutate(x_log = log10(x))
      bw <- scott_bw(d_plot$x_log, d_plot$peso_ingreso)
      if (!is.finite(bw) || bw <= 0) bw <- 0.1

      p <- ggplot(d_plot, aes(x = x_log, weight = peso_ingreso,
                              color = momento, fill = momento)) +
        geom_histogram(binwidth = bw, position = "identity",
                       alpha = 0.4, linewidth = 0.25) +
        scale_y_continuous(
          labels = label_number(big.mark = ".", decimal.mark = ",")
        ) +
        labs(y = "Frecuencia ponderada")

      breaks_pesos <- c(100, 1000, 10000, 100000, 1000000)
      p <- p + scale_x_continuous(
        breaks = log10(breaks_pesos),
        labels = formatC(breaks_pesos, big.mark = ".",
                         decimal.mark = ",", format = "d")
      )
    } else {
      ecdf_df <- d |>
        arrange(momento, x) |>
        group_by(momento) |>
        mutate(cum_w = cumsum(peso_ingreso) / sum(peso_ingreso)) |>
        ungroup()

      p <- ggplot(ecdf_df, aes(x = x, y = cum_w, color = momento)) +
        geom_step(linewidth = 0.8, alpha = 0.85) +
        scale_y_continuous(labels = label_percent(),
                           breaks = seq(0, 1, 0.25)) +
        scale_x_log10(
          labels = label_number(big.mark = ".", decimal.mark = ",")
        ) +
        labs(y = "Acumulada (% del grupo ≤ x)")
    }

    p +
      scale_color_manual(values = paleta_cmp) +
      scale_fill_manual(values  = paleta_cmp) +
      labs(x = "Ingreso individual (pesos corrientes)",
           color = NULL, fill = NULL,
           subtitle = sprintf("%s — %s",
                              input$grupo_cmp, input$periodo_cmp)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.title = element_blank())
  })

  # Pestaña: Pobreza ITF / CBT (ECDF del ingreso por adulto equiv. en múltiplos de la CBT)

  # rojo reservado para la línea de la CBT, así que las curvas van en azul/verde
  paleta_pob <- c("Antes (original)"   = "#1f78b4",
                  "Después (ajustado)" = "#33a02c")

  ecdf_pob_periodo <- reactive({
    ecdf_pob |> filter(periodo_lbl == input$periodo_pob)
  })

  output$plot_ecdf_pob <- renderPlot({
    d <- ecdf_pob_periodo()
    if (nrow(d) == 0) return(NULL)

    # piso para que las personas con ITF == 0 (ratio 0) caigan en el extremo
    # izquierdo del eje log en vez de desaparecer; su masa ya queda contada en
    # la acumulada porque ordenamos por el ratio original.
    piso <- 0.05

    ecdf_df <- d |>
      select(ratio_antes, ratio_despues, PONDIH) |>
      pivot_longer(c(ratio_antes, ratio_despues),
                   names_to = "momento", values_to = "ratio") |>
      mutate(momento = factor(
        if_else(momento == "ratio_antes", "Antes (original)", "Después (ajustado)"),
        levels = c("Antes (original)", "Después (ajustado)")
      )) |>
      arrange(momento, ratio) |>
      group_by(momento) |>
      mutate(cum_w  = cumsum(PONDIH) / sum(PONDIH),
             x_plot = pmax(ratio, piso)) |>
      ungroup()

    breaks_x <- c(0.05, 0.1, 0.25, 0.5, 1, 2, 4, 8, 16)
    labels_x <- c("0", "0,1", "0,25", "0,5", "1", "2", "4", "8", "16")

    ggplot(ecdf_df, aes(x = x_plot, y = cum_w, color = momento)) +
      annotate("rect", xmin = piso, xmax = 1, ymin = -Inf, ymax = Inf,
               fill = "#E15759", alpha = 0.07) +
      geom_step(linewidth = 0.9, alpha = 0.9) +
      geom_vline(xintercept = 1, linetype = "dashed",
                 color = "#E15759", linewidth = 0.7) +
      annotate("text", x = 1, y = 0.03, label = "CBT",
               color = "#E15759", hjust = -0.18, vjust = 0,
               fontface = "bold", size = 4.2) +
      scale_x_log10(breaks = breaks_x, labels = labels_x) +
      scale_y_continuous(labels = label_percent(), breaks = seq(0, 1, 0.25)) +
      scale_color_manual(values = paleta_pob) +
      labs(x = "ITF por adulto equivalente, en múltiplos de la CBT del hogar (escala log)",
           y = "Acumulada (% de personas ≤ x)",
           color = NULL,
           subtitle = sprintf(
             "Ingreso familiar por adulto equivalente sobre la línea de pobreza — %s",
             input$periodo_pob)) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", legend.title = element_blank(),
            panel.grid.minor = element_blank())
  })

  output$info_pob_ecdf <- renderText({
    d <- ecdf_pob_periodo()
    if (nrow(d) == 0) return("Sin datos para este trimestre.")
    w  <- sum(d$PONDIH, na.rm = TRUE)
    pa <- sum(d$PONDIH[d$ratio_antes   < 1], na.rm = TRUE) / w
    pd <- sum(d$PONDIH[d$ratio_despues < 1], na.rm = TRUE) / w
    sprintf(
      "Personas con ITF < CBT del hogar (pobres + indigentes): %s antes → %s después del ajuste (Δ %.1f pp). Población ponderada (PONDIH): %s.",
      label_percent(accuracy = 0.1)(pa),
      label_percent(accuracy = 0.1)(pd),
      (pd - pa) * 100,
      format(round(w), big.mark = ".", decimal.mark = ",")
    )
  })

  # Pestaña: Sensibilidad (tabla gt de pobreza por múltiplo de la línea)

  output$tabla_sensib <- render_gt({
    metric   <- input$escenario_sensib
    es_delta <- metric == "delta_pp"

    df <- sensib |>
      arrange(periodo, k) |>
      mutate(periodo_lbl = sem_label_fn(periodo)) |>
      select(periodo, periodo_lbl, k_lbl, valor = all_of(metric)) |>
      pivot_wider(names_from = k_lbl, values_from = valor) |>
      arrange(periodo) |>
      select(-periodo)

    cols_k <- setdiff(names(df), "periodo_lbl")  # "0,5 LP" ... "2 LP", en orden

    g <- df |>
      gt(rowname_col = "periodo_lbl") |>
      tab_header(
        title    = "Sensibilidad de la pobreza al valor de la línea",
        subtitle = "% de personas con ITF del hogar por debajo de k líneas de pobreza (k·CBT), por semestre"
      ) |>
      tab_spanner(label   = "Líneas de pobreza (múltiplos de la CBT del hogar)",
                  columns = all_of(cols_k)) |>
      tab_source_note(
        "Fuente: EPH continua + brecha MLER/EPH por percentil. Ponderado por PONDIH."
      )

    if (es_delta) {
      lim <- max(abs(as.matrix(df[cols_k])), na.rm = TRUE)
      g <- g |>
        fmt_number(columns = all_of(cols_k), decimals = 1) |>
        data_color(columns = all_of(cols_k),
                   palette = c("#2166ac", "white", "#b2182b"),
                   domain  = c(-lim, lim))
    } else {
      g <- g |> fmt_percent(columns = all_of(cols_k), decimals = 1)
    }

    # resaltar la columna de la línea oficial (1 LP) = "current rate" del cuadro.
    # en modo Δ el fondo lo pone data_color, así que ahí solo va negrita + encabezado.
    g <- g |>
      tab_style(style     = cell_text(weight = "bold"),
                locations = cells_body(columns = "1 LP")) |>
      tab_style(style     = list(cell_fill(color = "#41b6c4"),
                                 cell_text(color = "white", weight = "bold")),
                locations = cells_column_labels(columns = "1 LP"))

    if (!es_delta) {
      g <- g |>
        tab_style(style     = cell_fill(color = "#d8f0f3"),
                  locations = cells_body(columns = "1 LP"))
    }

    g |>
      opt_row_striping() |>
      tab_options(container.height        = px(720),
                  table.font.size         = px(13),
                  data_row.padding        = px(3),
                  heading.title.font.size = px(16))
  })

  # Pestaña: Distribución del ingreso

  output$dist_gini <- renderPlot({
    long <- bind_rows(
      dist_serie |> transmute(periodo, escenario = "Original", gini = gini_Original),
      dist_serie |> transmute(periodo, escenario = "Ajustada", gini = gini_Ajustada)
    ) |>
      mutate(escenario = factor(escenario, levels = c("Original", "Ajustada")))

    max_g  <- max(long$gini, na.rm = TRUE)
    max_dg <- max(abs(dist_serie$delta_gini), na.rm = TRUE)
    fac_g  <- (max_g * 0.18) / max_dg

    ggplot() +
      geom_col(data = dist_serie, aes(periodo, delta_gini * fac_g),
               fill = color_barras, color = NA, width = 0.16, alpha = 0.85) +
      geom_text_repel(data = dist_serie,
                      aes(periodo, delta_gini * fac_g,
                          label = sprintf("%+.3f", delta_gini)),
                      color = color_barras, size = 2.8, fontface = "bold",
                      nudge_y = max_g * 0.02, direction = "y",
                      min.segment.length = Inf, seed = 1) +
      geom_line(data = long, aes(periodo, gini, color = escenario, linetype = escenario),
                linewidth = 0.7) +
      geom_point(data = long, aes(periodo, gini, color = escenario), size = 1.9) +
      geom_text_repel(data = long |> filter(escenario == "Original"),
                      aes(periodo, gini, label = sprintf("%.3f", gini)),
                      color = col_orig, size = 2.8, nudge_y = 0.012, direction = "y",
                      min.segment.length = 0, segment.alpha = 0.4, seed = 2) +
      geom_text_repel(data = long |> filter(escenario == "Ajustada"),
                      aes(periodo, gini, label = sprintf("%.3f", gini)),
                      color = col_aj, size = 2.8, nudge_y = -0.012, direction = "y",
                      min.segment.length = 0, segment.alpha = 0.4, seed = 3) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . / fac_g, name = "Δ Gini (Ajustada − Original)",
                            labels = label_number(accuracy = 0.001))) +
      scale_x_continuous(breaks = sort(unique(dist_serie$periodo)), labels = sem_label_fn) +
      scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
      scale_linetype_manual(values = c(Original = "solid", Ajustada = "dashed")) +
      labs(x = NULL, y = "Coeficiente de Gini", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$dist_media_mediana <- renderPlot({
    cl <- dist_serie |>
      select(periodo, Media = cambio_media, Mediana = cambio_mediana) |>
      pivot_longer(c(Media, Mediana), names_to = "estadistico", values_to = "cambio") |>
      mutate(estadistico = factor(estadistico, levels = c("Media", "Mediana")))

    ggplot(cl, aes(periodo, cambio, color = estadistico, linetype = estadistico)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) +
      geom_text_repel(aes(label = label_percent(accuracy = 0.1)(cambio)),
                      size = 2.7, show.legend = FALSE, direction = "y",
                      min.segment.length = 0, segment.alpha = 0.35, seed = 4) +
      scale_y_continuous(labels = label_percent(accuracy = 0.1)) +
      scale_x_continuous(breaks = sort(unique(dist_serie$periodo)), labels = sem_label_fn) +
      scale_color_manual(values = c(Media = col_aj, Mediana = "#6a3d9a")) +
      scale_linetype_manual(values = c(Media = "solid", Mediana = "dashed")) +
      labs(x = NULL, y = "Variación %", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$dist_brecha <- renderPlot({
    long <- bind_rows(
      dist_serie |> transmute(periodo, escenario = "Original", gap = gap_d10_d1_Original),
      dist_serie |> transmute(periodo, escenario = "Ajustada", gap = gap_d10_d1_Ajustada)
    ) |>
      mutate(escenario = factor(escenario, levels = c("Original", "Ajustada")))

    max_b  <- max(long$gap, na.rm = TRUE)
    max_db <- max(abs(dist_serie$delta_gap), na.rm = TRUE)
    fac_b  <- (max_b * 0.18) / max_db

    ggplot() +
      geom_col(data = dist_serie, aes(periodo, delta_gap * fac_b),
               fill = color_barras, color = NA, width = 0.16, alpha = 0.85) +
      geom_text_repel(data = dist_serie,
                      aes(periodo, delta_gap * fac_b,
                          label = sprintf("%+.1f", delta_gap)),
                      color = color_barras, size = 2.8, fontface = "bold",
                      nudge_y = max_b * 0.02, direction = "y",
                      min.segment.length = Inf, seed = 5) +
      geom_line(data = long, aes(periodo, gap, color = escenario, linetype = escenario),
                linewidth = 0.7) +
      geom_point(data = long, aes(periodo, gap, color = escenario), size = 1.9) +
      geom_text_repel(data = long |> filter(escenario == "Original"),
                      aes(periodo, gap, label = sprintf("%.1f", gap)),
                      color = col_orig, size = 2.8, nudge_y = max_b * 0.03, direction = "y",
                      min.segment.length = 0, segment.alpha = 0.4, seed = 6) +
      geom_text_repel(data = long |> filter(escenario == "Ajustada"),
                      aes(periodo, gap, label = sprintf("%.1f", gap)),
                      color = col_aj, size = 2.8, nudge_y = -max_b * 0.03, direction = "y",
                      min.segment.length = 0, segment.alpha = 0.4, seed = 7) +
      scale_y_continuous(
        sec.axis = sec_axis(~ . / fac_b, name = "Δ brecha (Ajustada − Original)",
                            labels = label_number(accuracy = 0.1))) +
      scale_x_continuous(breaks = sort(unique(dist_serie$periodo)), labels = sem_label_fn) +
      scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
      scale_linetype_manual(values = c(Original = "solid", Ajustada = "dashed")) +
      labs(x = NULL, y = "Ingreso medio D10 / D1 (veces)", color = NULL, linetype = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))
  })

  output$dist_categoria <- renderPlot({
    ggplot(cat_occ, aes(periodo_lbl, grupo, fill = cambio_pct)) +
      geom_tile(color = "white", linewidth = 0.4) +
      geom_text(aes(label = label_percent(accuracy = 1)(cambio_pct)),
                size = 2.4, color = "grey15") +
      scale_fill_gradient(low = "#f7fbff", high = "#08519c",
                          labels = label_percent(accuracy = 1)) +
      labs(x = NULL, y = NULL, fill = "Cambio %") +
      theme_minimal(base_size = 13) +
      theme(panel.grid = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right")
  })

  periodo_dist_sel <- reactive({
    ps <- sort(unique(dist_lorenz$periodo))
    ps[match(input$periodo_dist, sem_label_fn(ps))]
  })

  output$dist_lorenz <- renderPlot({
    d <- dist_lorenz |>
      filter(periodo == periodo_dist_sel()) |>
      mutate(escenario = factor(escenario, levels = c("Original", "Ajustada")))
    if (nrow(d) == 0) return(NULL)

    ggplot(d, aes(p, L, color = escenario)) +
      geom_abline(slope = 1, intercept = 0, linetype = "dotted", color = "grey50") +
      geom_line(linewidth = 0.9) +
      scale_x_continuous(labels = label_percent(accuracy = 1)) +
      scale_y_continuous(labels = label_percent(accuracy = 1)) +
      scale_color_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
      coord_equal() +
      labs(x = "Proporción acumulada de población",
           y = "Proporción acumulada del ingreso", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank())
  })

  output$dist_deciles <- renderPlot({
    d <- dist_deciles |>
      filter(periodo == periodo_dist_sel()) |>
      mutate(escenario = factor(escenario, levels = c("Original", "Ajustada")))
    if (nrow(d) == 0) return(NULL)

    ggplot(d, aes(factor(decil), share, fill = escenario)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.72) +
      geom_text(aes(label = label_percent(accuracy = 0.1)(share)),
                position = position_dodge(width = 0.8), vjust = -0.4, size = 2.5,
                color = "grey25") +
      scale_y_continuous(labels = label_percent(accuracy = 1),
                         expand = expansion(mult = c(0, 0.08))) +
      scale_fill_manual(values = c(Original = col_orig, Ajustada = col_aj)) +
      labs(x = "Decil de IPCF", y = "Participación en el ingreso total", fill = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top", panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank())
  })
}

shinyApp(ui, server)

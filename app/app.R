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

# para los gráficos de semestrales (lo mismo de 03_graficos_pobreza_ajustada,R)
sem_label_fn <- function(x) {
  ano <- floor(x); sem <- round((x - ano) * 2) + 1
  sprintf("S%d %d", sem, ano)
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

# etiquetas a la derecha de la última barra pero quedan feas, las voy a sacar creo
delta_labels_last <- delta_grupo_sem |>
  filter(periodo == max(periodo)) |>
  arrange(desc(as.integer(grupo))) |>
  mutate(ymid = cumsum(share) - share / 2)

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
  )
)

# Server

server <- function(input, output, session) {

  # Pestaña: Tasa de pobreza

  output$plot_pobreza_total <- renderGirafe({
    tot_s <- pobreza_serie

    color_barras <- "#41b6c4"
    col_orig <- "#1f78b4"; col_aj <- "#e31a1c"; col_indec <- "#33a02c"

    max_tasa     <- max(c(tot_s$pobreza_original, tot_s$tasa_indec), na.rm = TRUE)
    max_delta    <- max(tot_s$delta_pp)
    scale_factor <- (max_tasa * 0.18) / max_delta

    df_long <- tot_s |>
      select(ANO4, SEMESTRE, periodo,
             pobreza_original, pobreza_ajustada, tasa_indec) |>
      pivot_longer(c(pobreza_original, pobreza_ajustada, tasa_indec),
                   names_to = "tipo", values_to = "tasa") |>
      mutate(tipo = recode(tipo,
                           pobreza_original = "Original",
                           pobreza_ajustada = "Ajustada",
                           tasa_indec       = "INDEC oficial"),
             tipo = factor(tipo, levels = c("Original", "Ajustada", "INDEC oficial"))) |>
      filter(!is.na(tasa))

    g <- ggplot() +
      geom_col_interactive(
        data = tot_s,
        aes(periodo, delta_pp * scale_factor,
            tooltip = sprintf("S%d %d · Δ ajuste = %.2f pp",
                              SEMESTRE, ANO4, delta_pp),
            data_id = as.character(periodo)),
        fill = color_barras, color = NA, width = 0.10, alpha = 0.85
      ) +
      geom_text_repel(
        data = tot_s,
        aes(periodo, delta_pp * scale_factor,
            label = sprintf("%.2f", delta_pp)),
        color = color_barras, size = 3.2, fontface = "bold",
        nudge_y = 0.008, direction = "y", min.segment.length = Inf, seed = 3
      ) +
      geom_line(data = df_long,
                aes(periodo, tasa, color = tipo, linetype = tipo),
                linewidth = 0.7) +
      geom_point_interactive(
        data = df_long,
        aes(periodo, tasa, color = tipo,
            tooltip = sprintf("S%d %d · %s: %s",
                              SEMESTRE, ANO4, tipo,
                              label_percent(accuracy = 0.1)(tasa)),
            data_id = as.character(periodo)),
        size = 1.9
      ) +
      geom_text_repel(
        data = df_long |> filter(tipo == "Original"),
        aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
        color = col_orig, size = 3.2,
        nudge_y = 0.020, direction = "y", min.segment.length = 0,
        segment.size = 0.2, segment.alpha = 0.4, seed = 1
      ) +
      geom_text_repel(
        data = df_long |> filter(tipo == "Ajustada"),
        aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
        color = col_aj, size = 3.2,
        nudge_y = -0.020, direction = "y", min.segment.length = 0,
        segment.size = 0.2, segment.alpha = 0.4, seed = 2
      ) +
      geom_text_repel(
        data = df_long |> filter(tipo == "INDEC oficial"),
        aes(periodo, tasa, label = label_percent(accuracy = 0.1)(tasa)),
        color = col_indec, size = 2.8, fontface = "italic",
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
      expand_limits(x = max(delta_grupo_sem$periodo) + 1.4) +
      labs(x = NULL, y = "Tasa de pobreza",
           color = NULL, linetype = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top",
            panel.grid.minor = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
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
      ggrepel::geom_text_repel(
        data = delta_labels_last,
        aes(x = periodo, y = ymid, label = grupo, color = grupo),
        inherit.aes = FALSE,
        hjust = 0, nudge_x = 0.35,
        direction = "y", min.segment.length = 0,
        segment.size = 0.3, segment.alpha = 0.5,
        fontface = "bold", size = 3.2,
        xlim = c(max(delta_grupo_sem$periodo) + 0.3, NA),
        show.legend = FALSE
      ) +
      scale_y_continuous(labels = label_percent(accuracy = 1),
                         expand = expansion(mult = c(0, 0.02))) +
      scale_x_continuous(
        breaks = sort(unique(delta_grupo_sem$periodo)),
        labels = sem_label_fn
      ) +
      scale_fill_manual(values = paleta_10, drop = FALSE, guide = "none") +
      scale_color_manual(values = paleta_10, drop = FALSE, guide = "none") +
      expand_limits(x = max(delta_grupo_sem$periodo) + 1.4) +
      labs(x = NULL, y = "Composición del Δ ITF") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank(),
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
        breaks = sort(unique(delta_grupo_sem$periodo)),
        labels = sem_label_fn
      ) +
      scale_fill_manual(values = paleta_10, drop = FALSE, guide = "none") +
      expand_limits(x = max(delta_grupo_sem$periodo) + 1.4) +
      labs(x = NULL, y = "Composición del Δ ITF — hogares que salieron de la pobreza",
           caption = "Fuentes: EPH continua + brecha MLER/EPH por percentil + Cuadro 1 INDEC. Ponderado por PONDIH (líneas) y peso de ingreso (composición).") +
      theme_minimal(base_size = 13) +
      theme(legend.position = "none",
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_blank(),
            axis.text.x = element_text(angle = 45, hjust = 1))

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
}

shinyApp(ui, server)

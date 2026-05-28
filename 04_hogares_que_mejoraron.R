# lista caso por caso de los hogares que mejoraron de estado por el ajuste.
# escupe un excel y un parquet usa el shiny.

library(arrow)
library(dplyr)
library(tidyr)
library(openxlsx)
library(eph)

dir.create("outputs")

base_aj <- read_parquet("bases/eph_ajustada_v2.parquet")

# AGLOMERADO no quedó en la base liviana del 04, lo traemos aparte
aglo <- open_dataset("bases/eph_combinada_2017_2024") |>
  select(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE, AGLOMERADO) |>
  distinct(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE, AGLOMERADO) |>
  collect() # no conozco bien esta función

nombres_aglo <- eph::diccionario_aglomerados |> # gracias equipo eph jaja
  as.data.frame() |>
  transmute(AGLOMERADO = as.integer(codigo), aglomerado_nombre = aglo)

# agregamos a una fila por hogar
hogares <- base_aj |>
  group_by(CODUSU, NRO_HOGAR, ANO4, TRIMESTRE, SEMESTRE, REGION) |>
  summarise(
    miembros            = first(IX_TOT),
    adequi_hogar        = first(adequi_hogar),
    CBA_hogar           = first(CBA_hogar),
    CBT_hogar           = first(CBT_hogar),
    ITF                 = first(ITF),
    ITF_ajustado        = first(ITF_ajustado),
    PONDIH              = first(PONDIH),
    situacion_original  = first(situacion),
    situacion_ajustada  = first(situacion_aj),
    n_patrones          = sum(es_patron_afect, na.rm = TRUE),
    n_asalariados       = sum(es_asalariado_afect, na.rm = TRUE),
    n_cuentapropistas   = sum(es_cuentapropista_afect, na.rm = TRUE),
    n_jubilados         = sum(es_jubilado_afect, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(aglo, by = c("CODUSU", "NRO_HOGAR", "ANO4", "TRIMESTRE")) |>
  left_join(nombres_aglo, by = "AGLOMERADO")

cat("Hogares-periodo totales:", nrow(hogares), "\n")

# mejora de estado = pasar de indigente/pobre a una situación mejor
orden_sit <- c("indigente" = 1L, "pobre" = 2L, "no_pobre" = 3L) # esto sería mejor en factores...

hogares <- hogares |>
  mutate(
    sit_orig_n     = orden_sit[situacion_original],
    sit_aj_n       = orden_sit[situacion_ajustada],
    mejoro         = !is.na(sit_aj_n) & !is.na(sit_orig_n) & sit_aj_n > sit_orig_n,
    delta_ITF      = ITF_ajustado - ITF,
    pct_aumento    = if_else(ITF > 0, (ITF_ajustado / ITF - 1), NA_real_),
    margen_orig    = ITF - CBT_hogar,
    margen_aj      = ITF_ajustado - CBT_hogar,
    tipo_movimiento = case_when(
      situacion_original == "indigente" & situacion_ajustada == "no_pobre" ~ "indigente → no_pobre",
      situacion_original == "indigente" & situacion_ajustada == "pobre"    ~ "indigente → pobre",
      situacion_original == "pobre"     & situacion_ajustada == "no_pobre" ~ "pobre → no_pobre",
      TRUE ~ "sin cambio"
    ),
    periodo_lbl = sprintf("S%d %d", SEMESTRE, as.integer(ANO4))
  )

mejoraron <- hogares |> filter(mejoro) |>
  arrange(desc(delta_ITF))

dir.create("bases", showWarnings = FALSE)
write_parquet(mejoraron, "bases/hogares_que_mejoraron.parquet")

# resumen por (año, semestre, tipo de movimiento)
resumen <- mejoraron |>
  count(ANO4, SEMESTRE, tipo_movimiento) |>
  pivot_wider(names_from = tipo_movimiento, values_from = n, values_fill = 0) |>
  arrange(ANO4, SEMESTRE) |>
  mutate(periodo = sprintf("S%d %d", SEMESTRE, as.integer(ANO4))) |>
  select(periodo, everything(), -ANO4, -SEMESTRE)

# personas ponderadas (PONDIH × miembros) que salieron de la pobreza
resumen_personas <- mejoraron |>
  mutate(personas_pond = miembros * PONDIH) |>
  group_by(ANO4, SEMESTRE, tipo_movimiento) |>
  summarise(personas = sum(personas_pond, na.rm = TRUE), .groups = "drop") |>
  pivot_wider(names_from = tipo_movimiento, values_from = personas, values_fill = 0) |>
  arrange(ANO4, SEMESTRE) |>
  mutate(periodo = sprintf("S%d %d", SEMESTRE, as.integer(ANO4))) |>
  select(periodo, everything(), -ANO4, -SEMESTRE)


# armado del Excel con formato condicional para lectura humana, no va a otro script
# obviamente de esto no entiendo nada, es complejo el paquete openxlsx

detalle <- mejoraron |>
  transmute(
    Periodo         = periodo_lbl,
    Aglomerado      = aglomerado_nombre,
    CODUSU          = CODUSU,
    NRO_HOGAR       = NRO_HOGAR,
    Miembros        = miembros,
    `Adequi hogar`  = round(adequi_hogar, 2),
    Patrones        = n_patrones,
    Asalariados     = n_asalariados,
    Cuentapropistas = n_cuentapropistas,
    Jubilados       = n_jubilados,
    `ITF original ($)`  = round(ITF, 0),
    `ITF ajustado ($)`  = round(ITF_ajustado, 0),
    `Aumento ($)`       = round(delta_ITF, 0),
    `% aumento`         = pct_aumento,
    `CBT hogar ($)`     = round(CBT_hogar, 0),
    `Margen original ($)` = round(margen_orig, 0),
    `Margen ajustado ($)` = round(margen_aj, 0),
    `Situación original`  = situacion_original,
    `Situación ajustada`  = situacion_ajustada,
    `Tipo movimiento`     = tipo_movimiento
  )

wb <- createWorkbook()

# hoja 1
addWorksheet(wb, "Resumen")
writeData(wb, "Resumen", "Resumen — cantidad de hogares-periodo que mejoraron",
          startCol = 1, startRow = 1)
addStyle(wb, "Resumen", createStyle(fontSize = 13, textDecoration = "bold"),
         rows = 1, cols = 1)
writeData(wb, "Resumen", resumen, startCol = 1, startRow = 3)

writeData(wb, "Resumen", "Resumen — personas ponderadas (PONDIH × miembros) que mejoraron",
          startCol = 1, startRow = nrow(resumen) + 6)
addStyle(wb, "Resumen", createStyle(fontSize = 13, textDecoration = "bold"),
         rows = nrow(resumen) + 6, cols = 1)
writeData(wb, "Resumen", resumen_personas,
          startCol = 1, startRow = nrow(resumen) + 8)

setColWidths(wb, "Resumen", cols = 1:5, widths = c(12, 18, 22, 22, 18))

# hoja 2
addWorksheet(wb, "Detalle")
writeData(wb, "Detalle", detalle, startRow = 1)
freezePane(wb, "Detalle", firstActiveRow = 2, firstActiveCol = 4)

estilo_pesos <- createStyle(numFmt = "$ #,##0")
estilo_pct   <- createStyle(numFmt = "0.0%")

cols_pesos <- which(names(detalle) %in% c(
  "ITF original ($)", "ITF ajustado ($)", "Aumento ($)",
  "CBT hogar ($)", "Margen original ($)", "Margen ajustado ($)"))
col_pct    <- which(names(detalle) == "% aumento")

addStyle(wb, "Detalle", estilo_pesos,
         rows = 2:(nrow(detalle) + 1), cols = cols_pesos, gridExpand = TRUE)
addStyle(wb, "Detalle", estilo_pct,
         rows = 2:(nrow(detalle) + 1), cols = col_pct, gridExpand = TRUE)

col_sit_orig <- which(names(detalle) == "Situación original")
col_sit_aj   <- which(names(detalle) == "Situación ajustada")

estilo_indigente <- createStyle(fgFill = "#E15759", fontColour = "white", halign = "center")
estilo_pobre     <- createStyle(fgFill = "#F28E2B", fontColour = "white", halign = "center")
estilo_no_pobre  <- createStyle(fgFill = "#59A14F", fontColour = "white", halign = "center")

for (col in c(col_sit_orig, col_sit_aj)) {
  conditionalFormatting(wb, "Detalle", cols = col,
                        rows = 2:(nrow(detalle) + 1),
                        type = "contains", rule = "indigente",
                        style = estilo_indigente)
  conditionalFormatting(wb, "Detalle", cols = col,
                        rows = 2:(nrow(detalle) + 1),
                        type = "contains", rule = "pobre",
                        style = estilo_pobre)
  conditionalFormatting(wb, "Detalle", cols = col,
                        rows = 2:(nrow(detalle) + 1),
                        type = "contains", rule = "no_pobre",
                        style = estilo_no_pobre)
}

col_aum  <- which(names(detalle) == "Aumento ($)")
col_pct2 <- which(names(detalle) == "% aumento")
conditionalFormatting(wb, "Detalle", cols = col_aum,
                      rows = 2:(nrow(detalle) + 1),
                      type = "dataBar", style = c("#41b6c4"))
conditionalFormatting(wb, "Detalle", cols = col_pct2,
                      rows = 2:(nrow(detalle) + 1),
                      type = "dataBar", style = c("#9ecae1"))

col_marg_aj <- which(names(detalle) == "Margen ajustado ($)")
conditionalFormatting(wb, "Detalle", cols = col_marg_aj,
                      rows = 2:(nrow(detalle) + 1),
                      type = "colourScale",
                      style = c("#E15759", "#FFFFFF", "#59A14F"))

addStyle(wb, "Detalle",
         createStyle(textDecoration = "bold", fgFill = "#E8E8E8",
                     halign = "center", border = "bottom"),
         rows = 1, cols = 1:ncol(detalle))

setColWidths(wb, "Detalle", cols = 1:ncol(detalle),
             widths = c(10, 28, 22, 11, 10, 12, 10, 12, 16, 11,
                        16, 16, 14, 11, 14, 18, 18, 18, 18, 22))

archivo <- "outputs/hogares_que_mejoraron.xlsx"
saveWorkbook(wb, archivo, overwrite = TRUE)

# descarga y joineo de bases de individuos y de hogares de la EPH
# probablemente sería mejor no joinear todas las columnas de cada base, no se necesitan todas...

library(eph)
library(tidyverse)
library(arrow)

anio_inicio <- 2003
anio_fin    <- 2025
trimestres  <- 1:4
output_dir  <- "bases"

# la EPH continua arranca en el 3er trimestre de 2003 (antes era puntual) y hay
# trimestres que INDEC no publicó: 2003 T1-T2, 2007 T3 (parcial), 2015 T3-T4 y
# 2016 T1-T2. el tryCatch de descargar_trimestre devuelve NULL en esos casos y
# el loop los saltea solo, así que no hace falta listarlos a mano.

# se crea el directorio para las bases, que está ignorado entereamente por git en el .gitignore

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# tuve muchos problemas de memoria con estos scripts. sin cerrar las conexiones me crasheaba R

cerrar_conexiones <- function() {
  open_cons <- getAllConnections()
  open_cons <- open_cons[open_cons > 2L]
  for (con in open_cons) tryCatch(close(getConnection(con)), error = function(e) NULL)
}

base_valida <- function(x) {
  # exigimos NRO_HOGAR y TRIMESTRE para descartar las bases EPH *puntual* que el
  # paquete devuelve para 2003 T1-T2 (antes de que arrancara la continua): traen
  # CODUSU pero no NRO_HOGAR/TRIMESTRE y rompen el join y el resto del pipeline.
  !is.null(x) && is.data.frame(x) && nrow(x) > 0 &&
    all(c("CODUSU", "NRO_HOGAR", "TRIMESTRE") %in% names(x))
}

# para cada trimestre que se descarga lo hacemos con esto
# el trycatch es clave porque hay trimestres no disponibles

descargar_trimestre <- function(anio, trim) {

  # caerramos las conexiones al salir de la función
  on.exit(cerrar_conexiones(), add = TRUE)

  ind <- tryCatch(
    suppressWarnings(get_microdata(year = anio, period = trim, type = "individual")),
    error = function(e) {
      message(str_glue("Error descargando base individual {anio} T{trim}: {e$message}"))
      NULL
    }
  )

  hog <- tryCatch(
    suppressWarnings(get_microdata(year = anio, period = trim, type = "hogar")),
    error = function(e) {
      message(str_glue("Error descargando base hogar {anio} T{trim}: {e$message}"))
      NULL
    }
  )

  # si falló la descarga de la individual o la hogar entonces no sirve el trimestre
  if (!base_valida(ind) || !base_valida(hog)) return(NULL)

  list(individual = ind, hogar = hog)
}

# el loop principal

for (anio in anio_inicio:anio_fin) {
  for (trim in trimestres) {

    if (anio == 2016 && trim %in% c(1L, 2L)) next

    fname <- sprintf("eph_%d_t%d.parquet", anio, trim)
    if (file.exists(file.path(output_dir, fname))) {
      message(str_glue("Saltando {fname} (ya existe)"))
      next
    }

    message(str_glue("Descargando {anio} T{trim}"))

    datos <- descargar_trimestre(anio, trim)
    if (is.null(datos)) next

    ind <- datos$individual
    hog <- datos$hogar

    # join eliminando columnas compartidas del hogar q ya están en la individual
    join_keys <- c("CODUSU", "NRO_HOGAR")
    cols_to_drop <- setdiff(intersect(names(ind), names(hog)), join_keys)
    hog_slim <- hog |> select(-all_of(cols_to_drop))
    base_joined <- ind |> left_join(hog_slim, by = join_keys)

    # correcciones de tipo por problemas en el bind_rows
    if ("CH05" %in% names(base_joined)) {
      base_joined <- base_joined |> mutate(CH05 = as.character(CH05))
    }
    if ("IMPUTA" %in% names(base_joined)) {
      base_joined <- base_joined |> mutate(IMPUTA = as.character(IMPUTA))
    }
    if ("MAS_500" %in% names(base_joined)) {
      base_joined <- base_joined |> mutate(MAS_500 = as.character(MAS_500))
    }

    # más correccions de tipos por problemas al unir con Arrow
    base_joined <- base_joined |> mutate(
      across(where(is.factor), as.character),
      across(where(is.logical), as.character),
      across(where(is.integer), as.numeric)
    )

    # un parquet individual para cada trimestre
    write_parquet(base_joined, file.path(output_dir, fname))
    message(sprintf("  Guardado: %s", fname))

    rm(ind, hog, hog_slim, datos, base_joined)
    gc()
  }
}

# juntamos toda la base. ahora que vamos de 2003 a 2025 los esquemas por
# trimestre son muy heterogéneos (años sin PONDIH/PONDII/PONDIIO, sin IX_TOT,
# columnas que cambian de tipo entre años), así que unify_schemas de arrow se
# rompe. en vez de eso nos quedamos sólo con las columnas que usa el pipeline
# (cols_combinada) y forzamos su tipo por trimestre antes de escribir. Las
# columnas que falten en un trimestre se rellenan con NA; el fallback a PONDERA
# para los pesos de ingreso se hace después, en el 02 y el 05.

# superset de columnas que necesitan los scripts 02/04/05
cols_combinada <- c(
  "CODUSU", "NRO_HOGAR", "COMPONENTE", "ANO4", "TRIMESTRE", "REGION",
  "AGLOMERADO", "CH04", "CH06", "CAT_OCUP", "PP07H", "PP07E", "PP04A",
  "PP04B_COD", "P21", "PP08J1", "PP08J2", "PP08J3", "V2_M", "P47T",
  "ITF", "IPCF", "IX_TOT", "PONDERA", "PONDIIO", "PONDIH", "PONDII"
)
# estas se comparan como texto (CODUSU es id, PP04B_COD es "9700" para serv dom)
cols_texto <- c("CODUSU", "PP04B_COD")

harmonizar_trimestre <- function(path) {
  df <- read_parquet(path)
  faltan <- setdiff(cols_combinada, names(df))
  for (col in faltan) df[[col]] <- NA      # columna ausente en ese año
  df <- df[cols_combinada]
  df |> mutate(
    across(all_of(cols_texto), as.character),
    across(all_of(setdiff(cols_combinada, cols_texto)),
           ~ suppressWarnings(as.numeric(.x)))
  )
}

parquet_files <- list.files(output_dir, pattern = "^eph_\\d{4}_t\\d\\.parquet$",
                            full.names = TRUE)

if (length(parquet_files) > 0) {
  dir_bases <- file.path(output_dir, str_glue("eph_combinada_{anio_inicio}_{anio_fin}"))
  if (dir.exists(dir_bases)) unlink(dir_bases, recursive = TRUE)
  dir.create(dir_bases, recursive = TRUE)

  # escribimos un parquet armonizado por trimestre dentro del directorio. todos
  # comparten esquema, así open_dataset() lo lee sin unify_schemas.
  for (i in seq_along(parquet_files)) {
    df <- harmonizar_trimestre(parquet_files[i])
    write_parquet(df, file.path(dir_bases, sprintf("part-%03d.parquet", i)))
    rm(df); gc(verbose = FALSE)
  }

  message(str_glue("Base combinada guardada en: {dir_bases} ({length(parquet_files)} trimestres)"))
} else {
  message("Algo falló, no hay base...")
}

# descargamos las canastas regionales con el paquete eph
canastas <- tryCatch(
  get_poverty_lines(regional = TRUE),
  error = function(e) {
    message(sprintf("Error descargando canastas regionales con paquete eph: %s", e$message))
    NULL
  }
)

if (!is.null(canastas)) {
  write_parquet(canastas, file.path(output_dir, "canastas_regionales.parquet"))
  message("Canastas regionales del paquete eph guardadas.")
}

# extendemos las canastas regionales con el anexo del informe INDEC, porque
# el cache de holatam/data que usa el paquete eph va con lag y nos perdemos
# trimestres recientes (ej. S2 2025)
informe <- file.path(output_dir, "cuadros_informe_pobreza_03_26.xls")

if (!is.null(canastas) && file.exists(informe)) {
  raw <- suppressMessages(
    readxl::read_excel(informe, sheet = "Series canastas anexo",
                       col_names = FALSE, .name_repair = "minimal")
  )

  anio_extra <- as.integer(raw[[2]][3])
  meses_es <- c(Enero = 1L, Febrero = 2L, Marzo = 3L, Abril = 4L,
                Mayo = 5L, Junio = 6L, Julio = 7L, Agosto = 8L,
                Septiembre = 9L, Octubre = 10L, Noviembre = 11L,
                Diciembre = 12L)
  meses_cols <- unname(meses_es[as.character(unlist(raw[4, -1]))])

  mapa_regiones <- tibble::tribble(
    ~region_xls,         ~region,     ~codigo,
    "Gran Buenos Aires", "GBA",        1,
    "Cuyo",              "Cuyo",      42,
    "Noreste",           "Noreste",   41,
    "Noroeste",          "Noroeste",  40,
    "Pampeana",          "Pampeana",  43,
    "Patagonia",         "Patagonia", 44
  )

  parse_bloque <- function(filas, nombre) {
    raw[filas, ] |>
      setNames(c("region_xls", paste0("m", meses_cols))) |>
      tidyr::pivot_longer(-region_xls, names_to = "mc",
                          values_to = "valor") |>
      mutate(mes = as.integer(sub("^m", "", mc)),
             valor = as.numeric(valor),
             TRIMESTRE = (mes - 1L) %/% 3L + 1L) |>
      group_by(region_xls, TRIMESTRE) |>
      summarise(n = sum(!is.na(valor)),
                v = mean(valor, na.rm = TRUE),
                .groups = "drop") |>
      filter(n == 3L) |>
      transmute(region_xls, TRIMESTRE, !!nombre := v)
  }

  cba_q <- parse_bloque(7:12,  "CBA")
  cbt_q <- parse_bloque(32:37, "CBT")

  nuevo <- cba_q |>
    inner_join(cbt_q, by = c("region_xls", "TRIMESTRE")) |>
    inner_join(mapa_regiones, by = "region_xls") |>
    mutate(periodo = sprintf("%d.%d", anio_extra, TRIMESTRE)) |>
    select(region, periodo, CBA, CBT, codigo)

  if (nrow(nuevo) > 0) {
    canastas_ext <- bind_rows(
      anti_join(canastas, nuevo, by = c("region", "periodo")),
      nuevo
    ) |> arrange(periodo, region)
    write_parquet(canastas_ext,
                  file.path(output_dir, "canastas_regionales.parquet"))
    message(sprintf("Canastas extendidas con %d filas del informe INDEC (%s)",
                    nrow(nuevo), basename(informe)))
  }
}

# ============================================================================
# canasta combinada 2003-2025: usamos las canastas regionales del Excel del
# CEPED (bases/Canastas.xlsx, CBA/CBT por adulto equivalente, "NM-EMP") para
# 2003-2016 y las de INDEC/paquete eph para 2017-2025. En el solapamiento de
# 2016 las dos fuentes coinciden casi al decimal, así que el empalme es limpio.
# El resto del pipeline (02 y 05) lee este archivo, no canastas_regionales.
# ============================================================================
canastas_xlsx_path <- file.path(output_dir, "Canastas.xlsx")

if (file.exists(canastas_xlsx_path) && !is.null(canastas)) {

  # mapeo de las columnas regionales del Excel al nombre/código de región EPH
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
    readxl::read_excel(canastas_xlsx_path, sheet = "Hoja1", .name_repair = "minimal")
  )
  # el Excel viene por posición: año, mes, trimestre, 6 CBA y 6 CBT (GBA, Cuyo,
  # NEA, NOA, Pamp, Patag). Le ponemos nombres y casteamos todo a numérico.
  names(cx) <- c("ano", "mes", "trim",
                 paste0("CBA_", regiones_xlsx$suf),
                 paste0("CBT_", regiones_xlsx$suf))
  cx <- cx |> mutate(across(everything(), as.numeric))

  # mensual -> trimestral (promedio de los 3 meses) y a formato largo por región
  canastas_hist <- cx |>
    pivot_longer(
      cols = -c(ano, mes, trim),
      names_to = c(".value", "suf"),
      names_sep = "_"
    ) |>
    group_by(ano, trim, suf) |>
    summarise(CBA = mean(CBA, na.rm = TRUE),
              CBT = mean(CBT, na.rm = TRUE), .groups = "drop") |>
    inner_join(regiones_xlsx, by = "suf") |>
    transmute(region, periodo = sprintf("%d.%d", ano, trim), CBA, CBT, codigo)

  # cortamos el histórico del Excel en 2016 y pegamos INDEC/eph de 2017 en adelante.
  # Leemos canastas_regionales.parquet del disco (no el objeto `canastas` en
  # memoria) porque ese archivo ya tiene la extensión con el informe INDEC, que
  # agrega los trimestres recientes que el cache de holatam todavía no trae.
  anios_hist <- as.integer(substr(canastas_hist$periodo, 1, 4))
  canastas_recientes <- read_parquet(
      file.path(output_dir, "canastas_regionales.parquet")) |>
    filter(as.integer(substr(periodo, 1, 4)) >= 2017)

  canastas_comb <- bind_rows(
    canastas_hist |> filter(anios_hist <= 2016),
    canastas_recientes
  ) |>
    arrange(periodo, region)

  write_parquet(canastas_comb,
                file.path(output_dir, "canastas_combinadas.parquet"))
  message(sprintf(
    "Canastas combinadas 2003-2025 guardadas (%d filas, %s a %s).",
    nrow(canastas_comb), min(canastas_comb$periodo), max(canastas_comb$periodo)))
} else {
  message("No se pudo armar la canasta combinada (falta Canastas.xlsx o las canastas eph).")
}

message("Proceso finalizado.")

# descarga y joineo de bases de individuos y de hogares de la EPH
# probablemente sería mejor no joinear todas las columnas de cada base, no se necesitan todas...

library(eph)
library(tidyverse)
library(arrow)

anio_inicio <- 2016
anio_fin    <- 2025
trimestres  <- 1:4
output_dir  <- "bases"

# se crea el directorio para las bases, que está ignorado entereamente por git en el .gitignore

dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)

# tuve muchos problemas de memoria con estos scripts. sin cerrar las conexiones me crasheaba R

cerrar_conexiones <- function() {
  open_cons <- getAllConnections()
  open_cons <- open_cons[open_cons > 2L]
  for (con in open_cons) tryCatch(close(getConnection(con)), error = function(e) NULL)
}

base_valida <- function(x) {
  !is.null(x) && is.data.frame(x) && nrow(x) > 0 && "CODUSU" %in% names(x)
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

# juntamos toda la base con arrow para no cargar todo en memoria y que crashee

parquet_files <- list.files(output_dir, pattern = "^eph_\\d{4}_t\\d\\.parquet$",
                            full.names = TRUE)

if (length(parquet_files) > 0) {
  ds <- open_dataset(parquet_files, format = "parquet", unify_schemas = TRUE)

  # directorio para la base combinada (guardamos en varios archivos con arrow)
  dir_bases <- file.path(output_dir, str_glue("eph_combinada_{anio_inicio}_{anio_fin}"))

  ds |> write_dataset(path = dir_bases, format = "parquet")

  message(str_glue("Base combinada guardada en: {dir_bases}"))
  rm(ds)
  gc()
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

message("Proceso finalizado.")

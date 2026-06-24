# Extensión de la serie hacia atrás: de 2016–2025 a 2003–2025

Documento de las decisiones, cambios y *caveats* de extender el análisis de
pobreza ajustada por subdeclaración a toda la EPH continua (2003 en adelante).

---

## 1. Objetivo

Pasar la serie de **2016–2025** a **2003–2025**, cubriendo todo el período de la
EPH continua, incluyendo los años de la intervención del INDEC (2007–2015), para
poder contrastar la serie reconstruida con lo que el INDEC publicaba oficialmente.

## 2. Cobertura temporal y huecos

La EPH continua arranca en el **3er trimestre de 2003** (antes era puntual). Hay
trimestres que el INDEC no publicó y que por lo tanto faltan en la serie:

| Hueco | Motivo |
|---|---|
| 2003 T1–T2 | Antes de la continua (el paquete `eph` devuelve la EPH *puntual*, que se descarta) |
| 2007 T3–T4 | Transición: el INDEC publicó un "4to trim 2007 / 1er trim 2008" en vez de semestres normales |
| 2015 T3–T4 y 2016 T1–T2 | Apagón estadístico (suspensión de la EPH) |

Resultado: **84 trimestres / 42 semestres** efectivos entre 2003 S2 y 2025 S2.
La descarga (`descargar_trimestre` en el `01`) saltea solos los trimestres no
disponibles vía `tryCatch`, y `base_valida()` exige `NRO_HOGAR` y `TRIMESTRE`
para descartar las bases puntuales de 2003 T1–T2.

## 3. Decisiones (acordadas con el usuario)

### 3.1. Canastas: fuente híbrida
El Excel del CEPED `bases/Canastas.xlsx` (CBA/CBT por adulto equivalente,
metodología empalmada **"NM-EMP"**) cubre **2003–2016 mensual**; el paquete `eph`
+ informe INDEC cubre **2015.4–2025.4**. No hay una sola fuente para todo el
período, así que:

- **2003–2016** → `Canastas.xlsx` (promediando los 3 meses de cada trimestre).
- **2017–2025** → `canastas_regionales.parquet` (paquete `eph` + extensión INDEC).

Se combinan en **`bases/canastas_combinadas.parquet`** (552 filas = 92 trimestres
× 6 regiones, 2003.1–2025.4), que es lo que ahora leen el `02` y el `05`.

En el **solapamiento de 2016 las dos fuentes coinciden casi al decimal** (idénticas
de 2016-T2 en adelante), así que el empalme es limpio. El mapeo de regiones del
Excel a los códigos EPH:

| Excel | Región EPH | Código |
|---|---|---|
| GBA | GBA | 1 |
| Cuyo | Cuyo | 42 |
| NEA | Noreste | 41 |
| NOA | Noroeste | 40 |
| Pamp | Pampeana | 43 |
| Patag | Patagonia | 44 |

### 3.2. Pesos de ingreso: fallback a PONDERA (solo donde faltan)
Los ponderadores de ingreso **PONDIH / PONDIIO / PONDII no existen en la EPH
continua de la intervención** (~2008–2015 la columna viene vacía; 2007 solo medio
año; 2003–2006 y 2016+ sí los tienen). Decisión: **donde la columna es `NA`,
usar `PONDERA`**; el resto del pipeline funciona igual.

> **Clave (ver bug en sección 5): se rellenan solo los `NA`, nunca los ceros.**
> `PONDIH==0` no es un peso faltante: es la marca del INDEC para *excluir de las
> estadísticas de ingreso* (hogares con no respuesta de ingresos). En 2016+ es
> ~20–25% de los casos.

Consecuencia metodológica: en los años de intervención, al usar `PONDERA`, **no
se aplica la exclusión por no respuesta de ingresos** (esos años incluyen a todos),
a diferencia del método INDEC.

### 3.3. Línea verde (INDEC oficial) histórica
La pobreza oficial del INDEC ahora se arma de **dos fuentes** (en el `03`):

- `cuadros_informe_pobreza_03_26.xls` (Cuadro 1) → **2016 S2 – 2025 S2**.
- `bases/sh_pobrezaeindigencia_continua.xls` → **2003 S1 – 2013 S1**: fila
  "Total aglomerados urbanos", sub-columna **Pobreza › Personas** de cada semestre.

Se incluyen a propósito los valores oficiales **intervenidos** (subdeclarados) de
2007–2013 — ej. **2013 S1 = 4,7%** — porque ese es justamente el contraste con la
serie reconstruida. Queda un **hueco 2013 S2 – 2016 S1** (apagón). El período
híbrido "4to trim 2007 / 1er trim 2008" se descarta (no mapea a un semestre).

## 4. Cambios por script

| Script | Cambio |
|---|---|
| `01_bajada_bases.R` | Descarga 2003–2025; descarta bases puntuales; combina en `eph_combinada_2003_2025` con **esquema armonizado de 27 columnas** (no `unify_schemas`, porque los esquemas por año difieren mucho); arma `canastas_combinadas.parquet`. |
| `02_pobreza_ajustada_asalariados.R` | Span 2003–2025; lee la canasta combinada; **fallback de pesos** (solo NA); filtro de brecha ampliado a 2003. |
| `03_plots_pobreza_ajustada.R` | Suma la **línea verde histórica** desde `sh_pobrezaeindigencia_continua.xls`. |
| `04_hogares_que_mejoraron.R` | Apunta al directorio combinado nuevo. |
| `05_app_densidades_preprocesar.R` | Span 2003–2025; canasta combinada; fallback de pesos; alimenta `app/data/`. |
| `06_distribucion_ingreso.R` | Reconstruye el tamaño de hogar si faltara `IX_TOT` (en la práctica nunca falta; queda como guarda). |
| `07_canastas.R` | **Nuevo**: gráfico comparando las canastas del paquete `eph` vs `Canastas.xlsx` (CEPED). |

## 5. El bug que detectamos y corregimos (PONDIH==0)

La primera versión del fallback usaba `if_else(!is.na(PONDIH) & PONDIH > 0,
PONDIH, PONDERA)`, que **también reemplazaba los ceros** por un `PONDERA`
positivo, *antes* de `calculate_poverty`. Eso anuló la exclusión que hace el
INDEC (`situacion = NA` cuando `PONDIH==0`), volvió a contar a ~25% de la gente
(más pobre en promedio) y la tasa "Original" saltó **~15 pp por encima del INDEC**
(2016 S2: 47,0% en vez de 30,3%).

**Corrección:** rellenar **solo `NA`**: `if_else(is.na(PONDIH), PONDERA, PONDIH)`,
en el `02` (PONDIH, PONDIIO) y el `05` (PONDIIO, PONDII). Con eso la "Original"
vuelve a pegar con el INDEC (gap medio 2016+ = **−0,02 pp**) y desaparece el salto
artificial 2015→2016.

## 6. Caveats metodológicos

1. **Reconstrucción ≠ oficial en los años "honestos" (2003–2006).** La serie
   reconstruida corre **~12 pp por encima** de la oficial de la época. No es un
   error: la canasta CEPED (NM-EMP) tiene líneas más altas que la canasta INDEC
   vieja. La línea verde lo hace visible.

2. **Intervención 2007–2015 sin pesos de ingreso.** Esos años usan `PONDERA`, así
   que no quedan calibrados por ingreso como el método INDEC y no aplican la
   exclusión por no respuesta. La pobreza/indigencia de esos años hay que leerla
   con ese recaudo.

3. **2007 S1 mezcla esquemas de peso** (T1 trae PONDIH, T2 cae a PONDERA). Es una
   limitación de los datos de la transición; el efecto es chico (37,0%, entre sus
   vecinos).

4. **Línea verde con valores intervenidos.** 2007–2013 muestra lo que el INDEC
   publicó (manipulado a la baja, 4,7% en 2013), no una estimación corregida. Es
   intencional, para el contraste.

5. **Densidad de los gráficos.** Con 42 semestres, las series de líneas/barras del
   `03` y `06` quedan cargadas (etiquetas por punto). Funcionan, pero se podrían
   afinar (etiquetar uno de cada dos semestres, ensanchar, etc.).

6. **Verificación de la app: programática, no visual.** La app se validó con
   `testServer` (arranca y todos los reactivos corren), pero **no se hizo un QA
   visual en el navegador**. Además: (a) el `ecdf_pob` pasó a ~4 millones de
   filas, así que conviene mirar memoria/tiempo de carga en el deploy de Posit;
   (b) `app/app.R` ya tenía cambios sin commitear de antes de esta tarea, ajenos
   a la extensión.

## 7. Validaciones corridas

- **Empalme de canastas (`join`):** `CBT_hogar` poblado al **100% en todos los
  años** 2003–2025. Ninguna falla silenciosa del join.
- **Match con INDEC 2016+:** "Original" vs oficial dentro de ±0,2 pp; indigencia
  2016 S2 = **6,1% exacto**.
- **Fingerprint del fallback:** `situacion=NA` ~15–27% en años con pesos de
  ingreso (exclusión por no respuesta) y exactamente 0 en la intervención
  (PONDERA, sin exclusión) — el comportamiento esperado.
- **Distribución:** Gini en rango realista 0,42–0,54, declinando en los 2000;
  2016 = 0,438 (coincide con el INDEC), sin salto artificial 2015→2016.
- **Comparación de canastas (`07`):** idénticas de 2016-T2 en adelante; máxima
  diferencia 6,6% en 2015-T4 (irrelevante para el pipeline, que usa CEPED para
  todo 2015–2016).
- **Brecha MLER/EPH 2003–2015 auditada:** 99 percentiles por año, **0 NAs**, con
  estructura coherente (brecha < 1 en percentiles bajos, > 1 en altos; % que
  ajusta crece de 44% en 2003 a ~83% en 2023). No son placeholders.
- **App Shiny levantada con los datos 2003–2025:** arranca (HTTP 200), la UI
  trae las 5 pestañas y **todos los outputs reactivos renderizan sin error**
  (probado con `shiny::testServer` para 2003-T3, 2010-T3 y 2025-T4, en ECDF e
  histograma, más la tabla de hogares en 2003 y 2025). Los reactivos parsean el
  período y filtran, sin años hardcodeados.

## 8. Archivos

**Nuevos / regenerados (en `bases/`, ignorado por git):**
- `eph_combinada_2003_2025/` (base combinada armonizada)
- `canastas_combinadas.parquet` (canasta híbrida 2003–2025)
- parquets por trimestre 2003–2015 (`eph_AAAA_tT.parquet`)
- resultados, serie de distribución, datos de la app, etc. regenerados

**Insumos nuevos (en `bases/`):**
- `Canastas.xlsx` (CEPED, canastas NM-EMP 2003–2016)
- `sh_pobrezaeindigencia_continua.xls` (INDEC oficial 2003–2013)

**Versionado en git:**
- scripts `01`–`05` y `06` modificados; `07_canastas.R` nuevo
- `app/data/*.parquet` y `outputs/*.png` regenerados

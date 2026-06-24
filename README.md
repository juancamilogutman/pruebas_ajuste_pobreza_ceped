# pruebas_ajuste_pobreza_ceped

La idea es ver cuánto cambiaría la pobreza por ingresos tal como la mide el INDEC en Argentina 
si corregimos por la subdeclaración y subcaptación de ingresos que aparece al cruzando EPH con MLER
que tanto estudiaron Ignacio Lautaro "Nacho" Paola y Damián Kennedy.

La serie cubre toda la EPH continua, de **2003 a 2025** (con los huecos que no
publicó el INDEC: 2003 T1-T2, 2007 T3-T4 y el apagón 2015 T3 – 2016 T2). Para los
años de la intervención usamos las canastas empalmadas del CEPED. Las decisiones,
los *caveats* y las validaciones de esa extensión hacia atrás están en
[extension_2003_2025.md](extension_2003_2025.md).

El pipeline va por scripts numerados: `01` baja las bases y arma la canasta
combinada, `02` calcula la pobreza original y ajustada, `03`–`06` hacen los
gráficos y los insumos de la app, `07` compara las dos fuentes de canastas y
`08` corre el análisis de sensibilidad de la pobreza al valor de la línea
(headcount bajo 0,5 / 1 / 1,5 / 2 líneas, original vs ajustada).

Hay app de shiny para explorar y poder ir puliendo el laburo
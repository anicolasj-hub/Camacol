# Prueba técnica · Coordinación Económica · Camacol

Andrés Nicolás Jiménez Díaz. Todo el desarrollo está en **R**. Se abre `Prueba_CAMACOL.Rproj` en RStudio y los scripts se corren en orden; `00_setup.R` instala los paquetes que falten.

## Productos

| Producto exigido | Archivo | Resultado |
|---|---|---|
| Script de integración GEIH | `scripts/01_integracion_geih.R` | 533.851 personas-mes (ene–ago 2026), 5 módulos, validada contra el DANE |
| Script de limpieza de proyectos | `scripts/02_limpieza_vivienda.R` | 240 proyectos y 1.043 tipologías, con diccionario y bitácora |
| Script de minería de datos | `scripts/03_mineria.R` y `scripts/03b_prediccion_precios.R` | Modelo hedónico + PLN + LASSO; predicción con bosque aleatorio y XGBoost ajustados, SHAP y ejercicio de predicción |
| Documento de justificación | `docs/Justificacion_mineria_datos.docx` | 468 palabras |
| **Tablero de control (principal)** | `tablero_excel/Tablero_construccion_GEIH2026.xlsx` (+ `scripts/07_datos_tablero_excel.R`) | Excel con filtros desplegables, 11 gráficos, lecturas automáticas y la base de 16.699 ocupados en construcción; sin macros |
| Tablero de Excel en la web | `tablero_web/monitor_construccion_geih2026.html` (+ `scripts/08_tablero_html_monitor.R`) | Réplica del Excel en un solo HTML: mismos filtros, lecturas, tarjetas y gráficos, más el resumen de la base y la metodología |
| Tablero en Shiny | `tablero/app.R` (+ `scripts/04_preparar_tablero.R`) | 4 pestañas; calcula en vivo desde los microdatos |
| Tablero en la web | `tablero_web/tablero_construccion_geih2026.html` (+ `scripts/06_tablero_html.R`) | Mismo tablero en un solo HTML; se abre con doble clic y se puede publicar |
| Video del tablero | guion en `docs/guion_video.md` | ≤ 2 minutos, sobre el tablero en Excel |
| Scraping (opcional) | `scripts/05_scraping.R` | Oferta actual de proyectos en Fincaraíz y comparación con 2021 |

## Estructura

```
Prueba_CAMACOL.Rproj
scripts/      00_setup.R … 08_tablero_html_monitor.R (03b: predicción)
tablero_excel/ libro de Excel, datos/ (tablas que lee) y construir_tablero_excel.py
tablero/      app.R, datos_tablero.rds (Shiny)
tablero_web/  plantilla.html y tablero_construccion_geih2026.html (versión web)
datos/raw/            insumos originales (no se modifican); GEIH/ con los .zip del DANE
datos/procesados/     bases limpias e integradas
salidas/              validaciones, tablas y gráficos (mineria/, prediccion/)
docs/                 documento de justificación y guion del video
```

## Cómo correrlo

**Requisitos:** R 4.1 o superior (probado en R 4.3.3) y RStudio. Se necesita internet solo la primera vez, para instalar paquetes; `00_setup.R` instala los que falten y actualiza los que estén por debajo de la versión mínima.

1. Abrir `Prueba_CAMACOL.Rproj` en RStudio. Así la raíz del proyecto queda bien fijada.
2. Ejecutar `ejecutar_todo.R` con *Source*. Corre `01`, `02`, `03`, `03b`, `04`, `06`, `07` y `08` en orden en 2 a 3 minutos y guarda la sesión en `salidas/sesion_R.txt`. También se puede correr cada script por separado, en el mismo orden.
3. Ver el tablero en **Excel**: abrir `tablero_excel/Tablero_construccion_GEIH2026.xlsx` y elegir los filtros amarillos de la hoja *Tablero* (detalle más abajo). También hay otras dos versiones con las mismas cifras:
   - **Shiny**, con `shiny::runApp("tablero")`. Calcula en vivo desde los microdatos y necesita R abierto. Para publicarlo, ver las instrucciones al inicio de `tablero/app.R`.
   - **Web**, abriendo `tablero_web/tablero_construccion_geih2026.html` con doble clic. Trae los indicadores precalculados en R para todas las combinaciones de filtros. No necesita R, Power BI ni Tableau, pero sí internet para cargar la librería de gráficos.
4. Opcional: correr `05_scraping.R`. Necesita internet y sus resultados dependen de la oferta publicada el día en que se corra.

Los resultados son deterministas: semilla fija (`set.seed(2026)`) y particiones de validación cruzada reproducibles. Dos corridas desde cero dan cifras idénticas.

## 01 · Integración GEIH 2026

- **Insumo:** lee directamente los .zip mensuales del DANE.
- **Módulos:** une características generales, hogar y vivienda, fuerza de trabajo, ocupados y no ocupados.
- **Llaves:** `directorio + secuencia_p` (hogar) y `+ orden` (persona).
- **Control de las uniones:** cada `left_join` verifica la cardinalidad y falla si queda un registro sin pareja. Los nombres de archivo distintos entre meses (abril, agosto) se ubican por patrón.
- **Validación:** las cifras de agosto coinciden con el boletín del DANE. TGP 63,3 %, TO 57,4 %, TD 9,4 %, 23.685 mil ocupados y 1.790 mil en construcción.

## 02 · Limpieza de vivienda nueva

| Problema detectado | Tratamiento |
|---|---|
| 137 duplicados exactos en *Base* y 17 en *Tipos* | Se eliminan |
| Proyecto de prueba ("CONSTRUCTORA TEST") | Se elimina |
| *Estrato* corrido en 8 proyectos (trae el precio mínimo del proyecto, verificado 8/8) | NA + bandera |
| *Área* compuesta (m², habitaciones, baños, parqueaderos, "Áreas desde") | Se separa en variables |
| Formato numérico colombiano ("2.432 m²" = 2 432 m²) | Parser propio |
| Faltantes como "--" y "0 m²"; "Más de 10" parqueaderos | NA + bandera |
| Publicación relativa ("hace 3 meses") | Días publicados y fecha aproximada |
| Ciudades con espacios y corregimientos | Municipio, código DIVIPOLA, departamento y mercado |
| Áreas de lote (> 1.000 m²), área privada > construida | Banderas; excluidas del precio/m² |

Validación cruzada entre hojas: 913 tipologías emparejadas por *link + precio*, con 99,8 % de concordancia en área y 99,9 % en alcobas.

## 03 · Minería de datos

¿Cuánto del precio por m² explican los fundamentales y cuánto la narrativa del proyecto? R² fuera de muestra (validación cruzada agrupada por proyecto, 5 × 5):

| Modelo | R² | Error medio |
|---|---|---|
| Línea base: mediana por mercado y estrato | 0,54 | 22,5 % |
| MCO: atributos y localización | 0,69 | 17,9 % |
| MCO hedónico + PLN | 0,72 | 16,4 % |
| LASSO + bolsa de palabras | 0,69 | 17,8 % |
| Bosque aleatorio | 0,65 | 17,9 % |

Las primas y los gráficos están en `salidas/mineria/`.

### 03b · Predicción con aprendizaje automático

Se apartan 48 proyectos (20 %) como prueba antes de ajustar nada. Los hiperparámetros se eligen con validación cruzada agrupada en el resto.

| Modelo (conjunto de prueba) | R² | Error mediano | Dentro de ±20 % |
|---|---|---|---|
| Línea base | 0,35 | 19,8 % | 54 % |
| MCO hedónico + PLN | 0,50 | 14,8 % | 65 % |
| Bosque aleatorio ajustado | 0,56 | 12,3 % | 62 % |
| XGBoost ajustado | 0,63 | 11,7 % | 68 % |
| Ensamble MCO + XGBoost (elegido por validación cruzada) | 0,60 | 13,2 % | 68 % |

Variables más relevantes (SHAP): estrato y mercado explican el 60 % de lo que mueve la predicción. Ejercicio con cuatro viviendas hipotéticas e intervalo del 80 % en `salidas/prediccion/`.

## 07 · Tablero de control en Excel

`07_datos_tablero_excel.R` exporta a `tablero_excel/datos/` los indicadores de las 1.428 combinaciones de filtros (territorio × sexo × ventana × mes, los mismos del tablero web) y la base de microdatos de los ocupados en construcción. El libro lee esas tablas y calcula todo con fórmulas (INDEX/MATCH, SUMIFS): no usa macros ni tablas dinámicas y funciona en Excel para Windows y Mac.

| Hoja | Contenido |
|---|---|
| Inicio | Instrucciones, índice con vínculos y cifras clave de agosto |
| Tablero | Filtros de territorio, mes, ventana y sexo; cuatro secciones (empleo, ciudades, hogares, serie) con tarjetas, 9 gráficos y textos de lectura que se reescriben con cada filtro; aviso automático de baja precisión |
| Resumen_base | Ocupados por mes y subsector, perfil del trabajador (sexo, educación, posición, tamaño de empresa) con selector de mes, condiciones laborales (ARL, contrato, horas) y 2 gráficos |
| Metodología | Definiciones y validación contra el boletín del DANE |
| Base_construccion | 16.699 ocupados encuestados (ene–ago) con factor de expansión, como tabla de Excel |
| Datos_* | Indicadores precalculados que alimentan el tablero |
| Cálculos, Listas | Fórmulas intermedias y opciones de los filtros |

El libro se arma con `tablero_excel/construir_tablero_excel.py` (Python con openpyxl), porque los paquetes de R para Excel no escriben gráficos nativos ligados a celdas. El script 07 lo llama al final si encuentra Python; si no, conserva el libro entregado.

## 08 · El tablero de Excel en versión web

`08_tablero_html_monitor.R` arma `tablero_web/monitor_construccion_geih2026.html` con la plantilla `tablero_web/plantilla_monitor.html`. Tiene la misma estructura del Excel: filtros amarillos, franjas de lectura, tarjetas, las cuatro secciones del tablero y las hojas *Resumen de la base* (perfil del trabajador por mes, calculado desde los microdatos) y *Metodología*. Se abre con doble clic en cualquier navegador. Necesita internet solo para la librería de gráficos; sin conexión, las cifras y los textos siguen funcionando.

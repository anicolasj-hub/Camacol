# Guion del video del tablero (máximo 2 minutos)

**Antes de grabar:** en RStudio, abre el proyecto y corre `shiny::runApp("tablero")`. Abre el tablero en el navegador a pantalla completa, con *Total nacional*, *Agosto*, *Mensual* y *Todos*. Graba la pantalla con QuickTime (Archivo → Nueva grabación de pantalla). El texto completo dura unos 1:50 leído a ritmo normal (≈ 270 palabras).

---

**0:00 – 0:15 · Apertura** *(pestaña "Empleo en construcción")*

> Este tablero monitorea el empleo en la construcción con los microdatos de la GEIH 2026 del DANE, de enero a agosto. Está hecho en R con Shiny, sobre una base que integra cinco módulos y ocho meses, y sus cifras coinciden con el boletín oficial.

**0:15 – 0:40 · Indicadores clave** *(señalar las cuatro tarjetas)*

> En agosto, la construcción ocupó 1,79 millones de personas, el 7,6 % del empleo del país, 1,8 % menos que en julio. Su informalidad, medida como no cotización a pensión, es de 63 %, ocho puntos por encima del total. El ingreso mediano es un salario mínimo.

**0:40 – 1:00 · Composición** *(gráficos de subsector y posición ocupacional)*

> Seis de cada diez ocupados trabajan en edificaciones; el resto, en obras civiles y actividades especializadas. El sector es más asalariado que el promedio, pero cuatro de cada diez trabajan por cuenta propia.

**1:00 – 1:20 · Filtros y ciudades** *(cambiar a "Trimestre móvil", luego pestaña "Ciudades")*

> Los filtros cambian el territorio, el mes, la ventana y el sexo. Para las ciudades se usa el trimestre móvil y, si la muestra es pequeña, el tablero lo advierte. Villavicencio, Santa Marta y Riohacha tienen el mayor peso de la construcción en el empleo. La informalidad va de 44 % en Manizales a casi 90 % en Florencia.

**1:20 – 1:40 · Hogares y vivienda** *(pestaña "Hogares y vivienda")*

> Para la demanda de vivienda: el 41 % de los hogares vive en arriendo y apenas el 5 % está pagando la suya. En Bucaramanga, Villavicencio y Medellín más de la mitad son arrendatarios, una demanda potencial de vivienda propia.

**1:40 – 1:55 · Cierre** *(pestaña "Datos y metodología", clic en "Descargar CSV")*

> Por último, esta pestaña documenta la metodología y permite descargar la serie de cualquier territorio. Todo el proceso, desde la descarga hasta el tablero, es reproducible en R.

---

**Presentación de apoyo (opcional):** no hace falta. El tablero se explica solo y dos minutos alcanzan justo para recorrerlo.

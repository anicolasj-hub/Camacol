"""
construir_tablero_excel.py
Arma el tablero de control en Excel a partir de las tablas que produce R
(scripts/07_datos_tablero_excel.R).

Prueba técnica · Coordinación Económica · Camacol
Autor: Andrés Nicolás Jiménez Díaz

El libro no usa macros ni tablas dinámicas: los filtros son listas
desplegables y todos los indicadores y gráficos se calculan con fórmulas
(INDEX/MATCH, SUMIFS, LARGE) sobre las hojas de datos. Funciona en Excel
para Windows y Mac.

Uso:  python3 tablero_excel/construir_tablero_excel.py   (desde la raíz del proyecto)
"""

import re
import unicodedata
from pathlib import Path
import pandas as pd
from openpyxl import Workbook
from openpyxl.styles import Font, PatternFill, Alignment, Border, Side
from openpyxl.utils import get_column_letter
from openpyxl.worksheet.datavalidation import DataValidation
from openpyxl.worksheet.table import Table, TableStyleInfo
from openpyxl.formatting.rule import FormulaRule
from openpyxl.comments import Comment
from openpyxl.chart import LineChart, BarChart, Reference
from openpyxl.chart.label import DataLabelList
from openpyxl.chart.shapes import GraphicalProperties
from openpyxl.drawing.line import LineProperties
from openpyxl.worksheet.pagebreak import Break

def sin_tildes(x):
    """Mismo nombre de columna que slug() en 07_datos_tablero_excel.R: sin tildes y con _ por separador."""
    return re.sub(r"[^A-Za-z0-9]+", "_", unicodedata.normalize("NFKD", x).encode("ascii", "ignore").decode())

RAIZ = Path(__file__).resolve().parents[1]
DATOS = RAIZ / "tablero_excel" / "datos"
SALIDA = RAIZ / "tablero_excel" / "Tablero_construccion_GEIH2026.xlsx"

# ---------------------------------------------------------------------------
# Estilo
# ---------------------------------------------------------------------------
FUENTE = "Arial"
AZUL_OSCURO = "1F3A5F"
AZUL = "2A78D6"
AZUL_CLARO = "B9D4F5"
NARANJA = "EB6834"
VERDE = "1BAF7A"
GRIS = "A3A8B0"
GRIS_TEXTO = "4A5361"
FONDO_TARJETA = "F5F7FA"
BORDE = "D9DEE5"
AMARILLO_INPUT = "FFF4C2"

def f(size=10, bold=False, color="121720", italic=False):
    return Font(name=FUENTE, size=size, bold=bold, color=color, italic=italic)

fill = lambda c: PatternFill("solid", start_color=c, end_color=c)
lado = Side(style="thin", color=BORDE)
borde = Border(left=lado, right=lado, top=lado, bottom=lado)
centro = Alignment(horizontal="center", vertical="center", wrap_text=True)
izq = Alignment(horizontal="left", vertical="center", wrap_text=True)
der = Alignment(horizontal="right", vertical="center")

FMT_MILES = '#,##0" mil"'
FMT_ENTERO = '#,##0'
FMT_PCT = '0.0%'
FMT_PCT_SIGNO = '+0.0%;-0.0%;0.0%'
FMT_PESOS = '"$"#,##0'
OCULTA_CERO = '0.0%;-0.0%;;'   # porcentaje que deja en blanco los ceros
FMT_PP = '+0.0" p.p.";-0.0" p.p.";0.0" p.p."'

# ---------------------------------------------------------------------------
# Datos
# ---------------------------------------------------------------------------
empleo = pd.read_csv(DATOS / "empleo.csv")
hogares = pd.read_csv(DATOS / "hogares.csv")
ciudades = pd.read_csv(DATOS / "ciudades.csv")
hog_ciudad = pd.read_csv(DATOS / "hogares_ciudad.csv")
base = pd.read_csv(DATOS / "base_construccion.csv")
lista = lambda n: pd.read_csv(DATOS / f"lista_{n}.csv")["valor"].tolist()
L = {n: lista(n) for n in ["territorios", "meses", "ventanas", "sexos", "ciudades23",
                            "subsectores", "posiciones", "tenencias"]}
TRECE = ["Bogotá D.C.", "Medellín A.M.", "Cali A.M.", "Barranquilla A.M.", "Bucaramanga A.M.",
         "Manizales A.M.", "Pasto", "Pereira A.M.", "Ibagué", "Cúcuta A.M.", "Villavicencio",
         "Montería", "Cartagena"]

wb = Workbook()
wb.remove(wb.active)
hojas = ["Inicio", "Tablero", "Resumen_base", "Metodología", "Base_construccion",
         "Datos_empleo", "Datos_hogares", "Datos_ciudades", "Datos_hogares_ciudad",
         "Cálculos", "Listas"]
ws = {h: wb.create_sheet(h) for h in hojas}
Q = lambda h: f"'{h}'"   # nombre de hoja entre comillas para las fórmulas


# ---------------------------------------------------------------------------
# Hojas de datos (tablas de Excel)
# ---------------------------------------------------------------------------
def escribir_tabla(hoja, df, nombre, formatos, anchos=None, nota=None):
    sh = ws[hoja]
    sh.append(list(df.columns))
    for fila in df.itertuples(index=False):
        sh.append([None if (isinstance(v, float) and pd.isna(v)) else v for v in fila])
    n, k = len(df) + 1, len(df.columns)
    ref = f"A1:{get_column_letter(k)}{n}"
    t = Table(displayName=nombre, ref=ref)
    t.tableStyleInfo = TableStyleInfo(name="TableStyleLight9", showRowStripes=True)
    sh.add_table(t)
    for c in range(1, k + 1):
        col = df.columns[c - 1]
        letra = get_column_letter(c)
        sh.column_dimensions[letra].width = (anchos or {}).get(col, max(12, min(34, len(col) + 2)))
        fmt = next((v for pat, v in formatos.items() if pat in col), None)
        for r in range(1, n + 1):
            cel = sh.cell(r, c)
            cel.font = f(9, bold=(r == 1), color="FFFFFF" if r == 1 else "121720")
            if r > 1 and fmt:
                cel.number_format = fmt
    sh.freeze_panes = "B2"
    if nota:
        sh["A1"].comment = Comment(nota, "Tablero")
    return n

fmt_empleo = {"miles": '#,##0.0', "participacion": FMT_PCT, "informal": FMT_PCT, "bajo_smmlv": FMT_PCT,
              "ingreso": FMT_PESOS, "encuestados": FMT_ENTERO, "sub_": '#,##0.0', "posc_": FMT_PCT, "post_": FMT_PCT}
N_EMP = escribir_tabla("Datos_empleo", empleo, "DatosEmpleo", fmt_empleo, {"clave": 34, "territorio": 20},
                       "Indicadores de empleo precalculados en R (06_tablero_html.R y 07_datos_tablero_excel.R) "
                       "para cada territorio, sexo, ventana y mes. Fuente: DANE, GEIH 2026.")
fmt_hog = {"miles": '#,##0.0', "arriendo_mediano": FMT_PESOS, "arriendo": FMT_PCT, "pagando": FMT_PCT,
           "propia": FMT_PCT, "hacinamiento": FMT_PCT, "encuestados": FMT_ENTERO, "ten_": FMT_PCT}
N_HOG = escribir_tabla("Datos_hogares", hogares, "DatosHogares", fmt_hog, {"clave": 34, "territorio": 20},
                       "Indicadores de hogares (jefes de hogar) por territorio, sexo del jefe, ventana y mes.")
N_CIU = escribir_tabla("Datos_ciudades", ciudades, "DatosCiudades",
                       {"participacion": FMT_PCT, "informal": FMT_PCT, "encuestados": FMT_ENTERO},
                       {"clave": 30, "ciudad": 20}, "Indicadores de las 23 ciudades en trimestre móvil.")
N_HCI = escribir_tabla("Datos_hogares_ciudad", hog_ciudad, "DatosHogaresCiudad",
                       {"arriendo_mediano": FMT_PESOS, "arriendo": FMT_PCT}, {"clave": 30, "ciudad": 20},
                       "Hogares en arriendo y arriendo mediano por ciudad, trimestre móvil.")
N_BAS = escribir_tabla("Base_construccion", base, "BaseConstruccion",
                       {"ingreso": FMT_PESOS, "fex": '#,##0.00'},
                       {"ciudad": 20, "departamento": 18, "zona": 30, "nivel_educativo": 22, "subsector": 28,
                        "posicion": 26, "tamano_empresa": 16},
                       "Microdatos de los ocupados en construcción (CIIU 41-43), GEIH enero-agosto de 2026. "
                       "Cada fila es una persona encuestada; 'fex' es su factor de expansión. La suma de fex de un "
                       "mes estima los ocupados de ese mes; para promediar k meses, divida por k. "
                       "informal = 1 si no cotiza a pensión.")

# Rango de cada columna de datos (sin encabezado), p. ej. R_EMP('H')
rng = lambda hoja, n: (lambda col: f"{Q(hoja)}!${col}$2:${col}${n}")
R_EMP, R_HOG = rng("Datos_empleo", N_EMP), rng("Datos_hogares", N_HOG)
R_CIU, R_HCI, R_BAS = rng("Datos_ciudades", N_CIU), rng("Datos_hogares_ciudad", N_HCI), rng("Base_construccion", N_BAS)
col_emp = {c: get_column_letter(i + 1) for i, c in enumerate(empleo.columns)}
col_hog = {c: get_column_letter(i + 1) for i, c in enumerate(hogares.columns)}
col_bas = {c: get_column_letter(i + 1) for i, c in enumerate(base.columns)}


# ---------------------------------------------------------------------------
# Listas para los filtros
# ---------------------------------------------------------------------------
li = ws["Listas"]
columnas_listas = [("Territorios", L["territorios"]), ("Meses", L["meses"]), ("Ventanas", L["ventanas"]),
                   ("Sexos", L["sexos"]), ("Ciudades (23)", L["ciudades23"]), ("13 ciudades", TRECE),
                   ("Posiciones", L["posiciones"]), ("Tenencias", L["tenencias"]), ("Subsectores", L["subsectores"])]
LISTA_RNG = {}
for j, (titulo, valores) in enumerate(columnas_listas, start=1):
    letra = get_column_letter(j)
    li.cell(1, j, titulo).font = f(10, True)
    for i, v in enumerate(valores, start=2):
        li.cell(i, j, v).font = f(10)
    li.column_dimensions[letra].width = 30
    LISTA_RNG[titulo] = f"Listas!${letra}$2:${letra}${len(valores) + 1}"


# ---------------------------------------------------------------------------
# Hoja de cálculos (todo lo que leen los indicadores y los gráficos)
# ---------------------------------------------------------------------------
ca = ws["Cálculos"]
CA = Q("Cálculos")
ca["A1"] = "Cálculos del tablero (no editar): leen los filtros de la hoja Tablero y buscan los indicadores en las hojas de datos."
ca["A1"].font = f(11, True)
ca.column_dimensions["A"].width = 30
for c in "BCDEFGHIJKLMNOPQRSTUVWXYZ":
    ca.column_dimensions[c].width = 15

params = [
    ("Territorio", "=Tablero!$B$5"),
    ("Mes (nombre)", "=Tablero!$G$5"),
    ("Mes (número)", f"=MATCH(B3,{LISTA_RNG['Meses']},0)"),
    ("Ventana", "=Tablero!$J$5"),
    ("Sexo", "=Tablero!$M$5"),
    ("Mes efectivo", '=IF(AND(B5="Trimestre móvil",B4<3),3,B4)'),
    ("Mes para ciudades (trimestre)", "=MAX(B4,3)"),
    ("Clave", '=B2&"|"&B6&"|"&B5&"|"&B7'),
    ("Clave periodo anterior", '=B2&"|"&B6&"|"&B5&"|"&(B7-1)'),
    ("Fila en Datos_empleo", f"=MATCH(B9,{R_EMP('A')},0)"),
    ("Fila anterior en Datos_empleo", f'=IFERROR(MATCH(B10,{R_EMP("A")},0),"")'),
    ("Fila en Datos_hogares", f"=MATCH(B9,{R_HOG('A')},0)"),
    ("Periodo mostrado", f"=INDEX({R_EMP('F')},B11)"),
    ("Desplazamiento de la serie", '=IF(B5="Trimestre móvil",2,0)'),
]
for i, (et, form) in enumerate(params, start=2):
    ca.cell(i, 1, et).font = f(10)
    ca.cell(i, 2, form).font = f(10)
P = {et: f"{CA}!$B${i}" for i, (et, _) in enumerate(params, start=2)}
FILA_E, FILA_EA, FILA_H = P["Fila en Datos_empleo"], P["Fila anterior en Datos_empleo"], P["Fila en Datos_hogares"]
emp = lambda c: f"INDEX({R_EMP(col_emp[c])},{FILA_E})"
hog = lambda c: f"INDEX({R_HOG(col_hog[c])},{FILA_H})"

# Indicadores del periodo elegido
ca["D2"] = "Indicador"; ca["E2"] = "Valor"
for c in ("D2", "E2"):
    ca[c].font = f(10, True)
ca.column_dimensions["D"].width = 42
kpis = [
    ("Ocupados en construcción (miles)", f"={emp('construccion_miles')}", FMT_MILES),
    ("Variación frente al periodo anterior",
     f'=IF({FILA_EA}="","",{emp("construccion_miles")}/INDEX({R_EMP(col_emp["construccion_miles"])},{FILA_EA})-1)', FMT_PCT_SIGNO),
    ("Encuestados en construcción", f"={emp('encuestados_construccion')}", FMT_ENTERO),
    ("Participación en el empleo", f"={emp('participacion')}", FMT_PCT),
    ("Ocupados totales (miles)", f"={emp('ocupados_miles')}", FMT_MILES),
    ("Peso de edificaciones en el sector", f"={emp('sub_Construccion_de_edificios')}/{emp('construccion_miles')}", FMT_PCT),
    ("Informalidad en construcción", f"={emp('informal_construccion')}", FMT_PCT),
    ("Informalidad del total de ocupados", f"={emp('informal_total')}", FMT_PCT),
    ("Brecha de informalidad (p.p.)", f"=({emp('informal_construccion')}-{emp('informal_total')})*100", FMT_PP),
    ("Ingreso mediano en construcción", f"={emp('ingreso_med_construccion')}", FMT_PESOS),
    ("Ingreso mediano del total", f"={emp('ingreso_med_total')}", FMT_PESOS),
    ("Construcción: ganan menos de 1 SMMLV", f"={emp('bajo_smmlv_construccion')}", FMT_PCT),
    ("Hogares (miles)", f"={hog('hogares_miles')}", FMT_MILES),
    ("Hogares en arriendo", f"={hog('arriendo')}", FMT_PCT),
    ("Hogares pagando su vivienda", f"={hog('pagando')}", FMT_PCT),
    ("Hogares con vivienda propia pagada", f"={hog('propia')}", FMT_PCT),
    ("Arriendo mensual mediano", f"={hog('arriendo_mediano')}", FMT_PESOS),
    ("Arriendo mediano / salario mínimo", f"={hog('arriendo_mediano')}/1750905", FMT_PCT),
    ("Hogares en hacinamiento crítico", f"={hog('hacinamiento')}", FMT_PCT),
    ("Encuestados (hogares)", f"={hog('encuestados_hogares')}", FMT_ENTERO),
    ("Hogares arrendatarios (miles)", f"={hog('hogares_miles')}*{hog('arriendo')}", FMT_MILES),
]
K = {}
for i, (et, form, fmt) in enumerate(kpis, start=3):
    ca.cell(i, 4, et).font = f(10)
    c = ca.cell(i, 5, form); c.font = f(10); c.number_format = fmt
    K[et] = (f"{CA}!$E${i}", fmt)

# Serie mensual del territorio elegido (filas 30 a 38)
S0 = 30
enc = ["Orden", "Mes", "Clave", "Fila", "Periodo", "Construcción (miles)", "Participación",
       "Informalidad construcción", "Informalidad total", "Edificios", "Obras civiles",
       "Actividades especializadas", "Ocupados (miles)", "Ingreso mediano construcción",
       "Ingreso mediano total", "Encuestados"]
for j, e in enumerate(enc, start=1):
    ca.cell(S0, j, e).font = f(10, True)
ca.cell(S0 - 1, 1, "Serie del territorio y sexo elegidos. En trimestre móvil hay 6 periodos (desde enero–marzo): las filas sobrantes quedan como valor no disponible (NA) a propósito, para que el gráfico no dibuje esos puntos.").font = f(10, True)
serie_cols = [("Construcción (miles)", "construccion_miles", '#,##0.0'), ("Participación", "participacion", FMT_PCT),
              ("Informalidad construcción", "informal_construccion", FMT_PCT), ("Informalidad total", "informal_total", FMT_PCT),
              ("Edificios", "sub_Construccion_de_edificios", '#,##0.0'), ("Obras civiles", "sub_Obras_de_ingenieria_civil", '#,##0.0'),
              ("Actividades especializadas", "sub_Actividades_especializadas", '#,##0.0'),
              ("Ocupados (miles)", "ocupados_miles", '#,##0.0'), ("Ingreso mediano construcción", "ingreso_med_construccion", FMT_PESOS),
              ("Ingreso mediano total", "ingreso_med_total", FMT_PESOS), ("Encuestados", "encuestados_construccion", FMT_ENTERO)]
for i in range(1, 9):
    r = S0 + i
    ca.cell(r, 1, i)
    ca.cell(r, 2, f"=A{r}+{P['Desplazamiento de la serie']}")
    ca.cell(r, 3, f'={P["Territorio"]}&"|"&{P["Sexo"]}&"|"&{P["Ventana"]}&"|"&B{r}')
    ca.cell(r, 4, f'=IFERROR(MATCH(C{r},{R_EMP("A")},0),"")')
    ca.cell(r, 5, f'=IF(D{r}="","",INDEX({R_EMP("F")},D{r}))')
    for j, (_, c, fmt) in enumerate(serie_cols, start=6):
        cel = ca.cell(r, j, f'=IF($D{r}="",NA(),INDEX({R_EMP(col_emp[c])},$D{r}))')
        cel.number_format = fmt
    for j in range(1, 17):
        ca.cell(r, j).font = f(10)
SER = lambda j: (S0 + 1, S0 + 8, j)   # filas y columna de cada serie

# Posición ocupacional del periodo elegido (filas 42 a 48)
P0 = 41
ca.cell(P0 - 1, 1, "Posición ocupacional del periodo elegido (% de los ocupados)").font = f(10, True)
for j, e in enumerate(["Posición", "Construcción", "Total ocupados"], start=1):
    ca.cell(P0, j, e).font = f(10, True)
for i, pos in enumerate(L["posiciones"], start=1):
    r = P0 + i
    slug = sin_tildes(pos)
    ca.cell(r, 1, pos).font = f(10)
    for j, pref in [(2, "posc_"), (3, "post_")]:
        cel = ca.cell(r, j, f"={emp(pref + slug)}"); cel.number_format = FMT_PCT; cel.font = f(10)

# Tenencia de la vivienda (filas 53 a 57)
T0 = 52
ca.cell(T0 - 1, 1, "Tenencia de la vivienda del periodo elegido (% de hogares)").font = f(10, True)
for j, e in enumerate(["Tenencia", "% de hogares"], start=1):
    ca.cell(T0, j, e).font = f(10, True)
for i, ten in enumerate(L["tenencias"], start=1):
    r = T0 + i
    slug = sin_tildes(ten)
    ca.cell(r, 1, ten).font = f(10)
    cel = ca.cell(r, 2, f"={hog('ten_' + slug)}"); cel.number_format = FMT_PCT; cel.font = f(10)

# Rankings de ciudades (participación, informalidad, arriendo), 23 filas cada uno
def ranking(fila0, titulo, rango_clave, rango_valor, fmt):
    """Bloque: valores por ciudad, valor con desempate y lista ordenada de mayor a menor,
    partida en dos series (territorio seleccionado / demás) para resaltar en el gráfico."""
    ca.cell(fila0 - 1, 1, titulo).font = f(10, True)
    enc = ["Ciudad", "Clave", "Fila", "Valor", "Valor con desempate", "", "Puesto", "Ciudad ordenada",
           "Valor ordenado", "Territorio seleccionado", "Demás ciudades"]
    for j, e in enumerate(enc, start=1):
        ca.cell(fila0, j, e).font = f(10, True)
    a, b = fila0 + 1, fila0 + 23
    for i, ciudad in enumerate(L["ciudades23"], start=1):
        r = fila0 + i
        ca.cell(r, 1, ciudad)
        ca.cell(r, 2, f'=A{r}&"|"&{P["Sexo"]}&"|"&{P["Mes para ciudades (trimestre)"]}')
        ca.cell(r, 3, f"=MATCH(B{r},{rango_clave},0)")
        ca.cell(r, 4, f"=INDEX({rango_valor},C{r})").number_format = fmt
        ca.cell(r, 5, f"=D{r}+ROW()*1E-9")
        ca.cell(r, 7, i)
        ca.cell(r, 8, f"=INDEX($A${a}:$A${b},MATCH(I{r},$E${a}:$E${b},0))")
        ca.cell(r, 9, f"=LARGE($E${a}:$E${b},G{r})").number_format = fmt
        sel = (f'OR(H{r}={P["Territorio"]},AND({P["Territorio"]}="13 ciudades y A.M.",'
               f'ISNUMBER(MATCH(H{r},{LISTA_RNG["13 ciudades"]},0))))')
        # 0 en lugar de NA(): la barra no se dibuja y el formato oculta la etiqueta
        ca.cell(r, 10, f"=IF({sel},I{r},0)").number_format = OCULTA_CERO
        ca.cell(r, 11, f"=IF({sel},0,I{r})").number_format = OCULTA_CERO
        for j in range(1, 12):
            ca.cell(r, j).font = f(10)
    return a, b

R0_PAR = 62
RK_PAR = ranking(R0_PAR, "Ranking: participación de la construcción en el empleo (trimestre móvil)",
                 R_CIU("A"), R_CIU("F"), FMT_PCT)
R0_INF = R0_PAR + 27
RK_INF = ranking(R0_INF, "Ranking: informalidad en construcción (trimestre móvil)", R_CIU("A"), R_CIU("G"), FMT_PCT)
R0_ARR = R0_INF + 27
RK_ARR = ranking(R0_ARR, "Ranking: hogares en arriendo (trimestre móvil)", R_HCI("A"), R_HCI("E"), FMT_PCT)


# ---------------------------------------------------------------------------
# Hoja Tablero
# ---------------------------------------------------------------------------
tb = ws["Tablero"]
tb.sheet_view.showGridLines = False
tb.sheet_view.zoomScale = 90
tb.column_dimensions["A"].width = 2
for c in range(2, 18):
    tb.column_dimensions[get_column_letter(c)].width = 11.5
tb.column_dimensions["R"].width = 2

def merge(rango, valor=None, font=None, alin=None, relleno=None, fmt=None, bordes=False):
    tb.merge_cells(rango)
    c = tb[rango.split(":")[0]]
    if valor is not None:
        c.value = valor
    if font: c.font = font
    if alin: c.alignment = alin
    if fmt: c.number_format = fmt
    if relleno or bordes:
        for fila in tb[rango]:
            for cel in fila:
                if relleno: cel.fill = relleno
                if bordes: cel.border = borde
    return c

merge("B1:Q1", "Monitor de empleo en construcción y vivienda · GEIH 2026", f(20, True, AZUL_OSCURO), izq)
merge("B2:Q2", "Fuente: DANE, Gran Encuesta Integrada de Hogares, enero a agosto de 2026. Cálculos propios en R. "
      "Construcción = divisiones 41, 42 y 43 de la CIIU Rev. 4. Cambie los filtros amarillos: todo el tablero se actualiza.",
      f(10, color=GRIS_TEXTO), izq)
tb.row_dimensions[1].height = 30
tb.row_dimensions[2].height = 30

for rango, txt in [("B4:E4", "TERRITORIO"), ("G4:H4", "MES DE REFERENCIA"), ("J4:K4", "VENTANA"),
                   ("M4:N4", "SEXO"), ("P4:Q4", "PERIODO MOSTRADO")]:
    merge(rango, txt, f(9, True, GRIS_TEXTO), izq)
entradas = [("B5:E5", "Total nacional", "Territorios"), ("G5:H5", "Agosto", "Meses"),
            ("J5:K5", "Mensual", "Ventanas"), ("M5:N5", "Todos", "Sexos")]
for rango, valor, lst in entradas:
    merge(rango, valor, f(12, True, "1F4FA0"), centro, fill(AMARILLO_INPUT), bordes=True)
    dv = DataValidation(type="list", formula1=f"={LISTA_RNG[lst]}", allow_blank=False,
                        showErrorMessage=True, errorTitle="Valor no válido",
                        error="Elija una opción de la lista.")
    tb.add_data_validation(dv)
    dv.add(rango.split(":")[0])
merge("P5:Q5", f"={P['Periodo mostrado']}", f(12, True, AZUL_OSCURO), centro, fill(FONDO_TARJETA), bordes=True)
tb.row_dimensions[5].height = 26
aviso = merge("B6:Q6",
              f'=IF(AND({P["Ventana"]}="Trimestre móvil",{P["Mes (número)"]}<3),'
              f'"El trimestre móvil empieza en marzo: se muestra enero–marzo. ","")&'
              f'IF({K["Encuestados en construcción"][0]}<150,"Baja precisión: solo "&{K["Encuestados en construcción"][0]}&'
              f'" encuestados en construcción en este periodo y territorio. "&IF({P["Ventana"]}="Mensual",'
              f'"Use el trimestre móvil o un territorio mayor.","Interprete con cautela o elija un territorio mayor."),"")',
              f(10, True, "7A4B00"), izq)
tb.conditional_formatting.add("B6:Q6", FormulaRule(formula=['LEN($B$6)>0'], fill=fill("FFE6A8")))
tb.freeze_panes = "A7"

def seccion(fila, texto):
    merge(f"B{fila}:Q{fila}", texto, f(13, True, "FFFFFF"), izq, fill(AZUL_OSCURO))
    tb.row_dimensions[fila].height = 24

def tarjeta(col0, fila0, titulo, valor, sub1=None, sub2=None):
    """Tarjeta de 4 columnas: título, valor grande y dos líneas de apoyo (etiqueta + valor)."""
    c0, c3 = get_column_letter(col0), get_column_letter(col0 + 3)
    c2 = get_column_letter(col0 + 2)
    for r in range(fila0, fila0 + 5):
        for cc in range(col0, col0 + 4):
            cel = tb.cell(r, cc); cel.fill = fill(FONDO_TARJETA)
            cel.border = Border(left=lado if cc == col0 else None, right=lado if cc == col0 + 3 else None,
                                top=lado if r == fila0 else None, bottom=lado if r == fila0 + 4 else None)
    merge(f"{c0}{fila0}:{c3}{fila0}", titulo, f(10, True, GRIS_TEXTO), izq)
    ref, fmt = valor
    merge(f"{c0}{fila0 + 1}:{c3}{fila0 + 2}", f"={ref}", f(22, True, AZUL_OSCURO), Alignment(horizontal="left", vertical="center"), fmt=fmt)
    for k, sub in enumerate([sub1, sub2]):
        if sub is None:
            continue
        r = fila0 + 3 + k
        etiqueta, valor_sub = sub
        if valor_sub is None:
            merge(f"{c0}{r}:{get_column_letter(col0 + 3)}{r}", etiqueta, f(9, color=GRIS_TEXTO), izq)
            continue
        ref_s, fmt_s = valor_sub
        merge(f"{c0}{r}:{c2}{r}", etiqueta, f(9, color=GRIS_TEXTO), izq)
        cel = tb.cell(r, col0 + 3, f"={ref_s}")
        cel.font = f(10, True, "121720"); cel.alignment = der; cel.number_format = fmt_s
    tb.row_dimensions[fila0 + 1].height = 18
    tb.row_dimensions[fila0 + 2].height = 18

def estilo_eje(chart, fmt_y=None, fmt_x=None):
    chart.x_axis.delete = False
    chart.y_axis.delete = False
    if fmt_y: chart.y_axis.number_format = fmt_y; chart.y_axis.numFmt.sourceLinked = False
    if fmt_x: chart.x_axis.number_format = fmt_x; chart.x_axis.numFmt.sourceLinked = False
    chart.y_axis.majorGridlines.spPr = GraphicalProperties(ln=LineProperties(solidFill="E3E6EB"))
    chart.style = 2

def color_serie(s, color, linea=False, ancho=22000, guion=None):
    if linea:
        s.graphicalProperties.line.solidFill = color
        s.graphicalProperties.line.width = ancho
        if guion: s.graphicalProperties.line.dashStyle = guion
        s.marker.symbol = "circle"; s.marker.size = 7
        s.marker.graphicalProperties = GraphicalProperties(solidFill=color, ln=LineProperties(solidFill=color))
        s.smooth = False
    else:
        s.graphicalProperties = GraphicalProperties(solidFill=color, ln=LineProperties(solidFill="FFFFFF"))

ref = lambda c1, r1, c2, r2: Reference(ca, min_col=c1, min_row=r1, max_col=c2, max_row=r2)
ANCHO, ALTO, ALTO_RANK = 17.8, 8.6, 13.5

def lectura(fila, formula, alto=36):
    """Caja de texto dinámico que resume el periodo elegido (útil para narrar el tablero)."""
    merge(f"B{fila}:Q{fila}", formula, f(11, color=AZUL_OSCURO), izq, fill("EAF2FC"))
    tb.cell(fila, 2).border = Border(left=Side(style="thick", color=AZUL))
    tb.row_dimensions[fila].height = alto

# FIXED() respeta el separador decimal del sistema (coma en Excel en español), a diferencia de TEXT()
pct = lambda ref: f'FIXED({ref}*100,1)&" %"'
num = lambda ref: f"FIXED({ref},0)"
v = lambda et: K[et][0]
PERIODO_TXT = (f'IF({P["Ventana"]}="Mensual",{P["Mes (nombre)"]},'
               f'"Trimestre "&LOWER({P["Periodo mostrado"]}))')
SEXO_TXT = f'IF({P["Sexo"]}="Todos",""," · "&{P["Sexo"]})'

# --- Sección 1: empleo
seccion(8, "1. Empleo en construcción")
var_ = v("Variación frente al periodo anterior")
brecha = v("Brecha de informalidad (p.p.)")
lectura(9, f'={PERIODO_TXT}&" · "&{P["Territorio"]}&{SEXO_TXT}&": "&{num(v("Ocupados en construcción (miles)"))}'
           f'&" mil personas trabajan en construcción, el "&{pct(v("Participación en el empleo"))}&" del empleo"'
           f'&IF({var_}="",""," ("&IF({var_}>=0,"+","−")&FIXED(ABS({var_})*100,1)&" % frente al periodo anterior)")'
           f'&". El "&{pct(v("Informalidad en construcción"))}&" es informal, "&FIXED(ABS({brecha}),1)&" p.p. "'
           f'&IF({brecha}>=0,"más","menos")&" que el total de ocupados, y el ingreso laboral mediano es de $"'
           f'&{num(v("Ingreso mediano en construcción"))}&" al mes."')
tarjeta(2, 10, "Ocupados en construcción", K["Ocupados en construcción (miles)"],
        ("Variación frente al periodo anterior", K["Variación frente al periodo anterior"]),
        ("Encuestados en construcción", K["Encuestados en construcción"]))
tarjeta(6, 10, "Participación en el empleo total", K["Participación en el empleo"],
        ("Ocupados totales", K["Ocupados totales (miles)"]),
        ("Peso de edificaciones en el sector", K["Peso de edificaciones en el sector"]))
tarjeta(10, 10, "Informalidad en construcción", K["Informalidad en construcción"],
        ("Informalidad del total de ocupados", K["Informalidad del total de ocupados"]),
        ("Brecha frente al total", K["Brecha de informalidad (p.p.)"]))
tarjeta(14, 10, "Ingreso laboral mediano en construcción", K["Ingreso mediano en construcción"],
        ("Ingreso mediano del total", K["Ingreso mediano del total"]),
        ("Ganan menos de 1 SMMLV", K["Construcción: ganan menos de 1 SMMLV"]))

cats = ref(5, S0 + 1, 5, S0 + 8)
c1 = LineChart(); c1.title = "Ocupados en construcción (miles)"
c1.add_data(ref(6, S0, 6, S0 + 8), titles_from_data=True); c1.set_categories(cats)
color_serie(c1.series[0], AZUL, linea=True); c1.legend = None
estilo_eje(c1, '#,##0'); c1.y_axis.scaling.min = 0
c1.width, c1.height = ANCHO, ALTO
tb.add_chart(c1, "B16")

c2 = BarChart(); c2.type = "col"; c2.grouping = "stacked"; c2.overlap = 100; c2.gapWidth = 60
c2.title = "Ocupados por subsector (miles)"
c2.add_data(ref(10, S0, 12, S0 + 8), titles_from_data=True); c2.set_categories(cats)
for s, col in zip(c2.series, [AZUL, NARANJA, VERDE]):
    color_serie(s, col)
estilo_eje(c2, '#,##0'); c2.legend.position = "b"
c2.width, c2.height = ANCHO, ALTO
tb.add_chart(c2, "J16")

c3 = LineChart(); c3.title = "Informalidad: construcción frente al total"
c3.add_data(ref(8, S0, 9, S0 + 8), titles_from_data=True); c3.set_categories(cats)
color_serie(c3.series[0], AZUL, linea=True); color_serie(c3.series[1], GRIS, linea=True, guion="dash")
estilo_eje(c3, '0%'); c3.legend.position = "b"
c3.width, c3.height = ANCHO, ALTO
tb.add_chart(c3, "B34")

c4 = BarChart(); c4.type = "bar"; c4.grouping = "clustered"; c4.gapWidth = 50
c4.title = "Posición ocupacional (% de los ocupados)"
c4.add_data(ref(2, P0, 3, P0 + 7), titles_from_data=True); c4.set_categories(ref(1, P0 + 1, 1, P0 + 7))
color_serie(c4.series[0], AZUL); color_serie(c4.series[1], GRIS)
estilo_eje(c4, '0%'); c4.x_axis.scaling.orientation = "maxMin"; c4.legend.position = "b"
c4.width, c4.height = ANCHO, ALTO
tb.add_chart(c4, "J34")

# --- Sección 2: ciudades
seccion(52, "2. Ciudades: participación e informalidad de la construcción")
a_p, b_p = RK_PAR; a_i, b_i = RK_INF
pos_par = f'MATCH({P["Territorio"]},{CA}!$H${a_p}:$H${b_p},0)'
pos_inf = f'MATCH({P["Territorio"]},{CA}!$H${a_i}:$H${b_i},0)'
per_ciu = f'LOWER(INDEX({R_CIU("E")},{CA}!$C${a_p}))'
lectura(53, f'="Trimestre "&{per_ciu}&{SEXO_TXT}&". "&IF(ISNUMBER({pos_par}),{P["Territorio"]}&" ocupa el puesto "'
            f'&{pos_par}&" de 23 en participación de la construcción y el "&{pos_inf}&" en informalidad. ","")'
            f'&"Mayor participación: "&{CA}!$H${a_p}&" ("&{pct(f"{CA}!$I${a_p}")}&"); menor: "'
            f'&{CA}!$H${b_p}&" ("&{pct(f"{CA}!$I${b_p}")}&"). Mayor informalidad: "&{CA}!$H${a_i}&" ("'
            f'&{pct(f"{CA}!$I${a_i}")}&")."')
merge("B54:Q54", "23 ciudades y áreas metropolitanas, trimestre móvil que termina en el mes elegido (si elige enero o "
      "febrero se muestra enero–marzo). En azul oscuro, el territorio seleccionado.", f(9, color=GRIS_TEXTO), izq)

def grafico_ranking(titulo, rk, ancla, fmt):
    a, b = rk
    ch = BarChart(); ch.type = "bar"; ch.grouping = "clustered"; ch.overlap = 100; ch.gapWidth = 40
    ch.title = titulo
    ch.add_data(ref(10, a - 1, 11, b), titles_from_data=True); ch.set_categories(ref(8, a, 8, b))
    color_serie(ch.series[0], AZUL_OSCURO); color_serie(ch.series[1], AZUL_CLARO)
    for s in ch.series:
        s.dLbls = DataLabelList(); s.dLbls.showVal = True; s.dLbls.numFmt = OCULTA_CERO
        s.dLbls.showSerName = s.dLbls.showCatName = s.dLbls.showLegendKey = False
    estilo_eje(ch, '0%'); ch.x_axis.scaling.orientation = "maxMin"; ch.legend.position = "b"
    ch.width, ch.height = ANCHO, ALTO_RANK
    tb.add_chart(ch, ancla)

grafico_ranking("Participación de la construcción en el empleo", RK_PAR, "B56", "0.0%")
grafico_ranking("Informalidad en construcción", RK_INF, "J56", "0.0%")

# --- Sección 3: hogares
seccion(83, "3. Hogares y vivienda: la demanda potencial")
JEFE_TXT = (f'IF({P["Sexo"]}="Todos",""," · hogares con "&IF({P["Sexo"]}="Mujeres","jefa","jefe"))')
lectura(84, f'={PERIODO_TXT}&" · "&{P["Territorio"]}&{JEFE_TXT}&": el "&{pct(v("Hogares en arriendo"))}'
            f'&" de los hogares vive en arriendo ("&{num(v("Hogares arrendatarios (miles)"))}&" mil hogares) y solo el "'
            f'&{pct(v("Hogares pagando su vivienda"))}&" está pagando su vivienda. Los arrendatarios son la demanda '
            f'potencial de vivienda propia; el arriendo mediano, $"&{num(v("Arriendo mensual mediano"))}'
            f'&", equivale al "&FIXED({v("Arriendo mediano / salario mínimo")}*100,0)&" % de un salario mínimo."')
tarjeta(2, 85, "Hogares que viven en arriendo", K["Hogares en arriendo"],
        ("Hogares arrendatarios", K["Hogares arrendatarios (miles)"]), ("Total de hogares", K["Hogares (miles)"]))
tarjeta(6, 85, "Hogares pagando su vivienda", K["Hogares pagando su vivienda"],
        ("Vivienda propia ya pagada", K["Hogares con vivienda propia pagada"]),
        ("Encuestados (hogares)", K["Encuestados (hogares)"]))
tarjeta(10, 85, "Arriendo mensual mediano", K["Arriendo mensual mediano"],
        ("Equivale a este % del salario mínimo", K["Arriendo mediano / salario mínimo"]), None)
tarjeta(14, 85, "Hogares en hacinamiento crítico", K["Hogares en hacinamiento crítico"],
        ("Más de 3 personas por cuarto", None), None)

c7 = BarChart(); c7.type = "bar"; c7.gapWidth = 50
c7.title = "Tenencia de la vivienda (% de hogares)"
c7.add_data(ref(2, T0, 2, T0 + 5), titles_from_data=True); c7.set_categories(ref(1, T0 + 1, 1, T0 + 5))
color_serie(c7.series[0], AZUL); c7.legend = None
c7.series[0].dLbls = DataLabelList(); c7.series[0].dLbls.showVal = True; c7.series[0].dLbls.numFmt = "0.0%"
c7.series[0].dLbls.showSerName = c7.series[0].dLbls.showCatName = c7.series[0].dLbls.showLegendKey = False
estilo_eje(c7, '0%'); c7.x_axis.scaling.orientation = "maxMin"
c7.width, c7.height = ANCHO, ALTO
tb.add_chart(c7, "B91")
grafico_ranking("Hogares en arriendo por ciudad", RK_ARR, "J91", "0.0%")
merge("B109:I111", "Los hogares arrendatarios son la demanda potencial de vivienda propia. El filtro de sexo se aplica "
      "al jefe o jefa del hogar.", f(9, color=GRIS_TEXTO), izq)

# --- Sección 4: tabla de la serie
seccion(119, "4. Serie del territorio seleccionado")
encab = ["Periodo", "Ocupados (miles)", "Construcción (miles)", "Participación", "Informalidad construcción",
         "Informalidad total", "Ingreso mediano construcción", "Ingreso mediano total", "Encuestados en construcción"]
fuente_col = [5, 13, 6, 7, 8, 9, 14, 15, 16]   # columnas en Cálculos
fmts = [None, '#,##0.0', '#,##0.0', FMT_PCT, FMT_PCT, FMT_PCT, FMT_PESOS, FMT_PESOS, FMT_ENTERO]
for j, e in enumerate(encab):
    cel = tb.cell(121, 2 + j, e)
    cel.font = f(9, True, "FFFFFF"); cel.fill = fill(AZUL); cel.alignment = centro; cel.border = borde
tb.row_dimensions[121].height = 32
for i in range(8):
    r, rc = 122 + i, S0 + 1 + i
    for j, (cc, fmt) in enumerate(zip(fuente_col, fmts)):
        letra = get_column_letter(cc)
        form = (f"={CA}!${letra}${rc}" if j == 0 else
                f'=IF({CA}!$E${rc}="","",{CA}!${letra}${rc})')
        cel = tb.cell(r, 2 + j, form)
        cel.font = f(10); cel.border = borde; cel.alignment = centro if j == 0 else der
        if fmt: cel.number_format = fmt
        if i % 2: cel.fill = fill(FONDO_TARJETA)
merge("B131:J134", "Cifras expandidas con el factor FEX_C18 (en trimestre móvil, FEX_C18/3). Informalidad: ocupados que no "
      "cotizan a pensión. Ingreso: mediana de INGLABO entre ocupados con ingreso positivo. Ver la hoja Metodología.",
      f(9, color=GRIS_TEXTO), izq)
c8 = LineChart(); c8.title = "Participación de la construcción en el empleo"
c8.add_data(ref(7, S0, 7, S0 + 8), titles_from_data=True); c8.set_categories(cats)
color_serie(c8.series[0], AZUL, linea=True); c8.legend = None
estilo_eje(c8, '0.0%')
c8.width, c8.height = 15.4, 7.4
tb.add_chart(c8, "K120")


# ---------------------------------------------------------------------------
# Resumen de la base de microdatos (cruces con SUMIFS)
# ---------------------------------------------------------------------------
rb = ws["Resumen_base"]
rb.sheet_view.showGridLines = False
rb.column_dimensions["A"].width = 2
rb.column_dimensions["B"].width = 34
for c in range(3, 12):
    rb.column_dimensions[get_column_letter(c)].width = 13
rb["B1"] = "Resumen de la base de ocupados en construcción"; rb["B1"].font = f(16, True, AZUL_OSCURO)
rb["B2"] = ("Cruces calculados con SUMIFS sobre la hoja Base_construccion (16.699 encuestados). Cada cifra suma el factor de "
            "expansión: son ocupados estimados, en miles. Sirve para verificar el tablero y para armar tablas dinámicas propias.")
rb["B2"].font = f(10, color=GRIS_TEXTO); rb["B2"].alignment = izq; rb.merge_cells("B2:K3"); rb.row_dimensions[2].height = 30

fex, mes_c = R_BAS(col_bas["fex"]), R_BAS(col_bas["mes"])
sub_c, sexo_c = R_BAS(col_bas["subsector"]), R_BAS(col_bas["sexo"])
inf_c, edu_c, pos_c = R_BAS(col_bas["informal"]), R_BAS(col_bas["nivel_educativo"]), R_BAS(col_bas["posicion"])

rb["B5"] = "Ocupados en construcción por mes y subsector (miles)"; rb["B5"].font = f(11, True)
cab = ["Subsector"] + L["meses"]
for j, e in enumerate(cab):
    c = rb.cell(6, 2 + j, e); c.font = f(9, True, "FFFFFF"); c.fill = fill(AZUL); c.alignment = centro
for i, s in enumerate(L["subsectores"] + ["Total"]):
    r = 7 + i
    rb.cell(r, 2, s).font = f(10, bold=(s == "Total"))
    for m in range(1, 9):
        if s == "Total":
            form = f"=SUM({get_column_letter(2 + m)}7:{get_column_letter(2 + m)}9)"
        else:
            form = f'=SUMIFS({fex},{mes_c},{m},{sub_c},$B{r})/1000'
        c = rb.cell(r, 2 + m, form); c.number_format = '#,##0.0'; c.font = f(10, bold=(s == "Total"))
rb["B11"] = "La fila Total de agosto (1.790 mil) coincide con el boletín técnico del DANE."
rb["B11"].font = f(9, italic=True, color=GRIS_TEXTO)

rb["B13"] = "Perfil de los ocupados en construcción (% del total y tasa de informalidad)"; rb["B13"].font = f(11, True)
rb["B14"] = "Mes del perfil (elija)"; rb["B14"].font = f(10, True, GRIS_TEXTO); rb["B14"].alignment = der
rb.merge_cells("C14:D14")
rb["C14"] = "Agosto"; rb["C14"].font = f(11, True, "1F4FA0"); rb["C14"].fill = fill(AMARILLO_INPUT)
rb["C14"].alignment = centro; rb["C14"].border = borde; rb["D14"].border = borde
dv_rb = DataValidation(type="list", formula1=f"={LISTA_RNG['Meses']}", allow_blank=False)
rb.add_data_validation(dv_rb); dv_rb.add("C14")
rb["E14"] = f"=MATCH(C14,{LISTA_RNG['Meses']},0)"; rb["E14"].font = f(8, color="A3A8B0")
rb["E14"].comment = Comment("Número del mes elegido (lo usan las fórmulas de esta tabla).", "Tablero")
M_RB = "$E$14"
for j, e in enumerate(["Característica y categoría", "% de ocupados", "Informalidad"]):
    c = rb.cell(15, 2 + j, e); c.font = f(9, True, "FFFFFF"); c.fill = fill(AZUL); c.alignment = centro
tam_c = R_BAS(col_bas["tamano_empresa"])
perfil = [("Sexo", sexo_c, ["Hombre", "Mujer"]),
          ("Nivel educativo", edu_c, ["Primaria o menos", "Secundaria y media", "Técnica o tecnológica", "Universitaria", "Posgrado"]),
          ("Posición ocupacional", pos_c, ["Asalariado", "Cuenta propia", "Patrón o empleador", "Trabajador sin remuneración"]),
          ("Tamaño de la empresa", tam_c, ["Trabaja solo", "2 a 10", "11 a 50", "51 a 200", "201 o más"])]
total_mes = f"SUMIFS({fex},{mes_c},{M_RB})"
r, FILAS_GRUPO = 16, {}
for nombre, rango_col, cats_p in perfil:
    c = rb.cell(r, 2, nombre); c.font = f(10, True, AZUL_OSCURO)
    for cc in range(2, 5):
        rb.cell(r, cc).fill = fill("EAF2FC")
    r += 1
    FILAS_GRUPO[nombre] = (r, r + len(cats_p) - 1)
    for cat in cats_p:
        c = rb.cell(r, 2, cat); c.font = f(10); c.alignment = Alignment(indent=1)
        c = rb.cell(r, 3, f'=SUMIFS({fex},{mes_c},{M_RB},{rango_col},B{r})/{total_mes}'); c.number_format = FMT_PCT; c.font = f(10)
        c = rb.cell(r, 4, f'=IFERROR(SUMIFS({fex},{mes_c},{M_RB},{rango_col},B{r},{inf_c},1)/'
                          f'SUMIFS({fex},{mes_c},{M_RB},{rango_col},B{r},{inf_c},"<>"),"")')
        c.number_format = FMT_PCT; c.font = f(10)
        r += 1
rb.cell(r, 2, "Informalidad: proporción que no cotiza a pensión dentro de cada categoría (sin pensionados).").font = f(9, italic=True, color=GRIS_TEXTO)

r += 2
rb.cell(r, 2, "Condiciones laborales en construcción, mes elegido").font = f(11, True)
r += 1
for j, e in enumerate(["Indicador", "Valor", ""]):
    c = rb.cell(r, 2 + j, e); c.font = f(9, True, "FFFFFF"); c.fill = fill(AZUL); c.alignment = centro
arl_c, con_c, hor_c = R_BAS(col_bas["afiliado_arl"]), R_BAS(col_bas["contrato_escrito"]), R_BAS(col_bas["horas_semana"])
condiciones = [
    ("Afiliados a ARL (riesgos laborales)", f'=SUMIFS({fex},{mes_c},{M_RB},{arl_c},1)/SUMIFS({fex},{mes_c},{M_RB},{arl_c},"<>")', FMT_PCT),
    ("Contrato escrito (entre quienes tienen contrato)", f'=SUMIFS({fex},{mes_c},{M_RB},{con_c},1)/SUMIFS({fex},{mes_c},{M_RB},{con_c},"<>")', FMT_PCT),
    ("Horas trabajadas a la semana (promedio)",
     f'=SUMPRODUCT(({mes_c}={M_RB})*{fex}*{hor_c})/SUMIFS({fex},{mes_c},{M_RB},{hor_c},"<>")', '0.0'),
    ("Encuestados en el mes", f'=COUNTIFS({mes_c},{M_RB})', FMT_ENTERO),
]
for et, form, fmt in condiciones:
    r += 1
    rb.cell(r, 2, et).font = f(10)
    c = rb.cell(r, 3, form); c.number_format = fmt; c.font = f(10)

def grafico_perfil(titulo, grupo, ancla):
    a, b = FILAS_GRUPO[grupo]
    ch = BarChart(); ch.type = "bar"; ch.gapWidth = 50; ch.title = titulo
    ch.add_data(Reference(rb, min_col=4, min_row=a, max_row=b), titles_from_data=False)
    ch.set_categories(Reference(rb, min_col=2, min_row=a, max_row=b))
    color_serie(ch.series[0], AZUL); ch.legend = None
    ch.series[0].dLbls = DataLabelList(); ch.series[0].dLbls.showVal = True; ch.series[0].dLbls.numFmt = "0%"
    ch.series[0].dLbls.showSerName = ch.series[0].dLbls.showCatName = ch.series[0].dLbls.showLegendKey = False
    estilo_eje(ch, '0%'); ch.x_axis.scaling.orientation = "maxMin"
    ch.y_axis.scaling.min = 0; ch.y_axis.scaling.max = 1
    ch.width, ch.height = 15.5, 7.2
    rb.add_chart(ch, ancla)

grafico_perfil("Informalidad por nivel educativo", "Nivel educativo", "F13")
grafico_perfil("Informalidad por tamaño de la empresa", "Tamaño de la empresa", "F29")


# ---------------------------------------------------------------------------
# Hoja Inicio
# ---------------------------------------------------------------------------
ini = ws["Inicio"]
ini.sheet_view.showGridLines = False
ini.column_dimensions["A"].width = 2
ini.column_dimensions["B"].width = 34
ini.column_dimensions["C"].width = 70
ini["B2"] = "Monitor de empleo en construcción y vivienda"; ini["B2"].font = f(22, True, AZUL_OSCURO)
ini["B3"] = "GEIH 2026 (DANE), enero a agosto · Prueba técnica, Coordinación Económica · Andrés Nicolás Jiménez Díaz"
ini["B3"].font = f(11, color=GRIS_TEXTO)

ini["B5"] = "Cómo usarlo"; ini["B5"].font = f(13, True, AZUL_OSCURO)
pasos = ["1. Vaya a la hoja Tablero.",
         "2. Elija en las celdas amarillas el territorio, el mes, la ventana (mensual o trimestre móvil) y el sexo.",
         "3. Indicadores, gráficos y tablas se actualizan solos: no hay macros ni botones.",
         "4. Para ciudades use el trimestre móvil: con un solo mes la muestra puede ser pequeña (el tablero lo avisa)."]
for i, p_ in enumerate(pasos):
    c = ini.cell(6 + i, 2, p_); c.font = f(11); ini.merge_cells(start_row=6 + i, start_column=2, end_row=6 + i, end_column=3)

ini["B11"] = "Contenido del libro"; ini["B11"].font = f(13, True, AZUL_OSCURO)
contenido = [("Tablero", "Indicadores y gráficos de empleo, ciudades y hogares, con filtros."),
             ("Resumen_base", "Cruces de la base de microdatos con SUMIFS (verificación y perfil del trabajador)."),
             ("Metodología", "Definiciones, validación con el DANE y cómo se construyó cada cifra."),
             ("Base_construccion", "Microdatos de los 16.699 ocupados en construcción encuestados (ene–ago), listos para tablas dinámicas."),
             ("Datos_empleo", "1.428 combinaciones de indicadores de empleo (territorio × sexo × ventana × mes)."),
             ("Datos_hogares", "1.428 combinaciones de indicadores de hogares y vivienda."),
             ("Datos_ciudades", "Indicadores de las 23 ciudades en trimestre móvil."),
             ("Datos_hogares_ciudad", "Arriendo por ciudad en trimestre móvil."),
             ("Cálculos", "Fórmulas intermedias que alimentan el tablero (no editar)."),
             ("Listas", "Opciones de las listas desplegables.")]
for i, (hoja, desc) in enumerate(contenido):
    r = 12 + i
    c = ini.cell(r, 2, hoja); c.hyperlink = f"#{Q(hoja)}!A1"; c.font = Font(name=FUENTE, size=11, color=AZUL, underline="single")
    ini.cell(r, 3, desc).font = f(11)

ini["B23"] = "Cifras clave · agosto de 2026, total nacional"; ini["B23"].font = f(13, True, AZUL_OSCURO)
clave_ago = '"Total nacional|Todos|Mensual|8"'
fila_ago = f"MATCH({clave_ago},{R_EMP('A')},0)"
fila_hago = f"MATCH({clave_ago},{R_HOG('A')},0)"
cifras = [("Ocupados en construcción", f"=INDEX({R_EMP(col_emp['construccion_miles'])},{fila_ago})", FMT_MILES),
          ("Participación en el empleo total", f"=INDEX({R_EMP(col_emp['participacion'])},{fila_ago})", FMT_PCT),
          ("Informalidad en construcción", f"=INDEX({R_EMP(col_emp['informal_construccion'])},{fila_ago})", FMT_PCT),
          ("Ingreso laboral mediano en construcción", f"=INDEX({R_EMP(col_emp['ingreso_med_construccion'])},{fila_ago})", FMT_PESOS),
          ("Hogares que viven en arriendo", f"=INDEX({R_HOG(col_hog['arriendo'])},{fila_hago})", FMT_PCT)]
for i, (et, form, fmt) in enumerate(cifras):
    r = 24 + i
    ini.cell(r, 2, et).font = f(11)
    c = ini.cell(r, 3, form); c.font = f(11, True); c.number_format = fmt; c.alignment = Alignment(horizontal="left")
ini["B30"] = ("Las cifras de agosto coinciden con el boletín técnico GEIH del DANE (1.790 mil ocupados en construcción; "
              "23.685 mil ocupados; desempleo 9,4 %).")
ini["B30"].font = f(10, italic=True, color=GRIS_TEXTO); ini.merge_cells("B30:C30")


# ---------------------------------------------------------------------------
# Hoja Metodología
# ---------------------------------------------------------------------------
me = ws["Metodología"]
me.sheet_view.showGridLines = False
me.column_dimensions["A"].width = 2; me.column_dimensions["B"].width = 34; me.column_dimensions["C"].width = 95
me["B2"] = "Metodología"; me["B2"].font = f(18, True, AZUL_OSCURO)
defs = [
    ("Fuente", "Microdatos de la Gran Encuesta Integrada de Hogares (GEIH) 2026 del DANE, enero a agosto. Se integraron 5 "
               "módulos (características generales, hogar y vivienda, fuerza de trabajo, ocupados y no ocupados) con R."),
    ("Construcción", "Ocupados en las divisiones 41 (construcción de edificios), 42 (obras de ingeniería civil) y 43 "
                     "(actividades especializadas) de la CIIU Rev. 4 A.C."),
    ("Expansión", "Cada encuestado se multiplica por el factor FEX_C18. En trimestre móvil se suman tres meses y se divide por 3."),
    ("Trimestre móvil", "Promedio de los tres meses que terminan en el mes elegido (por ejemplo, junio–agosto). El primero "
                        "disponible es enero–marzo."),
    ("Informalidad", "Ocupados que no cotizan a pensión (pensionados excluidos). Es una aproximación por seguridad social; la "
                     "medición oficial del DANE usa otra definición."),
    ("Ingreso laboral", "Mediana ponderada de INGLABO entre ocupados con ingreso positivo. Umbral de 1 SMMLV de 2026 "
                        "($1.750.905) con tolerancia de 1 %, porque muchos reportan el mínimo redondeado ($1.750.000)."),
    ("Hogares", "Una fila por jefe o jefa de hogar. Tenencia según P5090; arriendo mediano entre hogares arrendatarios; "
                "hacinamiento crítico = más de 3 personas por cuarto."),
    ("Precisión", "Con menos de 150 encuestados en construcción el tablero muestra un aviso de baja precisión. Los rankings "
                  "de ciudades usan siempre el trimestre móvil y solo las 23 ciudades principales."),
    ("Cómo se construyó", "R: 01_integracion_geih.R (integración y validación), 04 y 06 (indicadores para todas las "
                          "combinaciones de filtros), 07_datos_tablero_excel.R (tablas de este libro). El libro se arma con "
                          "tablero_excel/construir_tablero_excel.py y calcula todo con fórmulas de Excel."),
]
for i, (t, d) in enumerate(defs):
    r = 4 + i
    me.cell(r, 2, t).font = f(11, True)
    c = me.cell(r, 3, d); c.font = f(10); c.alignment = izq
    me.row_dimensions[r].height = 42

r0 = 4 + len(defs) + 1
me.cell(r0, 2, "Validación con el DANE · agosto de 2026, total nacional").font = f(13, True, AZUL_OSCURO)
for j, e in enumerate(["Indicador", "Este libro", "Boletín DANE"]):
    c = me.cell(r0 + 1, 2 + j, e); c.font = f(10, True, "FFFFFF"); c.fill = fill(AZUL); c.alignment = centro
me.column_dimensions["D"].width = 16
val = [("Ocupados (miles)", f"=INDEX({R_EMP(col_emp['ocupados_miles'])},{fila_ago})", 23685, '#,##0'),
       ("Ocupados en construcción (miles)", f"=INDEX({R_EMP(col_emp['construccion_miles'])},{fila_ago})", 1790, '#,##0')]
for i, (et, form, dane, fmt) in enumerate(val):
    r = r0 + 2 + i
    me.cell(r, 2, et).font = f(10)
    c = me.cell(r, 3, form); c.number_format = fmt; c.font = f(10, True); c.alignment = Alignment(horizontal="left")
    c = me.cell(r, 4, dane); c.number_format = fmt; c.font = f(10, color="0000FF")
    c.comment = Comment("Dato del boletín técnico GEIH agosto de 2026 (DANE): "
                        "https://www.dane.gov.co/files/operaciones/GEIH/bol-GEIH-ago2026.pdf", "Tablero")


# ---------------------------------------------------------------------------
# Detalles finales
# ---------------------------------------------------------------------------
for h in ["Cálculos", "Listas"]:
    ws[h].sheet_properties.tabColor = "A3A8B0"
for h in ["Inicio", "Tablero", "Resumen_base", "Metodología"]:
    ws[h].sheet_properties.tabColor = AZUL
for h in ["Base_construccion", "Datos_empleo", "Datos_hogares", "Datos_ciudades", "Datos_hogares_ciudad"]:
    ws[h].sheet_properties.tabColor = VERDE
# Impresión: el tablero cabe en el ancho de una página vertical
tb.print_area = "A1:R135"
tb.page_setup.orientation = "portrait"
tb.page_setup.fitToWidth, tb.page_setup.fitToHeight = 1, 0
tb.sheet_properties.pageSetUpPr.fitToPage = True
tb.print_options.horizontalCentered = True
for fila in (51, 82, 118):
    tb.row_breaks.append(Break(id=fila))

for h, orient in [("Inicio", "portrait"), ("Resumen_base", "landscape"), ("Metodología", "landscape")]:
    ws[h].page_setup.orientation = orient
    ws[h].page_setup.fitToWidth, ws[h].page_setup.fitToHeight = 1, 0
    ws[h].sheet_properties.pageSetUpPr.fitToPage = True

wb.active = wb.sheetnames.index("Tablero")
wb.calculation.fullCalcOnLoad = True

import os
if os.environ.get("VISTA_PREVIA"):   # solo para revisar el render en PDF: oculta las hojas largas
    for h in hojas:
        if h.startswith("Datos") or h in ("Base_construccion", "Cálculos", "Listas"):
            ws[h].sheet_state = "hidden"
    SALIDA = Path(os.environ["VISTA_PREVIA"])
wb.save(SALIDA)
print(f"Libro guardado: {SALIDA}")

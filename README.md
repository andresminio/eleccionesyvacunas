# Población vacunada en el contexto del proceso electoral argentino 2021
En este documento se observa la evolución de la tasa de población vacunada para COVID durante el año 2021, con el propósito de analizar la protección de grupos de riesgo y población general previo a las elecciones legislativas en Argentina.
Los datos provienen del Registro Federal de Vacunación (NOMIVAC) del Ministerio de Salud y comprenden casi 100 millones de registros de dosis aplicadas desde 2020. Pueden consultarse en https://www.datos.gob.ar/th/dataset/salud-vacunas-contra-covid-19-dosis-aplicadas-republica-argentina 

En este proyecto se descargó el dataset, se particionó en 38 archivos para poder ser procesados, se estimo la cantidad de vacunados por distrito y por día y se constituyó un nuevo datas set con esa información. Adicionalmente, se graficó la curva de vacunación y de casos por distrito y día.  

En la carpeta encontraras:
-Código para procesar la información de NOMIVAC (consolidacion vacunas.R)
-Data set resultante del procesamiento (vacunaspordia.csv) y de casos reportados por día (casos.csv)
-Código para graficar la información (graficos casos vacunas.R)
-Gráficos de vacunación y casos por distrito (vacunados.TIFF y casos.TIFF)


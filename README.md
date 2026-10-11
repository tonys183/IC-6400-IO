# IC-6400 — Investigación de Operaciones

Repositorio del curso Investigación de Operaciones (IC-6400).

## Tarea 1 (Análisis bibliométrico)

La tarea consistía en analizar literatura científica relacionada con la Investigación de Operaciones para estudiar su evolución, los autores y fuentes más relevantes y las principales líneas de investigación. El trabajo incluía indicadores de producción científica, una red de coocurrencia de palabras clave, un mapa temático y un análisis adicional de colaboración entre autores.

Para resolverla, se utilizaron registros bibliográficos de PubMed y una base depurada, procesados mediante un script en R con `bibliometrix`, `igraph` y `ggplot2`. El código genera tablas descriptivas y figuras de producción anual, coocurrencia, estructura temática y colaboración científica. La carpeta incluye las bases bibliográficas, el código de análisis, las figuras y anexos para reproducir la preparación de los datos.

## Tarea 2 (Análisis de un sistema de colas M/M/1)

La tarea consistía en estudiar un sistema real de una sola cola y un solo servidor mediante datos de llegadas y tiempos de atención. El objetivo era evaluar si las observaciones se ajustaban a las distribuciones de Poisson y exponencial, estimar las tasas de llegada y servicio, y analizar indicadores como la utilización del servidor, la cantidad promedio de unidades en cola y los tiempos de espera.

Para resolverla, se desarrolló un script en R que importa los datos desde Excel, prepara los tiempos de llegada, espera y servicio, y realiza pruebas de bondad de ajuste mediante chi-cuadrado. También se utilizó el paquete `queuecomputer` para reproducir la atención con un servidor y obtener resúmenes por día, junto con el cálculo de las medidas teóricas del modelo M/M/1. La carpeta incluye el script, los datos utilizados, la especificación y los artículos de referencia.

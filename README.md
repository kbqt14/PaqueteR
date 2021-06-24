Los datos a importar deben estar contenidos en un excel deben tener los siguientes titulos organizados en columnas Tratamiento Tiempo Longitud y se lo debe llamar "DatosTabla", Se debe comprobar que el archivo está abierto en la sección de Environment en RStudio. Algunas formas de ayuda para localizar los archivos son la función file.choose() Observacion: Si bien los datos que estan indicados son para la variable longitud, esta variable puede ser cambiada por otra que sea de interes por el investigador Un ejemplo de importar el excel en la consola es el siguiente:

DatosTabla <- read_excel("Aquisedebeutilizarfile.choose()parasaberlaruta/DatosTabla.xlsx",col_types = c("text","text","numeric"))

un ejemplo de utiizacion de PaquetePlantico sería:

install.packages("devtools")

library(devtools)

require(devtools)

install_github("kbqt14/PaquetePlantico")

library(PaquetePlantico)

require("PaquetePlantico")

require(dplyr)

require(readxl)

require(psych)

require(ggplot2)

Donde a dar run

Tabla7A(Datostabla) aparece el analisis de la varianza y un boxplot del tiempo 7.

El paquete cuenta con la posibilidad de filtrar los distintos tiempos con sus respectivos tratamientos y generar asi una tabla resumen con el analisis de varianza a distintos tiempos y prueba a posteriori de Tukey y grafica de boxplot en el tiempo que uno indique.

Se debe utilizar de la siguiente forma para obtener el analisis de varianza, Test de Tukey y boxplot:

Tabla_t_A(DatosTabla, 1 )para ANOVA y boxplot de los tratamientos del tiempo 1,

Tabla_t_A(DatosTabla, 2 )para ANOVA y boxplot de los tratamientos del tiempo 2, y así sucesivamente para los tiempos de interés

Se debe utilizar de la siguiente forma para obtener la desviación estándar:

Tabla7DE(DatosTabla) para la desviación estándar del tiempo 7

Tabla6DE(DatosTabla) para la desviación estándar del tiempo 6,

Tabla5DE(DatosTabla) para la desviación estándar del tiempo 5,

Tabla4DE(DatosTabla) para la desviación estándar del tiempo 4,

Tabla3DE(DatosTabla) para la desviación estándar del tiempo 3

Tabla2DE(DatosTabla) para la desviación estándar del tiempo 2,

Tabla1DE(DatosTabla) para la desviación estándar del tiempo 1

Se debe utilizar de la siguiente forma para obtener el resumen de los estadisticos de mediana, media, desviación estandar:

Resumentiempo7(DatosTabla) para el resumen del tiempo 7,

Resumentiempo6(DatosTabla) para el resumen del tiempo 6,

Resumentiempo5(DatosTabla) para el resumen del tiempo 5,

Resumentiempo4(DatosTabla) para el resumen del tiempo 4,

Resumentiempo3(DatosTabla) para el resumen del tiempo 3,

Resumentiempo2(DatosTabla) para el resumen del tiempo 2,

Resumentiempo1(DatosTabla) para el resumen del tiempo 1

Se debe utilizar de la siguiente forma para obtener los gráficos de barras de las medias de los tratamiento en un periodo de tiempo determinado con su desviación estandar:

Graficadebarratiempo7(DatosTabla) para el grafico de barras del tiempo 7,

Graficadebarratiempo6(DatosTabla) para el grafico de barras del tiempo 6,

Graficadebarratiempo5(DatosTabla) para el grafico de barras del tiempo 5,

Graficadebarratiempo4(DatosTabla) para el grafico de barras del tiempo 4,

Graficadebarratiempo3(DatosTabla) para el grafico de barras del tiempo 3,

Graficadebarratiempo2(DatosTabla) para el grafico de barras del tiempo 2,

Graficadebarratiempo1(DatosTabla) para el grafico de barras del tiempo 1

El objetivo es poder utilizar los datos estadisticos descriptivos y de esta manera obtener conclusiones respecto a las plantas que son sometidas a estrés por con concentraciones de distintas sustancias.

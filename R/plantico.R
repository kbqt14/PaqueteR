#' El paquete tiene como finalidad que los usuarios puedan procesar los datos tomados en distintos tiempos de experimentos con plantas, para realizar conclusiones en base a una estadistica descriptiva donde se pueden obtener las distintas medias, desviaciones estandar y graficos del tipo boxplot de cada tratamiento en el tiempo indicado. En el paquete la variable utilizada es longitud sin embargo puede ser utilizada otra variable previa modificaciones en las funciones 
#' @param x 
#' @keywords
#' @export
#' @examples
#' Para instalar y hacer uso del paquete se debe contar con
#' require(devtools)
#' install_github("kbqt14/PaquetePlantico")
#'require(dplyr)
#'Los datos a importar deben estar contenidos en un excel deben tener los siguientes titulos organizados en columnas Tratamiento	Tiempo	Longitud y se lo debe llamar "DatosTabla", Se debe comprobar que el archivo está abierto en la sección de Environment en RStudio. Algunas formas de ayuda para localizar los archivos son la función file.choose()
#'Un ejemplo de importar el excel en la consola es el siguiente DatosTabla <- read_excel("Aquisedebeutilizarfile.choose()parasaberlaruta/DatosTabla.xlsx",col_types = c("text","text","numeric"))
#'un ejemplo de utiizacion de PaquetePlantico sería:
#'install.packages("devtools")
#'library(devtools)
#'require(devtools)
#'install_github("kbqt14/PaquetePlantico")
#'library(PaquetePlantico)
#'require("PaquetePlantico")
#'install.packages("dplyr")
#'library(dplyr)
#'require(dplyr)
#'install.packages("readxl")
#'library(readxl)
#'require(readxl)
#install.packages("psych")
#library(psych)
#require(psych)
#install.packages("ggplot2")
#library(ggplot2)
#require(ggplot2)
#'DatosTabla <- read_excel("Aquisedebeutilizarfile.choose()parasaberlaruta/DatosTabla.xlsx",col_types = c("text","text","numeric"))
# Resumentiempo_t se le pasa los datos y el tiempo y genera un resumen de estadisticos descriptivos 
# de los datos del tiempo t

Resumentiempo_t <- function(datos, t){
  Tiempo<- filter(datos, Tiempo== as.character(t))
  ResumenTiempo_t<- describeBy(Tiempo$Longitud,Tiempo$Tratamiento)
  ResumenTiempo_t
}


# Tabla_t_A se le pasan los datos y el tiempo del cual se quiere generar un boxplot y un ANOVA 
# en el tiempo deseado

Tabla_t_A <- function(DatosTabla, t){
  Tiempo_t<- filter(DatosTabla, Tiempo== as.character(t))
  ResumenTiempo_t<- describeBy(Tiempo_t$Longitud,Tiempo_t$Tratamiento)
  ResumenTiempo_t
  boxplot(Longitud~Tratamiento, data=Tiempo_t, main = paste("Tiempo", t), 
          col = c(2,3,4))
  AnovaTiempo_t<- aov(Longitud~Tratamiento, data=Tiempo_t)
  TukeyHSD(AnovaTiempo_t)
}



# Si se quisiera colocar todos los boxplots de los tiempos hasta t, se puede correr el sgte codigo:

# par(mfrow=c(2,3))

# Como 7 queda impar, se puede empezar desde el tiempo 2 hasta el 7:

# for (t in 2:7){Tabla_t_M(DatosTabla, t)}


# Graficabarratiempo_t genera un grafico de barras de los datos en el tiempo t

Graficadebarratiempo_t <- function(DatosTabla, t){
  Tiempo_t<- filter(DatosTabla, Tiempo== as.character(t))
  
  Tiempo_t<-Tiempo_t %>%  group_by(Tratamiento) %>%
  summarise_at(vars(Longitud), list(mean=mean,sd=sd))
  
  Tiempo_t_graf <-ggplot(Tiempo_t, aes(x=Tratamiento, y=mean,
                                       ymin=mean-sd, ymax= mean+sd))
  
  
  Tiempo_t_graf + geom_bar(aes(color = Tratamiento), stat = "identity", fill ="white") +
    geom_errorbar(aes(color = Tratamiento), width = 0.2)+ labs(x="Tratamiento",y="Longitud")
  
}

# Con esta funcion solo es necesario pasar los datos de la tabla, y el tiempo hasta el cual se quiere
# graficar las lineas sombreadas con su desvio estandar.
grafico_lineas_tiempo_t<- function(datos, t){
  data <- as.data.frame(datos[datos$Tiempo <=t,])
  
  medias_y_desvios <- aggregate(. ~ Tratamiento + Tiempo, data, function(x) c(media = mean(x), desvio = sd(x)))
  
  medias_y_desvios_2 <- data.frame(Tratamiento = as.factor(medias_y_desvios$Tratamiento), Tiempo = as.factor(medias_y_desvios$Tiempo), Longitud_media = medias_y_desvios$Longitud[,1], Long.sd = medias_y_desvios$Longitud[,2])
  
  ggplot(medias_y_desvios_2, aes(x = Tiempo, y = Longitud_media, group = Tratamiento))+
    geom_line(aes(color = Tratamiento))+
    geom_point(aes(color = Tratamiento)) +
    labs(title = "Longitud de raices segun tratamiento", y = "Longitud (cm)", x = "Tiempo (dias)") +
    geom_ribbon(aes(ymin=Longitud_media-Long.sd, ymax=Longitud_media+Long.sd, fill = Tratamiento), alpha = 0.15)
  
}


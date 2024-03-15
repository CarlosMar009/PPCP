library(astsa)
library(tseries)
library(TTR)
library(ggplot2)
library(dynlm)
library(coin)
library(forecast)
library(nortest)

datos<-read.csv("Horas_Hombre.txt",header=FALSE)
Datatimeseries<-ts(datos,start=2018,frequency = 12)
Datatimeseries

plot(Datatimeseries)

adf.test(Datatimeseries)

#El p-value nos da mucho menor al nivel de significancia de 0,05. Se rechaza la hipotesis nula.
#La serie es estacionaria bajo un nivel de confianza del 95%.

#Se quiere determinar mediant el analisis de graficos si la serie de tiempo tiene estacionalidad.

plot(decompose(Datatimeseries))
ggseasonplot(Datatimeseries)
ggseasonplot(Datatimeseries,polar=TRUE)

#Con los graficos anteriores, se pudo observar que no existe una estacionalidad significativa que
#se deba usar en el modelo.

#Para la partición de los datos de entrenamiento y los datos de prueba decidimos usar una relación
#70/30 para la totalidad de los meses debido a no existe estacionalidad y no es necesario analizar 
#el comportamiento por año.

DataTraining<-datos[1:51,]
DataTesting<-datos[52:72,]
TSTraining<-ts(DataTraining)
TSTesting<-ts(DataTesting)

plot(TSTraining)
plot(TSTesting)

#Analizaremos la estacionaridad para ambos grupos de datos.

adf.test(TSTraining)
adf.test(TSTesting)

#Con la prueba Dicky Fuller se mantiene el comportamiento estacionario para el grupo de datos de training debido
#a que el p-value es menor al nivel de significancia establecido (0,05). Por otro lado, prueba no captura
#el comportamiento estacionario para el grupo de testing al darnos un p-value- de 0,6329 mucho mayor al nivel de
#significancia. Esto probablemente debido a la potencia baja de la prueba y no tener la suficiente
#cantidad de datos para capturar el comportamiento de la serie.

#Para comprobar esto, debemos empezar a prestarle datos al testing para dado el caso el p.value empiece a 
#decrecer, se podria determinar que si es estacionaria.

#A medida que agregamos datos al testing uno a uno. observamos una disminución del p-value. No fue hasta
#que tuvimos una partición de 33:72, en el testin que su p-value fue menor a 0,05 con un valor de
#0,0369.

par(mfrow=(c(1,2)))
acf(TSTraining)
pacf(TSTraining)

#Con los graficos acf y pacf se pudo determinar que existe una autocorrelación de la serie de tiempo, ya que 
#para ambas graficas se presentaban resagos significativos que superaban las bandas de significancia. Por ende, es plausible
#utilizar el modelo ARMA para hacer la predicción de la demanda.

#Observamos en el grafico de ACF como se cortaban las bandas en los resagos 1, 11 y 12. Sin embargo, cuando
#hicimos la prueba Dicky Fuller se observo que el lag order fue de 3; es decir, que más allá de 3 periodos no aporta 
#expliación para el comportamiento de la variable. Tambien, teniendo en cuenta el contexto, se considera que tener en cuenta 
#hasta 12 meses atras (casi un año) para el numero de horas hombre requeridas en el siguiente mes, no seria un modelo 
#parsimonioso (tiene un nivel de complejidad mucho mayor al tener en cuenta 12 meses). Por el momento, para el componente de 
#media movil q=1.

#Observamos en el grafico de ACF como se cortaban las bandas en los resagos 1 y 11. Por el momento, se escogera 
#un componente de correlación parcial p=1. 

ModeloARMA1<-arima(TSTraining,order=c(1,0,1))
summary(ModeloARMA1)
tsdiag(ModeloARMA1)
#En modelo1 con parametros de (1,0,1) se observo con la función tsdiag un comportamiento normal de los residuos
#alrededor del 0 entre (-2,2). Para la grafica ACF de los residuos se observa una autocorrelación de los 
#mismos en los periodos anteriores 11 y 12.

ModeloARMA2<-arima(TSTraining,order=c(1,0,11))
summary(ModeloARMA2)
tsdiag(ModeloARMA2)

#En este modelo2 con los parametros (1,0,11) se mantiene el comportamiento de los residuos con media cero 
#en el rango de (-3,2). No se observa en el grafico ACF de los residuos una autocorrelación, ay que no se 
#encuentra un resago que corte las bandas de significancia.

#PARAMETRIZACIÓN
#Debido a se desea optimizar el modelo con la metrica de error MAD y tener en cuenta la complejidad 
#del modelo (AIC). Debemos usar estas metricas para comparar el comportamiento en el (1,0,1) y (1,0,11).

#Se pudo observar que con p=1 y q=11, el MAD es menor que un modelo con p=1 y q=1 (18.38).
#A su vez, se tuvo en cuenta la complejidad del modelo, ya que en este modelo el AIC=515.37 y
# en el otro un AIC=515.74. 

#Por lo anterior, decidimos usar una configuración para el modelo de (1,0,11).

PronosticoARMA<-forecast::forecast(ModeloARMA2,h=21)
PronosticoValores<-as.numeric(PronosticoARMA$mean) 
ErrorABSTesting<-abs(PronosticoValores-TSTesting)
MADTesting<-mean(ErrorABSTesting)
MADTesting

ErroresABSTraining<-as.numeric(abs(PronosticoARMA$residuals))
MADTraining<-mean(ErroresABSTraining)
MADTraining

shapiro.test(ErrorABSTesting)
shapiro.test(ErroresABSTraining)

#Ningun de los conjuntos de datos tienen un comportamiento normal debido a que p-value
#de ambos es menor al nivel de significancia de 0,05. Continuamos con pruebas
#no parametricas.

fligner.test(x=list(ErroresABSTraining,ErrorABSTesting))
#No se cumple la homogeneidad de varianzas debido a que se rechaza la Ho de que las varianzas son 
#homogeneas con un p-value de 0,0204.

wilcox.test(ErroresABSTraining,ErrorABSTesting)
#Se acepta la hipotesis nula de que las medias de ambas metricas de error son iguales 
#debido a que el p-value es mayor al nivel de significancia (0,3097).

#PRUEBA DE INDEPENDENCIA
#Con el gráfico de p-value for Ljung-Box statistic se logra observar los errores del modelo escogido
#son independientes ya que el p-value es mayor al nivel de significancia (0,05).


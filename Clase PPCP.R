library(astsa)
library(tseries)
library(forecast)
library(ggplot2)
library(dynlm)
library(nortest)
library(coin)
library (TTR)

Data<-read.csv("Paper Sales.txt",header =F)
DataTimeSeries<-ts(Data,start=1,frequency=1)
DataTimeSeries

plot(DataTimeSeries)

DataTraining<-Data[1:60,]
DataTesting<-Data[61:84,]

TSTraining<-ts(DataTraining,start=1,frequency=1)
TSTesting<-ts(DataTesting,start=1,frequency=1)

#plot the data
par(mfrow=(c(2,1)))
plot(TSTraining)
plot(TSTesting)

#Prueba
adf.test(TSTraining)
adf.test(TSTesting)

modelo1<-HoltWinters(DataTraining,alpha=0.613, beta=0.167, gamma=FALSE)
plot(modelo1)

#MODELO ARIMA
plot(DataTimeSeries)

datos_dif=diff(DataTimeSeries)
plot(datos_dif)

DataTraining_diff<-Data[1:60,]
DataTesting_diff<-Data[61:83,]



adf.test(DataTraining)
adf.test(DataTesting)

acf(DataTimeSeries)
pacf(DataTimeSeries)

ModeloARMA=arima(TSTraining, order=c(1,1,0))
ModeloARMA
residuos<-residuals(ModeloARMA)
residuosabsolutosTrain<-abs((residuos))
pronostico<.predict(ModeloARMA,n.ahead=24)$pred
residuosabsolutosTest<-abs(pronostico-as.numeric(residuosabsolutosTrain))


mean(residuduosabsolutosTrain)
mean(residuduosabsolutosTrain)



#
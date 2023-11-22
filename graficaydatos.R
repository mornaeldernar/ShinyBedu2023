# Instalar la librería por primera vez
# install.packages("dlyr")
# install.packages("stringr")

# Cargas la libreria
library(dplyr) # manipulamos datos
library(stringr) # manipulamos cadenas de texto
library(xlsx)
library(readr)

library(forecast)
library(tseries)

graficaPrediccion <- function(){
  
  load("Japan_world.RData")
  load("Japan_world_timeseries.RData")
  load("Japan_world_timeseries.decomp.RData")
  load("modelo_arima_w1.RData")
  prediccion_w<-forecast(modelo_arima_w1, h=12)
  
  plot(prediccion_w, main = "Predicción de visitas en Japón")
}
grafica1 <- function(df, pais) {
  if(pais == "Todos"){
    japan_timeseries <- ts(df$Visitantes,start=c(2017,1),end=c(2023,12),frequency=12)
  }else {
    japan_timeseries <- ts(df[df$Pais == pais,]$Visitantes,start=c(2017,1),end=c(2023,12),frequency=12)
  }
  inicio_covid<- strptime('2020-01-23',format='%Y-%m-%d')
  olimpiadas_date <- as.Date('2021-07-23')
  plot(japan_timeseries, main=paste("PAIS: ",pais),xlab="Fecha", ylab="# Visitantes")
  abline(v=2020,lwd=2, col='red');
  
  abline(v = as.numeric(('2021.583')), lwd = 2, col = 'blue')
  
  
  
  legend("topright", legend = c("COVID-19 Start", "Olimpiadas"), lty = 1, col = c("red", "blue"))
}

leeDatos <- function(df){
  paises<-unique(df$Pais)
  nombresPaises <- c("Todos",iconv(paises, "latin1", "UTF-8"))
  return(nombresPaises)
}

leeDatos2 <- function(df){
  periodo <- c("Todos",unique(df$Periodo))
  return(periodo)
}


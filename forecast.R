
library(dplyr) # libreria para la manipulacion de datos
library(stringr) # libreria para la manipulacion  de cadenas de texto

#install.packages("ggplot2") # por primera vez se instala la libreria
library(ggplot2) # libreria para graficar datos


setwd("C:\\Users\\Hernandezk\\Desktop\\KYHM") # linea opcional para especificar donde vive nuestro proyecto

archivo<-"Japan.csv" # asignar el nombre del csv a la variable archivo que contiene los datos

#df[col1] = df{col1 *2}

df <- read.csv("Japan.csv") # Carga un archivo csv llamado archivo, y lo llama df
#df <- data.frame(df) # convierte la variable df en un dataframe
df <- rename(df,  Pais=Country.Area, 
             Mes=Month, 
             Anio=Year,
             Visitantes=Visitor) # se renombran las columnas

paises <- unique(df$Pais)  # Variable que contiene la lista de paises que aparecen al menos una vez

df <- df %>%  # Este anidamiento no es una forma natural de expresar un secuencia de operaciones. El operador %>% nos permite escribir una secuencia de operaciones de izquierda a derecha:
  mutate(Mes = str_replace(df$Mes, "[.]","")) # mutate(): La función mutate del paquete dplyr se utiliza para agregar nuevas variables o modificar variables existentes en un marco de datos.

df <- df %>%
  mutate(Mes = as.integer(factor(df$Mes, levels = month.abb))) # como la columna de mes son letras, los cambiamos a numeros  

df <- df %>%
  mutate(M0 =  ifelse(df$Mes < 10 , str_replace(paste(0, df$Mes),"[ ]",""), df$Mes))
# crea una columna llamada M0, para cambiar 1 o 2 a 01 y 02

df <- df %>%
  mutate(Periodo = paste(df$Anio, sep="-", df$M0,"01"))

df <- df %>%
  mutate(Periodo = as.Date(df$Periodo))
df <- df %>% select('Pais','Visitantes','Periodo')
save(df, file = "df.Rdata")

# Se filtran datos para México

dfMex<-subset(df, df$Pais=="Mexico")
dfMex<-subset(dfMex, dfMex$Visitantes!=0)

#hist(dfMex$Visitantes, prob = T, ylab = "", xlab = "", main = "")
points(dfMex$Visitantes, dnorm(dfMex$Visitantes), type = "l")
japan_mex_timeseries <- ts(dfMex$Visitantes,start=c(2017,1),end=c(2023,6),frequency=12)
plot(japan_mex_timeseries)
japan_mex_timeseries.decomp <- decompose(japan_mex_timeseries, type = "mult")
japan_mex_timeseries.decomp
plot(japan_mex_timeseries.decomp, xlab = "Tiempo", 
     sub = "Descomposición de los datos de visitantes de Japón")

library(forecast)
library(tseries)

# Se gráfica la función de autocorrelación y la función de autocorrelación parcial
ggtsdisplay(japan_mex_timeseries,main='Visitantes mexicanos en Japón (ACF y PACF)')
# se observa Autocorrelación significativa solo en el primer lag
# Autocorrelación significativa solo en el primer retraso (lag) estacional

# Test de Dickey-Fuller
adf<-adf.test(japan_mex_timeseries)
adf$p.value

# Se realiza un modelo ARMA con una combinación de un modelo ARIMA no estacional y uno estacional
# Diferenciación de orden 1 para hacer estacionaria la serie de tiempo.Parte no estacional
# En la parte estacional se utiliza un término autorregresivo estacional
modelo_arima_m1<- arima(japan_mex_timeseries, order = c(0, 1, 0),
                       seasonal = list(order=c(1,0,0)))
#Modelo sin diferenciación de orden 1 en la parte no estacional
modelo_arima_m2<- arima(japan_mex_timeseries, order = c(0, 0, 0),
                      seasonal = list(order=c(1,0,0)))
# Se comparan modelos
AIC(modelo_arima_m1,modelo_arima_m2)
BIC(modelo_arima_m1,modelo_arima_m2)
# El modelo 1 es mejor que el 2

#Modelo con AR(1) y diferenciación de orden 1 e la parte no estacional
modelo_arima_m3<-arima(japan_mex_timeseries, order = c(1, 1, 0),
                     seasonal = list(order=c(1,0,0)))
AIC(modelo_arima_m1,modelo_arima_m3)
BIC(modelo_arima_m1,modelo_arima_m3)
# Se decide por el modelo 1

# Pronóstico
prediccion<- forecast(modelo_arima_m1, h=12) 
plot(prediccion, main = "Predicciones visitas de mexicanos a Japón")
print(prediccion) #Datos predichos 


# Totales Japón -----------------------------------------------------------

# Se calculan los totales por periodo para Japón 

Japan_world <- df %>% group_by(Periodo) %>% 
  summarise(Visitantes = sum(Visitantes),
            .groups = 'drop')

save(Japan_world, file="Japan_world.RData")
points(Japan_world$Visitantes, dnorm(Japan_world$Visitantes), type = "l")
Japan_world_timeseries <- ts(Japan_world$Visitantes,start=c(2017,1),end=c(2023,6),frequency=12)
plot(Japan_world_timeseries)

save(Japan_world_timeseries, file="Japan_world_timeseries.RData")
Japan_world_timeseries.decomp <- decompose(Japan_world_timeseries, type = "mult")
Japan_world_timeseries.decomp

save(Japan_world_timeseries.decomp, file="Japan_world_timeseries.decomp.RData")
plot(Japan_world_timeseries.decomp, xlab = "Tiempo", 
     sub = "Descomposición de los datos de visitantes de Japón")

# Se gráfica la función de autocorrelación y la función de autocorrelación parcial
ggtsdisplay(Japan_world_timeseries,main='Visitantes en Japón (ACF y PACF)')
# se observa Autocorrelación significativa solo en el primer lag
# Autocorrelación significativa solo en el primer retraso (lag) estacional

#Test Dickey-Fuller
adf<-adf.test(Japan_world_timeseries)
adf$p.value
#Es estacionaria

# Se realiza un modelo ARMA con una combinación de un modelo ARIMA no estacional y uno estacional
# Diferenciación de orden 1 para hacer estacionaria la serie de tiempo y un MA(1). Parte no estacional
# En la parte estacional se utiliza un término autorregresivo estacional
modelo_arima_w1<- arima(Japan_world_timeseries, order = c(0, 1, 1),
                      seasonal = list(order=c(1,0,0)))
#Modelo sin diferenciación de orden 1 en la parte no estacional
modelo_arima_w2<- arima(Japan_world_timeseries, order = c(0, 0, 1),
                      seasonal = list(order=c(1,0,0)))
# Se comparan modelos
AIC(modelo_arima_w1,modelo_arima_w2)
BIC(modelo_arima_w1,modelo_arima_w2)
# El modelo 1 es mejor que el 2

#Modelo con AR(1) y diferenciación de orden 1 e la parte no estacional
modelo_arima_w3<-arima(Japan_world_timeseries, order = c(1, 1, 1),
                     seasonal = list(order=c(1,0,0)))
AIC(modelo_arima_w1,modelo_arima_w3)
BIC(modelo_arima_w1,modelo_arima_w3)
# Se decide por el modelo 1

# Pronóstico
prediccion_w<- forecast(modelo_arima_w1, h=12) 
plot(prediccion_w, main = "Predicción de visitas en Japón")
print(prediccion_w) #Datos predichos 
#install.packages('xlsx')
library(xlsx)
write.xlsx(prediccion_w, "prediccion_w.xlsx")
save(modelo_arima_w1, file="modelo_arima_w1.RData")
save(prediccion_w, file="prediccion_w.RData")
sink("modelo_arima_w1.txt")
  modelo_arima_w1
sink()

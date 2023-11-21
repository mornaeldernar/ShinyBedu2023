
library(dplyr) # libreria para la manipulacion de datos
library(stringr) # libreria para la manipulacion  de cadenas de texto

install.packages("ggplot2") # por primera vez se instala la libreria
library(ggplot2) # libreria para graficar datos


setwd("/Users/sergiogeraldo/Documents/Cursos/BEDU - Data Scientist/Módulo 5 R/17 R") # linea opcional para especificar donde vive nuestro proyecto

archivo <- "Japan.csv" # asignar el nombre del csv a la variable archivo que contiene los datos

'df[col1] = df[col1*2]'

df <- read.csv(archivo) # Carga un archivo csv llamado archivo, y lo llama df
df <- data.frame(df) # convierte la variable df en un dataframe
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
  mutate(Periodo = as.Date(df$Periodo)) # Cambia el tipo de dato de númerico a fecha

df <- df %>% select('Pais','Visitantes','Periodo') # Se seleccionan las 3 columnas de interés
save(df, file = "df.Rdata")

dfMex <- subset(df, df$Pais=="Mexico")

# hist(x = df$Periodo, y = dfMex$Visitantes, prob = T, ylab = "", xlab = "", main = "")
# points(dfMex$Visitantes, dnorm(dfMex$Visitantes), type = "l")

japan_mex_timeseries <- ts(dfMex$Visitantes,start=c(2017,1),end=c(2023,6),frequency=12) # Gráfica serie de tiempo para visualizar los visitantes mexicanos de 2017 a 2023
plot(japan_mex_timeseries)

japan_all_timeseries <- ts(df$Visitantes,start=c(2017,1),end=c(2023,6),frequency=12) # Gráfica serie de tiempo para visualizar los visitantes de 2017 a 2023
plot(japan_all_timeseries)

japan_mex_timeseries.decomp <- decompose(japan_mex_timeseries, type = "mult")
japan_mex_timeseries.decomp
plot(japan_mex_timeseries.decomp, xlab = "Tiempo")

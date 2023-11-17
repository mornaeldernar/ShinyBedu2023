
library(dplyr)
library(stringr)
grafica1 <- function(df, pais) {
  if(pais == "Todos"){
    japan_timeseries <- ts(df$Visitantes,start=c(2017,1),end=c(2023,12),frequency=12)
  }else {
    japan_timeseries <- ts(df[df$Pais == pais,]$Visitantes,start=c(2017,1),end=c(2023,12),frequency=12)
  }
  plot(japan_timeseries)
  
}
leeDatos <- function(df){
  nombresPaises <- c("Todos",unique(df$Pais))
  return(nombresPaises)
}

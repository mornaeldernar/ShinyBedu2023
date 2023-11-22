library(tidyverse)
#devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(lubridate)


load("Jp_map.Rdata")
df_tidy <- df
df_formatted <- df_tidy %>%
  mutate(mes = month(Periodo)) %>%
  mutate(anio = year(Periodo))

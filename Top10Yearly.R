library(tidyverse)
#devtools::install_github('thomasp85/gganimate')
library(gganimate)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
df <- read.csv("Japan.csv") # Carga un archivo csv llamado archivo, y lo llama df
df <- data.frame(df)
df <- rename(df,  Pais=Country.Area, 
             Mes=Month, 
             Anio=Year,
             Visitantes=Visitor)
df <- df %>%  
  mutate(Mes = str_replace(Mes, "[.]","")) # mutate(): La función mutate del paquete dplyr se utiliza para agregar nuevas variables o modificar variables existentes en un marco de datos.
# crea una columna llamada M0, para cambiar 1 o 2 a 01 y 02

df <- df %>%
  mutate(Mes = as.integer(factor(df$Mes, levels = month.abb))) # como la columna de mes son letras, los cambiamos a numeros  
df <- df %>%
  mutate(M0 =  ifelse(df$Mes < 10 , str_replace(paste(0, df$Mes),"[ ]",""), df$Mes))
df$Mes <- df$M0

top_10 <- df %>%
  group_by(Anio, Mes) %>%
  slice_max(order_by = Visitantes, n = 10)

print(top_10)

top10_formatted <- top_10 %>%
  # The * 1 makes it possible to have non-integer ranks while sliding
  mutate(rank = rank(-Visitantes),
         Value_lbl = paste0(" ",round(Visitantes/1000),"K"),
         state = paste0(Anio,Mes)) %>%
  group_by(Pais) %>% 
  filter(rank <=10) %>%
  ungroup()
save(top10_formatted,file='top10.RData')

load('top10.RData')
staticplot <-  ggplot(top10_formatted, aes(rank, group = Pais, 
                                       fill = as.factor(Pais), color = as.factor(Pais))) +
  geom_tile(aes(y = Visitantes/2,
                height = Visitantes,
                width = 0.9), alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(Pais, " ")), vjust = 0.2, hjust = 1) +
  
  geom_text(aes(y=Visitantes,label = Value_lbl, hjust=0)) +
  coord_flip(clip = "off", expand = FALSE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.background=element_blank(),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.grid.major.x = element_line( size=.1, color="grey" ),
        panel.grid.minor.x = element_line( size=.1, color="grey" ),
        plot.title=element_text(size=25, hjust=0.5, face="bold", colour="grey", vjust=-1),
        plot.subtitle=element_text(size=18, hjust=0.5, face="italic", color="grey"),
        plot.caption =element_text(size=8, hjust=0.5, face="italic", color="grey"),
        plot.background=element_blank(),
        plot.margin = margin(2,2, 2, 4, "cm"))



anim <- staticplot +
  transition_states(state, transition_length = 4, state_length = 1, wrap = FALSE) +
  view_follow(fixed_x = TRUE)  +
  labs(title = 'Visitantes a Japon en el mes: {closest_state}',  
       subtitle  =  "Top 10",
  ) 
#devtools::install_github("r-rust/gifski")

#install.packages("gifski")

animate(anim, 78, fps = 2,  width = 1200, height = 1000, 
        renderer = gifski_renderer("www/gganim.gif"))

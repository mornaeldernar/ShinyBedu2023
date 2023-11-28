#install.packages("reshape")
library(ggplot2)
library(reshape)
library(dplyr)
setwd('D:/bedu/Final2023R/prototype-day')
load('top10.RData')
x <- melt(top10_formatted)
reshape(x, Pais ~ state + Visitantes)
data_long <- x %>% 
  group_by(Pais, state) %>%
  summarise(Visitantes = sum(Visitantes))
data_wide <- spread(data_long, key = "Pais", value = "Visitantes", fill = 0)

plot(data_wide$state,data_wide$China,
     type = "l",
     col = 2,
     ylim = c(- 15, 40),
     xlab = "Year",
     ylab = "Values")
lines(data_wide$state,                             # Draw second time series
      data_wide$Taiwan,
      type = "l",
      col = 3)
lines(data_wide$state,                             # Draw second time series
      data_wide$`South Korea`,
      type = "l",
      col = 3)

legend("topright",                           # Add legend to plot
       c("China", "Taiwan", "South Korea"),
       lty = 1,
       col = 2:4)

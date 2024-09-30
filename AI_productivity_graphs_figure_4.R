setwd("C:/Users/alcokhart/Desktop/")
library(tidyverse)
library(data.table)
library(ggplot2)
library(DaisTheme)
library(readr)

TFP_adoption <- read_csv("C:/Users/alockhart/Desktop/did_results.csv")
TFP_adoption <- as.data.table(TFP_adoption)


fig_4 <- ggplot(TFP_adoption, aes(x = Year, y = `Difference in Productivity Growth`)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, colour="#eb0072")+
  labs(title = "Figure 4",
       subtitle = "Difference between Treatment and Control on TFP Growth",
       x = "Year",
       y = "Difference in Total Factor Productivity Growth")+
  geom_hline(yintercept = 0)+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

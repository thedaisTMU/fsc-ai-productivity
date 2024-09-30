setwd("C:/Users/alcokhart/Desktop/")
library(tidyverse)
library(data.table)
library(ggplot2)
library(DaisTheme)
library(readr)

TFP_adoption <- read_csv("C:/Users/alockhart/Desktop/did_results.csv")
TFP_adoption$Treatment <- factor(TFP_adoption$Treatment, levels=c("Before", "After"))
TFP_adoption <- as.data.table(TFP_adoption)



fig_4 <- ggplot(TFP_adoption, aes(x = Year, y = `Difference in Productivity Growth`, group=1)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(colour=Treatment))+
  scale_color_manual(values = c("Before" = "#eb0072", "After" = "#004c9b"))+
  geom_errorbar(aes(ymin = Minimum, ymax = Maximum), width = 0.2) +  # Add error bars
  labs(title = "Figure 4",
       subtitle = "Difference between Treatment and Control on TFP Growth",
       x = "Year",
       y = "Difference in Total Factor Productivity Growth")+
  geom_hline(yintercept = 0)+
  geom_vline(xintercept = 2020)+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 




fig_4 <- ggplot(TFP_adoption, aes(x = factor(Year), fill=Treatment)) +
  dais.base.theme()+
  geom_boxplot(
    aes(
      ymin = Minimum,
      lower = Minimum,
      middle = `Difference in Productivity Growth`,
      upper = Maximum,
      ymax = Maximum
    ),
    stat = "identity"
  ) +
  scale_fill_manual(values = c("Before" = "#eb0072", "After" = "#004c9b"))+
  labs(title = "Figure 4",
       subtitle = "Difference between Treatment and Control on TFP Growth",
       x = "Year",
       y = "Difference in Total Factor Productivity Growth")+
  geom_hline(yintercept = 0)+
  #geom_vline(xintercept = 2020)+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

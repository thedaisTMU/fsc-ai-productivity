setwd("C:/Users/alockhart/Desktop/FSC AI Productivity 2024/")
library(tidyverse)
library(data.table)
library(ggplot2)
library(DaisTheme)
library(readr)

TFP_adoption <- read_csv("C:/Users/alockhart/Desktop/FSC AI Productivity 2024/TFP_adoption.csv")
TFP_adoption <- as.data.table(TFP_adoption)

TFP_adoption$financial_year <- as.integer(TFP_adoption$financial_year)
TFP_adoption$C320010 <- as.character(TFP_adoption$C320010)
TFP_adoption[C320010 == "1", C320010 := "Adopters"]
TFP_adoption[C320010 == "3", C320010 := "Non-adopters"]

fig_1 <- ggplot(TFP_adoption, aes(x = financial_year, y = mean_tfp, group =C320010)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = C320010))+
  labs(title = "Figure 1",
       subtitle = "Total Factor Productivity by Year and AI Adoption",
       x = "Year",
       y = "Total Factor Productivity",
       color = "Group")+
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = ""))+
  scale_color_manual(values = c("Adopters" = "#eb0072", "Non-adopters" = "#004c9b"))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

TFPG_adoption <- read_csv("TFPG_adoption.csv")
TFPG_adoption <- as.data.table(TFPG_adoption)
TFPG_adoption$financial_year <- as.integer(TFPG_adoption$financial_year)
TFPG_adoption$C320010 <- as.character(TFPG_adoption$C320010)
TFPG_adoption[C320010 == "1", C320010 := "Adopters"]
TFPG_adoption[C320010 == "3", C320010 := "Non-adopters"]

fig_2 <- ggplot(TFPG_adoption, aes(x = financial_year, y = mean_tfp, group =C320010)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = C320010))+
  labs(title = "Figure 2",
       subtitle = "Total Factor Productivity Growth by Year and AI Adoption",
       x = "Year",
       y = "Total Factor Productivity Growth",
       color = "Group")+
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = ""))+
  scale_color_manual(values = c("Adopters" = "#eb0072", "Non-adopters" = "#004c9b"))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

TFP_empsize <- read_csv("TFP_empsize.csv")
TFP_empsize <- as.data.table(TFP_empsize)
TFP_empsize$EMPSIZE <- as.character(TFP_empsize$EMPSIZE)
TFP_empsize[EMPSIZE == "1", EMPSIZE := "Small"]
TFP_empsize[EMPSIZE == "2", EMPSIZE := "Medium"]
TFP_empsize[EMPSIZE == "3", EMPSIZE := "Large"]

fig_3 <- ggplot(TFP_empsize, aes(x = financial_year, y = mean_tfp, group = EMPSIZE)) +
  dais.base.theme()+
  geom_line(linewidth = 0.75, aes(color = EMPSIZE))+
  labs(title = "Figure 3",
       subtitle = "Total Factor Productivity by Year and Firm Size",
       x = "Year",
       y = "Total Factor Productivity Growth",
       color = "Firm size")+
  scale_y_continuous(labels = scales::dollar_format(prefix = "$", suffix = ""))+
  scale_color_manual(values = c("Small" = "#eb0072", "Medium" = "#004c9b", "Large"= "#000000"))+
  theme(legend.text = element_text(size = 12),
        legend.title =element_text(size = 12),
        axis.title.x = element_text(size=12),
        axis.title.y = element_text(size=12),
        plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 14)) 

export.dais.plot("Exported/Figure_1.pdf",fig_1,p.height = 6.5)
export.dais.plot("Exported/Figure_1.svg",fig_1,p.height = 6.5,type="svg")
export.dais.plot("Exported/Figure_2.pdf",fig_2,p.height = 6.5)
export.dais.plot("Exported/Figure_2.svg",fig_2,p.height = 6.5,type="svg")
export.dais.plot("Exported/Figure_3.pdf",fig_3,p.height = 6.5)
export.dais.plot("Exported/Figure_3.svg",fig_3,p.height = 6.5,type="svg")

################################################################################
################################## Fig.S5 ######################################
################################################################################
library(ggplot2) # version 3.5.1
library(openxlsx) # version 4.2.5.2
library(ggpubr) # version 0.6.0

color_select <- c("October 2022" = "#C693BE", "October 2023" = "#1072BD")

Figure_S5 <- read.xlsx("Figure_S5.xlsx", sheet = "Mesocosm_native_biomass", colNames = TRUE, rowNames = F)
Figure_S5$Time <- as.factor(Figure_S5$Time)
Figure_S5$Time <- gsub("_", " ", Figure_S5$Time)

ggplot(Figure_S5, aes(x = Richness, y = log10(Native_biomass), fill = Time)) +
  geom_point(size = 2, shape = 21) +
  stat_cor(aes(label = paste(after_stat(r.label)), color = Time), 
           label.x = 9.5, label.y = c(2.3, 2.1), size = 4) +
  geom_smooth(aes(color = Time), method = "lm", formula = y ~ x, 
              linetype = 1, se = FALSE, size = 0.8) +
  scale_color_manual(values = color_select) +
  scale_fill_manual(values = color_select) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 12)) +
  theme_bw() + 
  theme(panel.grid = element_blank(),  
        axis.text = element_text(color = "black", size = 11),  
        axis.title = element_text(size = 13),
        legend.text = element_text(color = "black", size = 10),  
        legend.title = element_blank(), 
        legend.position = c(0.8,0.2)) + 
  labs(x = "Native plant richness \n(Number of species added)", 
       y = expression(paste("Native plant biomass (g,  ", log[scriptstyle(10)], ")"))) -> Fig_S5; Fig_S5


  
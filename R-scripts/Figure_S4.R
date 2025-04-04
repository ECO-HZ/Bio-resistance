################################################################################
################################## Fig.S4 ######################################
################################################################################
library(ggplot2) # version 3.5.1
library(openxlsx) # version 4.2.5.2
library(ggpubr) # version 0.6.0
library(patchwork) # version 1.2.0

## Define visualization style
color_select = c("October 2021" = '#1CB3B0', 
                 "October 2022" = '#C693BE', 
                 "October 2023" = "#1072BD")

mytheme <- theme_bw() + 
  theme(panel.grid = element_blank(),  
        axis.text = element_text(color = "black", size = 11),  
        axis.title = element_text(size = 13),  
        legend.position = "none",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text= element_text(size=10),
        legend.background = element_blank(),
        plot.tag = element_text(size = 14, face = "bold")) 

################################## Fig.S4a #####################################
## Loading data
Field_Herbivore <- read.xlsx("Figure_S4.xlsx", sheet = "Field_Herbivore", rowNames = F, colNames = T)
Field_Herbivore$Time <- as.factor(Field_Herbivore$Time)
Field_Herbivore$Time <- gsub("_", " ", Field_Herbivore$Time)

cor.test(Field_Herbivore$Richness, Field_Herbivore$Herbivore_richness)
cor.test(Field_Herbivore$Richness, Field_Herbivore$Herbivore_abundance)

ggplot(Field_Herbivore, aes(x = Richness, y = Herbivore_richness)) +
  geom_point(size = 2, shape = 21, aes(fill = Time)) +
  stat_cor(aes(label = paste(after_stat(r.label)), color = Time), 
           label.x = 2.1, label.y = c(10, 9.2, 8.4), size = 4) +
  geom_smooth(aes(color = Time), method = "lm", formula = y ~ x, 
              linetype = 2, se = FALSE, size = 0.8) +
  geom_smooth(data =  Field_Herbivore, mapping = aes(x = Richness, y = Herbivore_richness),
              method = "lm", formula = y ~ x, linetype = 1, se = FALSE, size = 1, color = "black") +
  scale_color_manual(values = color_select) +
  scale_fill_manual(values = color_select) +
  scale_y_continuous(limits = c(0,11)) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 12)) +
  mytheme + theme(legend.position = c(0.8, 0.85)) + 
  labs(y = "Herbivore richness", 
       x = "Native plant richness \n(Number of species added)",
       tag = "(a)") -> Fig_S4a; Fig_S4a

################################## Fig.S4b #####################################
ggplot(Field_Herbivore, aes(x = Richness, y = Herbivore_abundance)) +
  geom_point(size = 2, alpha = 1, shape = 21, aes(fill = Time)) +
  stat_cor(aes(label = paste(after_stat(r.label)), color = Time), 
           label.x = 9, label.y = c(22, 20.5, 19), size = 4) +
  geom_smooth(aes(color = Time, linetype = Time), method = "lm", formula = y ~ x, 
              se = FALSE, size = 0.8) +
  geom_smooth(data =  Field_Herbivore, mapping = aes(x = Richness, y = Herbivore_abundance),
              method = "lm", formula = y ~ x, linetype = 1, se = FALSE, size = 1, color = "black") +
  scale_color_manual(values = color_select) +
  scale_fill_manual(values = color_select) +
  scale_linetype_manual(values = c("dashed", "solid", "solid")) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 12))+
  mytheme + 
  labs(y = "Herbivore abundance", 
       x = "Native plant richness\n(Number of species added)",
       tag = "(b)") -> Fig_S4b; Fig_S4b

## Combined
(Fig_S4a|Fig_S4b) -> Fig_S4; Fig_S4

### Notice that,
### For more picture details, we have further adjusted it in Adobe illustrator.
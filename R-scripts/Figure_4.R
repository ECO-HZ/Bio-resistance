################################################################################
################################## Fig 4 #######################################
################################################################################
library(openxlsx) # version 4.2.5.2
library(ggplot2) # version 3.5.1
library(dplyr) # version 1.1.1
library(ggpubr) # version 0.6.0
library(lme4) # version 1.1-34
library(lmerTest) # version 3.1-3
library(emmeans) # version 1.10.6
library(patchwork) # version 1.2.0

## Define visualization style
color_select <- c("October 2021" = '#1CB3B0', "October 2022" = "#C693BE", "October 2023" = '#1072BD')

mytheme <- theme_bw() + 
  theme(panel.grid = element_blank(),  
        axis.text = element_text(color = "black", size = 11),  
        axis.title = element_text(size = 13),  
        #legend.position = "none",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text= element_text(size=10),
        legend.background = element_blank(),
        plot.tag = element_text(size = 14, face = "bold")) 

################################## Fig 4a ######################################
## Loading data
Fig4a_data <- read.xlsx("Figure_4.xlsx", sheet = "Figure_4a", colNames = TRUE, rowNames = FALSE)
Fig4a_data$Time <- gsub("_", " ", Fig4a_data$Time)
# more results about the linear mixed models please see Table S7 in supporting information

Fig4a_data$Defoliation_log10 <- log10(Fig4a_data$Defoliation + 1)
summary_data <- Fig4a_data %>% 
  group_by(Time, Richness, Origin) %>% 
  summarise(Mean = mean(Defoliation_log10),SE = sd(Defoliation_log10)/sqrt(n()))

ggplot(summary_data, aes(x = Richness, y = Mean, shape = Origin)) +
  geom_point(summary_data,mapping = aes(fill = Time, shape = Origin, group = Time),
             position = position_dodge(width = 1), size = 2) +  
  geom_errorbar(summary_data, mapping = aes(ymin = Mean - SE, ymax = Mean + SE, color = Time, group = Time), 
                width = 0.5, size = 0.5, position = position_dodge(width = 1)) + 
  geom_smooth(subset(Fig4a_data, Origin == "Invader"), mapping = aes(x = Richness, y = Defoliation_log10), method = "lm",
              formula = y ~ x, linetype = 2, se = FALSE, size = 0.8, colour = "black")+
  geom_smooth(subset(Fig4a_data, Origin == "Native"), mapping = aes(x = Richness, y = Defoliation_log10), method = "lm",
              formula = y ~ x, linetype = 2, se = FALSE, size = 0.8,colour="#FED789FF")+
  scale_shape_manual(values = c(24, 21)) +
  scale_color_manual(values = color_select)+
  scale_fill_manual(values = color_select)+
  scale_x_continuous(breaks = c(2, 4, 6, 8, 12)) + 
  mytheme + theme(legend.position = "none") + 
  labs(x = "Native plant richness \n(Number of species added)", 
       y = expression(atop(
         paste("Defoliation"),
         paste("(%, leaf area removed,  ", log[10](x+1), ")"))),
       tag = "(a)") -> Fig_4a; Fig_4a

################################## Fig 4b ######################################
## Loading data
Fig4b_data <- read.xlsx("Figure_4.xlsx", sheet = "Figure_4b", colNames = TRUE, rowNames = FALSE)

Soil_effect_mod <- lmer(Soil_effect ~ Richness*Origin + (1|plot), Fig4b_data)
aov_Soil_effect <- as.data.frame(anova(Soil_effect_mod))
aov_Soil_effect$p_adj <- round(p.adjust(aov_Soil_effect$`Pr(>F)`, method = "BH"), 5) 
aov_Soil_effect[, c(1,2,5)] <- round(aov_Soil_effect[, c(1,2,5)], digits = 2)
aov_Soil_effect$`Pr(>F)` <- round(aov_Soil_effect$`Pr(>F)`, 5)
print(aov_Soil_effect) 
emt_Soil_effect_mod <- emtrends(Soil_effect_mod, pairwise ~ Origin, var = "Richness")
test(emt_Soil_effect_mod, adjust = "BH")

ggplot(Fig4b_data, aes(x = Richness, y = Soil_effect, fill = Origin, shape = Origin)) + 
  geom_point(size = 2, alpha = 1, position = position_dodge(width = 0)) + 
  geom_smooth(aes(color = Origin), method = "lm", 
              formula = y ~ x, linetype = 2, se = FALSE, size = 0.8) + 
  scale_shape_manual(values = c(24, 21)) +
  scale_fill_manual(values = c('black', '#FED789FF')) +
  scale_color_manual(values = c('black', '#FED789FF')) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 12)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  mytheme + theme(legend.position = c(0.20, 0.15)) + 
  labs(x = "Native plant richness\n(Number of species added)", 
       y = "Soil effect on total biomass\n[ln (non-sterilized/sterilized)]",
       tag = "(b)") -> Fig_4b; Fig_4b
  
################################## Fig 4c ######################################
## Loading data
Fig4c_data <- read.xlsx("Figure_4.xlsx", sheet = "Figure_4c", colNames = TRUE, rowNames = FALSE)
## liner model (OLS)
anova(lm(Soil_effect ~ log10(AP_re_abun*100), data = Fig4c_data))

ggplot(Fig4c_data, aes(x = log10(AP_re_abun*100), y = Soil_effect)) +
  geom_point(size = 2, alpha = 1, shape = 24, fill = "black") +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "black", size = 0.8) + 
  stat_cor(aes(label = paste(after_stat(r.label))), r.digits = 2, 
           label.x = 0.55, label.y = -1.3, size = 4) +
  mytheme + 
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  labs(x = expression(paste(italic("A. philoxeroides"), " relative abundance (%, log"[10], ")")),
       y = "Soil effect on total biomass\n[ln (non-sterilized/sterilized)]", 
       tag = "(c)") -> Fig_4c; Fig_4c
  
################################## Fig 4d ######################################
## Loading data
Fig4d_data <- read.xlsx("Figure_4.xlsx", sheet = "Figure_4d", colNames = TRUE, rowNames = FALSE)
Fig4d_data$Time <- gsub("_", " ", Fig4d_data$Time)
Fig4d_data$Time <- as.factor(Fig4d_data$Time)

# more results about the linear mixed models please see Table S6 in supporting information
cor.test(Fig4d_data$Richness, log10(Fig4d_data$Native_biomass))
Native_bio_mod <- lmer(log10(Native_biomass) ~ Richness*Time + (1|plot), Fig4d_data)
aov_Native_bio_mod <- as.data.frame(anova(Native_bio_mod))
aov_Native_bio_mod$p_adj <- round(p.adjust(aov_Native_bio_mod$`Pr(>F)`, method = "BH"), 5) 
aov_Native_bio_mod[, c(1,2,5)] <- round(aov_Native_bio_mod[, c(1,2,5)], digits = 2)
aov_Native_bio_mod$`Pr(>F)` <- round(aov_Native_bio_mod$`Pr(>F)`, 5)
print(aov_Native_bio_mod)

emt_Native_bio_mod <- emtrends(Native_bio_mod, pairwise ~ Time, var = "Richness")
test(emt_Native_bio_mod, adjust = "BH")

ggplot(Fig4d_data, aes(y = log10(Native_biomass), x = Richness, fill = Time)) +
  geom_point(size=2,alpha=1,shape=21) +
  geom_smooth(aes(color = Time, linetype = Time), method = "lm", 
              formula = y ~ x,se = FALSE, size = 0.8)+
  stat_cor(aes(label = paste(after_stat(r.label)), color = Time), r.digits = 2, 
           label.x = 5.5, label.y = c(0.40,0.20,0.00), size = 4) +
  scale_color_manual(values = color_select) +
  scale_fill_manual(values = color_select) +
  scale_linetype_manual(values = c("solid", "dashed", "solid")) + 
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 12)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.01)) + 
  mytheme + theme(legend.position = c(0.8,0.18)) + 
  labs(x = "Native plant richness \n(Number of species added)", 
       y = expression(paste("Native plant biomass (g,  ", log[scriptstyle(10)], ")")),
       tag = "(d)") -> Fig_4d; Fig_4d

## Combined 
(Fig_4a/Fig_4c)|(Fig_4b/Fig_4d) -> Fig_4; Fig_4

### Notice that,
### For more picture details, we have further adjusted it in Adobe illustrator.

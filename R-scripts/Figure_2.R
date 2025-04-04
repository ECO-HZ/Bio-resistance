################################################################################
################################## Fig 2 #######################################
################################################################################
library(openxlsx) # version 4.2.5.2
library(ggplot2) # version 3.5.1
library(ggeffects) # version 1.5.0
library(patchwork) # version 1.2.0

## Define visualization style
mytheme <- theme_bw() + 
  theme(panel.grid = element_blank(),  
        axis.text = element_text(color = "black", size = 11),  
        axis.title = element_text(size = 13),  
        legend.position = "none",
        legend.key = element_blank(),
        legend.title = element_text(size=11),
        legend.text= element_text(size=10),
        legend.background = element_blank(),
        plot.tag = element_text(size = 14, face = "bold")) 
Fig2_color <- c("2017" = '#26B8E9', "2019" = "#35B676", "2020" = '#FFB636', "2021" = '#5C66A9')

################################## Fig 2a ######################################
## Loading data
Fig2a_data <- read.xlsx("Figure_2&Table_S4&Table_S5&Table_AP_def&Her_rich&path.xlsx", sheet = "Field_survey", colNames = TRUE, rowNames = FALSE)
dim(Fig2a_data)

## relationship between relative abundances of A. philoxeroides and all other alien species
cor.test(log10(Fig2a_data$AP_re_abun*100), log10(Fig2a_data$Other_exotic_re_abun*100+1)) 
# R = -0.24, p < 0.001
## relationship between overall richness of alien species (including A. philoxeroides) and the native plant richness
cor.test(Fig2a_data$All_exotic_rich, Fig2a_data$Native_rich) 
# R = 0.30, p < 0.001
## relationship between richness of other alien species (excluding A. philoxeroides) and the native plant richness
cor.test(Fig2a_data$Other_exotic_rich, Fig2a_data$Native_rich) 
# R = 0.30, p < 0.001
## relationship between richness of other alien species (excluding A. philoxeroides) and the relative abundance of A. philoxeroides 
cor.test(Fig2a_data$Other_exotic_rich, log10(Fig2a_data$AP_re_abun*100)) 
# R = -0.23, p < 0.001

##
Fig2a_data <- subset(Fig2a_data, Herb_rich != "NA" & Path_re_abun != "NA")
dim(Fig2a_data)
Fig2a_data$Year <- as.factor(Fig2a_data$Year)

# more results about the linear model please see Table S4 in supporting information
## Relationship between the relative abundance of A. philoxeroides with native plant richness,
## insect herbivore richness, and annual mean temperature, respectively.
cor.test(Fig2a_data$Native_rich, log10(Fig2a_data$AP_re_abun*100)) 
# R = -0.25, p < 0.001
cor.test(Fig2a_data$Herb_rich, log10(Fig2a_data$AP_re_abun*100)) 
# R = -0.41, p < 0.001
cor.test(Fig2a_data$Ann_M_Tem, log10(Fig2a_data$AP_re_abun*100)) 
# R = -0.24, p < 0.001

##
cor.test(Fig2a_data$Native_rich, log10(Fig2a_data$AP_re_abun*100))
# Total database: R = -0.25, p < 0.001
cor.test(subset(Fig2a_data, Year == "2017")$Native_rich, log10(subset(Fig2a_data, Year == "2017")$AP_re_abun*100))
# Year--2017: R = -0.21, p = 0.157
cor.test(subset(Fig2a_data, Year == "2019")$Native_rich, log10(subset(Fig2a_data, Year == "2019")$AP_re_abun*100)) 
# Year--2019: R = -0.64, p < 0.001
cor.test(subset(Fig2a_data, Year == "2020")$Native_rich, log10(subset(Fig2a_data, Year == "2020")$AP_re_abun*100)) 
# Year--2020: R = 0.08, p = 0.605
cor.test(subset(Fig2a_data, Year == "2021")$Native_rich, log10(subset(Fig2a_data, Year == "2021")$AP_re_abun*100)) 
# Year--2021: R = -0.16, p = 0.144

ggplot(Fig2a_data, aes(x = Native_rich, y = log10(AP_re_abun*100))) +
  geom_point(Fig2a_data, mapping = aes(x = Native_rich, y = log10(AP_re_abun*100), fill = Year),
             size = 2, alpha = 1,shape = 21,position = position_dodge(width = 0)) +
  geom_smooth(Fig2a_data, mapping = aes(x = Native_rich, y = log10(AP_re_abun*100)),
              method = "lm", formula = y ~ x, se = F, size = 1, linetype = 1, alpha = 1, color = "black") + 
  geom_smooth(subset(Fig2a_data, Year == "2017"), 
              mapping = aes(x = Native_rich, y = log10(AP_re_abun*100)), method = "lm",
              formula = y ~ x, linetype = 2, se = FALSE, size = 0.8, colour = "#26B8E9")+
  geom_smooth(subset(Fig2a_data, Year == "2019"), 
              mapping = aes(x = Native_rich, y = log10(AP_re_abun*100)), method = "lm",
              formula = y ~ x, linetype = 1, se = FALSE, size = 0.8, colour = "#35B676")+
  geom_smooth(subset(Fig2a_data, Year == "2020"), 
              mapping = aes(x = Native_rich, y = log10(AP_re_abun*100)), method = "lm",
              formula = y ~ x, linetype = 2, se = FALSE, size = 0.8, colour = "#FFB636")+
  geom_smooth(subset(Fig2a_data, Year == "2021"), 
              mapping = aes(x = Native_rich, y = log10(AP_re_abun*100)), method = "lm",
              formula = y ~ x, linetype = 2, se = FALSE, size = 0.8, colour = "#5C66A9")+
  scale_fill_manual(values = Fig2_color)+
  mytheme + theme(legend.position = c(0.15,0.2), legend.title = element_blank()) + 
  scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, 5)) +
  labs(x = NULL, 
       y = expression(atop(italic("A. philoxeroides"), 
                           paste("relative abundance (%,  ", log[scriptstyle(10)], ")"))),
       tag = "(a)") -> Fig_2a; Fig_2a

################################## Fig 2b ######################################
## Loading data
Fig2a_data <- read.xlsx("Figure_2&Table_S4&Table_S5&Table_AP_def&Her_rich&path.xlsx", sheet = "Field_survey", colNames = TRUE, rowNames = FALSE)
Fig2a_data <- subset(Fig2a_data, Herb_rich != "NA" & Path_re_abun != "NA")
Fig2a_data$Year <- as.factor(Fig2a_data$Year)

## all predictors were Z-transformed before this analyse
attributes_variable <- attributes(scale(Fig2a_data[c("Native_rich", "Ann_M_Tem")]))

Fig2a_data[c( "Invaded_time" ,"Native_rich", "Ann_M_Tem", "Path_re_abun", "Herb_rich")] <- 
  scale(Fig2a_data[c("Invaded_time" ,"Native_rich", "Ann_M_Tem", "Path_re_abun", "Herb_rich")])

## based on best model(lowest AIC, more details please see the Table S4 in supporting information)
Bestmodel <- lm(log10(AP_re_abun*100) ~ Ann_M_Tem + Herb_rich + Invaded_time + 
                  Native_rich + Year + Ann_M_Tem:Native_rich + 
                  Herb_rich:Native_rich + Native_rich:Year, data = Fig2a_data)
summary(Bestmodel)
anova_results <- as.data.frame(anova(Bestmodel))
anova_results$p_adj <- round(p.adjust(anova_results$`Pr(>F)`, method = "BH"), 5) 
anova_results[, 2:4] <- round(anova_results[, 2:4], digits = 2)
anova_results$`Pr(>F)` <- round(anova_results$`Pr(>F)`, 5)
print(anova_results) 

## visual interaction (annual mean temperature × native plant richness)
pred_mode <- ggeffect(Bestmodel, terms = c("Native_rich","Ann_M_Tem"))
eff_mod_data <- data.frame(pred_mode)
colnames(eff_mod_data)[1] = "Native_rich"
colnames(eff_mod_data)[2] = "AP_re_abun"
colnames(eff_mod_data)[6] = "Ann_M_Tem"

# back transform attributes_variable
eff_mod_data["Native_rich"] <- attributes_variable$`scaled:center`["Native_rich"] + 
  attributes_variable$`scaled:scale`["Native_rich"]*eff_mod_data["Native_rich"]
eff_mod_data$Ann_M_Tem <- ifelse(eff_mod_data$Ann_M_Tem == "-1", "- 1 SD", 
                                 ifelse(eff_mod_data$Ann_M_Tem == "0", "Mean", "+ 1 SD"))
eff_mod_data$Ann_M_Tem <- factor(eff_mod_data$Ann_M_Tem, levels = c("- 1 SD", "Mean", "+ 1 SD"))

ggplot()+
  geom_line(data = eff_mod_data, mapping = aes(Native_rich,AP_re_abun,color=factor(Ann_M_Tem)), size=1)+
  #geom_ribbon(data = eff_mod_data, mapping = aes(x = Native_rich, ymin=conf.low,ymax=conf.high,fill=factor(Ann_M_Tem)),alpha=0.3, colour= NA)+
  scale_color_manual(values=c("#1430B1", "#52847E", "#605D03"), name = "Annual mean temperature (°C)")+
  guides(color = guide_legend(override.aes = list(fill = NA)))+
  theme_bw() + mytheme +  theme(legend.position = c(0.35,0.21)) + 
  scale_y_continuous(labels = scales::label_comma(accuracy = 0.01)) + 
  labs(x = NULL,
       y = expression(atop(italic("A. philoxeroides"), 
                           paste("relative abundance (%,  ", log[scriptstyle(10)], ")"))),
       tag = "(b)") -> Fig_2b; Fig_2b

################################## Fig 2c ######################################
## Loading data
Fig2_data <- read.xlsx("Figure_2&Table_S4&Table_S5&Table_AP_def&Her_rich&path.xlsx", sheet = "Field_survey", colNames = TRUE, rowNames = FALSE)
Fig2_data <- subset(Fig2_data, Herb_rich != "NA" & Path_re_abun != "NA")
Fig2_data$Year <- as.factor(Fig2_data$Year)

# more results about the linear model please see Table S5 in supporting information
cor.test(Fig2_data$Native_rich, log10(Fig2_data$Path_re_abun*100)) 
# R = 0.02, p = 0.725

Fig_2c <- ggplot(Fig2_data, aes(x = Native_rich, y = log10(Path_re_abun*100))) + 
  geom_point(mapping = aes(x = Native_rich, y = log10(Path_re_abun*100), fill = Year), shape = 21, size = 2)+
  geom_smooth(method = "lm", formula = y ~ x, se = F, size = 1, linetype = 2, alpha = 1, color = "black") + 
  scale_fill_manual(values = Fig2_color)+
  mytheme + 
  scale_x_continuous(limits = c(0, 19), breaks = seq(0, 19, 5)) +
  scale_y_continuous(labels = scales::label_comma(accuracy = 0.01)) + 
  labs(x = "Native plant richness (# species)",
       y = expression(atop("Soil fungal pathogens", 
                           paste("relative abundance (%,  ", log[scriptstyle(10)], ")"))),
       tag = "(c)") -> Fig_2c; Fig_2c

################################## Fig 2d ######################################
## Loading data
Fig2_data <- read.xlsx("Figure_2&Table_S4&Table_S5&Table_AP_def&Her_rich&path.xlsx", sheet = "Field_survey", colNames = TRUE, rowNames = FALSE)
Fig2_data <- subset(Fig2_data, Herb_rich != "NA" & Path_re_abun != "NA" & Defoliation != "NA")
Fig2_data$Year <- as.factor(Fig2_data$Year)

# more results about the linear model please see Table S5 in supporting information

ggplot(Fig2_data, aes(x = Native_rich, y = log(Defoliation*100+1), fill = Year)) +
  geom_point(shape = 21, size = 2)+
  geom_smooth(method = "lm", formula = y ~ x, se = F, size = 1, linetype = 2, alpha = 1, color = "black") + 
  scale_x_continuous(limits = c(0, 17), breaks = seq(0, 17, 5)) +
  scale_fill_manual(values = Fig2_color)+
  mytheme + 
  scale_y_continuous(labels = scales::label_comma(accuracy = 0.01)) + 
  
  labs(x = "Native plant richness (# species)", 
       y = expression(atop(
         paste("Defoliation on ", italic("A. philoxeroides")),
         paste("(% leaf area removed, ",log[10](x+1), ")"))),
       tag = "(d)") -> Fig_2d; Fig_2d

## Combined
(Fig_2a|Fig_2b)/(Fig_2c|Fig_2d) -> Fig_2; Fig_2

### Notice that,
### For more picture details, we have further adjusted it in Adobe illustrator.

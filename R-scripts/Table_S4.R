################################################################################
################################## Table.S4 ####################################
################################################################################
library(openxlsx) # version 4.2.5.2
library(MuMIn) # version 1.46.0

## loading data
Table_S4 <- read.xlsx("Figure_2&Table_S4&Table_S5&Table_AP_def&Her_rich&path.xlsx", sheet = "Field_survey", rowNames = T, colNames = T)
Table_S4 <- subset(Table_S4, Herb_rich != "NA" & Path_re_abun != "NA")

## data transformation
Table_S4$Path_re_abun <- log10(Table_S4$Path_re_abun*100) 
Table_S4$AP_re_abun <- log10(Table_S4$AP_re_abun*100)
Table_S4$Other_exotic_re_abun <- log10(Table_S4$Other_exotic_re_abun*100+1)
Table_S4$Year <- as.factor(Table_S4$Year)

## all predictors were Z-transformed before this analyse
Table_S4[c("Invaded_time" ,"Native_rich", "Ann_M_Tem", "Path_re_abun","Herb_rich")] <- 
  scale(Table_S4[c("Invaded_time" ,"Native_rich", "Ann_M_Tem", "Path_re_abun","Herb_rich")])

################ Relative abundance of Alternanthera philoxeroides #############
## full model
options(na.action ="na.fail")
mod_full <-lm(AP_re_abun ~ Invaded_time + Native_rich + 
                Herb_rich + Native_rich:Herb_rich + 
                Path_re_abun + Native_rich:Path_re_abun  + 
                Ann_M_Tem + Native_rich:Ann_M_Tem + 
                Year + Native_rich:Year + 
                Type + Native_rich:Type, data = Table_S4)

hist(residuals(mod_full))
car::vif(mod_full)
summary(mod_full)

## multimodel inference
dd12 <- dredge(mod_full, subset = ~ Invaded_time &
                 dc(Native_rich, Year, Native_rich:Year) & 
                 dc(Native_rich, Type, Native_rich:Type) & 
                 dc(Native_rich, Herb_rich, Native_rich:Herb_rich) & 
                 dc(Native_rich, Path_re_abun, Native_rich:Path_re_abun) & 
                 dc(Native_rich, Ann_M_Tem, Native_rich:Ann_M_Tem))
mod_list1 = subset(dd12, subset = delta < 2)
Final_model = get.models(dd12,1)[[1]] 

## part of Table S4
anova_results <- as.data.frame(anova(Final_model))
anova_results$p_adj <- round(p.adjust(anova_results$`Pr(>F)`, method = "BH"), 5) 
anova_results[, 2:4] <- round(anova_results[, 2:4], digits = 2)
anova_results$`Pr(>F)` <- round(anova_results$`Pr(>F)`, 5)
anova_results$`Pr(>F)` <- ifelse(anova_results$`Pr(>F)` < 0.001, "<0.001", anova_results$`Pr(>F)`)
anova_results$p_adj <- ifelse(anova_results$p_adj < 0.001, "<0.001", anova_results$p_adj)
print(anova_results[,-c(2,3)]) 

## R² & AIC
print(paste0("R²: ", round(summary(Final_model)$r.squared, 3)))
print(paste0("AIC: ", round(AIC(Final_model), 3)))

################# Relative abundance of Other alien species ####################
## full model
options(na.action ="na.fail")
summary(Table_S4$Other_exotic_re_abun)
mod_full <- lm(Other_exotic_re_abun ~ Invaded_time + Native_rich + 
                Herb_rich + Native_rich:Herb_rich + 
                Path_re_abun + Native_rich:Path_re_abun  + 
                Ann_M_Tem + Native_rich:Ann_M_Tem + 
                Year + Native_rich:Year + 
                Type + Native_rich:Type, data = Table_S4)
hist(residuals(mod_full))
car::vif(mod_full)

## multimodel inference
dd12 <- dredge(mod_full, subset = ~ 
                 dc(Native_rich, Year, Native_rich:Year) & 
                 dc(Native_rich, Type, Native_rich:Type) & 
                 dc(Native_rich, Herb_rich, Native_rich:Herb_rich) & 
                 dc(Native_rich, Path_re_abun, Native_rich:Path_re_abun) & 
                 dc(Native_rich, Ann_M_Tem, Native_rich:Ann_M_Tem))
mod_list1 = subset(dd12, subset = delta < 2)
Final_model = get.models(dd12,1)[[1]] 
summary(Final_model)
## part of Table S4
anova_results <- as.data.frame(anova(Final_model))
anova_results$p_adj <- round(p.adjust(anova_results$`Pr(>F)`, method = "BH"), 5) 
anova_results[, 2:4] <- round(anova_results[, 2:4], digits = 2)
anova_results$`Pr(>F)` <- round(anova_results$`Pr(>F)`, 5)
print(anova_results) 

## R² & AIC
print(paste0("R²: ", round(summary(Final_model)$r.squared, 3)))
print(paste0("AIC: ", round(AIC(Final_model), 3)))


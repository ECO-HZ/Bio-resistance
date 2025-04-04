################################################################################
################################## Table.S5 ####################################
################################################################################
library(openxlsx) # version 4.2.5.2
library(MuMIn) # version 1.46.0

## loading data
Table_S5 <- read.xlsx("Figure_2&Table_S4&Table_S5&Table_AP_def&Her_rich&path.xlsx", sheet = "Field_survey", rowNames = TRUE, colNames = TRUE)

## data transformation
Table_S5$Year <- as.factor(Table_S5$Year)
Table_S5$Type <- as.factor(Table_S5$Type)

################################# Defoliation ##################################
Defoliation_data <- subset(Table_S5, Defoliation!= "NA")
Defoliation_mod = lm(log10(Defoliation*100+1) ~ Native_rich, data = Defoliation_data)

## part of Table S5
anova_Defoliation <- as.data.frame(anova(Defoliation_mod))
anova_Defoliation$p_adj <- round(p.adjust(anova_Defoliation$`Pr(>F)`, method = "BH"), 5) 
anova_Defoliation[, 2:4] <- round(anova_Defoliation[, 2:4], digits = 2)
anova_Defoliation$`Pr(>F)` <- round(anova_Defoliation$`Pr(>F)`, 5)
print(anova_Defoliation[,-c(2,3)]) 


######################### Insect herbivore richness ############################
Herbivore_data = subset(Table_S5, Herb_rich != "NA" & Path_re_abun != "NA")
Herbivore_data$Path_re_abun = log10(Herbivore_data$Path_re_abun*100) 

## all predictors were Z-transformed before this analyse
Herbivore_data[c("Invaded_time", "Native_rich", "Ann_M_Tem", "Path_re_abun", "Herb_rich")] = 
  scale(Herbivore_data[c("Invaded_time", "Native_rich", "Ann_M_Tem", "Path_re_abun", "Herb_rich")])

## multimodel inference
options(na.action ="na.fail")
mod_full <- lm(Herb_rich ~ Invaded_time + Native_rich +
                 Year +  Native_rich:Year + 
                 Type + Native_rich:Type + 
                 Ann_M_Tem + Native_rich:Ann_M_Tem, data = Herbivore_data)

hist(residuals(mod_full))

dd12 <- dredge(mod_full, subset = ~ Invaded_time &
                 dc(Native_rich, Year, Native_rich:Year) & 
                 dc(Native_rich, Type, Native_rich:Type) & 
                 dc(Native_rich, Ann_M_Tem, Native_rich:Ann_M_Tem))

mod_list1 = subset(dd12, subset = delta < 2)
Herbivore_mod = get.models(dd12,1)[[1]] 

## part of Table S5
anova_Herbivore <- as.data.frame(anova(Herbivore_mod))
anova_Herbivore$p_adj <- round(p.adjust(anova_Herbivore$`Pr(>F)`, method = "BH"), 4) 
anova_Herbivore$p_adj <- ifelse(anova_Herbivore$p_adj < 0.001, "<0.001", anova_Herbivore$p_adj)
anova_Herbivore[, 2:4] <- round(anova_Herbivore[, 2:4], digits = 2)
anova_Herbivore$`Pr(>F)` <- round(anova_Herbivore$`Pr(>F)`, 4)
anova_Herbivore <- anova_Herbivore[c(3,1,2,4,5), ]
print(anova_Herbivore[,-c(2,3)]) 

## R² & AIC
print(paste0("R²: ", round(summary(Herbivore_mod)$r.squared, 3)))
print(paste0("AIC: ", round(AIC(Herbivore_mod), 3)))


################### Relative abundance of soil pathogens #######################
Pathogens_data = subset(Table_S5, Herb_rich != "NA" & Path_re_abun != "NA")
Pathogens_data$Path_re_abun = log10(Pathogens_data$Path_re_abun*100) 

## all predictors were Z-transformed before this analyse
Pathogens_data[c("Invaded_time", "Native_rich", "Ann_M_Tem", "Path_re_abun", "Herb_rich")] = 
  scale(Pathogens_data[c("Invaded_time", "Native_rich", "Ann_M_Tem", "Path_re_abun", "Herb_rich")])

## multimodel inference
options(na.action ="na.fail")
mod_full <- lm(Path_re_abun ~ Invaded_time + Native_rich +
                 Year +  Native_rich:Year + 
                 Type + Native_rich:Type + 
                 Ann_M_Tem + Native_rich:Ann_M_Tem, data = Pathogens_data)

hist(residuals(mod_full))

dd12 <- dredge(mod_full, subset = ~ Invaded_time &
                 dc(Native_rich, Year, Native_rich:Year) & 
                 dc(Native_rich, Type, Native_rich:Type) & 
                 dc(Native_rich, Ann_M_Tem, Native_rich:Ann_M_Tem))

mod_list1 = subset(dd12, subset = delta < 2)
Pathogens_mod = get.models(dd12,1)[[1]] 

## part of Table S5
anova_Pathogens <- as.data.frame(anova(Pathogens_mod))
anova_Pathogens$p_adj <- round(p.adjust(anova_Pathogens$`Pr(>F)`, method = "BH"), 5) 
anova_Pathogens$p_adj <- ifelse(anova_Pathogens$p_adj < 0.001, "<0.001", anova_Pathogens$p_adj)
anova_Pathogens[, 2:4] <- round(anova_Pathogens[, 2:4], digits = 2)
anova_Pathogens$`Pr(>F)` <- round(anova_Pathogens$`Pr(>F)`, 5)
anova_Pathogens <- anova_Pathogens[c(3,1,2,4,5,6,7), ]
print(anova_Pathogens[,-c(2,3)]) 

## R² & AIC
print(paste0("R²: ", round(summary(Pathogens_mod)$r.squared, 3)))
print(paste0("AIC: ", round(AIC(Pathogens_mod), 3)))

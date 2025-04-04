################################################################################
################################## Table.S6 ####################################
################################################################################
library(openxlsx) # version 4.2.5.2
library(lme4) # version 1.1-34
library(lmerTest) # version 3.1-3

############################## Field experiment ################################
#################  Relative abundance of A. philoxeroides ######################
Abun_AP_field <- read.xlsx("Table_S6.xlsx", sheet = "Field_AP_re_abun", rowNames = FALSE, colNames = TRUE)
Abun_AP_field$AP_re_abun <- log10(Abun_AP_field$AP_re_abun*100)

Abun_AP_mod_field <-lmer(AP_re_abun ~ Richness*Time + (1|plot), Abun_AP_field)

## part of Table S6
anova_Abun_AP_field <- as.data.frame(anova(Abun_AP_mod_field))
anova_Abun_AP_field$p_adj <- round(p.adjust(anova_Abun_AP_field$`Pr(>F)`, method = "BH"), 5) 
anova_Abun_AP_field$p_adj <- ifelse(anova_Abun_AP_field$p_adj < 0.001, "<0.001", anova_Abun_AP_field$p_adj)
anova_Abun_AP_field[, c(1,2,5)] <- round(anova_Abun_AP_field[, c(1,2,5)], digits = 2)
anova_Abun_AP_field$`Pr(>F)` <- round(anova_Abun_AP_field$`Pr(>F)`, 5)
anova_Abun_AP_field$`Pr(>F)` <- ifelse(anova_Abun_AP_field$`Pr(>F)` < 0.001, "<0.001", anova_Abun_AP_field$`Pr(>F)`)

print(anova_Abun_AP_field[,-c(1,2)]) 

## SD & LRT for random factor
paste0("SD = ",round(data.frame(VarCorr(Abun_AP_mod_field))[1,5], 3))
paste0("LRT = ",round(ranova(Abun_AP_mod_field)[2,4], 2), ", p = ", round(ranova(Abun_AP_mod_field)[2,6], 3))

############################## Field experiment ################################
#################  Aboveground biomass of A. philoxeroides #####################
Bio_AP_field <- read.xlsx("Table_S6.xlsx", sheet = "Field_AP_Native_biomass", rowNames = FALSE, colNames = TRUE)
Bio_AP_field$AP_biomass <- log10(Bio_AP_field$AP_biomass)

Bio_AP_mod_field <-lmer(AP_biomass ~ Richness*Time + (1|plot), Bio_AP_field)

## part of Table S5
anova_Bio_AP_field <- as.data.frame(anova(Bio_AP_mod_field))
anova_Bio_AP_field$p_adj <- round(p.adjust(anova_Bio_AP_field$`Pr(>F)`, method = "BH"), 5) 
anova_Bio_AP_field$p_adj <- ifelse(anova_Bio_AP_field$p_adj < 0.001, "<0.001", anova_Bio_AP_field$p_adj)
anova_Bio_AP_field[, c(1,2,5)] <- round(anova_Bio_AP_field[, c(1,2,5)], digits = 2)
anova_Bio_AP_field$`Pr(>F)` <- round(anova_Bio_AP_field$`Pr(>F)`, 5)
anova_Bio_AP_field$`Pr(>F)` <- ifelse(anova_Bio_AP_field$`Pr(>F)` < 0.001, "<0.001", anova_Bio_AP_field$`Pr(>F)`)
print(anova_Bio_AP_field[,-c(1,2)]) 

## SD & LRT for random factor
paste0("SD = ",round(data.frame(VarCorr(Bio_AP_mod_field))[1,5], 3))
paste0("LRT = ",round(ranova(Bio_AP_mod_field)[2,4], 2), ", p = ", round(ranova(Bio_AP_mod_field)[2,6], 3))

############################## Field experiment ################################
####################  Aboveground biomass of native species ####################
Bio_Native_field <- read.xlsx("Table_S6.xlsx", sheet = "Field_AP_Native_biomass", rowNames = FALSE, colNames = TRUE)
Bio_Native_field$Native_biomass <- log10(Bio_Native_field$Native_biomass)

Bio_Native_mod_field <-lmer(Native_biomass ~ Richness*Time + (1|plot), Bio_Native_field)

## part of Table S5
anova_Bio_Nat_field <- as.data.frame(anova(Bio_Native_mod_field))
anova_Bio_Nat_field$p_adj <- round(p.adjust(anova_Bio_Nat_field$`Pr(>F)`, method = "BH"), 5) 
anova_Bio_Nat_field$p_adj <- ifelse(anova_Bio_Nat_field$p_adj < 0.001, "<0.001", anova_Bio_Nat_field$p_adj)
anova_Bio_Nat_field[, c(1,2,5)] <- round(anova_Bio_Nat_field[, c(1,2,5)], digits = 2)
anova_Bio_Nat_field$`Pr(>F)` <- round(anova_Bio_Nat_field$`Pr(>F)`, 5)
anova_Bio_Nat_field$`Pr(>F)` <- ifelse(anova_Bio_Nat_field$`Pr(>F)` < 0.001, "<0.001", anova_Bio_Nat_field$`Pr(>F)`)
print(anova_Bio_Nat_field[,-c(1,2)]) 

## SD & LRT for random factor
paste0("SD = ",round(data.frame(VarCorr(Bio_Native_mod_field))[1,5], 3))
paste0("LRT = ",round(ranova(Bio_Native_mod_field)[2,4], 2), ", p = ", round(ranova(Bio_Native_mod_field)[2,6], 3))

############################# Mesocosm experiment ##############################
#################  Relative abundance of A. philoxeroides ######################
Abun_AP_meso <- read.xlsx("Table_S6.xlsx", sheet = "Mesocosm_AP_re_abun", rowNames = FALSE, colNames = TRUE)
Abun_AP_meso$AP_re_abun <- log10(Abun_AP_meso$AP_re_abun*100)

Abun_AP_mod_meso <-lmer(AP_re_abun ~ Richness*Time + (1|plot), Abun_AP_meso)

## part of Table S6
anova_Abun_AP_meso <- as.data.frame(anova(Abun_AP_mod_meso))
anova_Abun_AP_meso$p_adj <- round(p.adjust(anova_Abun_AP_meso$`Pr(>F)`, method = "BH"), 5) 
anova_Abun_AP_meso$p_adj <- ifelse(anova_Abun_AP_meso$p_adj < 0.001, "<0.001", anova_Abun_AP_meso$p_adj)
anova_Abun_AP_meso[, c(1,2,5)] <- round(anova_Abun_AP_meso[, c(1,2,5)], digits = 2)
anova_Abun_AP_meso$`Pr(>F)` <- round(anova_Abun_AP_meso$`Pr(>F)`, 5)
anova_Abun_AP_meso$`Pr(>F)` <- ifelse(anova_Abun_AP_meso$`Pr(>F)` < 0.001, "<0.001", anova_Abun_AP_meso$`Pr(>F)`)
print(anova_Abun_AP_meso[,-c(1,2)]) 

## SD & LRT for random factor
paste0("SD = ",round(data.frame(VarCorr(Abun_AP_mod_meso))[1,5], 3))
paste0("LRT = ",round(ranova(Abun_AP_mod_meso)[2,4], 2), ", p = ", round(ranova(Abun_AP_mod_meso)[2,6], 3))

############################# Mesocosm experiment ##############################
#################  Aboveground biomass of A. philoxeroides #####################
Bio_AP_meso <- read.xlsx("Table_S6.xlsx", sheet = "Mesocosm_AP_biomass", rowNames = FALSE, colNames = TRUE)
Bio_AP_meso$AP_biomass <- log10(Bio_AP_meso$AP_biomass)

Bio_AP_mod_meso <-lmer(AP_biomass ~ Richness*Time + (1|plot), Bio_AP_meso)

## part of Table S5
anova_Bio_AP_meso <- as.data.frame(anova(Bio_AP_mod_meso))
anova_Bio_AP_meso$p_adj <- round(p.adjust(anova_Bio_AP_meso$`Pr(>F)`, method = "BH"), 5) 
anova_Bio_AP_meso$p_adj <- ifelse(anova_Bio_AP_meso$p_adj < 0.001, "<0.001", anova_Bio_AP_meso$p_adj)
anova_Bio_AP_meso[, c(1,2,5)] <- round(anova_Bio_AP_meso[, c(1,2,5)], digits = 2)
anova_Bio_AP_meso$`Pr(>F)` <- round(anova_Bio_AP_meso$`Pr(>F)`, 5)
anova_Bio_AP_meso$`Pr(>F)` <- ifelse(anova_Bio_AP_meso$`Pr(>F)` < 0.001, "<0.001", anova_Bio_AP_meso$`Pr(>F)`)
print(anova_Bio_AP_meso[,-c(1,2)]) 

## SD & LRT for random factor
paste0("SD = ",round(data.frame(VarCorr(Bio_AP_mod_meso))[1,5], 3))
paste0("LRT = ",round(ranova(Bio_AP_mod_meso)[2,4], 2), ", p = ", round(ranova(Bio_AP_mod_meso)[2,6], 3))

############################# Mesocosm experiment ##############################
####################  Aboveground biomass of native species ####################
Bio_Native_meso <- read.xlsx("Table_S6.xlsx", sheet = "Mesocosm_native_biomass", rowNames = FALSE, colNames = TRUE)
Bio_Native_meso$Native_biomass <- log10(Bio_Native_meso$Native_biomass)

Bio_Native_mod_meso <-lmer(Native_biomass ~ Richness*Time + (1|plot), Bio_Native_meso)

## part of Table S5
anova_Bio_Nat_meso <- as.data.frame(anova(Bio_Native_mod_meso))
anova_Bio_Nat_meso$p_adj <- round(p.adjust(anova_Bio_Nat_meso$`Pr(>F)`, method = "BH"), 5) 
anova_Bio_Nat_meso$p_adj <- ifelse(anova_Bio_Nat_meso$p_adj < 0.001, "<0.001", anova_Bio_Nat_meso$p_adj)
anova_Bio_Nat_meso[, c(1,2,5)] <- round(anova_Bio_Nat_meso[, c(1,2,5)], digits = 2)
anova_Bio_Nat_meso$`Pr(>F)` <- round(anova_Bio_Nat_meso$`Pr(>F)`, 5)
anova_Bio_Nat_meso$`Pr(>F)` <- ifelse(anova_Bio_Nat_meso$`Pr(>F)` < 0.001, "<0.001", anova_Bio_Nat_meso$`Pr(>F)`)
print(anova_Bio_Nat_meso[,-c(1,2)]) 

## SD & LRT for random factor
paste0("SD = ",round(data.frame(VarCorr(Bio_Native_mod_meso))[1,5], 3))
paste0("LRT = ",round(ranova(Bio_Native_mod_meso)[2,4], 2), ", p = ", round(ranova(Bio_Native_mod_meso)[2,6], 3))

################################################################################
################################## Table.S7 ####################################
################################################################################
library(openxlsx) # version 4.2.5.2
library(lme4) # version 1.1-34
library(lmerTest) # version 3.1-3

############################## Field experiment ################################
################################# Defoliation ##################################
## loading data
Defoliation_data <- read.xlsx("Figure_S4.xlsx", sheet = "Field_Defoliation", rowNames = F, colNames = T)
Defoliation_data$Defoliation = log10(Defoliation_data$Defoliation*100+1)
Defoliation_mod <- lmer(Defoliation ~ Richness*Time*Origin + (1|plot), Defoliation_data)

## part of Table S7
anova_Defoliation_mod <- as.data.frame(anova(Defoliation_mod))
anova_Defoliation_mod$p_adj <- round(p.adjust(anova_Defoliation_mod$`Pr(>F)`, method = "BH"), 5) 
anova_Defoliation_mod$p_adj <- ifelse(anova_Defoliation_mod$p_adj < 0.001, "<0.001", anova_Defoliation_mod$p_adj)
anova_Defoliation_mod[, c(1,2,5)] <- round(anova_Defoliation_mod[, c(1,2,5)], digits = 2)
anova_Defoliation_mod$`Pr(>F)` <- round(anova_Defoliation_mod$`Pr(>F)`, 5)
anova_Defoliation_mod$`Pr(>F)` <- ifelse(anova_Defoliation_mod$`Pr(>F)` < 0.001, "<0.001", anova_Defoliation_mod$`Pr(>F)`)
anova_Defoliation_mod <- anova_Defoliation_mod[c(1,2,4,3,5,6,7), ]
print(anova_Defoliation_mod[,-c(1,2)]) 

## SD & LRT for random factor
paste0("SD = ",round(data.frame(VarCorr(Defoliation_mod))[1,5], 3))
paste0("LRT = ",round(ranova(Defoliation_mod)[2,4], 2), ", p = ", round(ranova(Defoliation_mod)[2,6], 3))

############################## Field experiment ################################
############################# Herbivore richness ###############################
## loading data
Herbivore_data <- read.xlsx("Figure_S4.xlsx", sheet = "Field_Herbivore", colNames = TRUE, rowNames = FALSE)
Herb_rich_mod <- lmer(Herbivore_richness ~ Richness*Time + (1|plot), Herbivore_data)

## part of Table S7
anova_Herb_rich_mod <- as.data.frame(anova(Herb_rich_mod))
anova_Herb_rich_mod$p_adj <- round(p.adjust(anova_Herb_rich_mod$`Pr(>F)`, method = "BH"), 5) 
anova_Herb_rich_mod$p_adj <- ifelse(anova_Herb_rich_mod$p_adj < 0.001, "<0.001", anova_Herb_rich_mod$p_adj)
anova_Herb_rich_mod[, c(1,2,5)] <- round(anova_Herb_rich_mod[, c(1,2,5)], digits = 2)
anova_Herb_rich_mod$`Pr(>F)` <- round(anova_Herb_rich_mod$`Pr(>F)`, 5)
anova_Herb_rich_mod$`Pr(>F)` <- ifelse(anova_Herb_rich_mod$`Pr(>F)` < 0.001, "<0.001", anova_Herb_rich_mod$`Pr(>F)`)
print(anova_Herb_rich_mod[,-c(1,2)]) 

## SD & LRT for random factor
paste0("SD = ",round(data.frame(VarCorr(Herb_rich_mod))[1,5], 3))
paste0("LRT = ",round(ranova(Herb_rich_mod)[2,4], 2), ", p = ", round(ranova(Herb_rich_mod)[2,6], 3))


############################## Field experiment ################################
############################ Herbivore abundance ###############################
Herb_abun_mod <- lmer(Herbivore_abundance ~ Richness*Time + (1|plot), Herbivore_data)

## part of Table S7
anova_Herb_abun_mod <- as.data.frame(anova(Herb_abun_mod))
anova_Herb_abun_mod$p_adj <- round(p.adjust(anova_Herb_abun_mod$`Pr(>F)`, method = "BH"), 5) 
anova_Herb_abun_mod$p_adj <- ifelse(anova_Herb_abun_mod$p_adj < 0.001, "<0.001", anova_Herb_abun_mod$p_adj)
anova_Herb_abun_mod[, c(1,2,5)] <- round(anova_Herb_abun_mod[, c(1,2,5)], digits = 2)
anova_Herb_abun_mod$`Pr(>F)` <- round(anova_Herb_abun_mod$`Pr(>F)`, 5)
anova_Herb_abun_mod$`Pr(>F)` <- ifelse(anova_Herb_abun_mod$`Pr(>F)` < 0.001, "<0.001", anova_Herb_abun_mod$`Pr(>F)`)
print(anova_Herb_abun_mod[,-c(1,2)]) 

## SD & LRT for random factor
paste0("SD = ",round(data.frame(VarCorr(Herb_abun_mod))[1,5], 3))
paste0("LRT = ",round(ranova(Herb_abun_mod)[2,4], 2), ", p = ", round(ranova(Herb_abun_mod)[2,6], 3))


################################################################################
################################## Fig 3 #######################################
################################################################################
library(openxlsx) # version 4.2.5.2
library(ggplot2) # version 3.5.1
library(ggtext) # version 0.1.2
library(ggpubr) # version 0.6.1
library(lme4) # version 1.1-34
library(lmerTest) # version 3.1-3
library(emmeans) # version 1.10.6
library(patchwork) # version 1.2.0
library(cowplot) # version 1.1.3

## Define visualization style
color_select = c("October 2021" = '#1CB3B0', 
                 "May 2022" = "#7db954", "October 2022" = '#C693BE',
                 "May 2023" = '#FF9300', "October 2023" = "#1072BD")

sample_order = c("October 2021", "May 2022", "October 2022", "May 2023", "October 2023")

mytheme <- theme_bw() + 
  theme(panel.grid = element_blank(),  
        axis.text = element_text(color = "black", size = 11),  
        axis.title = element_text(size = 13),  
        #legend.position = "none",
        legend.key = element_blank(),
        legend.title = element_blank(),
        legend.text= element_text(size=10),
        legend.background = element_blank(),
        plot.title = element_textbox(
          size = 14, color = "black", fill = "grey90",
          box.color = "grey50",padding = margin(5, 5, 5, 5), margin = margin(b = 0),       
          halign = 0.5, width = grid::unit(1, "npc")), #r = unit(3, "pt")     
        plot.tag = element_text(size = 14, face = "bold")) 

################################## Fig 3a ######################################
## Loading data
Fig3a_data <- read.xlsx("Figure_3.xlsx", sheet = "Figure_3a", colNames = TRUE, rowNames = FALSE)
Fig3a_data$Time <- gsub("_", " ", Fig3a_data$Time)
Fig3a_data$Time <- factor(Fig3a_data$Time, levels = sample_order)

## 
mod_AP_re_abun <- lmer(log10(AP_re_abun*100) ~ Richness*Time + (1|plot), data = Fig3a_data)
anova(mod_AP_re_abun)
emt_AP_re_abun <- emtrends(mod_AP_re_abun, pairwise ~ Time, var = "Richness")
test(emt_AP_re_abun, adjust = "BH")
slope_letter <- multcomp::cld(emt_AP_re_abun, alpha = 0.05, Letters = letters, adjust = "BH", decreasing = T)
print(slope_letter)

ggplot(Fig3a_data, aes(y = log10(AP_re_abun*100), x = Richness, fill=Time)) +
  geom_point(size=2,alpha=1,shape=21,position = position_dodge(width = 0)) +
  geom_smooth(aes(color=Time),method = "lm", formula = y ~ x, se = F, size = 0.8, linetype=1, alpha = 1) + 
  stat_cor(aes(label = paste(after_stat(r.label), sep = "~"), color = Time), 
           label.x = 9, label.y = rev(c(1.0, 1.2, 1.4, 1.6, 1.8)), size = 4) +
  scale_color_manual(values = color_select) +
  scale_fill_manual(values = color_select) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 12)) +
  mytheme +  
  labs(x = NULL, 
       y = expression(atop(italic("A. philoxeroides"), 
                           paste("relative abundance (%,  ", log[scriptstyle(10)], ")"))),
       tag = "(a)", title = c("Field experiment")) -> Fig_3a; Fig_3a


################################## Fig 3b ######################################
## Loading data
Fig3b_data <- read.xlsx("Figure_3.xlsx", sheet = "Figure_3b", colNames = TRUE, rowNames = FALSE)
Fig3b_data$Time <- gsub("_", " ", Fig3b_data$Time)
Fig3b_data$Time <- factor(Fig3b_data$Time, levels = sample_order)

mod_AP_bio <- lmer(log10(AP_biomass) ~ Richness*Time + (1|plot), data=Fig3b_data)
anova(mod_AP_bio)
emt_AP_bio <- emtrends(mod_AP_bio, pairwise ~ Time, var = "Richness")
test(emt_AP_bio, adjust = "BH")
slope_letter <- multcomp::cld(emt_AP_bio, alpha = 0.05, Letters = letters, adjust = "BH", decreasing = T)
print(slope_letter)

ggplot(Fig3b_data, aes(x = Richness, y = log10(AP_biomass), fill=Time)) +
  geom_point(size = 2, alpha = 1, shape = 21) +
  geom_smooth(aes(color = Time, linetype = Time), method = "lm", 
              formula = y ~x, se = F,size = 0.8,alpha = 1) + 
  stat_cor(aes(label = paste(after_stat(r.label)), color = Time), 
           label.x = 9, label.y = c(1.45, 1.20, 0.95), size = 4) +  
  scale_color_manual(values = color_select) +
  scale_fill_manual(values = color_select) +
  scale_linetype_manual(values = c("dashed", "solid", "solid")) + 
  scale_x_continuous(breaks = c(2, 4, 6, 8, 12)) +
  scale_y_continuous(labels = scales::label_comma(accuracy =0.1)) + 
  annotate("text", x = 10, y = -0.9, label = "a", size = 5, color = "black") +
  annotate("text", x = 10, y = -0.53, label = "b", size = 5, color = "black") +
  annotate("text", x = 10, y = 0.08, label = "b", size = 5, color = "black") +
  mytheme +  
  labs(x = "Native plant richness\n(Number of species added)",
       y = expression(atop(italic("A. philoxeroides"), 
                           paste("aboveground biomass (g,  ", log[scriptstyle(10)], ")"))),
       tag = "(b)") -> Fig_3b; Fig_3b


################################## Fig 3c ######################################
## Loading data
Fig3c_data <- read.xlsx("Figure_3.xlsx", sheet = "Figure_3c", colNames = TRUE, rowNames = FALSE)
Fig3c_data$Time <- gsub("_", " ", Fig3c_data$Time)
Fig3c_data$Time <- factor(Fig3c_data$Time, levels = sample_order)

mod_AP_re_abun <- lmer(log10(AP_re_abun*100)~ Richness*Time + (1|plot), data=Fig3c_data)
anova(mod_AP_re_abun)
emt_AP_re_abun <- emtrends(mod_AP_re_abun, pairwise ~ Time, var = "Richness")
test(emt_AP_re_abun, adjust = "BH")
slope_letter <- multcomp::cld(emt_AP_re_abun, alpha = 0.05, Letters = letters, adjust = "BH", decreasing = T)
print(slope_letter)

library(ggtext)
ggplot(Fig3c_data, aes(y = log10(AP_re_abun*100), x = Richness, fill=Time)) +
  geom_point(size = 2,alpha = 1,shape=21,position = position_dodge(width = 0)) +
  geom_smooth(aes(color = Time),method = "lm", formula = y ~ x, se = F, size = 0.8, linetype = 1, alpha = 1) + 
  stat_cor(aes(label = paste(after_stat(r.label)), color = Time), 
           label.x = 2.1, label.y = c(1.5, 1.45, 1.4, 1.35), size = 4) +  
  scale_color_manual(values = color_select) +
  scale_fill_manual(values = color_select) +
  scale_x_continuous(breaks = c(2, 4, 6, 8, 12))+
  mytheme + 
  annotate("text", x = 10, y = 1.55, label = "b", size = 5, color = "black") +
  annotate("text", x = 10, y = 1.63, label = "b", size = 5, color = "black") +
  annotate("text", x = 10, y = 1.82, label = "a", size = 5, color = "black") +
  annotate("text", x = 10, y = 1.87, label = "a", size = 5, color = "black") +
  labs(x = NULL, y = NULL, tag = "(c)",
       title = "Mesocosm experiment") -> Fig_3c; Fig_3c


################################## Fig 3d ######################################
## Loading data
Fig3d_data <- read.xlsx("Figure_3.xlsx", sheet = "Figure_3d", colNames = TRUE, rowNames = FALSE)
Fig3d_data$Time <- gsub("_", " ", Fig3d_data$Time)
Fig3d_data$Time <- factor(Fig3d_data$Time, levels = sample_order)

mod_AP_bio <- lmer(log10(AP_biomass) ~ Richness * Time + (1|plot), data = Fig3d_data)
anova(mod_AP_bio)
emt_AP_bio <- emtrends(mod_AP_bio, pairwise ~ Time, var = "Richness")
test(emt_AP_bio, adjust = "BH")
slope_letter <- multcomp::cld(emt_AP_bio, alpha = 0.05, Letters = letters, adjust = "BH", decreasing = T)
print(slope_letter)

ggplot(Fig3d_data, aes(x = Richness, y = log10(AP_biomass), fill=Time)) +
  geom_point(size = 2,alpha = 1,shape = 21,position = position_dodge(width = 0)) +
  geom_smooth(aes(color=Time),method = "lm", formula = y ~ x, se = F,size = 0.8,linetype = 1,alpha = 1) + 
  stat_cor(aes(label = paste(after_stat(r.label)), color = Time), 
           label.x = 9, label.y = c(2.85, 2.72), size = 4) +  
  scale_color_manual(values = color_select) +
  scale_fill_manual(values = color_select) +
  scale_x_continuous(breaks = c(0, 2, 4, 6, 8, 12)) +
  mytheme +
  annotate("text", x = 10, y = 2.51, label = "b", size = 5, color = "black") +
  annotate("text", x = 10, y = 2.19, label = "a", size = 5, color = "black") +
  labs(x = "Native plant richness\n(Number of species added)",
       y = NULL, tag = "(d)") -> Fig_3d; Fig_3d



## Combined
shared_legend <- get_legend(Fig_3a + theme(legend.position = "right",
                                           legend.key = element_blank(),
                                           legend.title = element_blank(),
                                           legend.background = element_blank(),
                                           legend.text = element_text(size = 8)))

Fig_3 <- (Fig_3a + theme(legend.position = "none") | 
          Fig_3c + theme(legend.position = "none")) /
         (Fig_3b + theme(legend.position = "none") | 
          Fig_3d + theme(legend.position = "none"))

Fig_3 <- Fig_3 + 
  inset_element(shared_legend,left = 0.85,right = 0.95,
                bottom = 0.25,top = 0.45,align_to = "full")
print(Fig_3)

### Notice that,
### For more picture details, we have further adjusted it in Adobe illustrator.


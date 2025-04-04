################################################################################
################################## Fig.S2 ######################################
################################################################################
library(cowplot) # version 1.1.3
library(ggplot2) # version 3.5.1
library(openxlsx) # version 4.2.5.2
library(dplyr) # version 1.1.1
library(tidyr) # version 1.3.1
library(ggh4x) # version 0.2.8
library(patchwork) # version 1.2.0

FigS2a_data <- read.xlsx("Figure_S2.xlsx", sheet = "Plant_community_composition", rowNames = FALSE, colNames = TRUE)
FigS2a_data_plot <- FigS2a_data[, -c(19,20)]
rownames(FigS2a_data_plot) <- FigS2a_data_plot$Plot
Species_latin <- colnames(FigS2a_data[,-c(19,20)])

FigS2a_data_plot_long <- FigS2a_data %>%
  pivot_longer(cols = Species_latin,names_to = "Species", values_to = "Count")
head(FigS2a_data_plot_long)
unique(FigS2a_data_plot_long$Native_richness)

Richness_label = c(`2` = "Native richness = 2", `4` = "Native richness = 4",
                   `6` = "Native richness = 6", `8` = "Native richness = 8",
                   `12` = "Native richness = 12") 

Richness_strip <- strip_themed(background_x = elem_list_rect(fill = c('#7db954', '#1CB3B0', '#FF9300', '#C693BE', '#1072BD')))
FigS2a_data_plot_long$Species <- factor(FigS2a_data_plot_long$Species,levels = Species_latin)
levels(FigS2a_data_plot_long$Species) <- gsub("_", " ", levels(FigS2a_data_plot_long$Species))
Species_color = (c("#440154","#481769","#472A7A","#433D84","#3D4E8A","#355E8D","#2E6D8E","#297B8E",
                   "#23898E","#1F978B","#21A585","#2EB37C","#46C06F","#65CB5E","#89D548","#B0DD2F","#D8E219","#FDE725"))


ggplot(FigS2a_data_plot_long, aes(Plot, (Species))) +
  geom_tile(data =  subset(FigS2a_data_plot_long, Count != "NA"),aes(fill = Species), color = "white") + 
  scale_fill_manual(values = (Species_color)) +  
  facet_wrap2(~ Native_richness, scales = "free_x", ncol = 5,
              labeller = labeller(Native_richness = Richness_label),
              strip = Richness_strip, strip.position = "bottom") +
  theme_bw() + scale_y_discrete(limits = rev) + 
  theme(panel.grid=element_blank(), legend.position = "none",
        strip.placement = "outside",
        strip.background = element_rect(color = "white", size = 0.5, linetype = "solid"),
        panel.spacing.x = unit(0, "lines"),
        strip.text.x = element_text(size = 11, color = "white"),
        axis.ticks.x = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(color = "black", face = "italic", size = 11)) -> FigS2_partA; FigS2_partA


###
FigS2b_data <- read.xlsx("Figure_S2.xlsx", sheet = "Frequency", rowNames = F, colNames = T)
head(FigS2b_data)
## new col: species latin names
FigS2b_data$Species_latin <- substr(FigS2b_data$Species, 3, nchar(FigS2b_data$Species))
FigS2b_data$Species_latin <- gsub("_", " ", FigS2b_data$Species_latin)
Species_order <- FigS2b_data$Species_latin
FigS2b_data$Species_latin <- factor(FigS2b_data$Species_latin,levels = Species_order)
Species_color = (c("#440154","#481769","#472A7A","#433D84","#3D4E8A","#355E8D","#2E6D8E","#297B8E",
                   "#23898E","#1F978B","#21A585","#2EB37C","#46C06F","#65CB5E","#89D548","#B0DD2F","#D8E219","#FDE725"))


plot_func <- function(data, y_var, title = "", xlab = "") {
  p <- ggplot(data, aes(x = "", y = .data[[y_var]], fill = Species_latin)) +
    geom_bar(stat = "identity", width = 1) +
    scale_fill_manual(values = (Species_color)) +  
    geom_text(aes(label = .data[[y_var]]), 
              position = position_stack(vjust = 0.5), size = 3.5, color = "white") +
    labs(title = title, x = xlab) +     
    coord_polar("y", start = 0, clip = "off") +
    theme_void() +
    theme(axis.title.y = element_text(size = 12, angle = 90, vjust = 0.5),
          legend.position = "none",
          panel.grid = element_blank(),
          panel.border = element_blank())
  return(p)
}

p1 <- plot_func(FigS2b_data, "Fre_2_Native_rich", "", "Frequency")  
p2 <- plot_func(FigS2b_data, "Fre_4_Native_rich")
p3 <- plot_func(FigS2b_data, "Fre_6_Native_rich")
p4 <- plot_func(FigS2b_data, "Fre_8_Native_rich")
p5 <- plot_func(FigS2b_data, "Fre_12_Native_rich")
FigS2_partB <- plot_grid(p1, p2, p3, p4, p5, labels = c('','','','',''), ncol = 5, nrow = 1, 
                         rel_heights = c(1.2, 1.2), align = "hv", rel_widths = c(1.2, 1.2))

## Combined 
(FigS2_partA/FigS2_partB) + plot_layout(heights = c(0.7,0.3))

### Notice that,
### For more picture details, we have further adjusted it in Adobe illustrator.

################################################################################
################################## Fig 1 #######################################
################################################################################
library(openxlsx) # version 4.2.8
library(sf) # version 1.0-19
library(ggspatial) # version 1.1.9
library(ggplot2) # version 3.5.1
library(cowplot)  # version 1.1.3

################################## Fig 1a ######################################
Fig1_Map <- read.xlsx("Figure_1.xlsx", sheet = "Field_survey", colNames = TRUE, rowNames = FALSE)
china_map <- read_sf("China_map.json")
custom_map <- read_sf("custom.geo.json")
class(custom_map)
ggplot(data = custom_map) +    
  geom_sf(data = custom_map[custom_map$subregion %in% "Eastern Asia", ], color = "black", linewidth = 0.5, fill = NA) + 
  geom_sf(data = china_map, color = "black", linewidth = 0.5, fill = NA) +
  geom_sf(data = china_map[c(3,12,10,14,15:20),], color = "black", linewidth = 0.5, fill = "grey92") +
  theme_bw() +      
  theme(legend.position = "none",  
        panel.grid = element_blank(), 
        plot.background = element_blank(), 
        axis.text = element_blank(), 
        axis.title = element_blank(),  
        axis.ticks = element_blank(),  
        axis.line = element_blank(), 
        panel.border = element_rect(color = "black", fill = NA, size = 0.3), 
        panel.background = element_rect(fill = NA, color = NA))+
  annotate("text", x = 85, y = 10, label = "East Asia", size = 4, color = "black") -> p1; p1


province_labels <- data.frame(  
  name = c("Hebei", "Shandong", "Anhui", "Henan", "Hubei", "Jiangsu", "Jiangxi", "Hunan", "Guangdong", "Guangxi"),  
  Longitude = c(115, 117, 118, 112, 112, 119.2, 116, 111, 115.5, 108),  
  Latitude = c(38.0, 36.6, 31, 34, 31, 33.5, 29.2, 28, 23.5, 23.5)) 

ggplot() +    
  geom_sf(data = china_map[c(3,12,10,14,15:20),],size = 5,fill = "grey98", color = "black") + 
  geom_text(data = province_labels, aes(x = Longitude, y = Latitude, label = name), size = 4.5, colour = "black") +  
  geom_point(data = Fig1_Map, mapping = aes(x = Longitude,y = Latitude, fill = factor(Year)),
             pch = 21, size = 2.5, alpha = 1, color = "black") + 
  theme_bw() +
  scale_color_manual(values = c("#26B8E9","#35B676",'#FFB636','#5C66A9'))+
  scale_fill_manual(values = c("#26B8E9","#35B676",'#FFB636','#5C66A9'))+
  scale_x_continuous(limits = c(105,120),breaks = c(105,110,115,120)) + 
  scale_y_continuous(limits = c(20,40),breaks = c(20,25,30,35,40)) + 
  annotation_scale(location = "bl", style = "ticks",line_width = 1.5,pad_y = unit(0.5, "cm"),text_cex = 1.2) +
  theme(text = element_text(size = 11),
        legend.position = c(0.83,0.10),
        panel.grid = element_blank(), 
        axis.text = element_text(color="black", size=14),
        panel.background = element_rect(fill = NA),
        legend.title = element_blank(),
        legend.text = element_text(size=12),
        legend.background = element_blank(), 
        plot.tag = element_text(size = 16, face = "bold"))+
  labs(x = NULL, y = NULL, tag = "(a)") -> p2; p2

ggdraw() + draw_plot(p2) +
  draw_plot(p1, x = 0.245, y = 0.68, width = 0.30, height = 0.35) -> Fig_1a; Fig_1a

 
################################## Fig_1b ######################################
## Prior Model (more details please see Fig. S3 in the supporting information)
library(lavaan) # version 0.6-19
library(AICcmodavg) # version 2.3-4

## load database
data1 = read.xlsx("Figure_1.xlsx", sheet = "Field_survey", rowNames = TRUE, colNames = TRUE)
SEM_data = subset(data1, Herb_rich != "NA" & Path_re_abun != "NA") 
dim(SEM_data)
SEM_data$Type = ifelse(SEM_data$Type == "Natural", 0, 1)
SEM_data$Path_re_abun = log10(SEM_data$Path_re_abun*100) 
SEM_data$AP_re_abun = log10(SEM_data$AP_re_abun*100)

SEM_data[c("AP_re_abun", "Type", "Year", "Invaded_time", "Native_rich",  "Ann_M_Tem",  "Path_re_abun", "Herb_rich")] = 
  scale(SEM_data[c("AP_re_abun", "Type", "Year", "Invaded_time", "Native_rich", "Ann_M_Tem", "Path_re_abun", "Herb_rich")])

colnames(SEM_data)

model_0 =  "
AP_re_abun  ~  b2*Ann_M_Tem + b3*Native_rich + b5*Path_re_abun + b6*Herb_rich
Path_re_abun ~ b8*Ann_M_Tem + b9*Native_rich
Herb_rich ~ b12*Ann_M_Tem + b13*Native_rich
Native_rich ~ b14*Ann_M_Tem
Path_re_abun ~~ Herb_rich
"

fit_model_0 <- sem(model_0, data = SEM_data)
show(fit_model_0 )
summary(fit_model_0, standardized = TRUE, rsq = TRUE)
# Finding the missing path
mi0<-modindices(fit_model_0);print(mi0[mi0$mi>3.0,])

### deleted the path "Path_re_abun ~ Native_rich"
model_1 =  "
AP_re_abun  ~  b2*Ann_M_Tem + b3*Native_rich + b5*Path_re_abun + b6*Herb_rich
Path_re_abun ~ b8*Ann_M_Tem + b9*Native_rich
Herb_rich ~ b12*Ann_M_Tem + b13*Native_rich
Native_rich ~ b14*Ann_M_Tem
Path_re_abun ~~ Herb_rich
b9 == 0
"

fit_model_1 <- sem(model_1, data = SEM_data)
show(fit_model_1 )
summary(fit_model_1, standardized = TRUE, rsq = TRUE)


### deleted the path "AP_re_abun ~ Path_re_abun"
model_2 =  "
AP_re_abun  ~  b2*Ann_M_Tem + b3*Native_rich + b5*Path_re_abun + b6*Herb_rich
Path_re_abun ~ b8*Ann_M_Tem + b9*Native_rich
Herb_rich ~ b12*Ann_M_Tem + b13*Native_rich
Native_rich ~ b14*Ann_M_Tem
Path_re_abun ~~ Herb_rich
b9 == 0
b5 == 0
"

fit_model_2 <- sem(model_2, data = SEM_data)
show(fit_model_2)
summary(fit_model_2, standardized = TRUE, rsq = TRUE)

fitMeasures(fit_model_0, c("chisq", "df", "pvalue", "cfi", "rmsea", "aic"))
fitMeasures(fit_model_1, c("chisq", "df", "pvalue", "cfi", "rmsea", "aic"))
fitMeasures(fit_model_2, c("chisq", "df", "pvalue", "cfi", "rmsea", "aic"))
aictab(list(fit_model_0, fit_model_1, fit_model_2), c("mod0", "mod1", "mod2"))

# No need for model selection as only fit_model_2 was a good fit 
### We used PowerPoint to plot the results of the structural equation model.

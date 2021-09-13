#Created by: Elise Miller
#Date started: 08/16/2021
#Date last edited: 08/16/2021
#Description: Growth data analyses for all weeks in cohort 1

#Attach files
attach(Thesis_data_Aug25)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

#Order the plot 
Thesis_data_Aug25$Light_treatment <- factor (Thesis_data_Aug25$Light_treatment, levels = c("High", "Medium", "Low"))

#Filter the data
High <- Thesis_data_Aug25 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_Aug25 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_Aug25 %>%
  filter(Light_treatment == "Low") 


#PLOTS
#==================================================================================================

#Stem length
jpeg("Stem_length2", width = 500, height = 500)
Stem_length2 <- ggplot(Thesis_data_Aug25, aes(x = Light_treatment, y = Stem_length_cm, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(26, 26)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(24.5, 24.5)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(20, 20)) 
Stem_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Leaf area
jpeg("Leaf_area2", width = 500, height = 500)
Leaf_area2 <- ggplot(Thesis_data_Aug25, aes(x = Light_treatment, y = Total_leaf_area_cm2, 
                                           fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Leaf area (cm2)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(350, 350)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(310, 310)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(330, 330))
Leaf_area2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Root length
jpeg("Root_length2", width = 500, height = 500)
Root_length2 <- ggplot(Thesis_data_Aug25, aes(x = Light_treatment, y = Root_length_cm, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(32, 32)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(30, 30)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(27, 27))
Root_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Stem diameter
jpeg("Stem_diameter2", width = 500, height = 500)
Stem_diameter2 <- ggplot(Thesis_data_Aug25, aes(x = Light_treatment, y = Stem_diameter_mm, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem diameter (mm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(4.4, 4.4)) +  
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(4.0, 4.0)) + 
  geom_signif(comparisons = list(c("Medium","Low")), map_signif_level = TRUE, 
              y_position = c(3.7, 3.7))  
Stem_diameter2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Source to sink
jpeg("Sink_source2", width = 500, height = 500)
Sink_source2 <- ggplot(Thesis_data_Aug25, aes(x = Light_treatment, y = Sink_to_source_cm, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Distance from sink to source (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(3.2, 3.2)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(3, 3)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(2.2, 2.2))
Sink_source2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Height to diameter ratio 

H_d2 <- ggplot(Thesis_data_Aug25, aes(x = Light_treatment, y = H.D, 
                                              fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Height:Diameter") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(26, 26)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(24.5, 24.5)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(20, 20)) 
H_d2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))

#Correlation plots
#Stem length vs Root length 
ggscatter(Thesis_data_Aug25, x = "Stem_length_cm", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Root length (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)

#Interesting, R = 0.82, p < 0.0001
#######################################################################
#Compare the correlations between the different light treatments
    ggscatter(High, x = "Stem_length_cm", y = "Root_length_cm", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem length (cm)", ylab = "Root length (cm)") + 
      geom_point(aes(shape = Light_treatment, color = Age..J.H.), size = 2.5, stroke = 2)
    #Interesting, R = 0.78, p < 0.0001
    
    ggscatter(Medium, x = "Stem_length_cm", y = "Root_length_cm", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem length (cm)", ylab = "Root length (cm)") + 
      geom_point(aes(shape = Light_treatment, color = Age..J.H.), size = 2.5, stroke = 2)
    #Interesting, R = 0.79, p < 0.0001
    
    ggscatter(Low, x = "Stem_length_cm", y = "Root_length_cm", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem length (cm)", ylab = "Root length (cm)") + 
      geom_point(aes(shape = Light_treatment, color = Age..J.H.), size = 2.5, stroke = 2)
    #Interesting, R = 0.8, p < 0.0001

#######################################################################

#Stem length vs leaf area 

ggscatter(Thesis_data_Aug25, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.93, p < 0.0001

############################################################################
#Correlation for the different light treatments
    ggscatter(High, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem length (cm)", ylab = "Leaf area (cm2)") + 
      geom_point(aes(shape = Light_treatment, color = Age..J.H.), size = 2.5, stroke = 2)
    #Interesting, R = 0.92, p < 0.0001
    
    ggscatter(Medium, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem length (cm)", ylab = "Leaf area (cm2)") + 
      geom_point(aes(shape = Light_treatment, color = Age..J.H.), size = 2.5, stroke = 2)
    #Interesting, R = 0.9, p < 0.0001
    
    ggscatter(Low, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem length (cm)", ylab = "Leaf area (cm2)") + 
      geom_point(aes(shape = Light_treatment, color = Age..J.H.), size = 2.5, stroke = 2)
    #Interesting, R = 0.9, p < 0.0001
###########################################################################
ggscatter(Thesis_data_Aug25, x = "Stem_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Stem diameter") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.95, p < 0.0001

###############################################################################
#Correlation in the different light treatments 
    ggscatter(High, x = "Stem_length_cm", y = "Stem_diameter_mm", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem length (cm)", ylab = "Stem Diameter (mm)") + 
      geom_point(aes(shape = Light_treatment, color = Age..J.H.), size = 2.5, stroke = 2)
    #Interesting, R = 0.93, p < 0.0001
    
    ggscatter(Medium, x = "Stem_length_cm", y = "Stem_diameter_mm", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem length (cm)", ylab = "Stem Diameter (mm)") + 
      geom_point(aes(shape = Light_treatment, color = Age..J.H.), size = 2.5, stroke = 2)
    #Interesting, R = 0.95, p < 0.0001
    
    ggscatter(Low, x = "Stem_length_cm", y = "Stem_diameter_mm", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson", 
              xlab = "Stem length (cm)", ylab = "Stem Diameter (mm)") + 
      geom_point(aes(shape = Light_treatment, color = Age..J.H.), size = 2.5, stroke = 2)
    #Interesting, R = 0.84, p < 0.0001
    
    
ggscatter(Thesis_data_Aug25, x = "Stem_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.24, p < 0.01

ggscatter(Thesis_data_Aug25, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Want to figure out to scale the colors to make it so that the color matches with the light
#R = 0.86, p < 0.0001
ggscatter(High, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.47, p < 0.001
ggscatter(Medium, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.55, p < 0.0001
ggscatter(Low, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Not interesting 

#Total leaf area 
ggscatter(Thesis_data_Aug25, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.8, p < 0.0001

ggscatter(High, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.8, p < 0.0001

ggscatter(Medium, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.77, p < 0.0001

ggscatter(Low, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.8, p < 0.0001

ggscatter(Thesis_data_Aug25, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting R = 0.94, p < 0.0001

ggscatter(High, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting R = 0.91, p < 0.0001

ggscatter(Medium, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting R = 0.91, p < 0.0001

ggscatter(Low, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting R = 0.79, p < 0.0001


ggscatter(Thesis_data_Aug25, x = "Total_leaf_area_cm2", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Leaf area (cm2)", ylab = "Sink to source (cm)") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Not interesting

ggscatter(Thesis_data_Aug25, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.51, p < 0.0001

ggscatter(Medium, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.56, p < 0.0001

ggscatter(High, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.38, p < 0.01

ggscatter(Low, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#No connection

ggscatter(High, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Interesting, R = 0.38, p < 0.01
ggscatter(Medium, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Interesting, R = 0.56, p < 0.0001
ggscatter(Low, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting 


#Root length 
ggscatter(Thesis_data_Aug25, x = "Root_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, r = 0.86, p = < 0.0001


ggscatter(Thesis_data_Aug25, x = "Root_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Sink to source (cm)")
#Not interesting 

ggscatter(Thesis_data_Aug25, x = "Root_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.48, p < 0.0001

#Stem diameter
ggscatter(Thesis_data_Aug25, x = "Stem_diameter_mm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "stem diameter (mm)", ylab = "Sink to source (cm)") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = -0.18, p < 0.05

ggscatter(Thesis_data_Aug25, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#R = 0.51, p < 0.0001

#Photosynthesis

#Light 
ggscatter(Thesis_data_Aug25, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.47, p < 0.0001
ggscatter(High, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Not interesting
ggscatter(Medium, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Not interesting
ggscatter(Low, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Interesting, R = 0.78, p = 0.0017

#Height
ggscatter(Thesis_data_Aug25, x = "Photosynthesis", y = "Stem_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem length (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.42, p < 0.0001

#Diameter 
ggscatter(Thesis_data_Aug25, x = "Photosynthesis", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.48, p < 0.0001

#Root length
ggscatter(Thesis_data_Aug25, x = "Photosynthesis", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Root length (cm)") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.46, p < 0.0001

#Respiration
#Light 
ggscatter(Thesis_data_Aug25, x = "Respiration", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Not interesting 
ggscatter(High, x = "Respiration", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Not interesting 
ggscatter(Medium, x = "Respiration", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Not interesting 
ggscatter(Low, x = "Respiration", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Not interesting 

#Height
ggscatter(Thesis_data_Aug25, x = "Respiration", y = "Stem_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Respiration", ylab = "Stem length (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = -0.49, p < 0.0001

#Diameter 
ggscatter(Thesis_data_Aug25, x = "Respiration", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Respiration", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#R = -0.61, p < 0.01

ggscatter(High, x = "Respiration", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Respiration", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#R = -0.58, p < 0.0001

ggscatter(Medium, x = "Respiration", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Respiration", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#R = -0.67, p < 0.0001

ggscatter(Low, x = "Respiration", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Respiration", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Light_treatment), size = 2.5, stroke = 2)
#R = -0.66, p < 0.019

#Root length
ggscatter(Thesis_data_Aug25, x = "Respiration", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Root length (cm)") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = -0.36, p < 0.0001

#Total Leaf Area
ggscatter(Thesis_data_Aug25, x = "Respiration", y = "Total_leaf_area_cm2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Respiration", ylab = "Root length (cm)") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = -0.58, p < 0.0001

#Histograms 
#Stem length 
hist(High$Stem_length_cm)
#Fairly normal
hist(Medium$Stem_length_cm)
#Right-skewed
hist(Low$Stem_length_cm)
#Fairly normal
hist(Thesis_data_Aug25$Stem_length_cm)
#Right-skewed

#Root length
hist(High$Root_length_cm)
#Normal? This plot is weird
hist(Medium$Root_length_cm)
#Normal?
hist(Low$Root_length_cm)
#Kind of right-skewed? 
hist(Thesis_data_Aug25$Root_length_cm)
#Right-skewed

#Stem diameter
hist(High$Stem_diameter_mm)
#Fairly normal
hist(Medium$Stem_diameter_mm)
#Fairly normal 
hist(Low$Stem_diameter_mm)
#Fairly normal 
hist(Thesis_data_Aug25$Stem_diameter_mm)
#Fairly normal

#Sink to source
hist(High$Sink_to_source_cm)
#Fairly normal
hist(Medium$Sink_to_source_cm)
#Fairly normal
hist(Low$Sink_to_source_cm)
#Left skew 
hist(Thesis_data_Aug25$Sink_to_source_cm)
#Fairly normal 

#STATISTICAL ANALYSES
#==================================================================================================

#Stem length
#ANOVA
stem.aov <- aov(Stem_length_cm ~ Light_treatment, data = Thesis_data_Aug25)
summary(stem.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment   2   1646   822.9   26.91 1.39e-10 ***
#Residuals       137   4190    30.6     

#Tukey multiple pairwise-comparisons
TukeyHSD(stem.aov)
#              diff        lwr       upr     p adj
#Medium-High -3.6910  -6.311792 -1.070208 0.0031111
#Low-High    -8.6045 -11.384269 -5.824731 0.0000000
#Low-Medium  -4.9135  -7.693269 -2.133731 0.0001474

#Leaf area
#ANOVA
leaf.aov <- aov(Total_leaf_area_cm2 ~ Light_treatment, data = Thesis_data_Aug25)
summary(leaf.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment   2 330140  165070   25.87 2.95e-10 ***
#Residuals       137 874257    6381                         

#Tukey multiple pairwise-comparisons
TukeyHSD(leaf.aov)
#diff       lwr        upr     p adj
#Medium-High  -28.15668  -66.01304   9.699679 0.1862103
#Low-High    -118.29539 -158.44812 -78.142653 0.0000000
#Low-Medium   -90.13871 -130.29144 -49.985973 0.0000012


#Root length
#ANOVA
root.aov <- aov(Root_length_cm ~ Light_treatment, data = Thesis_data_Aug25)
summary(root.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment   2   3645  1822.4   40.28 1.74e-14 ***
#Residuals       137   6198    45.2   

#Tukey multiple pairwise-comparisons
TukeyHSD(root.aov)
#diff       lwr        upr     p adj
#Medium-High  -1.5060  -4.693448  1.681448 0.5037175
#Low-High    -11.9595 -15.340299 -8.578701 0.0000000
#Low-Medium  -10.4535 -13.834299 -7.072701 0.0000000

#Stem diameter
#ANOVA
diameter.aov <- aov(Stem_diameter_mm ~ Light_treatment, data = Thesis_data_Aug25)
summary(diameter.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment   2  50.42   25.21   34.05 9.87e-13 ***
#Residuals       137 101.42    0.74     

#Tukey multiple pairwise-comparisons
TukeyHSD(diameter.aov)
#          diff        lwr         upr     p adj
#Medium-High -0.36000 -0.7677377  0.04773767 0.095228
#Low-High    -1.46505 -1.8975211 -1.03257889 0.000000
#Low-Medium  -1.10505 -1.5375211 -0.67257889 0.000000

#Sink to source
#ANOVA
sinktosource.aov <- aov(Sink_to_source_cm ~ Light_treatment, data = Thesis_data_Aug25)
summary(sinktosource.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment   2   3.68  1.8419   2.083  0.128
#Residuals       137 121.14  0.8843        

#Tukey multiple pairwise-comparisons
TukeyHSD(sinktosource.aov)
#        diff         lwr       upr     p adj
#Medium-High 0.0540 -0.39162695 0.4996270 0.9555874
#Low-High    0.3825 -0.09015876 0.8551588 0.1376119
#Low-Medium  0.3285 -0.14415876 0.8011588 0.2296896

#Photosynthesis
#ANOVA
photo.aov <- aov(Photosynthesis ~ Light_treatment, data = Thesis_data_Aug25)
summary(photo.aov)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2  970.1   485.0   45.12 4.91e-14 ***
#Residuals       84  903.0    10.7      

#Tukey multiple pairwise-comparisons
TukeyHSD(photo.aov)
#         diff        lwr       upr     p adj
#Medium-High -1.970752  -3.795497 -0.1460069 0.0311419
#Low-High    -9.920864 -12.418286 -7.4234413 0.0000000
#Low-Medium  -7.950112 -10.501012 -5.3992114 0.0000000

#Respiration
#ANOVA
resp.aov <- aov(Respiration ~ Light_treatment, data = Thesis_data_Aug25)
summary(resp.aov)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2  2.088  1.0442    9.06 0.000281 ***
#Residuals       81  9.335  0.1153      

#Tukey multiple pairwise-comparisons
TukeyHSD(resp.aov)
#         diff        lwr       upr     p adj
#Medium-High -0.2079114 -0.399625659 -0.01619711 0.0303248
#Low-High     0.2623565 -0.005214855  0.52992788 0.0558483
#Low-Medium   0.4702679  0.197033015  0.74350278 0.0002761

#Leaf number
leafnumber.aov<- aov(Leaf_number ~ Light_treatment, data = Thesis_data_Aug25)
summary(leafnumber.aov)
#                  Df Sum Sq Mean Sq F value   Pr(>F)  
#Light_treatment   2   1050   525.2   32.78 2.32e-12 ***
#Residuals       137   2195    16.0 

TukeyHSD(leafnumber.aov)
#         diff        lwr       upr     p adj
#Medium-High -2.740 -4.636863 -0.8431369 0.0023451
#Low-High    -6.865 -8.876927 -4.8530729 0.0000000
#Low-Medium  -4.125 -6.136927 -2.1130729 0.0000095

#Developing leaves
developingleaves.aov<- aov(Developing_leaves ~ Light_treatment, data = Thesis_data_Aug25)
summary(developingleaves.aov)
#                  Df Sum Sq Mean Sq F value   Pr(>F)  
#Light_treatment   2  55.32  27.658   28.36 4.95e-11 ***
#Residuals       137 133.62   0.975       

TukeyHSD(developingleaves.aov)
#         diff        lwr       upr     p adj
#Medium-High -0.26 -0.7280098  0.2080098 0.3885481
#Low-High    -1.50 -1.9963994 -1.0036006 0.0000000
#Low-Medium  -1.24 -1.7363994 -0.7436006 0.0000001


#Height to diameter ratio 
hd.aov<- aov(H.D ~ Light_treatment, data = Thesis_data_Aug25)
summary(hd.aov)
#                  Df Sum Sq Mean Sq F value   Pr(>F)  
#Light_treatment   2   3301  1650.7   6.814 0.00151 **
#Residuals       137  33188   242.2       

TukeyHSD(hd.aov)
#         diff        lwr       upr     p adj
#Medium-High -10.2567429 -17.63251 -2.880973 0.0035672
#Low-High     -9.9759876 -17.79917 -2.152802 0.0083877
#Low-Medium    0.2807553  -7.54243  8.103941 0.9960216


#COMPARING AGES
#==================================================================================================
#Stats

#Made age a character to allow for plotting
High$Age..J.H. = as.character(High$Age..J.H.)
Medium$Age..J.H. = as.character(Medium$Age..J.H.)
Low$Age..J.H. = as.character(Low$Age..J.H.)

  #Stem length
  #ANOVA
  stemH.aov <- aov(Stem_length_cm ~ Age..J.H., data = High)
  summary(stemH.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4 2186.5   546.6   88.03 <2e-16 ***
  #Residuals   45  279.4     6.2      
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(stemH.aov)
  #              diff        lwr       upr     p adj
  #29-14      6.485  3.318414  9.651586 0.0000056
  #43-14      11.130  7.963414 14.296586 0.0000000
  #50-14      16.980 13.813414 20.146586 0.0000000
  #57-14      17.565 14.398414 20.731586 0.0000000
  #43-29      4.645  1.478414  7.811586 0.0012434
  #50-29      10.495  7.328414 13.661586 0.0000000
  #57-29      11.080  7.913414 14.246586 0.0000000
  #50-43      5.850  2.683414  9.016586 0.0000381
  #57-43      6.435  3.268414  9.601586 0.0000065
  #57-50      0.585 -2.581586  3.751586 0.9843491
  
  #Stem length
  #ANOVA
  stemM.aov <- aov(Stem_length_cm ~ Age..J.H., data = Medium)
  summary(stemM.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4 1437.7   359.4   81.74 <2e-16 ***
  #Residuals   45  197.9     4.4        
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(stemM.aov)
  #              diff        lwr       upr     p adj
  #29-14        1.805 -0.85962433  4.469624 0.3194366
  #43-14        6.490  3.82537567  9.154624 0.0000001
  #50-14        11.385  8.72037567 14.049624 0.0000000
  #57-14        13.975 11.31037567 16.639624 0.0000000
  #43-29        4.685  2.02037567  7.349624 0.0000882
  #50-29        9.580  6.91537567 12.244624 0.0000000
  #57-29        12.170  9.50537567 14.834624 0.0000000
  #50-43        4.895  2.23037567  7.559624 0.0000420
  #57-43        7.485  4.82037567 10.149624 0.0000000
  #57-50        2.590 -0.07462433  5.254624 0.0604002
  
  #Stem length
  #ANOVA
  stemL.aov <- aov(Stem_length_cm ~ Age..J.H., data = Low)
  summary(stemL.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3  58.42  19.472   23.16 1.58e-08 ***
  #Residuals   36  30.26   0.841        
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(stemL.aov)
  #              diff        lwr       upr     p adj
  #29-14 0.72 -0.3842779 1.824278 0.3107833
  #43-14 2.22  1.1157221 3.324278 0.0000243
  #50-14 3.07  1.9657221 4.174278 0.0000000
  #43-29 1.50  0.3957221 2.604278 0.0042914
  #50-29 2.35  1.2457221 3.454278 0.0000092
  #50-43 0.85 -0.2542779 1.954278 0.1812986
  
  #Stem diameter
  #ANOVA
  stemdiamH.aov <- aov(Stem_diameter_mm ~ Age..J.H., data = High)
  summary(stemdiamH.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4  44.51  11.126   63.54 <2e-16 ***
  #Residuals   45   7.88   0.175     
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(stemdiamH.aov)
  #              diff        lwr       upr     p adj
  #29-14        1.269  0.7372541 1.8007459 0.0000002
  #43-14        1.583  1.0512541 2.1147459 0.0000000
  #50-14        2.375  1.8432541 2.9067459 0.0000000
  #57-14        2.687  2.1552541 3.2187459 0.0000000
  #43-29        0.314 -0.2177459 0.8457459 0.4574950
  #50-29        1.106  0.5742541 1.6377459 0.0000041
  #57-29        1.418  0.8862541 1.9497459 0.0000000
  #50-43        0.792  0.2602541 1.3237459 0.0010199
  #57-43        1.104  0.5722541 1.6357459 0.0000043
  #57-50        0.312 -0.2197459 0.8437459 0.4639513

  #Stem diameter
  #ANOVA
  stemdiamM.aov <- aov(Stem_diameter_mm ~ Age..J.H., data = Medium)
  summary(stemdiamM.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4  40.54  10.136      77 <2e-16 ***
  #Residuals   45   5.92   0.132      
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(stemdiamM.aov)
  #              diff        lwr       upr     p adj
  #29-14        0.581  0.1199669 1.0420331 0.0071113
  #43-14        1.418  0.9569669 1.8790331 0.0000000
  #50-14        2.110  1.6489669 2.5710331 0.0000000
  #57-14        2.385  1.9239669 2.8460331 0.0000000
  #43-29        0.837  0.3759669 1.2980331 0.0000515
  #50-29        1.529  1.0679669 1.9900331 0.0000000
  #57-29        1.804  1.3429669 2.2650331 0.0000000
  #50-43        0.692  0.2309669 1.1530331 0.0009211
  #57-43        0.967  0.5059669 1.4280331 0.0000035
  #57-50        0.275 -0.1860331 0.7360331 0.4472967
  
  #Stem diameter
  #ANOVA
  stemdiamL.aov <- aov(Stem_diameter_mm ~ Age..J.H., data = Low)
  summary(stemdiamL.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3 1.6369  0.5456   21.07 4.71e-08 ***
  #Residuals   36 0.9322  0.0259      
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(stemdiamL.aov)
  #              diff        lwr       upr     p adj
  #29-14        0.269  0.075180142 0.4628199 0.0034444
  #43-14        0.364  0.170180142 0.5578199 0.0000719
  #50-14        0.562  0.368180142 0.7558199 0.0000000
  #43-29        0.095 -0.098819858 0.2888199 0.5565073
  #50-29        0.293  0.099180142 0.4868199 0.0013416
  #50-43        0.198  0.004180142 0.3918199 0.0437215
  
  #Leaf Area
  #ANOVA
  LeafAreaH.aov <- aov(Total_leaf_area_cm2 ~ Age..J.H., data = High)
  summary(LeafAreaH.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4 351623   87906      46 2.41e-15 ***
  #Residuals   45  85993    1911      
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(LeafAreaH.aov)
  #              diff        lwr       upr     p adj
  #29-14  72.6214  17.07175 128.17105 0.0048399
  #43-14 134.7189  79.16925 190.26855 0.0000001
  #50-14 202.2723 146.72265 257.82195 0.0000000
  #57-14 228.7497 173.20005 284.29935 0.0000000
  #43-29  62.0975   6.54785 117.64715 0.0215199
  #50-29 129.6509  74.10125 185.20055 0.0000004
  #57-29 156.1283 100.57865 211.67795 0.0000000
  #50-43  67.5534  12.00375 123.10305 0.0101174
  #57-43  94.0308  38.48115 149.58045 0.0001621
  #57-50  26.4774 -29.07225  82.02705 0.6592635
  
  #Leaf Area
  #ANOVA
  LeafAreaM.aov <- aov(Total_leaf_area_cm2 ~ Age..J.H., data = Medium)
  summary(LeafAreaM.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4 336775   84194   40.66 2.13e-14 ***
  #Residuals   45  93183    2071     
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(LeafAreaM.aov)
  #              diff        lwr       upr     p adj
  #29-14        22.3395 -35.48566  80.16466 0.8067633
  #43-14        93.6084  35.78324 151.43356 0.0003195
  #50-14        191.9447 134.11954 249.76986 0.0000000
  #57-14        195.6778 137.85264 253.50296 0.0000000
  #43-29        71.2689  13.44374 129.09406 0.0088830
  #50-29        169.6052 111.78004 227.43036 0.0000000
  #57-29        173.3383 115.51314 231.16346 0.0000000
  #50-43        98.3363  40.51114 156.16146 0.0001507
  #57-43        102.0694  44.24424 159.89456 0.0000826
  #57-50        3.7331 -54.09206  61.55826 0.9997366
  
  #Leaf Area
  #ANOVA
  LeafAreaL.aov <- aov(Total_leaf_area_cm2 ~ Age..J.H., data = Low)
  summary(LeafAreaL.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3   3581  1193.5   13.85 3.67e-06 ***
  #Residuals   36   3102    86.2    
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(LeafAreaL.aov)
  #              diff        lwr       upr     p adj
  #29-14  4.0559 -7.125158 15.23696 0.7633148
  #43-14 17.4738  6.292742 28.65486 0.0009015
  #50-14 23.1258 11.944742 34.30686 0.0000151
  #43-29 13.4179  2.236842 24.59896 0.0133969
  #50-29 19.0699  7.888842 30.25096 0.0002907
  #50-43  5.6520 -5.529058 16.83306 0.5310747
  
  #Sink to source
  #ANOVA
  SinkSourceH.aov <- aov(Sink_to_source_cm ~ Age..J.H., data = High)
  summary(SinkSourceH.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4  15.97   3.994   3.718 0.0107 *
  #Residuals   45  48.34   1.074   
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD( SinkSourceH.aov)
  #              diff        lwr       upr     p adj
  #29-14        0.135 -1.1820222 1.452022 0.9983686
  #43-14        0.580 -0.7370222 1.897022 0.7216201
  #50-14        0.530 -0.7870222 1.847022 0.7826592
  #57-14        1.610  0.2929778 2.927022 0.0096204
  #43-29        0.445 -0.8720222 1.762022 0.8713792
  #50-29        0.395 -0.9220222 1.712022 0.9125285
  #57-29        1.475  0.1579778 2.792022 0.0211889
  #50-43        -0.050 -1.3670222 1.267022 0.9999681
  #57-43        1.030 -0.2870222 2.347022 0.1902801
  #57-50        1.080 -0.2370222 2.397022 0.1543630
  
  #Sink to source
  #ANOVA
  SinkSourceM.aov <- aov(Sink_to_source_cm ~ Age..J.H., data = Medium)
  summary(SinkSourceM.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4   8.32  2.0800   2.563 0.0511 .
  #Residuals   45  36.52  0.8117    
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD( SinkSourceM.aov)
  #              diff        lwr       upr     p adj
  #29-14        0.280 -0.86483756 1.4248376 0.9565556
  #43-14        0.635 -0.50983756 1.7798376 0.5200788
  #50-14        0.490 -0.65483756 1.6348376 0.7420946
  #57-14        1.220  0.07516244 2.3648376 0.0315492
  #43-29        0.355 -0.78983756 1.4998376 0.9023740
  #50-29        0.210 -0.93483756 1.3548376 0.9847588
  #57-29        0.940 -0.20483756 2.0848376 0.1534531
  #50-43        -0.145 -1.28983756 0.9998376 0.9962818
  #57-43        0.585 -0.55983756 1.7298376 0.5981745
  #57-50        0.730 -0.41483756 1.8748376 0.3796385
  
  #Sink to source
  #ANOVA
  SinkSourceL.aov <- aov(Sink_to_source_cm ~ Age..J.H., data = Low)
  summary(SinkSourceL.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3  4.443  1.4811   7.067 0.000744 ***
  #Residuals   36  7.544  0.2096    
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD( SinkSourceL.aov)
  #              diff        lwr       upr     p adj
  #29-14  0.566  0.01462463 1.1173754 0.0423814
  #43-14  0.821  0.26962463 1.3723754 0.0015985
  #50-14  0.811  0.25962463 1.3623754 0.0018374
  #43-29  0.255 -0.29637537 0.8063754 0.6026341
  #50-29  0.245 -0.30637537 0.7963754 0.6328431
  #50-43 -0.010 -0.56137537 0.5413754 0.9999573
  
  #Leaf number
  #ANOVA
  LeafnumberH.aov <- aov(Leaf_number ~ Age..J.H., data = High)
  summary(LeafnumberH.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4  976.3  244.08    76.7 <2e-16 ***
  #Residuals   45  143.2    3.18         
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(LeafnumberH.aov)
  #              diff        lwr       upr     p adj
  #29-14        4.8  2.5331631  7.066837 0.0000029
  #43-14        8.2  5.9331631 10.466837 0.0000000
  #50-14        11.2  8.9331631 13.466837 0.0000000
  #57-14        12.0  9.7331631 14.266837 0.0000000
  #43-29        3.4  1.1331631  5.666837 0.0009300
  #50-29        6.4  4.1331631  8.666837 0.0000000
  #57-29        7.2  4.9331631  9.466837 0.0000000
  #50-43        3.0  0.7331631  5.266837 0.0042354
  #57-43        3.8  1.5331631  6.066837 0.0001886
  #57-50        0.8 -1.4668369  3.066837 0.8527068
  
  
  #Leaf number
  #ANOVA
  LeafnumberM.aov <- aov(Leaf_number ~ Age..J.H., data = Medium)
  summary(LeafnumberM.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4  851.2  212.80   74.06 <2e-16 ***
  #Residuals   45  129.3    2.87           
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(LeafnumberM.aov)
  #              diff        lwr       upr     p adj
  #29-14        2.5  0.3459885  4.654012 0.0155777
  #43-14        5.9  3.7459885  8.054012 0.0000000
  #50-14        10.5  8.3459885 12.654012 0.0000000
  #57-14        10.1  7.9459885 12.254012 0.0000000
  #43-29        3.4  1.2459885  5.554012 0.0004608
  #50-29        8.0  5.8459885 10.154012 0.0000000
  #57-29        7.6  5.4459885  9.754012 0.0000000
  #50-43        4.6  2.4459885  6.754012 0.0000024
  #57-43        4.2  2.0459885  6.354012 0.0000144
  #57-50        -0.4 -2.5540115  1.754012 0.9840444
  
  #Leaf number
  #ANOVA
  LeafnumberL.aov <- aov(Leaf_number ~ Age..J.H., data = Low)
  summary(LeafnumberL.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3  83.88  27.958   90.68 <2e-16 ***
  #Residuals   36  11.10   0.308              
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(LeafnumberL.aov)
  #              diff        lwr       upr     p adj
  #29-14  0.7  0.03119698 1.368803 0.0373097
  #43-14  3.1  2.43119698 3.768803 0.0000000
  #50-14  3.3  2.63119698 3.968803 0.0000000
  #43-29  2.4  1.73119698 3.068803 0.0000000
  #50-29  2.6  1.93119698 3.268803 0.0000000
  #50-43  0.2 -0.46880302 0.868803 0.8514597
  
  #Photosynthesis
  #ANOVA
  PhotosynH.aov <- aov(Photosynthesis ~ Age..J.H., data = High)
  summary( PhotosynH.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3   39.5   13.16   0.869  0.466
  #Residuals   36  544.9   15.14                
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(PhotosynH.aov)
  #              diff        lwr       upr     p adj
  #43-29  1.95134144 -2.734758 6.637441 0.6788602
  #50-29 -0.73572822 -5.421828 3.950371 0.9742074
  #57-29  0.05572704 -4.630372 4.741827 0.9999880
  #50-43 -2.68706966 -7.373169 1.999030 0.4225240
  #57-43 -1.89561440 -6.581714 2.790485 0.6981251
  #57-50  0.79145526 -3.894644 5.477555 0.9682481
  
  #Photosynthesis
  #ANOVA
  PhotosynM.aov <- aov(Photosynthesis ~ Age..J.H., data = Medium)
  summary( PhotosynM.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3  35.96  11.987   1.386  0.266
  #Residuals   30 259.36   8.645                    
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(PhotosynM.aov)
  #              diff        lwr       upr     p adj
  #43-29 -1.9698579 -6.699790 2.760074 0.6727830
  #50-29 -0.0762516 -4.806184 4.653681 0.9999690
  #57-29  0.5828049 -4.147127 5.312737 0.9867798
  #50-43  1.8936063 -1.681886 5.469099 0.4851900
  #57-43  2.5526628 -1.022830 6.128156 0.2328615
  #57-50  0.6590565 -2.916436 4.234549 0.9581562
  
  #Photosynthesis
  #ANOVA
  PhotosynL.aov <- aov(Photosynthesis ~ Age..J.H., data = Low)
  summary( PhotosynL.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    1 14.858  14.858   19.57 0.00102 **
  #Residuals   11  8.349   0.759                    
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(PhotosynL.aov)
  #              diff        lwr       upr     p adj
  #50-43 2.19745 1.104273 3.290626 0.0010212
  
  #Respiration
  #ANOVA
  RespH.aov <- aov(Respiration ~ Age..J.H., data = High)
  summary(RespH.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3  1.518  0.5059   6.715 0.00107 **
  #Residuals   35  2.637  0.0753                     
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(RespH.aov)
  #              diff        lwr       upr     p adj
  #43-29        -0.01116681 -0.3422198  0.31988617 0.9997248
  #50-29        -0.39026165 -0.7213146 -0.05920867 0.0155794
  #57-29        -0.41067652 -0.7508011 -0.07055191 0.0127920
  #50-43        -0.37909484 -0.7101478 -0.04804186 0.0195913
  #57-43        -0.39950971 -0.7396343 -0.05938510 0.0160393
  #57-50        -0.02041487 -0.3605395  0.31970974 0.9984607
  
  #Respiration
  #ANOVA
  RespM.aov <- aov(Respiration ~ Age..J.H., data = Medium)
  summary(RespM.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3  2.742  0.9141   17.03 1.44e-06 ***
  #esiduals   29  1.557  0.0537                        
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(RespM.aov)
  #              diff        lwr       upr     p adj
  #43-29 -0.3151288 -0.6944813  0.06422376 0.1304074
  #50-29 -0.5059634 -0.8794341 -0.13249260 0.0048172
  #57-29 -0.8788713 -1.2523420 -0.50540053 0.0000030
  #50-43 -0.1908346 -0.4808881  0.09921894 0.2972574
  #57-43 -0.5637425 -0.8537960 -0.27368899 0.0000632
  #57-50 -0.3729079 -0.6552253 -0.09059057 0.0061074
  
  #Respiration
  #ANOVA
  RespL.aov <- aov(Respiration ~ Age..J.H., data = Low)
  summary(RespL.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    1 0.0332 0.03322   0.392  0.545
  #Residuals   10 0.8482 0.08482                            
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(RespL.aov)
  #              diff        lwr       upr     p adj
  #50-43 -0.106724 -0.4866898 0.2732419 0.5454413
  
  #Root length
  #ANOVA
  RootH.aov <- aov(Root_length_cm ~ Age..J.H., data = High)
  summary(RootH.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4 2233.1   558.3   52.31 2.34e-16 ***
  #Residuals   45  480.2    10.7                            
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(RootH.aov)
  #              diff        lwr       upr     p adj
  #29-14        13.70  9.5487895 17.8512105 0.0000000
  #43-14        12.56  8.4087895 16.7112105 0.0000000
  #50-14        19.66 15.5087895 23.8112105 0.0000000
  #57-14        16.29 12.1387895 20.4412105 0.0000000
  #43-29        -1.14 -5.2912105  3.0112105 0.9349965
  #50-29        5.96  1.8087895 10.1112105 0.0016312
  #57-29        2.59 -1.5612105  6.7412105 0.4016687
  #50-43        7.10  2.9487895 11.2512105 0.0001377
  #57-43        3.73 -0.4212105  7.8812105 0.0969100
  #57-50        -3.37 -7.5212105  0.7812105 0.1616633
  
  #Root length
  #ANOVA
  RootM.aov <- aov(Root_length_cm ~ Age..J.H., data = Medium)
  summary(RootM.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    4 2166.9   541.7   45.67 2.73e-15 ***
  #Residuals   45  533.7    11.9                                
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(RootM.aov)
  #              diff        lwr       upr     p adj
  #29-14        10.395  6.0186443 14.771356 0.0000002
  #43-14        12.985  8.6086443 17.361356 0.0000000
  #50-14        18.240 13.8636443 22.616356 0.0000000
  #57-14        17.510 13.1336443 21.886356 0.0000000
  #43-29        2.590 -1.7863557  6.966356 0.4552553
  #50-29        7.845  3.4686443 12.221356 0.0000639
  #57-29        7.115  2.7386443 11.491356 0.0002999
  #50-43        5.255  0.8786443  9.631356 0.0114135
  #57-43        4.525  0.1486443  8.901356 0.0395185
  #57-50        -0.730 -5.1063557  3.646356 0.9893323
  
  #Root length
  #ANOVA
  RootL.aov <- aov(Root_length_cm ~ Age..J.H., data = Low)
  summary(RootL.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3  393.5  131.16   12.09 1.26e-05 ***
  #Residuals   36  390.5   10.85                                 
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(RootL.aov)
  #              diff        lwr       upr     p adj
  #29-14 2.455 -1.5118304  6.42183 0.3556510
  #43-14 3.645 -0.3218304  7.61183 0.0812419
  #50-14 8.610  4.6431696 12.57683 0.0000065
  #43-29 1.190 -2.7768304  5.15683 0.8502782
  #50-29 6.155  2.1881696 10.12183 0.0009838
  #50-43 4.965  0.9981696  8.93183 0.0093203
  
  
  
#Have to re-download the csv
attach(Thesis_data_Aug25)

#Re-filter
High <- Thesis_data_Aug25 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_Aug25 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_Aug25 %>%
  filter(Light_treatment == "Low") 



Thesis_data_Aug25$Photosynthesis = as.character(Thesis_data_Aug25$Photosynthesis)

#Add all 3 plots together 

ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Stem_length_cm, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Stem Length (cm)") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50")) 

#High
ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Stem_length_cm, group = Age..J.H.)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Stem Length (cm)") + 
  geom_signif(comparisons = list(c("14","29")), map_signif_level = TRUE, 
              y_position = c(11, 11)) 
  

#Leaf Area
ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Total_leaf_area_cm2, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Leaf Area (cm2)") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

#Root Length
ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Root_length_cm, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Root Length (cm)") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

#Stem diameter
ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Stem_diameter_mm, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Stem Diameter (mm)") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Leaf_number, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Leaf number") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = H.D, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Height:Diameter") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Photosynthesis, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Photosynthesis") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Photosynthesis, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Photosynthesis") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Photosynthesis, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Photosynthesis") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

ggplot(subset(Thesis_data_Aug25, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Respiration, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Respiration") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

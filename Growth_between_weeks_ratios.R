#Created by: Elise Miller
#Date started: 08/25/2021
#Date last edited: 08/25/2021
#Description: Comparison of the growth between weeks for Cohort 1 

#Attach files
attach(Growth_between_weeks_ratios)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(tibble)
library(readr)
library(car)


#Order the plot 
Growth_between_weeks_ratios$Light_treatment <- factor (Growth_between_weeks_ratios$Light_treatment, levels = c("High", "Medium", "Low"))

#PLOTS
#==================================================================================================

High <- Growth_between_weeks_ratios %>%
  filter(Light_treatment == "High") 

Medium <- Growth_between_weeks_ratios %>%
  filter(Light_treatment == "Medium") 

Low <- Growth_between_weeks_ratios %>%
  filter(Light_treatment == "Low") 

#Made age a character to allow for plotting
High$Weeks = as.character(High$Weeks)
Medium$Weeks = as.character(Medium$Weeks)
Low$Weeks = as.character(Low$Weeks)

Growth_between_weeks_ratios$Photosynthesis = as.character(Growth_between_weeks_ratios$Photosynthesis)

#Stem_length

Stem_lengthH <- ggplot(High, aes(x = Weeks, y = Stem_length_cm, 
                                 fill = Light_treatment)) + 
  ylim(0, 8) +
  geom_point(shape = 17, color = "darkorange") + 
  labs(x = "", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  


Stem_lengthM <- ggplot(Medium, aes(x = Weeks, y = Stem_length_cm, 
                                   fill = Light_treatment)) + 
  ylim(0, 8) +
  geom_point(shape = 15, color = "darkgoldenrod2") + 
  labs(x = "Weeks", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Stem_lengthL <- ggplot(Low, aes(x = Weeks, y = Stem_length_cm, 
                                fill = Light_treatment)) + 
  ylim(0, 8) +
  geom_point(shape = 19, color = "gray50") +
  labs(x = "", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

#Combine all 3 plots 
ggarrange(Stem_lengthH, Stem_lengthM, Stem_lengthL, 
          labels = c("High", "Medium", "Low"), 
          ncol = 3, nrow = 1)


#Stem_diameter

Stem_diameterH <- ggplot(High, aes(x = Weeks, y = Stem_diameter_mm, 
                                 fill = Light_treatment)) + 
  ylim(0, 3.5) +
  geom_point(shape = 17, color = "darkorange") +  
  labs(x = "", y = "Stem Diameter (mm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  
Stem_diameterH


Stem_diameterM <- ggplot(Medium, aes(x = Weeks, y = Stem_diameter_mm, 
                                   fill = Light_treatment)) + 
  ylim(0, 3.5) +
  geom_point(shape = 15, color = "darkgoldenrod2") + 
  labs(x = "Weeks", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  
Stem_diameterM

Stem_diameterL <- ggplot(Low, aes(x = Weeks, y = Stem_diameter_mm, 
                                fill = Light_treatment)) + 
  ylim(0, 3.5) +
  geom_point(shape = 19, color = "gray50") +
  labs(x = "", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  
Stem_diameterL

#Combine all 3 plots 
ggarrange(Stem_diameterH, Stem_diameterM, Stem_diameterL, 
          labels = c("High", "Medium", "Low"), 
          ncol = 3, nrow = 1)


#Sink to source

Sink_to_sourceH <- ggplot(High, aes(x = Weeks, y = Sink_to_source_cm, 
                                   fill = Light_treatment)) + 
  ylim(0, 2) +
  geom_point(shape = 17, color = "darkorange") + 
  labs(x = "", y = "Distance of Sink to Source (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Sink_to_sourceM <- ggplot(Medium, aes(x = Weeks, y = Sink_to_source_cm, 
                                     fill = Light_treatment)) + 
  ylim(0, 2) +
  geom_point(shape = 15, color = "darkgoldenrod2") +  
  labs(x = "Weeks", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Sink_to_sourceL <- ggplot(Low, aes(x = Weeks, y = Sink_to_source_cm, 
                                  fill = Light_treatment)) + 
  ylim(0, 2) +
  geom_point(shape = 19, color = "gray50") +
  labs(x = "", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  


#Combine all 3 plots 
ggarrange(Sink_to_sourceH, Sink_to_sourceM, Sink_to_sourceL, 
          labels = c("High", "Medium", "Low"), 
          ncol = 3, nrow = 1)

#Leaf number

Leaf_numberH <- ggplot(High, aes(x = Weeks, y = Leaf_number, 
                                    fill = Light_treatment)) + 
  ylim(0, 3) +
  geom_point(shape = 17, color = "darkorange") +  
  labs(x = "", y = "Leaf number") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Leaf_numberM <- ggplot(Medium, aes(x = Weeks, y = Leaf_number, 
                                      fill = Light_treatment)) + 
  ylim(0, 3) +
  geom_point(shape = 15, color = "darkgoldenrod2") + 
  labs(x = "Weeks", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Leaf_numberL <- ggplot(Low, aes(x = Weeks, y = Leaf_number, 
                                   fill = Light_treatment)) + 
  ylim(0, 3) +
  geom_point(shape = 19, color = "gray50") + 
  labs(x = "", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  


#Combine all 3 plots 
ggarrange(Leaf_numberH, Leaf_numberM, Leaf_numberL, 
          labels = c("High", "Medium", "Low"), 
          ncol = 3, nrow = 1)


#Developing leaves

Developing_leavesH <- ggplot(High, aes(x = Weeks, y = Developing_leaves, 
                                 fill = Light_treatment)) + 
  ylim(0, 12) +
  geom_point(shape = 17, color = "darkorange") + 
  labs(x = "", y = "Developing Leaves") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Developing_leavesM <- ggplot(Medium, aes(x = Weeks, y = Developing_leaves, 
                                   fill = Light_treatment)) + 
  ylim(0, 12) +
  geom_point(shape = 15, color = "darkgoldenrod2") + 
  labs(x = "Weeks", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Developing_leavesL <- ggplot(Low, aes(x = Weeks, y = Developing_leaves, 
                                fill = Light_treatment)) + 
  ylim(0, 12) +
  geom_point(shape = 19, color = "gray50") +
  labs(x = "", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  


#Combine all 3 plots 
ggarrange(Developing_leavesH, Developing_leavesM, Developing_leavesL, 
          labels = c("High", "Medium", "Low"), 
          ncol = 3, nrow = 1)

#Total Leaf Area

Total_leaf_areaH <- ggplot(High, aes(x = Weeks, y = Total_leaf_area_cm2, 
                                       fill = Light_treatment)) + 
  ylim(0, 40) +
  geom_point(shape = 17, color = "darkorange") + 
  labs(x = "", y = "Total Leaf Area (cm2)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Total_leaf_areaM <- ggplot(Medium, aes(x = Weeks, y = Total_leaf_area_cm2, 
                                         fill = Light_treatment)) + 
  ylim(0, 40) +
  geom_point(shape = 15, color = "darkgoldenrod2") + 
  labs(x = "Weeks", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Total_leaf_areaL <- ggplot(Low, aes(x = Weeks, y = Total_leaf_area_cm2, 
                                      fill = Light_treatment)) + 
  ylim(0, 40) +
  geom_point(shape = 19, color = "gray50") +
  labs(x = "", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  


#Combine all 3 plots 
ggarrange(Total_leaf_areaH, Total_leaf_areaM, Total_leaf_areaL, 
          labels = c("High", "Medium", "Low"), 
          ncol = 3, nrow = 1)

#Root Length

Root_lengthH <- ggplot(High, aes(x = Weeks, y = Root_length_cm, 
                                     fill = Light_treatment)) + 
  ylim(0, 4) +
  geom_point(shape = 17, color = "darkorange") + 
  labs(x = "", y = "Root Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Root_lengthM <- ggplot(Medium, aes(x = Weeks, y = Root_length_cm, 
                                       fill = Light_treatment)) + 
  ylim(0, 4) +
  geom_point(shape = 15, color = "darkgoldenrod2") + 
  labs(x = "Weeks", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

Root_lengthL <- ggplot(Low, aes(x = Weeks, y = Root_length_cm, 
                                    fill = Light_treatment)) + 
  ylim(0, 4) +
  geom_point(shape = 19, color = "gray50") +
  labs(x = "", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  


#Combine all 3 plots 
ggarrange(Root_lengthH, Root_lengthM, Root_lengthL, 
          labels = c("High", "Medium", "Low"), 
          ncol = 3, nrow = 1)

#Photosynthesis

PhotosynthesisH <- ggplot(High, aes(x = Weeks, y = Photosynthesis, 
                                 fill = Light_treatment)) + 
  ylim(-6, 2.5) +
  geom_point(shape = 17, color = "darkorange") + 
  labs(x = "", y = "Photosynthesis") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

PhotosynthesisM <- ggplot(Medium, aes(x = Weeks, y = Photosynthesis, 
                                   fill = Light_treatment)) + 
  ylim(-6, 2.5) +
  geom_point(shape = 15, color = "darkgoldenrod2") + 
  labs(x = "Weeks", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

PhotosynthesisL <- ggplot(Low, aes(x = Weeks, y = Photosynthesis, 
                                fill = Light_treatment)) + 
  ylim(-6, 2.5) +
  geom_point(shape = 19, color = "gray50") + 
  labs(x = "", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  


#Combine all 3 plots 
ggarrange(PhotosynthesisH, PhotosynthesisM, PhotosynthesisL, 
          labels = c("High", "Medium", "Low"), 
          ncol = 3, nrow = 1)

#Respiration

RespirationH <- ggplot(High, aes(x = Weeks, y = Respiration, 
                                    fill = Light_treatment)) + 
  ylim(0, 2.5) +
  geom_point(shape = 17, color = "darkorange") + 
  labs(x = "", y = "Respiration") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

RespirationM <- ggplot(Medium, aes(x = Weeks, y = Respiration, 
                                      fill = Light_treatment)) + 
  ylim(0, 2.5) +
  geom_point(shape = 15, color = "darkgoldenrod2") + 
  labs(x = "Weeks", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  

RespirationL <- ggplot(Low, aes(x = Weeks, y = Respiration, 
                                   fill = Light_treatment)) + 
  ylim(0, 2.5) +
  geom_point(shape = 19, color = "gray50") + 
  labs(x = "", y = "") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  


#Combine all 3 plots 
ggarrange(RespirationH, RespirationM, RespirationL, 
          labels = c("High", "Medium", "Low"), 
          ncol = 3, nrow = 1)

#STATISTICAL ANALYSES
#==================================================================================================

#Check assumptions

ggscatter( 
  Growth_between_weeks_ratios, x = "Weeks", y = "Stem_length_cm", 
  color = "Light_treatment", add = "reg.line" 
  ) + 
  stat_regline_equation(
    aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = Light_treatment)
  )

stemlength <- aov(Stem_length_cm ~ Weeks + Light_treatment, Growth_between_weeks_ratios)
Anova(stemlength, type ="III")
TukeyHSD(stemlength)

rootlength <- aov(Root_length_cm ~ Light_treatment + Weeks, Growth_between_weeks_ratios)
Anova(rootlength, type ="III")
TukeyHSD(rootlength)


stemdiameter <- aov(Stem_diameter_mm ~ Light_treatment + Weeks, Growth_between_weeks_ratios)
Anova(stemdiameter, type ="III")
TukeyHSD(stemdiameter)

leafarea <- aov(Total_leaf_area_cm2 ~ Light_treatment + Weeks, Growth_between_weeks_ratios)
Anova(leafarea , type ="III")
TukeyHSD(leafarea)

leafnumber <- aov(Leaf_number ~ Light_treatment + Weeks, Growth_between_weeks_ratios)
Anova(leafnumber, type ="III")
TukeyHSD(leafnumber)

developingleaves <- aov(Developing_leaves ~ Light_treatment + Weeks, Growth_between_weeks_ratios)
Anova(developingleaves, type ="III")
TukeyHSD(developingleaves)

sinksource <- aov(Sink_to_source_cm ~ Light_treatment + Weeks, Growth_between_weeks_ratios)
Anova(sinksource, type ="III")
TukeyHSD(sinksource)

photosynthesis <- aov(Photosynthesis ~ Light_treatment + Weeks, Growth_between_weeks_ratios)
Anova(photosynthesis, type ="III")
TukeyHSD(photosynthesis)

respiration <- aov(Respiration ~ Light_treatment + Weeks, Growth_between_weeks_ratios)
Anova(respiration, type ="III")
TukeyHSD(respiration)










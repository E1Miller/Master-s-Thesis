#Created by: Elise Miller
#Date started: 09/08/2021
#Date last edited: 09/08/2021
#Description: Growth data analyses for week 2 data for both cohorts

#Attach files
attach(Thesis_data_Sept8)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)
library(scales)

#Order the plot
Thesis_data_Sept8$Light_treatment <- factor (Thesis_data_Sept8$Light_treatment, levels = c("High", "Medium", "Low"))


#SUMMARY 
#==================================================================================================
#Filtering the data
High_2 <- Thesis_data_Sept8 %>%
  filter(Light_treatment == "High" & Cohort == "2") 

Medium_2 <- Thesis_data_Sept8 %>%
  filter(Light_treatment == "Medium" & Cohort == "2") 

Low_2 <- Thesis_data_Sept8 %>%
  filter(Light_treatment == "Low" & Cohort == "2") 

High_all <- High_2 <- Thesis_data_Sept8 %>%
  filter(Light_treatment == "High" & Age..J.H. == "14") 

Medium_all <- Thesis_data_Sept8 %>%
  filter(Light_treatment == "Medium" & Age..J.H. == "14") 

Low_all <- Thesis_data_Sept8 %>%
  filter(Light_treatment == "Low" & Age..J.H. == "14") 

Week_2 <- Thesis_data_Sept8 %>%
  filter(Age..J.H. == "14")

Cohort_2 <- Thesis_data_Sept8 %>%
  filter(Cohort == "2")

#COHORT 2

#Stem length 
#High summary 
summary(High_2$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.300   0.725   0.925   0.920   1.137   1.400 
sd(High_2$Stem_length_cm) #0.3481698

#Medium summary 
summary(Medium_2$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.900   1.000   1.050   1.095   1.175   1.400 
sd(Medium_2$Stem_length_cm) #0.1606411

#Low summary 
summary(Low_2$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.9000  0.9875  1.1500  1.1550  1.2750  1.6000 
sd(Low_2$Stem_length_cm) #0.2166026

#Leaf Area
#High summary 
summary(High$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.426   1.090   2.051   2.006   2.886   3.834 

#Medium summary 
summary(Medium$Total_leaf_area_cm2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6240  0.6787  0.7135  0.8073  0.9163  1.1760  

#Low summary 
summary(Low$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02200 0.09325 0.17200 0.21880 0.22000 0.88000  

#Root length 

#High summary 
summary(High_2$Root_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.500   3.625   4.600   4.600   5.275   7.200  
sd(High_2$Root_length_cm) # 1.372751

#Medium summary 
summary(Medium_2$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.300   2.750   3.525   3.735   4.500   5.800 
sd(Medium_2$Root_length_cm) # 1.149891

#Low summary 
summary(Low_2$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.950   1.925   2.150   2.055   2.312   2.900  
sd(Low_2$Root_length_cm) # 0.5545018

#Stem diameter

#High summary 
summary(High_2$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.560   0.575   0.600   0.662   0.770   0.810  
sd(High_2$Stem_diameter_mm) #0.1074761

#Medium summary 
summary(Medium_2$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.420   0.525   0.575   0.574   0.640   0.690 
sd(Medium_2$Stem_diameter_mm) #0.08435375

#Low summary 
summary(Low_2$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.370   0.410   0.420   0.421   0.420   0.510 
sd(Low_2$Stem_diameter_mm) #0.03541814

#Sink to source

#High summary
summary(High_2$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2500  0.4000  0.5500  0.5200  0.5875  0.9000 
sd(High_2$Sink_to_source_cm) #0.1932184

#Medium summary 
summary(Medium_2$Sink_to_source_cm)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4000  0.8000  0.8250  0.8200  0.8875  1.2000 
sd(Medium_2$Sink_to_source_cm) #0.1974842

#Low summary 
summary(Low_2$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.800   0.925   1.050   1.065   1.175   1.500  
sd(Low_2$Sink_to_source_cm) #0.205548

#Height:diameter

#High summary
summary(High_2$H.D)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.704  12.144  14.395  14.206  15.972  25.000 
sd(High_2$H.D) #5.501498

#Medium summary 
summary(Medium_2$H.D)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 14.49   15.84   19.26   19.53   21.07   26.92 
sd(Medium_2$H.D) #4.45648

#Low summary 
summary(Low_2$H.D)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 21.43   23.05   27.37   27.54   30.71   38.10 
sd(Low_2$H.D) #5.319549

################################################################################
#BOTH COHORTS

#Stem length 

#High summary 
summary(High_all$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.3000  0.7000  0.9500  0.9375  1.1000  1.6000 
sd(High_all$Stem_length_cm) #0.3157593

#Medium summary 
summary(Medium_all$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.7000  0.9375  1.0000  1.0300  1.1250  1.4000 
sd(Medium_all$Stem_length_cm) #0.1742654

#Low summary 
summary(Low_all$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.800   1.062   1.200   1.218   1.350   1.600 
sd(Low_all$Stem_length_cm) #0.2530212

#Leaf Area
#High summary 
summary(High$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.426   1.090   2.051   2.006   2.886   3.834 

#Medium summary 
summary(Medium$Total_leaf_area_cm2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6240  0.6787  0.7135  0.8073  0.9163  1.1760  

#Low summary 
summary(Low$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.02200 0.09325 0.17200 0.21880 0.22000 0.88000  

#Root length 

#High summary 
summary(High_all$Root_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.500   3.850   4.750   4.960   5.575   8.500 
sd(High_all$Root_length_cm) # 1.50906

#Medium summary 
summary(Medium_all$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.300   3.312   4.225   4.082   4.725   5.800 
sd(Medium_all$Root_length_cm) # 1.016997

#Low summary 
summary(Low_all$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.950   1.800   2.150   2.090   2.562   3.100  
sd(Low_all$Root_length_cm) # 0.5875014

#Stem diameter

#High summary 
summary(High_all$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4200  0.5775  0.6100  0.6550  0.7700  0.9200  
sd(High_all$Stem_diameter_mm) #0.1328434

#Medium summary 
summary(Medium_all$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4200  0.5275  0.5900  0.5730  0.6400  0.6900  
sd(Medium_all$Stem_diameter_mm) #0.0734202

#Low summary 
summary(Low_all$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2900  0.3975  0.4200  0.4440  0.4950  0.7200 
sd(Low_all$Stem_diameter_mm) #0.1018461

#Sink to source

#High summary
summary(High_all$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2500  0.5500  0.6000  0.6575  0.9000  1.0500 
sd(High_all$Sink_to_source_cm) #0.2249415

#Medium summary 
summary(Medium_all$Sink_to_source_cm)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.4000  0.7875  0.8500  0.8575  0.9125  1.2000 
sd(Medium_all$Sink_to_source_cm) #0.1822917

#Low summary 
summary(Low_all$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.750   0.975   1.125   1.132   1.255   1.500  
sd(Low_all$Sink_to_source_cm) #0.2338601

#Height:diameter

#High summary
summary(High_all$H.D)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.704  12.058  14.730  14.635  17.552  25.000
sd(High_all$H.D) #4.713531

#Medium summary 
summary(Medium_all$H.D)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 12.31   15.29   18.06   18.23   20.08   26.92 
sd(Medium_all$H.D) #3.806536

#Low summary 
summary(Low_all$H.D)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 20.16   22.43   24.00   28.54   31.14   50.00
sd(Low_all$H.D) #8.785062


#PLOTS
#==================================================================================================
#Order the plot
Thesis_data_Sept8$Light_treatment <- factor (Thesis_data_Sept8$Light_treatment, levels = c("High", "Medium", "Low"))

#Stem length
jpeg("Stem_length2", width = 500, height = 500)
Stem_length2 <- ggplot(Week_2, aes(x = Light_treatment, y = Stem_length_cm, 
                                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(1.63, 1.63)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(1.8, 1.8)) +
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(2, 2)) 
Stem_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Leaf area
jpeg("Leaf_area2", width = 500, height = 500)
Leaf_area2 <- ggplot(Thesis_data_Sept8, aes(x = Light_treatment, y = Total_leaf_area_cm2, 
                                              fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Leaf area (cm2)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(4.35, 4.35)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(2.5, 2.5)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(4, 4))
Leaf_area2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Root length
jpeg("Root_length2", width = 500, height = 500)
Root_length2 <- ggplot(Week_2, aes(x = Light_treatment, y = Root_length_cm, 
                                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(8.5, 8.5)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(7.0, 7.0)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(7.5, 7.5))
Root_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Stem diameter
jpeg("Stem_diameter2", width = 500, height = 500)
Stem_diameter2 <- ggplot(Week_2, aes(x = Light_treatment, y = Stem_diameter_mm, 
                                                  fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem diameter (mm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE,
              y_position = c(1, 1)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE,
              y_position = c(0.93, 0.93)) + 
  geom_signif(comparisons = list(c("Low","Medium")), map_signif_level = TRUE,
              y_position = c(0.8, 0.8))
Stem_diameter2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Source to sink
jpeg("Sink_source2", width = 500, height = 500)
Sink_source2 <- ggplot(Week_2, aes(x = Light_treatment, y = Sink_to_source_cm, 
                                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Distance from sink to source (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(1.65, 1.65)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(1.52, 1.52)) + 
  geom_signif(comparisons = list(c("High", "Medim")), map_signif_level = TRUE, 
              y_position = c(1.3, 1.3))
Sink_source2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

jpeg("HD", width = 500, height = 500)
HD2 <- ggplot(Week_2, aes(x = Light_treatment, y = H.D, 
                                       fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Height:Diameter") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(50.5, 50.5)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(55, 55)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(22, 22))
HD2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Correlation plots
#Stem length 
ggscatter(Week_2, x = "Stem_length_cm", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Root length (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
#Not that interesting 

ggscatter(Week_2, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)") 
#Not that interesting 

ggscatter(Week_2, x = "Stem_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)") + 
  geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
#Not that interesting 

ggscatter(Week_2, x = "Stem_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Sink to source") + 
  geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
#R=0.81,p < 0.0001

#Total leaf area 
ggscatter(Thesis_data_Sept8, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)")
#Interesting, r = 0.63, p < 0.001


ggscatter(Thesis_data_Sept8, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)")
#Interesting R = 0.7, p < 0.0001

ggscatter(Thesis_data_Sept8, x = "Total_leaf_area_cm2", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Leaf area (cm2)", ylab = "Sink to source (cm)")
#Interesting? R = -0.38, p = 0.04

#Root length 
ggscatter(Cohort_2, x = "Root_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Stem diameter (mm)") + 
  geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
#Interesting, r = 0.59, p < 0.0001


ggscatter(Cohort_2, x = "Root_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Sink to source (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
#Interesting, R = -0.35, p < 0.01

#Stem diameter
ggscatter(Week_2, x = "Stem_diameter_mm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "stem diameter (mm)", ylab = "Sink to source (cm)") + 
  geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
#Interesting, -0.41, p < 0.01

#Light intensity 

  #Stem diameter 
  ggscatter(Week_2, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson", 
            xlab = "stem diameter (mm)", ylab = "Light level") + 
    geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
  #Interesting, 0.68, p < 0.0001
  
  #Stem length 
  ggscatter(Week_2, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson", 
            xlab = "Stem length (cm)", ylab = "Light level") + 
    geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
  #Interesting, -0.29, p < 0.05
  
  #Sink to source
  ggscatter(Week_2, x = "Sink_to_source_cm", y = "Light_level..umol.m2.s.", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson", 
            xlab = "Stem length (cm)", ylab = "Light level") + 
    geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
  #Interesting, -0.59, p < 0.0001
  
  #Root length
  ggscatter(Cohort_2, x = "Root_length_cm", y = "Light_level..umol.m2.s.", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson", 
            xlab = "Stem length (cm)", ylab = "Light level") + 
    geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
  #Interesting, -0.72, p < 0.0001
  
  #Height:Diameter
  ggscatter(Week_2, x = "H.D", y = "Light_level..umol.m2.s.", 
            add = "reg.line", conf.int = TRUE, 
            cor.coef = TRUE, cor.method = "pearson", 
            xlab = "Stem length (cm)", ylab = "Light level") + 
    geom_point(aes(shape = Light_treatment, color = Cohort), size = 2.5, stroke = 2)
  #Interesting, -0.59, p < 0.0001
  
  
#STATISTICAL ANALYSES
#==================================================================================================

#ALL WEEK 2 DATA
#Stem length
#ANOVA
stem.aov <- aov(Stem_length_cm ~ Light_treatment, data = Week_2)
summary(stem.aov)
                #Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2  0.814  0.4070   6.291 0.0034 **
#Residuals       57  3.688  0.0647  

#Tukey multiple pairwise-comparisons
TukeyHSD(stem.aov)
#         diff        lwr       upr     p adj
#Medium-High 0.0925 -0.101059461 0.2860595 0.4877304
#Low-High    0.2800  0.086440539 0.4735595 0.0027366
#Low-Medium  0.1875 -0.006059461 0.3810595 0.0595212

#Leaf area
#ANOVA
leaf.aov <- aov(Total_leaf_area_cm2 ~ Light_treatment, data = Thesis_data_Sept8)
summary(leaf.aov)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  16.58   8.292   16.98 1.68e-05 ***
#Residuals       27  13.18   0.488                     

#Tukey multiple pairwise-comparisons
TukeyHSD(leaf.aov)
#diff       lwr        upr     p adj
#Medium-High -1.1983 -1.973138 -0.4234615 0.0019178
#Low-High    -1.7868 -2.561638 -1.0119615 0.0000130
#Low-Medium  -0.5885 -1.363338  0.1863385 0.1629310

#Root length
#ANOVA
root.aov <- aov(Root_length_cm ~ Light_treatment, data = Week_2)
summary(root.aov)
                #Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  86.51   43.26   35.49 9.76e-11 ***
#Residuals       57  69.48    1.22             

#Tukey multiple pairwise-comparisons
TukeyHSD(root.aov)
#diff       lwr        upr     p adj
#Medium-High -0.8775 -1.717647 -0.03735283 0.0387510
#Low-High    -2.8700 -3.710147 -2.02985283 0.0000000
#Low-Medium  -1.9925 -2.832647 -1.15235283 0.0000013

#Stem diameter
#ANOVA
diameter.aov <- aov(Stem_diameter_mm ~ Light_treatment, data = Week_2)
summary(diameter.aov)
                #Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2 0.4526 0.22629   20.32 2.18e-07 ***
#Residuals       57 0.6348 0.01114  

#Tukey multiple pairwise-comparisons
TukeyHSD(diameter.aov)
#          diff        lwr         upr     p adj
#Medium-High -0.082 -0.1623068 -0.001693236 0.0443544
#Low-High    -0.211 -0.2913068 -0.130693236 0.0000001
#Low-Medium  -0.129 -0.2093068 -0.048693236 0.0008258

#Sink to source
#ANOVA
sinktosource.aov <- aov(Sink_to_source_cm ~ Light_treatment, data = Week_2)
summary(sinktosource.aov)
                #Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  2.270  1.1350   24.58 2.01e-08 ***
#Residuals       57  2.632  0.0462    

#Tukey multiple pairwise-comparisons
TukeyHSD(sinktosource.aov)
#        diff         lwr       upr     p adj
#Medium-High 0.2000 0.03648188 0.3635181 0.0128308
#Low-High    0.4745 0.31098188 0.6380181 0.0000000
#Low-Medium  0.2745 0.11098188 0.4380181 0.0004694

#Height:Diameter
#ANOVA
HD.aov <- aov(H.D ~ Light_treatment, data = Week_2)
summary(HD.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2   2084    1042   27.45 4.48e-09 ***
#Residuals       57   2164      38    

#Tukey multiple pairwise-comparisons
TukeyHSD(HD.aov)
#        diff         lwr       upr     p adj
#Medium-High  3.593992 -1.094602  8.282585 0.164470
#Low-High    13.904679  9.216085 18.593273 0.000000
#Low-Medium  10.310687  5.622094 14.999281 0.000006


#COHORT 2 DATA
#Stem length
#ANOVA
stem2.aov <- aov(Stem_length_cm ~ Light_treatment, data = Cohort_2)
summary(stem2.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2 0.2982 0.14908   2.306  0.119
#Residuals       27 1.7455 0.06465    

#Tukey multiple pairwise-comparisons
TukeyHSD(stem2.aov)
#         diff        lwr       upr     p adj
#Medium-High 0.175 -0.10693094 0.4569309 0.2891375
#Low-High    0.235 -0.04693094 0.5169309 0.1159161
#Low-Medium  0.060 -0.22193094 0.3419309 0.8585194

#Leaf area
#ANOVA
leaf.aov <- aov(Total_leaf_area_cm2 ~ Light_treatment, data = Thesis_data_Sept8)
summary(leaf.aov)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  16.58   8.292   16.98 1.68e-05 ***
#Residuals       27  13.18   0.488                     

#Tukey multiple pairwise-comparisons
TukeyHSD(leaf.aov)
#diff       lwr        upr     p adj
#Medium-High -1.1983 -1.973138 -0.4234615 0.0019178
#Low-High    -1.7868 -2.561638 -1.0119615 0.0000130
#Low-Medium  -0.5885 -1.363338  0.1863385 0.1629310

#Root length
#ANOVA
root2.aov <- aov(Root_length_cm ~ Light_treatment, data = Cohort_2)
summary(root2.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  33.49  16.746    14.3 5.83e-05 ***
#Residuals       27  31.63   1.171            

#Tukey multiple pairwise-comparisons
TukeyHSD(root2.aov)
              #diff       lwr        upr     p adj
#Medium-High -0.865 -2.065094  0.3350939 0.1929190
#Low-High    -2.545 -3.745094 -1.3449061 0.0000442
#Low-Medium  -1.680 -2.880094 -0.4799061 0.0048487

#Stem diameter
#ANOVA
diameter2.aov <- aov(Stem_diameter_mm ~ Light_treatment, data = Cohort_2)
summary(diameter2.aov)
                #Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2 0.2974 0.14872    22.4 1.85e-06 ***
#Residuals       27 0.1793 0.00664  

#Tukey multiple pairwise-comparisons
TukeyHSD(diameter2.aov)
#          diff        lwr         upr     p adj
#Medium-High -0.088 -0.1783568  0.002356801 0.0573985
#Low-High    -0.241 -0.3313568 -0.150643199 0.0000013
#Low-Medium  -0.153 -0.2433568 -0.062643199 0.0007414


#Sink to source
#ANOVA
sinktosource2.aov <- aov(Sink_to_source_cm ~ Light_treatment, data = Cohort_2)
summary(sinktosource2.aov)
                #Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  1.490  0.7451   18.85 7.52e-06 ***
#Residuals       27  1.067  0.0395        

#Tukey multiple pairwise-comparisons
TukeyHSD(sinktosource2.aov)
#        diff         lwr       upr     p adj
#Medium-High 0.300 0.07954706 0.5204529 0.0061771
#Low-High    0.545 0.32454706 0.7654529 0.0000044
#Low-Medium  0.245 0.02454706 0.4654529 0.0271177

#Height:Diameter
#ANOVA
HD2.aov <- aov(H.D ~ Light_treatment, data = Cohort_2)
summary(HD2.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  901.5   450.7   17.24 1.5e-05 ***
#Residuals       27  705.8    26.1    

#Tukey multiple pairwise-comparisons
TukeyHSD(HD2.aov)
#        diff         lwr       upr     p adj
#Medium-High  5.325377 -0.3439221 10.99468 0.0686878
#Low-High    13.337613  7.6683133 19.00691 0.0000096
#Low-Medium   8.012235  2.3429361 13.68153 0.0044605


#CREATING RATIOS
#==================================================================================================

Thesis_data_Sept8$Root_Leaf <- Total_leaf_area_cm2/Root_length_cm

Root_leaf2 <- ggplot(Thesis_data_Sept8, aes(x = Light_treatment, y = Root_Leaf, 
                                              fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root leaf ratio") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) 
Root_leaf2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))  


Thesis_data_Sept8$Root_Shoot <- Root_length_cm/Stem_length_cm

Root_shoot2 <- ggplot(Thesis_data_Sept8, aes(x = Light_treatment, y = Root_Shoot, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root:shoot") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(9, 9)) + 
  geom_signif(comparisons = list(c("Medium","Low")), map_signif_level = TRUE, 
              y_position = c(8, 8)) 
Root_shoot2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))  


#Comparing the Cohorts
High_1 <- Thesis_data_Sept8 %>%
  filter(Light_treatment == "High" & Cohort == "1" & Age..J.H. == "14") 

Medium_1 <- Thesis_data_Sept8 %>%
  filter(Light_treatment == "Medium" & Cohort == "1" & Age..J.H. == "14") 

Low_1 <- Thesis_data_Sept8 %>%
  filter(Light_treatment == "Low" & Cohort == "1" & Age..J.H. == "14") 

t.test(High_1$Stem_length_cm, High_2$Stem_length_cm)

t.test(Medium_1$Stem_length_cm, Medium_2$Stem_length_cm)

t.test(Low_1$Stem_length_cm, Low_2$Stem_length_cm)

#Stem diameter

t.test(High_1$Stem_diameter_mm, High_2$Stem_diameter_mm)

t.test(Medium_1$Stem_diameter_mm, Medium_2$Stem_diameter_mm)

t.test(Low_1$Stem_length_cm, Low_2$Stem_length_cm)

#Sink to source

t.test(High_1$Sink_to_source_cm, High_2$Sink_to_source_cm)
#Statistically significantly different

t.test(Medium_1$Sink_to_source_cm, Medium_2$Sink_to_source_cm)

t.test(Low_1$Sink_to_source_cm, Low_2$Sink_to_source_cm)

#Root length

t.test(High_1$Root_length_cm, High_2$Root_length_cm)

t.test(Medium_1$Root_length_cm, Medium_2$Root_length_cm)

t.test(Low_1$Root_length_cm, Low_2$Root_length_cm)

#Height:Diameter
t.test(High_1$H.D, High_2$H.D)

t.test(Medium_1$H.D, Medium_2$H.D)

t.test(Low_1$H.D, Low_2$H.D)

###############################################################################
#ALLOMETRY 

#Stem length and root length 
ggplot(Thesis_data_Sept8, aes(x=log10(Stem_length_cm), y=log10(Root_length_cm), color=Light_treatment))  + 
  geom_point(size=1, color="grey80") + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_length_cm[Thesis_data_Sept8$Light_treatment == "High"]), y=log10(Thesis_data_Sept8$Root_length_cm[Thesis_data_Sept8$Light_treatment == "High"]), color="black", size=2, alpha=0.6) + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_length_cm[Thesis_data_Sept8$Light_treatment == "Medium"]), y=log10(Thesis_data_Sept8$Root_length_cm[Thesis_data_Sept8$Light_treatment == "Medium"]), color="white", size=1) + 
  geom_smooth(method="lm", se=F) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank()) + scale_x_continuous(name = "Stem Length)", labels = math_format(10^.x)) + 
  scale_y_continuous(name = "Root Length", labels = math_format(10^.x)) + 
  annotation_logticks(sides="lb") + theme(panel.grid.major=element_blank(), panel.border=element_blank())+ theme(axis.line = element_line())

#Stem length and stem diameter
ggplot(Thesis_data_Sept8, aes(x=log10(Stem_length_cm), y=log10(Stem_diameter_mm), color=Light_treatment))  + 
  geom_point(size=1, color="grey80") + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_length_cm[Thesis_data_Sept8$Light_treatment == "High"]), y=log10(Thesis_data_Sept8$Stem_diameter_mm[Thesis_data_Sept8$Light_treatment == "High"]), color="black", size=2, alpha=0.6) + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_length_cm[Thesis_data_Sept8$Light_treatment == "Medium"]), y=log10(Thesis_data_Sept8$Stem_diameter_mm[Thesis_data_Sept8$Light_treatment == "Medium"]), color="white", size=1) + 
  geom_smooth(method="lm", se=F) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank()) + scale_x_continuous(name = "Stem Length)", labels = math_format(10^.x)) + 
  scale_y_continuous(name = "Stem diameter", labels = math_format(10^.x)) + 
  annotation_logticks(sides="lb") + theme(panel.grid.major=element_blank(), panel.border=element_blank())+ theme(axis.line = element_line())

#Stem length and leaf area 
ggplot(Thesis_data_Sept8, aes(x=log10(Stem_length_cm), y=log10(Total_leaf_area_cm2), color=Light_treatment))  + 
  geom_point(size=1, color="grey80") + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_length_cm[Thesis_data_Sept8$Light_treatment == "High"]), y=log10(Thesis_data_Sept8$Total_leaf_area_cm2[Thesis_data_Sept8$Light_treatment == "High"]), color="black", size=2, alpha=0.6) + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_length_cm[Thesis_data_Sept8$Light_treatment == "Medium"]), y=log10(Thesis_data_Sept8$Total_leaf_area_cm2[Thesis_data_Sept8$Light_treatment == "Medium"]), color="white", size=1) + 
  geom_smooth(method="lm", se=F) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank()) + scale_x_continuous(name = "Stem Length", labels = math_format(10^.x)) + 
  scale_y_continuous(name = "Total leaf area (cm2)", labels = math_format(10^.x)) + 
  annotation_logticks(sides="lb") + theme(panel.grid.major=element_blank(), panel.border=element_blank())+ theme(axis.line = element_line())

#Stem length and leaf number 
ggplot(Thesis_data_Sept8, aes(x=log10(Stem_length_cm), y=log10(Leaf_number), color=Light_treatment))  + 
  geom_point(size=1, color="grey80") + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_length_cm[Thesis_data_Sept8$Light_treatment == "High"]), y=log10(Thesis_data_Sept8$Leaf_number[Thesis_data_Sept8$Light_treatment == "High"]), color="black", size=2, alpha=0.6) + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_length_cm[Thesis_data_Sept8$Light_treatment == "Medium"]), y=log10(Thesis_data_Sept8$Leaf_number[Thesis_data_Sept8$Light_treatment == "Medium"]), color="white", size=1) + 
  geom_smooth(method="lm", se=F) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank()) + scale_x_continuous(name = "Stem Length", labels = math_format(10^.x)) + 
  scale_y_continuous(name = "Leaf number", labels = math_format(10^.x)) + 
  annotation_logticks(sides="lb") + theme(panel.grid.major=element_blank(), panel.border=element_blank())+ theme(axis.line = element_line())

#Root length and stem diameter
ggplot(Thesis_data_Sept8, aes(x=log10(Root_length_cm), y=log10(Stem_diameter_mm), color=Light_treatment))  + 
  geom_point(size=1, color="grey80") + 
  annotate("point", x=log10(Thesis_data_Sept8$Root_length_cm[Thesis_data_Sept8$Light_treatment == "High"]), y=log10(Thesis_data_Sept8$Stem_diameter_mm[Thesis_data_Sept8$Light_treatment == "High"]), color="black", size=2, alpha=0.6) + 
  annotate("point", x=log10(Thesis_data_Sept8$Root_length_cm[Thesis_data_Sept8$Light_treatment == "Medium"]), y=log10(Thesis_data_Sept8$Stem_diameter_mm[Thesis_data_Sept8$Light_treatment == "Medium"]), color="white", size=1) + 
  geom_smooth(method="lm", se=F) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank()) + scale_x_continuous(name = "Root Length)", labels = math_format(10^.x)) + 
  scale_y_continuous(name = "Stem diameter", labels = math_format(10^.x)) + 
  annotation_logticks(sides="lb") + theme(panel.grid.major=element_blank(), panel.border=element_blank())+ theme(axis.line = element_line())

#Root length and total leaf area 
ggplot(Thesis_data_Sept8, aes(x=log10(Root_length_cm), y=log10(Total_leaf_area_cm2), color=Light_treatment))  + 
  geom_point(size=1, color="grey80") + 
  annotate("point", x=log10(Thesis_data_Sept8$Root_length_cm[Thesis_data_Sept8$Light_treatment == "High"]), y=log10(Thesis_data_Sept8$Total_leaf_area_cm2[Thesis_data_Sept8$Light_treatment == "High"]), color="black", size=2, alpha=0.6) + 
  annotate("point", x=log10(Thesis_data_Sept8$Root_length_cm[Thesis_data_Sept8$Light_treatment == "Medium"]), y=log10(Thesis_data_Sept8$Total_leaf_area_cm2[Thesis_data_Sept8$Light_treatment == "Medium"]), color="white", size=1) + 
  geom_smooth(method="lm", se=F) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank()) + scale_x_continuous(name = "Root Length)", labels = math_format(10^.x)) + 
  scale_y_continuous(name = "Total_leaf_area_cm2", labels = math_format(10^.x)) + 
  annotation_logticks(sides="lb") + theme(panel.grid.major=element_blank(), panel.border=element_blank())+ theme(axis.line = element_line())

#Stem diameter and total leaf area 
ggplot(Thesis_data_Sept8, aes(x=log10(Stem_diameter_mm), y=log10(Total_leaf_area_cm2), color=Light_treatment))  + 
  geom_point(size=1, color="grey80") + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_diameter_mm[Thesis_data_Sept8$Light_treatment == "High"]), y=log10(Thesis_data_Sept8$Total_leaf_area_cm2[Thesis_data_Sept8$Light_treatment == "High"]), color="black", size=2, alpha=0.6) + 
  annotate("point", x=log10(Thesis_data_Sept8$Stem_diameter_mm[Thesis_data_Sept8$Light_treatment == "Medium"]), y=log10(Thesis_data_Sept8$Total_leaf_area_cm2[Thesis_data_Sept8$Light_treatment == "Medium"]), color="white", size=1) + 
  geom_smooth(method="lm", se=F) + 
  theme_bw() + 
  theme(panel.grid.minor = element_blank()) + scale_x_continuous(name = "Stem Diameter", labels = math_format(10^.x)) + 
  scale_y_continuous(name = "Total leaf area", labels = math_format(10^.x)) + 
  annotation_logticks(sides="lb") + theme(panel.grid.major=element_blank(), panel.border=element_blank())+ theme(axis.line = element_line())




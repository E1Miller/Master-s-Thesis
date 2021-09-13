#Created by: Elise Miller
#Date started: 06/30/2021
#Date last edited: 06/30/2021
#Description: Growth data analyses for week 2 data

#Attach files
attach(Thesis_data_June_30)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

#Order the plot
Thesis_data_June_30$Light_treatment <- factor (Thesis_data_June_30$Light_treatment, levels = c("High", "Medium", "Low"))


#SUMMARY 
#==================================================================================================
#Filtering the data
High <- Thesis_data_June_30 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_June_30 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_June_30 %>%
  filter(Light_treatment == "Low") 

#Stem_length
group_by(Thesis_data_June_30, Light_treatment) %>%
  summarise(
    mean = mean(Stem_length_cm, na.rm = TRUE), 
    sd = sd(Stem_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean      sd
#1 High            0.955    0.298
#2 Medium          0.965    0.170
#3 Low             1.28     0.282

summary(Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.650   0.825   1.050   1.067   1.200   1.600 
sd(Stem_length_cm)

#High summary 
summary(High$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.650   0.700   0.975   0.955   1.100   1.600

#Medium summary 
summary(Medium$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.700   0.825   0.975   0.965   1.075   1.200 

#Low summary 
summary(Low$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.800   1.163   1.300   1.280   1.538   1.600 

#Total leaf area
group_by(Thesis_data_June_30, Light_treatment) %>%
  summarise(
    mean = mean(Total_leaf_area_cm2, na.rm = TRUE), 
    sd = sd(Total_leaf_area_cm2, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#1 High            2.01     1.17 
#2 Medium          0.807    0.197
#3 Low             0.219    0.244

summary(Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0220  0.2395  0.6920  1.0106  1.1547  3.8340

sd(Total_leaf_area_cm2)

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
group_by(Thesis_data_June_30, Light_treatment) %>%
  summarise(
    mean = mean(Root_length_cm, na.rm = TRUE), 
    sd = sd(Root_length_cm, na.rm = TRUE)
  )
#Light_treatment    mean    sd
#1 High             5.32    1.62 
#2 Medium           4.43    0.770
#3 Low              2.12    0.647

summary(Root_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.200   2.600   4.225   3.958   4.800   8.500

sd(Root_length_cm)

#High summary 
summary(High$Root_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.100   4.450   4.950   5.320   5.725   8.500  

#Medium summary 
summary(Medium$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 2.600   4.213   4.625   4.430   4.775   5.600 

#Low summary 
summary(Low$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.200   1.625   2.150   2.125   2.600   3.100  

#Stem diameter
group_by(Thesis_data_June_30, Light_treatment) %>%
  summarise(
    mean = mean(Stem_diameter_mm, na.rm = TRUE), 
    sd = sd(Stem_diameter_mm, na.rm = TRUE)
  )
# Light_treatment  mean     sd
#1 High            0.648    0.160 
#2 Medium          0.572    0.0653
#3 Low             0.467    0.140 

summary(Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2900  0.4575  0.5850  0.5623  0.6350  0.9200 

sd(Stem_diameter_mm)

#High summary 
summary(High$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.420   0.580   0.630   0.648   0.740   0.920  

#Medium summary 
summary(Medium$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.450   0.545   0.590   0.572   0.600   0.650

#Low summary 
summary(Low$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.2900  0.3675  0.4450  0.4670  0.5550  0.7200 

#Sink to source
group_by(Thesis_data_June_30, Light_treatment) %>%
  summarise(
    mean = mean(Sink_to_source_cm, na.rm = TRUE), 
    sd = sd(Sink_to_source_cm, na.rm = TRUE)
  )

#Light_treatment  mean      sd
#1 High            0.795    0.166
#2 Medium          0.895    0.167
#3 Low             1.20     0.251

summary(Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.600   0.775   0.900   0.963   1.137   1.500 

sd(Sink_to_source_cm)

#High summary
summary(High$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6000  0.6125  0.8750  0.7950  0.9000  1.0500 

#Medium summary 
summary(Medium$Sink_to_source_cm)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.6000  0.7750  0.9000  0.8950  0.9875  1.1500

#Low summary 
summary(Low$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.750   1.113   1.220   1.199   1.375   1.500 

#Height:diameter
group_by(Thesis_data_June_30, Light_treatment) %>%
  summarise(
    mean = mean(H.D, na.rm = TRUE), 
    sd = sd(H.D, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#1 High             15.1  4.03
#2 Medium           16.9  2.64
#3 Low              29.5 11.5 

summary(H.D)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.553  15.119  18.601  20.510  22.222  50.000 

sd(H.D)

#High summary
summary(High$H.D)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.553  12.326  15.091  15.065  17.872  21.591

#Medium summary 
summary(Medium$H.D)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 12.31   15.16   16.94   16.93   18.73   20.34 

#Low summary 
summary(Low$H.D)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 20.16   22.29   23.50   29.54   36.69   50.00 


#PLOTS
#==================================================================================================
#Order the plot
Thesis_data_June_30$Light_treatment <- factor (Thesis_data_June_30$Light_treatment, levels = c("High", "Medium", "Low"))

#Stem length
jpeg("Stem_length2", width = 500, height = 500)
Stem_length2 <- ggplot(Thesis_data_June_30, aes(x = Light_treatment, y = Stem_length_cm, 
                                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(1.63, 1.63)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(1.75, 1.75)) 
Stem_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Leaf area
jpeg("Leaf_area2", width = 500, height = 500)
Leaf_area2 <- ggplot(Thesis_data_June_30, aes(x = Light_treatment, y = Total_leaf_area_cm2, 
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
Root_length2 <- ggplot(Thesis_data_June_30, aes(x = Light_treatment, y = Root_length_cm, 
                                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(8.5, 8.5)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(7.0, 7.0))
Root_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Stem diameter
jpeg("Stem_diameter2", width = 500, height = 500)
Stem_diameter2 <- ggplot(Thesis_data_June_30, aes(x = Light_treatment, y = Stem_diameter_mm, 
                                                  fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem diameter (mm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, y_position = c(1, 1))  
Stem_diameter2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Source to sink
jpeg("Sink_source2", width = 500, height = 500)
Sink_source2 <- ggplot(Thesis_data_June_30, aes(x = Light_treatment, y = Sink_to_source_cm, 
                                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Distance from sink to source (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(1.65, 1.65)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(1.52, 1.52))
Sink_source2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

jpeg("HD", width = 500, height = 500)
HD2 <- ggplot(Thesis_data_June_30, aes(x = Light_treatment, y = H.D, 
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
ggscatter(Thesis_data_June_30, x = "Stem_length_cm", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Root length (cm)")
#Not that interesting 

ggscatter(Thesis_data_June_30, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Not that interesting 

ggscatter(Thesis_data_June_30, x = "Stem_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Not that interesting 

ggscatter(Thesis_data_June_30, x = "Stem_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#R=0.91,p < 0.0001

#Total leaf area 
ggscatter(Thesis_data_June_30, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)")
#Interesting, r = 0.63, p < 0.001


ggscatter(Thesis_data_June_30, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)")
#Interesting R = 0.7, p < 0.0001

ggscatter(Thesis_data_June_30, x = "Total_leaf_area_cm2", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Leaf area (cm2)", ylab = "Sink to source (cm)")
#Interesting? R = -0.38, p = 0.04

#Root length 
ggscatter(Thesis_data_June_30, x = "Root_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Stem diameter (mm)")
#Interesting, r = 0.56, p = 0.0013


ggscatter(Thesis_data_June_30, x = "Root_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Sink to source (cm)")
#Not interesting 

#Stem diameter
ggscatter(Thesis_data_June_30, x = "Stem_diameter_mm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "stem diameter (mm)", ylab = "Sink to source (cm)")
#Not interesting 


#STATISTICAL ANALYSES
#==================================================================================================

#Stem length
#ANOVA
stem.aov <- aov(Stem_length_cm ~ Light_treatment, data = Thesis_data_June_30)
summary(stem.aov)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2 0.6832  0.3416     5.2 0.0123 *
#Residuals       27 1.7735  0.0657  

#Tukey multiple pairwise-comparisons
TukeyHSD(stem.aov)
#         diff        lwr       upr     p adj
#Medium-High 0.010 -0.2741832 0.2941832 0.9958128
#Low-High    0.325  0.0408168 0.6091832 0.0225676
#Low-Medium  0.315  0.0308168 0.5991832 0.0275666

#Leaf area
#ANOVA
leaf.aov <- aov(Total_leaf_area_cm2 ~ Light_treatment, data = Thesis_data_June_30)
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
root.aov <- aov(Root_length_cm ~ Light_treatment, data = Thesis_data_June_30)
summary(root.aov)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  54.38  27.189   22.37 1.87e-06 ***
#Residuals       27  32.82   1.215              

#Tukey multiple pairwise-comparisons
TukeyHSD(root.aov)
#diff       lwr        upr     p adj
#Medium-High -0.890 -2.112477  0.3324765 0.1870053
#Low-High    -3.195 -4.417477 -1.9725235 0.0000018
#Low-Medium  -2.305 -3.527477 -1.0825235 0.0002094

#Stem diameter
#ANOVA
diameter.aov <- aov(Stem_diameter_mm ~ Light_treatment, data = Thesis_data_June_30)
summary(diameter.aov)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2 0.1652 0.08260   5.024  0.014 *
#Residuals       27 0.4439 0.01644     

#Tukey multiple pairwise-comparisons
TukeyHSD(diameter.aov)
#          diff        lwr         upr     p adj
#Medium-High -0.076 -0.2181805  0.06618051 0.3937793
#Low-High    -0.181 -0.3231805 -0.03881949 0.0105479
#Low-Medium  -0.105 -0.2471805  0.03718051 0.1787160

#Sink to source
#ANOVA
sinktosource.aov <- aov(Sink_to_source_cm ~ Light_treatment, data = Thesis_data_June_30)
summary(sinktosource.aov)
#Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2 0.8854  0.4427   11.19 0.000289 ***
#Residuals       27 1.0686  0.0396  

#Tukey multiple pairwise-comparisons
TukeyHSD(sinktosource.aov)
#        diff         lwr       upr     p adj
#Medium-High 0.100 -0.12059129 0.3205913 0.5079505
#Low-High    0.404  0.18340871 0.6245913 0.0002993
#Low-Medium  0.304  0.08340871 0.5245913 0.0055512

#Height:Diameter
#ANOVA
HD.aov <- aov(H.D ~ Light_treatment, data = Thesis_data_June_30)
summary(HD.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2   1240   619.8   11.95 0.000192 ***
#Residuals       27   1400    51.9   

#Tukey multiple pairwise-comparisons
TukeyHSD(HD.aov)
#        diff         lwr       upr     p adj
#Medium-High  1.862607 -6.123399  9.848612 0.8327781
#Low-High    14.471745  6.485740 22.457751 0.0003399
#Low-Medium  12.609139  4.623133 20.595144 0.0015574

#CREATING RATIOS
#==================================================================================================

Thesis_data_June_30$Root_Leaf <- Total_leaf_area_cm2/Root_length_cm

Root_leaf2 <- ggplot(Thesis_data_June_30, aes(x = Light_treatment, y = Root_Leaf, 
                                              fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root leaf ratio") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) 
Root_leaf2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))  


Thesis_data_June_30$Root_Shoot <- Root_length_cm/Stem_length_cm

Root_shoot2 <- ggplot(Thesis_data_June_30, aes(x = Light_treatment, y = Root_Shoot, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root:shoot") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(9, 9)) + 
  geom_signif(comparisons = list(c("Medium","Low")), map_signif_level = TRUE, 
              y_position = c(8, 8)) 
Root_shoot2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))  


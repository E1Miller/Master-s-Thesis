#Created by: Elise Miller
#Date started: 08/03/2021
#Date last edited: 08/03/2021
#Description: Growth data analyses for week 6 data

#Attach files
attach(Thesis_data_Aug3)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

#Order the plot 
Thesis_data_Aug3$Light_treatment <- factor (Thesis_data_Aug3$Light_treatment, levels = c("High", "Medium", "Low"))


#SUMMARY FOR WEEK 2, 4, AND 6
#==================================================================================================
#Filtering the data
High <- Thesis_data_Aug3 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_Aug3 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_Aug3 %>%
  filter(Light_treatment == "Low") 

#Stem_length
group_by(Thesis_data_Aug3, Light_treatment) %>%
  summarise(
    mean = mean(Stem_length_cm, na.rm = TRUE), 
    sd = sd(Stem_length_cm, na.rm = TRUE)
  )
#Light_treatment     mean    sd

#1 High              6.83  4.95
#2 Medium            3.73  3.35
#3 Low               2.26  1.06


summary(Stem_length_cm, is.na = FALSE)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#0.650   1.200   2.550   4.272   6.350  15.000     

sd(Stem_length_cm)

#High summary 
summary(High$Stem_length_cm)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 0.650   1.100   6.950    6.827    10.637  15.000 

#Medium summary 
summary(Medium$Stem_length_cm)
#Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
#0.700   1.125   2.500   3.730   5.575  13.300 

#Low summary 
summary(Low$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
#0.800   1.413   2.100   2.260   3.075   4.900   

#Total leaf area 
group_by(Thesis_data_Aug3, Light_treatment) %>%
  summarise(
    mean = mean(Total_leaf_area_cm2, na.rm = TRUE), 
    sd = sd(Total_leaf_area_cm2, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#1 High             71.1    65.6 
#2 Medium           39.5    53.8 
#3 Low              7.40    9.80

summary(Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.022   1.239   8.695  39.324  54.750 213.783     

sd(Total_leaf_area_cm2)

#High summary 
summary(High$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.426   3.024  59.641  71.119 113.394 213.783 

#Medium summary 
summary(Medium$Total_leaf_area_cm2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.624   1.001  14.959  39.457  45.635 200.488  

#Low summary 
summary(Low$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0220  0.2395  4.0370  7.3954  8.9025 36.2100  

#Root length 
group_by(Thesis_data_Aug3, Light_treatment) %>%
  summarise(
    mean = mean(Root_length_cm, na.rm = TRUE), 
    sd = sd(Root_length_cm, na.rm = TRUE)
  )
#Light_treatment    mean    sd
#1 High             14.1   7.36
#2 Medium           12.2   7.00
#3 Low              4.16   2.21

summary(Root_length_cm)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    
# 1.200   4.275   7.250  10.152  16.975  28.000       

sd(Root_length_cm)

#High summary 
summary(High$Root_length_cm)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#3.100   6.225  15.600  14.073  20.975  28.000 

#Medium summary 
summary(Medium$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 2.60    4.80   11.40   12.22   19.75   24.20 

#Low summary 
summary(Low$Root_length_cm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#  1.200   2.288   3.500   4.158   6.175   8.200    

#Stem diameter
group_by(Thesis_data_Aug3, Light_treatment) %>%
  summarise(
    mean = mean(Stem_diameter_mm, na.rm = TRUE), 
    sd = sd(Stem_diameter_mm, na.rm = TRUE)
  )
#  Light_treatment  mean    sd
#1 High              1.60   0.775
#2 Medium            1.24   0.677
#3 Low               0.678  0.203

summary(Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max.   
# 0.290   0.620   0.860   1.172   1.657   2.900     

sd(Stem_diameter_mm)

#High summary 
summary(High$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
# 0.420   0.780   1.750   1.599   2.283   2.900     

#Medium summary 
summary(Medium$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.450   0.610   1.095   1.238   1.605   2.840  

#Low summary 
summary(Low$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.2900  0.5250  0.7150  0.6780  0.8425  1.0300

#Sink to source
group_by(Thesis_data_Aug3, Light_treatment) %>%
  summarise(
    mean = mean(Sink_to_source_cm, na.rm = TRUE), 
    sd = sd(Sink_to_source_cm, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High              1.03  0.388
#2 Medium            1.2   0.384
#3 Low               1.66  0.483

summary(Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max.   
# 0.500   0.900   1.150   1.298   1.675   2.400    

sd(Sink_to_source_cm)

#High summary
summary(High$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.500   0.800   0.975   1.033   1.188   2.050 

#Medium summary 
summary(Medium$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 0.6000  0.9125  1.1000  1.2000  1.4875  2.0500  

#Low summary 
summary(Low$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#  0.750   1.255   1.825   1.661   2.038   2.400 


#SUMMARY FOR WEEK 6
#==================================================================================================
#Filtering the data
High <- Thesis_data_Aug3 %>%
  filter(Light_treatment == "High" & Age..J.H. == "43") 

Medium <- Thesis_data_Aug3 %>%
  filter(Light_treatment == "Medium" & Age..J.H. == "43") 

Low <- Thesis_data_Aug3 %>%
  filter(Light_treatment == "Low" & Age..J.H. == "43")

#Stem_length
group_by(Thesis_data_Aug3, Light_treatment) %>%
  filter(Age..J.H. == "43")  %>% 
  summarise(
    mean = mean(Stem_length_cm, na.rm = TRUE), 
    sd = sd(Stem_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean    sd

#1 High            12.1  2.66 
#2 Medium          7.46 3.08 
#3 Low             3.5  0.707


#High summary 
summary(High$Stem_length_cm)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 5.80   10.99   12.45   12.09   13.80   15.00 
sd(High$Stem_length_cm)

#Medium summary 
summary(Medium$Stem_length_cm)
#Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
#2.750   5.875   7.000   7.455   9.150  13.300 
sd(Medium$Stem_length_cm)

#Low summary 
summary(Low$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 2.600   3.200   3.300   3.500   3.825   4.900 
sd(Low$Stem_length_cm)

#Total leaf area NEED TO DO 
group_by(Thesis_data_Aug3, Light_treatment) %>%
  filter(Age..J.H. == "43")  %>% 
  summarise(
    mean = mean(Total_leaf_area_cm2, na.rm = TRUE), 
    sd = sd(Total_leaf_area_cm2, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#High               137.   50.8
#2 Medium           94.4  60.5
#3 Low              17.7  10.9

summary(Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0220  0.2395  0.6920  1.0106  1.1547  3.8340

sd(Total_leaf_area_cm2)

#High summary 
summary(High$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 48.45  109.83  150.41  136.72  166.62  213.78 

#Medium summary 
summary(Medium$Total_leaf_area_cm2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 24.40   44.23   93.24   94.42  136.40  200.49  

#Low summary 
summary(Low$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 3.774   9.983  14.588  17.693  27.096  36.210  

#Root length 
group_by(Thesis_data_Aug3, Light_treatment) %>%
  filter(Age..J.H. == "43")  %>% 
  summarise(
    mean = mean(Root_length_cm, na.rm = TRUE), 
    sd = sd(Root_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean    sd
#1 High            17.9   5.85
#2 Medium          17.4   5.31
#3 Low              5.77  1.68


#High summary 
summary(High$Root_length_cm)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#7.70   14.90   17.60   17.88   21.90   28.00    
sd(High$Root_length_cm)

#Medium summary 
summary(Medium$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 7.20   14.31   19.25   17.41   20.88   24.20  
sd(Medium$Root_length_cm)

#Low summary 
summary(Low$Root_length_cm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#  3.100   4.412   6.075   5.770   6.750   8.200
sd(Low$Root_length_cm)

#Stem diameter
group_by(Thesis_data_Aug3, Light_treatment) %>%
  filter(Age..J.H. == "43")  %>% 
  summarise(
    mean = mean(Stem_diameter_mm, na.rm = TRUE), 
    sd = sd(Stem_diameter_mm, na.rm = TRUE)
  )
#  Light_treatment  mean    sd
#1 High            2.23  0.520 
#2 Medium          1.99  0.494 
#3 Low             0.831 0.0835

#High summary 
summary(High$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.     
# 1.290   1.990   2.330   2.231   2.500   2.900 
sd(High$Stem_diameter_mm)

#Medium summary 
summary(Medium$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 1.410   1.595   1.895   1.990   2.388   2.840   
sd(Medium$Stem_diameter_mm)

#Low summary 
summary(Low$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.730   0.760   0.835   0.831   0.860   1.010 
sd(Low$Stem_diameter_mm)

#Sink to source
group_by(Thesis_data_Aug3, Light_treatment) %>%
  filter(Age..J.H. == "43")  %>% 
  summarise(
    mean = mean(Sink_to_source_cm, na.rm = TRUE), 
    sd = sd(Sink_to_source_cm, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High             1.38 0.450
#2 Medium           1.53 0.359
#3 Low              2.02 0.372


#High summary
summary(High$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#  0.600   1.125   1.250   1.375   1.762   2.050 
sd(High$Sink_to_source_cm)

#Medium summary 
summary(Medium$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 1.000   1.262   1.525   1.530   1.775   2.050 
sd(Medium$Sink_to_source_cm)

#Low summary 
summary(Low$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#   1.100   1.925   2.075   2.020   2.263   2.400
sd(Low$Sink_to_source_cm)

#Photosynthesis 
group_by(Thesis_data_Aug3, Light_treatment) %>%
  filter(Age..J.H. == "43")  %>% 
  summarise(
    mean = mean(Photosynthesis, na.rm = TRUE), 
    sd = sd(Photosynthesis, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High            11.3   4.86 
#2 Medium           7.39  2.98 
#3 Low             -0.682 0.213


#High summary
summary(High$Photosynthesis)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 2.30    9.71   13.10   11.26   13.57   18.30 
sd(High$Photosynthesis)

#Medium summary 
summary(Medium$Photosynthesis)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
# 3.60    4.77    7.40    7.39    9.70   11.75
sd(Medium$Photosynthesis)

#Low summary 
summary(Low$Photosynthesis)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
# -0.9270 -0.8200 -0.7420 -0.6818 -0.4600 -0.4600 
sd(Low$Photosynthesis)

#Respiration
group_by(Thesis_data_Aug3, Light_treatment) %>%
  filter(Age..J.H. == "43")  %>% 
  summarise(
    mean = mean(Respiration, na.rm = TRUE), 
    sd = sd(Respiration, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High            -0.968 0.222
#2 Medium          -1.15  0.241
#3 Low             -0.67  0.390


#High summary
summary(High$Respiration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# -1.8733 -1.2121 -0.9816 -1.0392 -0.7966 -0.3164 
sd(High$Respiration)

#Medium summary 
summary(Medium$Respiration)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
# -1.8999 -1.4940 -1.2672 -1.2471 -0.9883 -0.5744 
sd(Medium$Respiration)

#Low summary 
summary(Low$Respiration)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
# -1.5027 -0.8022 -0.6996 -0.7768 -0.6421 -0.4068
sd(Low$Respiration)

#Height : Diameter
group_by(Thesis_data_Aug3, Light_treatment) %>%
  filter(Age..J.H. == "43")  %>% 
  summarise(
    mean = mean(H.D, na.rm = TRUE), 
    sd = sd(H.D, na.rm = TRUE)
  )
#Light_treatment  mean    sd

#1 High             55.1 11.9 
#2 Medium           36.8 10.5 
#3 Low              41.9  5.79


#High summary 
summary(High$H.D)
# Min.   1st Qu.  Median  Mean    3rd Qu.    Max. 
# 43.12   47.84   53.56   55.12   56.67   84.21 
sd(High$H.D)

#Medium summary 
summary(Medium$H.D)
#Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
#17.08   31.08   36.14   36.83   40.68   52.99
sd(Medium$H.D)

#Low summary 
summary(Low$H.D)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 34.67   37.50   40.70   41.92   47.05   50.59  
sd(Low$H.D)

#PLOTS
#==================================================================================================
#Order the plot

Thesis_data_Aug3 <- group_by(Thesis_data_Aug3, Light_treatment) %>%
  filter(Age..J.H. == "43")

#Stem length
jpeg("Stem_length2", width = 500, height = 500)
Stem_length2 <- ggplot(Thesis_data_Aug3, aes(x = Light_treatment, y = Stem_length_cm, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(16, 16)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(15, 15)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(14, 14)) 
Stem_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Leaf area
jpeg("Leaf_area2", width = 500, height = 500)
Leaf_area2 <- ggplot(Thesis_data_Aug3, aes(x = Light_treatment, y = Total_leaf_area_cm2, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Leaf area (cm2)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(225, 225)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(201, 201)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(215, 215))
Leaf_area2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Root length
jpeg("Root_length2", width = 500, height = 500)
Root_length2 <- ggplot(Thesis_data_Aug3, aes(x = Light_treatment, y = Root_length_cm, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(29.5, 29.5)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(28, 28)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(25, 25))
Root_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Stem diameter
jpeg("Stem_diameter2", width = 500, height = 500)
Stem_diameter2 <- ggplot(Thesis_data_Aug3, aes(x = Light_treatment, y = Stem_diameter_mm, 
                                                 fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem diameter (mm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(3.2, 3.2)) +  
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(2.94, 2.94)) + 
  geom_signif(comparisons = list(c("Medium","Low")), map_signif_level = TRUE, 
              y_position = c(2.85, 2.85))  
Stem_diameter2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Source to sink
jpeg("Sink_source2", width = 500, height = 500)
Sink_source2 <- ggplot(Thesis_data_Aug3, aes(x = Light_treatment, y = Sink_to_source_cm, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Distance from sink to source (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(2.50, 2.50)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(2.4, 2.4)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(2.1, 2.1))
Sink_source2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Stem length
jpeg("HD2", width = 500, height = 500)
HD2 <- ggplot(Thesis_data_Aug3, aes(x = Light_treatment, y = H.D, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Height : Diameter") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(88, 88)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(84, 84)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(60, 60)) 
HD2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Correlation plots
#Stem length 
ggscatter(Thesis_data_Aug3, x = "Stem_length_cm", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Root length (cm)")

#Interesting, R = 0.7, p < 0.0001

ggscatter(Thesis_data_Aug3, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Interesting, R = 0.87, p < 0.0001

ggscatter(Thesis_data_Aug3, x = "Stem_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Interesting, R = 0.86, p < 0.0001

ggscatter(Thesis_data_Aug3, x = "Stem_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Interesting, R = -0.39, p = 0.033

ggscatter(Thesis_data_Aug3, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Want to figure out to scale the colors to make it so that the color matches with the light
#R = 0.76, p < 0.0001

ggscatter(High, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Not interesting 
ggscatter(Medium, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Not interesting
ggscatter(Low, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = -0.76, p = 0.011



#Total leaf area 
ggscatter(Thesis_data_Aug3, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)")
#Interesting, r = 0.65, p < 0.0001


ggscatter(Thesis_data_Aug3, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)")
#Interesting R = 0.8, p < 0.0001

ggscatter(Thesis_data_Aug3, x = "Total_leaf_area_cm2", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Leaf area (cm2)", ylab = "Sink to source (cm)")
#Interesting, R = -0.41, p = 0.0026

ggscatter(Thesis_data_Aug3, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Interesting, R = 0.65, p < 0.0001
ggscatter(High, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting
ggscatter(Medium, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting
ggscatter(Low, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting

#Root length 
ggscatter(Thesis_data_Aug3, x = "Root_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Stem diameter (mm)")
#Interesting, R = 0.77, p < 0.0001


ggscatter(Thesis_data_Aug3, x = "Root_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Sink to source (cm)")
#Interesting, R = -0.44, p = 0.015

ggscatter(Thesis_data_Aug3, x = "Root_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Interesting, R = 0.51, p < 0.01
ggscatter(High, x = "Root_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting 
ggscatter(Medium, x = "Root_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting
ggscatter(Low, x = "Root_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting

#Stem diameter
ggscatter(Thesis_data_Aug3, x = "Stem_diameter_mm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "stem diameter (mm)", ylab = "Sink to source (cm)")
#Interesting, R = -0.38, p = 0.037 

ggscatter(Thesis_data_Aug3, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#R = 0.59, p < 0.001

ggscatter(High, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting 
ggscatter(Medium, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#Close to being interesting (p = 0.071)
ggscatter(Low, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting 

#Photosynthesis
#Light 
ggscatter(Thesis_data_Aug3, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Interesting, R = 0.68, p < 0.001
ggscatter(High, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Not interesting
ggscatter(Medium, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Interesting, R = 0.68, p < 0.05
ggscatter(Low, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Not interesting

#Height
ggscatter(Thesis_data_Aug3, x = "Photosynthesis", y = "Stem_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem length (cm)")
#Interesting, R = 0.66, p < 0.0001

#Diameter 
ggscatter(Thesis_data_Aug3, x = "Photosynthesis", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem diameter (mm)")
#Interesting, R = 0.55, p = 0.0049

#Root length
ggscatter(Thesis_data_Aug3, x = "Photosynthesis", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Root length (cm)")
#Interesting, R = 0.65, p < 0.001

#Respiration 
#Light 
ggscatter(Thesis_data_Aug3, x = "Respiration", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
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
ggscatter(Thesis_data_Aug3, x = "Respiration", y = "Stem_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem length (cm)")
#Interesting, R = -0.34, p < 0.05

#Diameter 
ggscatter(Thesis_data_Aug3, x = "Respiration", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem diameter (mm)")
#Not interesting

#Root length
ggscatter(Thesis_data_Aug3, x = "Respiration", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Root length (cm)")
#Not interesting

#Histograms 
#Stem length 
hist(High$Stem_length_cm)
#Fairly normal
hist(Medium$Stem_length_cm)
#Right-skewed
hist(Low$Stem_length_cm)
#Fairly normal
hist(Thesis_data_Aug3$Stem_length_cm)
#Right-skewed

#Root length
hist(High$Root_length_cm)
#Normal? This plot is weird
hist(Medium$Root_length_cm)
#Normal?
hist(Low$Root_length_cm)
#Kind of right-skewed? 
hist(Thesis_data_Aug3$Root_length_cm)
#Right-skewed

#Stem diameter
hist(High$Stem_diameter_mm)
#Fairly normal
hist(Medium$Stem_diameter_mm)
#Fairly normal 
hist(Low$Stem_diameter_mm)
#Fairly normal 
hist(Thesis_data_Aug3$Stem_diameter_mm)
#Fairly normal

#Sink to source
hist(High$Sink_to_source_cm)
#Fairly normal
hist(Medium$Sink_to_source_cm)
#Fairly normal
hist(Low$Sink_to_source_cm)
#Left skew 
hist(Thesis_data_Aug3$Sink_to_source_cm)
#Fairly normal 

#STATISTICAL ANALYSES
#==================================================================================================

#Stem length
#ANOVA
stem.aov <- aov(Stem_length_cm ~ Light_treatment, data = Thesis_data_Aug3)
summary(stem.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2  369.3  184.64   32.42 6.64e-08 ***
#Residuals       27  153.8    5.69 

#Tukey multiple pairwise-comparisons
TukeyHSD(stem.aov)
#         diff        lwr       upr     p adj
#Medium-High -4.630  -7.276111 -1.983889 0.0005123
#Low-High    -8.585 -11.231111 -5.938889 0.0000000
#Low-Medium  -3.955  -6.601111 -1.308889 0.0026706

#Leaf area
#ANOVA
leaf.aov <- aov(Total_leaf_area_cm2 ~ Light_treatment, data = Thesis_data_Aug3)
summary(leaf.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  72817   36408   17.15 1.56e-05 ***
 # Residuals       27  57332    2123                      

#Tukey multiple pairwise-comparisons
TukeyHSD(leaf.aov)
#diff       lwr        upr     p adj
#Medium-High  -42.3088  -93.40405   8.786449 0.1189761
#Low-High    -119.0319 -170.12715 -67.936651 0.0000112
#Low-Medium   -76.7231 -127.81835 -25.627851 0.0025555

#Root length
#ANOVA
root.aov <- aov(Root_length_cm ~ Light_treatment, data = Thesis_data_Aug3)
summary(root.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  941.6   470.8   21.65 2.45e-06 ***
#Residuals       27  587.1    21.7

#Tukey multiple pairwise-comparisons
TukeyHSD(root.aov)
#diff       lwr        upr     p adj
#Medium-High  -0.465  -5.635453  4.705453 0.9729892
#Low-High    -12.110 -17.280453 -6.939547 0.0000103
#Low-Medium  -11.645 -16.815453 -6.474547 0.0000186

#Stem diameter
#ANOVA
diameter.aov <- aov(Stem_diameter_mm ~ Light_treatment, data = Thesis_data_Aug3)
summary(diameter.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2  11.21   5.602   32.25 6.98e-08 ***
#Residuals       27   4.69   0.174    

#Tukey multiple pairwise-comparisons
TukeyHSD(diameter.aov)
#          diff        lwr         upr     p adj
#Medium-High -0.241 -0.7031344  0.2211344 0.4112158
#Low-High    -1.400 -1.8621344 -0.9378656 0.0000001
#Low-Medium  -1.159 -1.6211344 -0.6968656 0.0000035

#Sink to source
#ANOVA
sinktosource.aov <- aov(Sink_to_source_cm ~ Light_treatment, data = Thesis_data_Aug3)
summary(sinktosource.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  2.267  1.1336   7.239 0.00304 **
#Residuals       27  4.228  0.1566     

#Tukey multiple pairwise-comparisons
TukeyHSD(sinktosource.aov)
#        diff         lwr       upr     p adj
#Medium-High 0.155 -0.28379648 0.5937965 0.6598549
#Low-High    0.645  0.20620352 1.0837965 0.0031236
#Low-Medium  0.490  0.05120352 0.9287965 0.0263106

#Respiration
#ANOVA
resp.aov <- aov(Respiration ~ Light_treatment, data = Thesis_data_Aug3)
summary(resp.aov)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2 0.5309 0.26544   4.157 0.0318 *
#Residuals       19 1.2133 0.06386    

#Tukey multiple pairwise-comparisons
TukeyHSD(resp.aov)
#         diff        lwr       upr     p adj
#Medium-High -0.1785089 -0.47348025 0.1164625 0.2964764
#Low-High     0.2983800 -0.12422570 0.7209857 0.1984536
#Low-Medium   0.4768889  0.04889946 0.9048783 0.0275350

#Photosynthesis
#ANOVA
photo.aov <- aov(Photosynthesis ~ Light_treatment, data = Thesis_data_Aug3)
summary(photo.aov)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2  475.7  237.87   17.63 3.2e-05 ***
#Residuals       21  283.3   13.49   

#Tukey multiple pairwise-comparisons
TukeyHSD(photo.aov)
#         diff        lwr       upr     p adj
#Medium-High  -3.8740  -8.127628  0.3796276 0.0785102
#Low-High    -11.9458 -17.016461 -6.8751394 0.0000196
#Low-Medium   -8.0718 -13.235508 -2.9080919 0.0020756

#Height : Diameter
#ANOVA
HD.aov <- aov(H.D ~ Light_treatment, data = Thesis_data_Aug3)
summary(HD.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2   1834   916.8   4.757 0.0109 *
#Residuals       87  16766   192.7    

#Tukey multiple pairwise-comparisons
TukeyHSD(HD.aov)
#         diff        lwr       upr     p adj
#Medium-High -10.796287 -19.343093 -2.249481 0.0094266
#Low-High     -3.333912 -11.880718  5.212894 0.6228068
#Low-Medium    7.462375  -1.084431 16.009181 0.0995256

#CREATING RATIOS
#==================================================================================================

Thesis_data_Aug3$Root_Leaf <- Total_leaf_area_cm2/Root_length_cm

Root_leaf2 <- ggplot(Thesis_data_Aug3, aes(x = Light_treatment, y = Root_Leaf, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root leaf ratio") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(9, 9)) + 
  geom_signif(comparisons = list(c("Medium","Low")), map_signif_level = TRUE, 
              y_position = c(8, 8)) 
Root_leaf2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))  


Thesis_data_Aug3$Root_Shoot <- Root_length_cm/Stem_length_cm

Root_shoot2 <- ggplot(Thesis_data_Aug3, aes(x = Light_treatment, y = Root_Shoot, 
                                              fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root:shoot") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(9, 9)) + 
  geom_signif(comparisons = list(c("Medium","Low")), map_signif_level = TRUE, 
              y_position = c(8, 8)) 
Root_shoot2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))  







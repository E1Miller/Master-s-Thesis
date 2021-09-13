#Created by: Elise Miller
#Date started: 07/16/2021
#Date last edited: 07/22/2021
#Description: Growth data analyses for week 4 data

#Attach files
attach(Thesis_data_July16)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

#Order the plot
Thesis_data_July16$Light_treatment <- factor (Thesis_data_July16$Light_treatment, levels = c("High", "Medium", "Low"))

Thesis_data_July16 <- group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")

#SUMMARY FOR WEEK 2 AND 4
#==================================================================================================
#Filtering the data
High <- Thesis_data_July16 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_July16 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_July16 %>%
  filter(Light_treatment == "Low") 

#Stem_length
group_by(Thesis_data_July16, Light_treatment) %>%
  summarise(
    mean = mean(Stem_length_cm, na.rm = TRUE), 
    sd = sd(Stem_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean    sd

#1 High           4.20    3.50 
#3 Medium         1.87    1.29 
#2 Low            1.64    0.501


summary(Stem_length_cm, is.na = FALSE)

#Min.   1st Qu.  Median  Mean    3rd Qu.  Max.   
#0.650   1.075   1.600   2.568   2.538  10.300

sd(Stem_length_cm)

#High summary 
summary(High$Stem_length_cm)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 0.650   0.9875   3.70    4.1975   7.075     10.30

#Medium summary 
summary(Medium$Stem_length_cm)
#Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
#0.7000  0.9875  1.2000  1.8675  2.4250  5.2000

#Low summary 
summary(Low$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
#0.800   1.238   1.600   1.640   2.100   2.500  

#Total leaf area NEED TO DO 
group_by(Thesis_data_July16, Light_treatment) %>%
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
group_by(Thesis_data_July16, Light_treatment) %>%
  summarise(
    mean = mean(Root_length_cm, na.rm = TRUE), 
    sd = sd(Root_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean    sd
#1 High            12.2   7.41
#2 Low              3.35  2.01
#3 Medium           9.63  6.33

summary(Root_length_cm)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    
# 1.200   3.100   4.950   8.383  13.662  22.700  

sd(Root_length_cm)

#High summary 
summary(High$Root_length_cm)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#3.100   4.975  11.700  12.170  18.575  22.700  

#Medium summary 
summary(Medium$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 2.600   4.638   6.450   9.627  13.662  20.900

#Low summary 
summary(Low$Root_length_cm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#  1.200   2.175   2.600   3.353   4.037   8.000   

#Stem diameter
group_by(Thesis_data_July16, Light_treatment) %>%
  summarise(
    mean = mean(Stem_diameter_mm, na.rm = TRUE), 
    sd = sd(Stem_diameter_mm, na.rm = TRUE)
  )
#  Light_treatment  mean    sd
#1 High            1.28    0.689
#3 Medium          0.862   0.371
#2 Low             0.602   0.203

summary(Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max.   
# 0.2900  0.5800  0.6850  0.9155  1.0925  2.3400  

sd(Stem_diameter_mm)

#High summary 
summary(High$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
# 0.420   0.640   1.205   1.282   1.837   2.340     

#Medium summary 
summary(Medium$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.4500  0.5900  0.7000  0.8625  1.0925  1.6500  

#Low summary 
summary(Low$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.2900  0.4300  0.6200  0.6015  0.7125  1.0300  

#Sink to source
group_by(Thesis_data_July16, Light_treatment) %>%
  summarise(
    mean = mean(Sink_to_source_cm, na.rm = TRUE), 
    sd = sd(Sink_to_source_cm, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High          0.862   0.204
#3 Medium        1.03    0.279
#2 Low           1.48    0.435

summary(Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max.   
# 0.500   0.850   1.075   1.127   1.300    2.200 

sd(Sink_to_source_cm)

#High summary
summary(High$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.5000  0.6875  0.9000  0.8625  1.0125  1.2000

#Medium summary 
summary(Medium$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 0.6000  0.8875  0.9750  1.0350  1.1125  1.6000 

#Low summary 
summary(Low$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#  0.750   1.188   1.450   1.482   1.900   2.200 


#SUMMARY FOR WEEK 4
#==================================================================================================
#Filtering the data
High <- Thesis_data_July16 %>%
  filter(Light_treatment == "High" & Age..J.H. == "29") 

Medium <- Thesis_data_July16 %>%
  filter(Light_treatment == "Medium" & Age..J.H. == "29") 

Low <- Thesis_data_July16 %>%
  filter(Light_treatment == "Low" & Age..J.H. == "29")

#Stem_length
group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")  %>% 
  summarise(
    mean = mean(Stem_length_cm, na.rm = TRUE), 
    sd = sd(Stem_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean    sd

# 1 High         7.44    1.54 
#3 Medium        2.77    1.30 
#2 Low            2      0.403


#High summary 
summary(High$Stem_length_cm)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 5.800   6.375   7.150   7.440   7.625  10.300
sd(High$Stem_length_cm)

#Medium summary 
summary(Medium$Stem_length_cm)
#Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
#1.200   1.900   2.500   2.770   3.525   5.200
sd(Medium$Stem_length_cm)

#Low summary 
summary(Low$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
#1.100   1.788   2.100   2.000   2.263   2.500
sd(Low$Stem_length_cm)

#Total leaf area  
group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")  %>% 
  summarise(
    mean = mean(Total_leaf_area_cm2, na.rm = TRUE), 
    sd = sd(Total_leaf_area_cm2, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#1 High            74.6  34.3 
#2 Low              4.27  2.10
#3 Medium          23.1  18.5 

summary(Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0220   0.6945   2.7945  17.5134  11.3488 115.3400 

sd(Total_leaf_area_cm2)

#High summary 
summary(High$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 6.00   54.61   80.67   74.63   98.55  115.34 

#Medium summary 
summary(Medium$Total_leaf_area_cm2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 4.622  10.873  14.959  23.147  33.667  63.969 

#Low summary 
summary(Low$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.429   2.222   4.814   4.275   6.048   6.641  

#Root length 
group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")  %>% 
  summarise(
    mean = mean(Root_length_cm, na.rm = TRUE), 
    sd = sd(Root_length_cm, na.rm = TRUE)
  )
#Light_treatment   mean    sd
#1 High            19.0   3.02
#3 Medium          14.8   4.90
#2 Low            4.58    2.18


#High summary 
summary(High$Root_length_cm)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#14.90   16.73   19.05   19.02   21.77   22.70   
sd(High$Root_length_cm)

#Medium summary 
summary(Medium$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 7.30   11.22   13.82   14.82   19.75   20.90 
sd(Medium$Root_length_cm)

#Low summary 
summary(Low$Root_length_cm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#  2.200   2.550   4.175   4.580   6.175   8.000 
sd(Low$Root_length_cm)

#Stem diameter
group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")  %>% 
  summarise(
    mean = mean(Stem_diameter_mm, na.rm = TRUE), 
    sd = sd(Stem_diameter_mm, na.rm = TRUE)
  )
#  Light_treatment  mean    sd
#1 High            1.92    0.285
#3 Medium          1.15    0.314
#2 Low             0.736   0.165

#High summary 
summary(High$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.     
# 1.490   1.730   1.855   1.917   2.155   2.340 
sd(High$Stem_diameter_mm)

#Medium summary 
summary(Medium$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.750   0.930   1.095   1.153   1.373   1.650   
sd(Medium$Stem_diameter_mm)

#Low summary 
summary(Low$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.4400  0.6725  0.7000  0.7360  0.8450  1.0300  
sd(Low$Stem_diameter_mm)

#Sink to source
group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")  %>% 
  summarise(
    mean = mean(Sink_to_source_cm, na.rm = TRUE), 
    sd = sd(Sink_to_source_cm, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High           0.93   0.225
#3 Medium         1.18   0.305
#2 Low            1.76   0.397


#High summary
summary(High$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.500   0.800   0.975   0.930   1.100   1.200 
sd(High$Sink_to_source_cm)

#Medium summary 
summary(Medium$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 0.7000  0.9875  1.1000  1.1750  1.4250  1.6000 
sd(Medium$Sink_to_source_cm)

#Low summary 
summary(Low$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#   0.900   1.650   1.900   1.765   1.988   2.200 
sd(Low$Sink_to_source_cm)

#Photosynthesis 
group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")  %>% 
  summarise(
    mean = mean(Photosynthesis, na.rm = TRUE), 
    sd = sd(Photosynthesis, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High           10.6   3.89
#3 Medium         9.40   1.63
#2 Low            NA      NA


#High summary
summary(High$Photosynthesis)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 3.436   8.544  10.464  10.621  11.890  17.539 
sd(High$Photosynthesis)

#Medium summary 
summary(Medium$Photosynthesis)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
# 7.118   8.784   9.858   9.398  10.471  10.760 
sd(Medium$Photosynthesis)

#Respiration
group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")  %>% 
  summarise(
    mean = mean(Respiration, na.rm = TRUE), 
    sd = sd(Respiration, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High             -0.841  0.222 
#2 Medium           -0.741  0.0877
#3 Low            NA      NA


#High summary
summary(High$Respiration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# -1.8733 -1.2121 -0.9816 -1.0392 -0.7966 -0.3164
sd(High$Photosynthesis)

#Medium summary 
summary(Medium$Respiration)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
# -1.8999 -1.4940 -1.2672 -1.2471 -0.9883 -0.5744 
sd(Medium$Photosynthesis)

#Height:diameter
group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")  %>% 
  summarise(
    mean = mean(H.D, na.rm = TRUE), 
    sd = sd(H.D, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#1 High             39.3  8.23
#2 Medium           23.3  6.44
#3 Low              28.0  7.00

summary(H.D)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 8.553  17.769  22.736  25.353  31.737  52.381  

sd(H.D)

#High summary
summary(High$H.D)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 27.07   33.48   37.00   39.27   45.47   52.38 

#Medium summary 
summary(Medium$H.D)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   11.01   20.36   24.60   23.31   26.60   31.52 

#Low summary 
summary(Low$H.D)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 17.74   23.05   26.81   28.00   32.84   38.64 


#PLOTS
#==================================================================================================
#Order the plot
Thesis_data_July16$Light_treatment <- factor (Thesis_data_July16$Light_treatment, levels = c("High", "Medium", "Low"))

Thesis_data_July16 <- group_by(Thesis_data_July16, Light_treatment) %>%
  filter(Age..J.H. == "29")

#Stem length
jpeg("Stem_length2", width = 500, height = 500)
Stem_length2 <- ggplot(Thesis_data_July16, aes(x = Light_treatment, y = Stem_length_cm, 
                                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(10.63, 10.63)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(8.63, 8.63)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(5.75, 5.75)) 
Stem_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Leaf area
jpeg("Leaf_area2", width = 500, height = 500)
Leaf_area2 <- ggplot(Thesis_data_July16, aes(x = Light_treatment, y = Total_leaf_area_cm2, 
                                           fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Leaf area (cm2)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(127, 127)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(75, 75)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(115, 115))
Leaf_area2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Root length
jpeg("Root_length2", width = 500, height = 500)
Root_length2 <- ggplot(Thesis_data_July16, aes(x = Light_treatment, y = Root_length_cm, 
                                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(25.5, 25.5)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(23.5, 23.5)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(22.0, 22.0))
Root_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Stem diameter
jpeg("Stem_diameter2", width = 500, height = 500)
Stem_diameter2 <- ggplot(Thesis_data_July16, aes(x = Light_treatment, y = Stem_diameter_mm, 
                                                  fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem diameter (mm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(2.5, 2.5)) +  
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(2.35, 2.35)) + 
  geom_signif(comparisons = list(c("Medium","Low")), map_signif_level = TRUE, 
              y_position = c(1.7, 1.7))  
Stem_diameter2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Source to sink
jpeg("Sink_source2", width = 500, height = 500)
Sink_source2 <- ggplot(Thesis_data_July16, aes(x = Light_treatment, y = Sink_to_source_cm, 
                                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Distance from sink to source (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(2.35, 2.35)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(2.25, 2.25)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(1.6, 1.6))
Sink_source2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

jpeg("HD2", width = 500, height = 500)
HD2 <- ggplot(Thesis_data_July16, aes(x = Light_treatment, y = H.D, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Height : Diameter") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(55, 55)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(52, 52)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(41, 41)) 
HD2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Correlation plots
#Stem length 
ggscatter(Thesis_data_July16, x = "Stem_length_cm", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Root length (cm)")

#Interesting, R = 0.67, p < 0.0001

ggscatter(Thesis_data_July16, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Interesting, R = 0.88, p < 0.0001

ggscatter(Thesis_data_July16, x = "Stem_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Stem diameter (mm)")
#Interesting, R = 0.89, p < 0.0001

ggscatter(Thesis_data_July16, x = "Stem_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Sink to source (cm)")
#Interesting, R = -0.42, p = 0.02

ggscatter(Thesis_data_July16, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Want to figure out to scale the colors to make it so that the color matches with the light
#Interesting, R = 0.7, p < 0.0001
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
#Not interesting 

#Total leaf area 
ggscatter(Thesis_data_July16, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)")
#Interesting, r = 0.69, p < 0.0001


ggscatter(Thesis_data_July16, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)")
#Interesting R = 0.7, p < 0.0001

ggscatter(Thesis_data_July16, x = "Total_leaf_area_cm2", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Leaf area (cm2)", ylab = "Sink to source (cm)")
#Interesting? R = -0.5, p = 0.0052

ggscatter(Thesis_data_July16, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Interesting, R = 0.74, p < 0.0001
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
ggscatter(Thesis_data_July16, x = "Root_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Stem diameter (mm)")
#Interesting, r = 0.78, p = < 0.0001


ggscatter(Thesis_data_July16, x = "Root_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Sink to source (cm)")
#Interesting, R = -0.65, p < 0.0001

ggscatter(Thesis_data_July16, x = "Root_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Interesting, R = 0.8, p < 0.0001
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
ggscatter(Thesis_data_July16, x = "Stem_diameter_mm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "stem diameter (mm)", ylab = "Sink to source (cm)")
#Interesting, R = -0.54, p < 0.01

ggscatter(Thesis_data_July16, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#R = 0.81, p < 0.0001
ggscatter(High, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting
ggscatter(Medium, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting
ggscatter(Low, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting

#Photosynthesis

#Light 
ggscatter(Thesis_data_July16, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Interesting, R = 0.59, p = 0.025

#Height
ggscatter(Thesis_data_July16, x = "Photosynthesis", y = "Stem_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem length (cm)")
  #INot interesting

#Diameter 
ggscatter(Thesis_data_July16, x = "Photosynthesis", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem diameter (mm)")
  #Not interesting 

#Root length
ggscatter(Thesis_data_July16, x = "Photosynthesis", y = "Root_length_cm", 
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
    hist(Thesis_data_July16$Stem_length_cm)
      #Right-skewed
    
  #Root length
    hist(High$Root_length_cm)
      #Normal? This plot is weird
    hist(Medium$Root_length_cm)
      #Normal?
    hist(Low$Root_length_cm)
      #Kind of right-skewed? 
    hist(Thesis_data_July16$Root_length_cm)
      #Right-skewed
    
  #Stem diameter
    hist(High$Stem_diameter_mm)
      #Fairly normal
    hist(Medium$Stem_diameter_mm)
      #Fairly normal 
    hist(Low$Stem_diameter_mm)
      #Fairly normal 
    hist(Thesis_data_July16$Stem_diameter_mm)
      #Fairly normal
    
  #Sink to source
    hist(High$Sink_to_source_cm)
      #Fairly normal
    hist(Medium$Sink_to_source_cm)
      #Fairly normal
    hist(Low$Sink_to_source_cm)
      #Left skew 
    hist(Thesis_data_July16$Sink_to_source_cm)
      #Fairly normal 

#STATISTICAL ANALYSES
#==================================================================================================

#Stem length
#ANOVA
stem.aov <- aov(Stem_length_cm ~ Light_treatment, data = Thesis_data_July16)
summary(stem.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2 173.32   86.66   61.29 9.17e-11 ***
#Residuals       27  38.18    1.41 

#Tukey multiple pairwise-comparisons
TukeyHSD(stem.aov)
#         diff        lwr       upr     p adj
#Medium-High -4.67 -5.988476 -3.3515237 0.0000000
#Low-High    -5.44 -6.758476 -4.1215237 0.0000000
#Low-Medium  -0.77 -2.088476  0.5484763 0.3313452

#Leaf area
#ANOVA
leaf.aov <- aov(Total_leaf_area_cm2 ~ Light_treatment, data = Thesis_data_July16)
summary(leaf.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  26519   13260   26.12 4.87e-07 ***
#Residuals       27  13707     508                   

#Tukey multiple pairwise-comparisons
TukeyHSD(leaf.aov)
                #diff      lwr        upr     p adj
#Medium-High -51.4802 -76.46388 -26.49652 0.0000658
#Low-High    -70.3523 -95.33598 -45.36862 0.0000005
#Low-Medium  -18.8721 -43.85578   6.11158 0.1659484


#Root length
#ANOVA
root.aov <- aov(Root_length_cm ~ Light_treatment, data = Thesis_data_July16)
summary(root.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2 1103.6   551.8   43.64 3.47e-09 ***
#Residuals       27  341.4    12.6                 

#Tukey multiple pairwise-comparisons
TukeyHSD(root.aov)
#diff       lwr        upr     p adj
#Medium-High  -4.195  -8.137735  -0.2522653 0.0353306
#Low-High    -14.440 -18.382735 -10.4972653 0.0000000
#Low-Medium  -10.245 -14.187735  -6.3022653 0.0000020



#Stem diameter
#ANOVA
diameter.aov <- aov(Stem_diameter_mm ~ Light_treatment, data = Thesis_data_July16)
summary(diameter.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2  7.174   3.587   51.98 5.52e-10 ***
#Residuals       27  1.863   0.069    

#Tukey multiple pairwise-comparisons
TukeyHSD(diameter.aov)
#          diff        lwr         upr     p adj
#Medium-High -0.764 -1.055286 -0.472714 0.0000017
#Low-High    -1.181 -1.472286 -0.889714 0.0000000
#Low-Medium  -0.417 -0.708286 -0.125714 0.0039771

#Sink to source
#ANOVA
sinktosource.aov <- aov(Sink_to_source_cm ~ Light_treatment, data = Thesis_data_July16)
summary(sinktosource.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  3.685  1.8423   18.34 9.33e-06 ***
#Residuals       27  2.712  0.1005  

#Tukey multiple pairwise-comparisons
TukeyHSD(sinktosource.aov)
#        diff         lwr       upr     p adj
#Medium-High 0.245 -0.1064533 0.5964533 0.2131990
#Low-High    0.835  0.4835467 1.1864533 0.0000082
#Low-Medium  0.590  0.2385467 0.9414533 0.0008152

#Photosynthesis
#ANOVA
photo.aov <- aov(Photosynthesis ~ Light_treatment, data = Thesis_data_July16)
summary(photo.aov)
                #Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  1  98.04   98.04   50.89 1.19e-05 ***
#Residuals       12  23.12    1.93 

#Tukey multiple pairwise-comparisons
TukeyHSD(photo.aov)
#         diff        lwr       upr     p adj
#Medium-High 5.857863 4.068748 7.646979 1.19e-05

#Height:Diameter
#ANOVA
HD.aov <- aov(H.D ~ Light_treatment, data = Thesis_data_July16)
summary(HD.aov)
                #Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2   1346   672.9   12.75 0.000126 ***
#Residuals       27   1425    52.8   

#Tukey multiple pairwise-comparisons
TukeyHSD(HD.aov)
#         diff        lwr       upr     p adj
# Medium-High -15.960264 -24.014770 -7.905758 0.0001110
# Low-High    -11.268891 -19.323397 -3.214385 0.0048733
# Low-Medium    4.691373  -3.363133 12.745879 0.3332118


#CREATING RATIOS
#==================================================================================================

Thesis_data_July16$Root_Leaf <- Total_leaf_area_cm2/Root_length_cm

Root_leaf2 <- ggplot(Thesis_data_July16, aes(x = Light_treatment, y = Root_Leaf, 
                                              fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root leaf ratio") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) 
Root_leaf2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))  


Thesis_data_July16$Root_Shoot <- Root_length_cm/Stem_length_cm

Root_shoot2 <- ggplot(Thesis_data_July16, aes(x = Light_treatment, y = Root_Shoot, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root:shoot") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(9, 9)) + 
  geom_signif(comparisons = list(c("Medium","Low")), map_signif_level = TRUE, 
              y_position = c(8, 8)) 
Root_shoot2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))  









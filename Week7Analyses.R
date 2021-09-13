#Created by: Elise Miller
#Date started: 08/06/2021
#Date last edited: 08/06/2021
#Description: Growth data analyses for week 7 data

#Attach files
attach(Thesis_data_Aug6)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

#Order the plot 
Thesis_data_Aug6$Light_treatment <- factor (Thesis_data_Aug6$Light_treatment, levels = c("High", "Medium", "Low"))


#SUMMARY FOR WEEK 2, 4, 6, and 7 
#==================================================================================================
#Filtering the data
High <- Thesis_data_Aug6 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_Aug6 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_Aug6 %>%
  filter(Light_treatment == "Low") 

#Stem_length
group_by(Thesis_data_Aug6, Light_treatment) %>%
  summarise(
    mean = mean(Stem_length_cm, na.rm = TRUE), 
    sd = sd(Stem_length_cm, na.rm = TRUE)
  )
#Light_treatment     mean    sd

#High             9.60  6.69
#2 Medium           5.88  4.81
#3 Low              2.78  1.51


summary(Stem_length_cm, is.na = FALSE)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#0.650   1.538   3.750   6.090  10.325  22.700     

sd(Stem_length_cm)

#High summary 
summary(High$Stem_length_cm)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 0.650   4.750  10.100   9.604  14.050  22.700  

#Medium summary 
summary(Medium$Stem_length_cm)
#Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
#0.700   1.200   4.200   5.885  10.500  14.100 

#Low summary 
summary(Low$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
#0.800   1.600   2.400   2.783   3.600   6.100    

#Total leaf area 
group_by(Thesis_data_Aug6, Light_treatment) %>%
  summarise(
    mean = mean(Total_leaf_area_cm2, na.rm = TRUE), 
    sd = sd(Total_leaf_area_cm2, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#1 High            104.   85.6
#2 Medium           77.8  84.2
#3 Low              11.4  13.1

summary(Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.022   2.514  25.005  64.524 109.319 256.194     

sd(Total_leaf_area_cm2)

#High summary 
summary(High$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.426   5.458  98.481 104.409 167.488 256.194  

#Medium summary 
summary(Medium$Total_leaf_area_cm2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.624   3.760  34.754  77.780 152.131 243.878  

#Low summary 
summary(Low$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.022   1.292   6.227  11.383  15.167  45.411 

#Root length 
group_by(Thesis_data_Aug6, Light_treatment) %>%
  summarise(
    mean = mean(Root_length_cm, na.rm = TRUE), 
    sd = sd(Root_length_cm, na.rm = TRUE)
  )
#Light_treatment    mean    sd
#1 High            16.8   8.04
#2 Medium          14.8   7.63
#3 Low              5.80  4.48

summary(Root_length_cm)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    
# 11.200   4.638  10.850  12.479  21.050  29.300       

sd(Root_length_cm)

#High summary 
summary(High$Root_length_cm)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#3.10    8.30   18.10   16.80   22.62   29.30

#Medium summary 
summary(Medium$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 2.60    6.80   16.73   14.84   21.27   26.50 

#Low summary 
summary(Low$Root_length_cm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#  1.200   2.550   4.525   5.803   7.425  22.000    

#Stem diameter
group_by(Thesis_data_Aug6, Light_treatment) %>%
  summarise(
    mean = mean(Stem_diameter_mm, na.rm = TRUE), 
    sd = sd(Stem_diameter_mm, na.rm = TRUE)
  )
#  Light_treatment  mean    sd
#1 High            1.95  0.962
#2 Medium          1.60  0.881
#3 Low             0.766 0.257

summary(Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max.   
# 0.290   0.680   1.065   1.440   2.228   3.950     

sd(Stem_diameter_mm)

#High summary 
summary(High$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
# 0.420   1.198   1.930   1.955   2.635   3.950      

#Medium summary 
summary(Medium$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.450   0.725   1.545   1.599   2.365   3.230  

#Low summary 
summary(Low$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.2900  0.6075  0.7500  0.7658  0.9150  1.4100 

#Sink to source
group_by(Thesis_data_Aug6, Light_treatment) %>%
  summarise(
    mean = mean(Sink_to_source_cm, na.rm = TRUE), 
    sd = sd(Sink_to_source_cm, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High             1.11 0.416
#2 Medium           1.25 0.481
#3 Low              1.75 0.554

summary(Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max.   
#   0.500   0.950   1.200   1.367   1.812   3.100    

sd(Sink_to_source_cm)

#High summary
summary(High$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.500   0.800   0.975   1.033   1.188   2.050 

#Medium summary 
summary(Medium$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 0.6000  0.9375  1.1000  1.2463  1.4625  3.1000   

#Low summary 
summary(Low$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#  0.750   1.285   1.875   1.748   2.100   3.000 


#SUMMARY FOR WEEK 7
#==================================================================================================
#Filtering the data
High <- Thesis_data_Aug6 %>%
  filter(Light_treatment == "High" & Age..J.H. == "50") 

Medium <- Thesis_data_Aug6 %>%
  filter(Light_treatment == "Medium" & Age..J.H. == "50") 

Low <- Thesis_data_Aug6 %>%
  filter(Light_treatment == "Low" & Age..J.H. == "50")

#Stem_length
group_by(Thesis_data_Aug6, Light_treatment) %>%
  filter(Age..J.H. == "50")  %>% 
  summarise(
    mean = mean(Stem_length_cm, na.rm = TRUE), 
    sd = sd(Stem_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean    sd

#1 High            17.9   3.50
#2 Medium          12.4   1.54
#3 Low              4.35  1.62


#High summary 
summary(High$Stem_length_cm)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 11.45   16.45   18.25     17.93   20.57   22.70  
sd(High$Stem_length_cm)

#Medium summary 
summary(Medium$Stem_length_cm)
#Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
#9.00   11.70   12.75     12.35   13.38   14.10 
sd(Medium$Stem_length_cm)

#Low summary 
summary(Low$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 1.350   3.300   4.725   4.350   5.700   6.100 
sd(Low$Stem_length_cm)

#Total leaf area  
group_by(Thesis_data_Aug6, Light_treatment) %>%
  filter(Age..J.H. == "50")  %>% 
  summarise(
    mean = mean(Total_leaf_area_cm2, na.rm = TRUE), 
    sd = sd(Total_leaf_area_cm2, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#1 High            204.   56.0
#2 Medium          193.   42.6
#3 Low            23.3  14.9

summary(Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.022   2.514  25.005  64.524 109.319 256.194

sd(Total_leaf_area_cm2)

#High summary 
summary(High$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 93.71  178.29  221.94  204.28  247.50  256.19  

#Medium summary 
summary(Medium$Total_leaf_area_cm2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 108.3   173.9   192.8   192.8   228.5   243.9  

#Low summary 
summary(Low$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 1.432  13.440  20.352  23.345  36.824  45.411 

#Root length 
group_by(Thesis_data_Aug6, Light_treatment) %>%
  filter(Age..J.H. == "50")  %>% 
  summarise(
    mean = mean(Root_length_cm, na.rm = TRUE), 
    sd = sd(Root_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean    sd
#1 High             25.0  2.61
#2 Medium           22.7  1.86
#3 Low              10.7  5.95


#High summary 
summary(High$Root_length_cm)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
# 21.70   22.73   24.75   24.98   26.48   29.30   
sd(High$Root_length_cm)

#Medium summary 
summary(Medium$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 20.30   21.60   22.30   22.67   22.82   26.50  
sd(Medium$Root_length_cm)

#Low summary 
summary(Low$Root_length_cm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#   2.05    6.60   10.30   10.73   12.15   22.00 
sd(Low$Root_length_cm)

#Stem diameter
group_by(Thesis_data_Aug6, Light_treatment) %>%
  filter(Age..J.H. == "50")  %>% 
  summarise(
    mean = mean(Stem_diameter_mm, na.rm = TRUE), 
    sd = sd(Stem_diameter_mm, na.rm = TRUE)
  )
#  Light_treatment  mean    sd
#1 High             3.02 0.615
#2 Medium           2.68 0.390
#3 Low              1.03 0.223

#High summary 
summary(High$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.     
# 1.690   2.705   3.230   3.023   3.297   3.950 
sd(High$Stem_diameter_mm)

#Medium summary 
summary(Medium$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 2.100   2.365   2.690   2.682   2.940   3.230  
sd(Medium$Stem_diameter_mm)

#Low summary 
summary(Low$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.5400  0.9525  1.0350  1.0290  1.1325  1.4100 
sd(Low$Stem_diameter_mm)

#Sink to source
group_by(Thesis_data_Aug6, Light_treatment) %>%
  filter(Age..J.H. == "50")  %>% 
  summarise(
    mean = mean(Sink_to_source_cm, na.rm = TRUE), 
    sd = sd(Sink_to_source_cm, na.rm = TRUE)
  )

#Light_treatment    mean    sd
#1 High             1.32  0.441
#2 Medium           1.38  0.706
#3 Low              2.01  0.692


#High summary
summary(High$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.800   1.012   1.250   1.325   1.712   1.950
sd(High$Sink_to_source_cm)

#Medium summary 
summary(Medium$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 0.9000  0.9625  1.0500  1.3850  1.4000  3.1000 
sd(Medium$Sink_to_source_cm)

#Low summary 
summary(Low$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#  1.000   1.650   1.950   2.010   2.575   3.000 
sd(Low$Sink_to_source_cm)

#Photosynthesis 
group_by(Thesis_data_Aug6, Light_treatment) %>%
  filter(Age..J.H. == "50")  %>% 
  summarise(
    mean = mean(Photosynthesis, na.rm = TRUE), 
    sd = sd(Photosynthesis, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High             9.88  3.90
#2 Medium           9.32  2.53
#3 Low              1.86  1.09


#High summary
summary(High$Photosynthesis)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 2.956   8.427  10.429   9.885  12.038  15.191 
sd(High$Photosynthesis)

#Medium summary 
summary(Medium$Photosynthesis)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
# 4.554   8.878   9.561   9.322  10.225  12.832 
sd(Medium$Photosynthesis)

#Low summary 
summary(Low$Photosynthesis)
#  Min.       1st Qu.  Median       Mean    3rd Qu.    Max.   
# -0.001061  1.407919  1.988024  1.862739  2.686721  3.147840  
sd(Low$Photosynthesis)

#Respiration 
group_by(Thesis_data_Aug6, Light_treatment) %>%
  filter(Age..J.H. == "50")  %>% 
  summarise(
    mean = mean(Respiration, na.rm = TRUE), 
    sd = sd(Respiration, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High            -1.23  0.389
#2 Medium          -1.25  0.264
#3 Low             -0.821 0.362


#High summary
summary(High$Respiration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# -1.8733 -1.4765 -1.2344 -1.2317 -0.9247 -0.7092  
sd(High$Respiration)

#Medium summary 
summary(Medium$Respiration)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
# -1.6136 -1.4753 -1.2437 -1.2475 -1.0822 -0.8382  
sd(Medium$Respiration)

#Low summary 
summary(Low$Respiration)
#  Min.       1st Qu.  Median  Mean    3rd Qu.    Max.   
# -1.5027 -   0.9224 -0.7011 -0.8213 -0.6468 -0.4068  
sd(Low$Respiration)

#Stem_length
group_by(Thesis_data_Aug6, Light_treatment) %>%
  filter(Age..J.H. == "50")  %>% 
  summarise(
    mean = mean(H.D, na.rm = TRUE), 
    sd = sd(H.D, na.rm = TRUE)
  )
#Light_treatment   mean    sd

#1 High             60.6 12.0 
#2 Medium           46.5  6.34
#3 Low              41.1 10.6 


#High summary 
summary(High$H.D)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 40.76   55.28   62.18   60.65   67.77   79.88  
sd(High$H.D)

#Medium summary 
summary(Medium$H.D)
#Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
#38.79   42.26   45.13   46.51   50.68   57.78 
sd(Medium$H.D)

#Low summary 
summary(Low$H.D)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 25.00   32.75   40.47   41.13   50.76   53.04 
sd(Low$H.D)

#PLOTS
#==================================================================================================
#Order the plot

Thesis_data_Aug6 <- group_by(Thesis_data_Aug6, Light_treatment) %>%
  filter(Age..J.H. == "50")

#Stem length
jpeg("Stem_length2", width = 500, height = 500)
Stem_length2 <- ggplot(Thesis_data_Aug6, aes(x = Light_treatment, y = Stem_length_cm, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(25, 25)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(23, 23)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(20, 20)) 
Stem_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Leaf area
jpeg("Leaf_area2", width = 500, height = 500)
Leaf_area2 <- ggplot(Thesis_data_Aug6, aes(x = Light_treatment, y = Total_leaf_area_cm2, 
                                           fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Leaf area (cm2)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(300, 300)) + 
  geom_signif(comparisons = list(c("Medium", "Low")), map_signif_level = TRUE, 
              y_position = c(275, 275)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(260, 260))
Leaf_area2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Root length
jpeg("Root_length2", width = 500, height = 500)
Root_length2 <- ggplot(Thesis_data_Aug6, aes(x = Light_treatment, y = Root_length_cm, 
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
Stem_diameter2 <- ggplot(Thesis_data_Aug6, aes(x = Light_treatment, y = Stem_diameter_mm, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem diameter (mm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(4.2, 4.2)) +  
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(4.0, 4.0)) + 
  geom_signif(comparisons = list(c("Medium","Low")), map_signif_level = TRUE, 
              y_position = c(3.7, 3.7))  
Stem_diameter2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Source to sink
jpeg("Sink_source2", width = 500, height = 500)
Sink_source2 <- ggplot(Thesis_data_Aug6, aes(x = Light_treatment, y = Sink_to_source_cm, 
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


#Correlation plots
#Stem length 
ggscatter(Thesis_data_Aug6, x = "Stem_length_cm", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Root length (cm)")

#Interesting, R = 0.82, p < 0.0001

ggscatter(Thesis_data_Aug6, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Interesting, R = 0.85, p < 0.0001

ggscatter(Thesis_data_Aug6, x = "Stem_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Interesting, R = 0.9, p < 0.0001

ggscatter(Thesis_data_Aug6, x = "Stem_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Not interesting 

ggscatter(Thesis_data_Aug6, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
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
#Interesting, R = 0.73, p = 0.017
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
ggscatter(Thesis_data_Aug6, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)")
#Interesting, R = 0.8, p < 0.0001

ggscatter(Thesis_data_Aug6, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)")
#Interesting R = 0.94, p < 0.0001

ggscatter(Thesis_data_Aug6, x = "Total_leaf_area_cm2", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Leaf area (cm2)", ylab = "Sink to source (cm)")
#Interesting? R = -0.51, p = 0.0038

ggscatter(Thesis_data_Aug6, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Interesting, R = 0.58, p = 0.00083
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
ggscatter(Thesis_data_Aug6, x = "Root_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Stem diameter (mm)")
#Interesting, r = 0.78, p = < 0.0001


ggscatter(Thesis_data_Aug6, x = "Root_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Sink to source (cm)")
#Interesting, R = -0.4, p < 0.05

ggscatter(Thesis_data_Aug6, x = "Root_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Interesting, R = 0.58, p < 0.001

#Stem diameter
ggscatter(Thesis_data_Aug6, x = "Stem_diameter_mm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "stem diameter (mm)", ylab = "Sink to source (cm)")
#Interesting, R = -0.51, p < 0.01

ggscatter(Thesis_data_Aug6, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#R = 0.65, p < 0.001

#Photosynthesis

#Light 
ggscatter(Thesis_data_Aug6, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Interesting, R = 0.47, p = 0.012
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
#Not interesting 

#Height
ggscatter(Thesis_data_Aug6, x = "Photosynthesis", y = "Stem_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem length (cm)")
#Interesting, R = 0.67, p < 0.0001

#Diameter 
ggscatter(Thesis_data_Aug6, x = "Photosynthesis", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem diameter (mm)")
#Interesting, R = 0.77, p < 0.0001

#Root length
ggscatter(Thesis_data_Aug6, x = "Photosynthesis", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Root length (cm)")
#Interesting, R = 0.62, p < 0.001

#Respiration
#Light 
ggscatter(Thesis_data_Aug6, x = "Respiration", y = "Light_level..umol.m2.s.", 
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
ggscatter(Thesis_data_Aug6, x = "Respiration", y = "Stem_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem length (cm)")
#Not interesting 

#Diameter 
ggscatter(Thesis_data_Aug6, x = "Respiration", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem diameter (mm)")
#R = -0.51, p < 0.01

#Root length
ggscatter(Thesis_data_Aug6, x = "Respiration", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Root length (cm)")
#Not interesting 

#Total Leaf Area
ggscatter(Thesis_data_Aug6, x = "Respiration", y = "Total_leaf_area_cm2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Root length (cm)")
#Interesting, R = -0.51, p < 0.01

#Histograms 
#Stem length 
hist(High$Stem_length_cm)
#Fairly normal
hist(Medium$Stem_length_cm)
#Right-skewed
hist(Low$Stem_length_cm)
#Fairly normal
hist(Thesis_data_Aug6$Stem_length_cm)
#Right-skewed

#Root length
hist(High$Root_length_cm)
#Normal? This plot is weird
hist(Medium$Root_length_cm)
#Normal?
hist(Low$Root_length_cm)
#Kind of right-skewed? 
hist(Thesis_data_Aug6$Root_length_cm)
#Right-skewed

#Stem diameter
hist(High$Stem_diameter_mm)
#Fairly normal
hist(Medium$Stem_diameter_mm)
#Fairly normal 
hist(Low$Stem_diameter_mm)
#Fairly normal 
hist(Thesis_data_Aug6$Stem_diameter_mm)
#Fairly normal

#Sink to source
hist(High$Sink_to_source_cm)
#Fairly normal
hist(Medium$Sink_to_source_cm)
#Fairly normal
hist(Low$Sink_to_source_cm)
#Left skew 
hist(Thesis_data_Aug6$Sink_to_source_cm)
#Fairly normal 

#STATISTICAL ANALYSES
#==================================================================================================

#Stem length
#ANOVA
stem.aov <- aov(Stem_length_cm ~ Light_treatment, data = Thesis_data_Aug6)
summary(stem.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2  932.5   466.2   81.07 3.86e-12 ***
#Residuals       27  155.3     5.8       

#Tukey multiple pairwise-comparisons
TukeyHSD(stem.aov)
#              diff        lwr       upr     p adj
#Medium-High  -5.585  -8.244095  -2.925905 5.05e-05
#Low-High    -13.585 -16.244095 -10.925905 0.00e+00
#Low-Medium   -8.000 -10.659095  -5.340905 1.00e-07

#Leaf area
#ANOVA
leaf.aov <- aov(Total_leaf_area_cm2 ~ Light_treatment, data = Thesis_data_Aug6)
summary(leaf.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2 205229  102614   59.53 1.26e-10 ***
#Residuals       27  46538    1724                      

#Tukey multiple pairwise-comparisons
TukeyHSD(leaf.aov)
#diff       lwr        upr     p adj
#Medium-High  -11.5259  -57.5605   34.5087 0.8100713
#Low-High    -180.9333 -226.9679 -134.8987 0.0000000
#Low-Medium  -169.4074 -215.4420 -123.3728 0.0000000

#Root length
#ANOVA
root.aov <- aov(Root_length_cm ~ Light_treatment, data = Thesis_data_Aug6)
summary(root.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2 1169.0   584.5   38.37 1.28e-08 ***
#Residuals       27  411.3    15.2  

#Tukey multiple pairwise-comparisons
TukeyHSD(root.aov)
#diff       lwr        upr     p adj
#Medium-High  -2.310  -6.637578  2.017578 0.3947663
#Low-High    -14.245 -18.572578 -9.917422 0.0000000
#Low-Medium  -11.935 -16.262578 -7.607422 0.0000007

#Stem diameter
#ANOVA
diameter.aov <- aov(Stem_diameter_mm ~ Light_treatment, data = Thesis_data_Aug6)
summary(diameter.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2 22.749  11.375   58.76 1.46e-10 ***
#Residuals       27  5.226   0.194   

#Tukey multiple pairwise-comparisons
TukeyHSD(diameter.aov)
#          diff        lwr         upr     p adj
#Medium-High -0.341 -0.828841  0.146841 0.2115215
#Low-High    -1.994 -2.481841 -1.506159 0.0000000
#Low-Medium  -1.653 -2.140841 -1.165159 0.0000000

#Sink to source
#ANOVA
sinktosource.aov <- aov(Sink_to_source_cm ~ Light_treatment, data = Thesis_data_Aug6)
summary(sinktosource.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  2  2.878  1.4391   3.683 0.0385 *
#Residuals       27 10.551  0.3908    

#Tukey multiple pairwise-comparisons
TukeyHSD(sinktosource.aov)
#        diff         lwr       upr     p adj
#Medium-High 0.060 -0.633137399 0.7531374 0.9749491
#Low-High    0.685 -0.008137399 1.3781374 0.0532182
#Low-Medium  0.625 -0.068137399 1.3181374 0.0831285

#Photosynthesis
#ANOVA
photo.aov <- aov(Photosynthesis ~ Light_treatment, data = Thesis_data_Aug6)
summary(photo.aov)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2  344.0   172.0   21.23 4.09e-06 ***
#Residuals       25  202.5     8.1     

#Tukey multiple pairwise-comparisons
TukeyHSD(photo.aov)
#         diff        lwr       upr     p adj
#Medium-High -0.5630564  -3.733604  2.607491 0.8981948
#Low-High    -8.0221274 -11.385000 -4.659254 0.0000097
#Low-Medium  -7.4590710 -10.821944 -4.096198 0.0000280

#Respiration
#ANOVA
resp.aov <- aov(Respiration ~ Light_treatment, data = Thesis_data_Aug6)
summary(resp.aov)
#Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  2 0.9086  0.4543   3.931 0.0334 *
#Residuals       24 2.7735  0.1156     

#Tukey multiple pairwise-comparisons
TukeyHSD(resp.aov)
#         diff        lwr       upr     p adj
#Medium-High -0.01572389 -0.395378569 0.3639308 0.9941211
#Low-High     0.41044796 -0.007911118 0.8288070 0.0552031
#Low-Medium   0.42617185  0.007812777 0.8445309 0.0452979


#Height : Diameter
#ANOVA
HD.aov <- aov(H.D ~ Light_treatment, data = Thesis_data_Aug6)
summary(HD.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment   2   2771  1385.4   5.886 0.00367 **
#Residuals       117  27537   235.4   

#Tukey multiple pairwise-comparisons
TukeyHSD(HD.aov)
#         diff        lwr       upr     p adj
#Medium-High -11.630679 -19.774245 -3.4871122 0.0027203
#Low-High     -7.380958 -15.524524  0.7626082 0.0840581
#Low-Medium    4.249720  -3.893846 12.3932868 0.4328349


#COMPARING AGES
#==================================================================================================

#Have to re-download the csv
attach(Thesis_data_Aug6)

#Re-filter
High <- Thesis_data_Aug6 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_Aug6 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_Aug6 %>%
  filter(Light_treatment == "Low") 

#Made age a character to allow for plotting
High$Age..J.H. = as.character(High$Age..J.H.)
Medium$Age..J.H. = as.character(Medium$Age..J.H.)
Low$Age..J.H. = as.character(Low$Age..J.H.)

Thesis_data_Aug6$Photosynthesis = as.character(Thesis_data_Aug6$Photosynthesis)

Stem_lengthH <- ggplot(High, aes(x = Age..J.H., y = Stem_length_cm, 
                                 fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Age", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  
Stem_lengthH


Stem_lengthM <- ggplot(Medium, aes(x = Age..J.H., y = Stem_length_cm, 
                                   fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Age", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  
Stem_lengthM

Stem_lengthL <- ggplot(Low, aes(x = Age..J.H., y = Stem_length_cm, 
                                fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Age", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  
Stem_lengthL

#Add all 3 plots together 

ggplot(subset(Thesis_data_Aug6, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50")), 
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

#Leaf Area
ggplot(subset(Thesis_data_Aug6, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50")), 
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
ggplot(subset(Thesis_data_Aug6, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50")), 
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
ggplot(subset(Thesis_data_Aug6, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50")), 
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








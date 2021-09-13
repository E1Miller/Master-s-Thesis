#Created by: Elise Miller
#Date started: 08/13/2021
#Date last edited: 08/13/2021
#Description: Growth data analyses for week 8 data

#Attach files
attach(Thesis_data_Aug13)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

#Order the plot 
Thesis_data_Aug13$Light_treatment <- factor (Thesis_data_Aug13$Light_treatment, levels = c("High", "Medium", "Low"))


#SUMMARY FOR WEEK 2, 4, 6, 7, and 8 
#==================================================================================================
#Filtering the data

Thesis_data_Aug13 <- Thesis_data_Aug13 %>%
  filter(Age..J.H. == "57") 

High <- Thesis_data_Aug13 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_Aug13 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_Aug13 %>%
  filter(Light_treatment == "Low") 

#Stem_length
group_by(Thesis_data_Aug13, Light_treatment) %>%
  summarise(
    mean = mean(Stem_length_cm, na.rm = TRUE), 
    sd = sd(Stem_length_cm, na.rm = TRUE)
  )
#Light_treatment     mean    sd

#1 High            11.4   7.09
#2 Medium           7.70  5.78
#3 Low              2.78  1.51


summary(Stem_length_cm, is.na = FALSE)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#0.65    1.70    5.75    7.61   12.72   24.20    

sd(Stem_length_cm)

#High summary 
summary(High$Stem_length_cm)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 0.650   5.975  12.050  11.387  17.650  24.200   

#Medium summary 
summary(Medium$Stem_length_cm)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
# 0.700   1.900   7.000   7.696  12.600  19.900  

#Low summary 
summary(Low$Stem_length_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
#0.800   1.600   2.400   2.783   3.600   6.100    

#Total leaf area 
group_by(Thesis_data_Aug13, Light_treatment) %>%
  summarise(
    mean = mean(Total_leaf_area_cm2, na.rm = TRUE), 
    sd = sd(Total_leaf_area_cm2, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#1 High            130.   94.5
#2 Medium          102.   93.7
#3 Low              11.4  13.1

summary(Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.022   3.819  40.087  85.823 165.116 318.770      

sd(Total_leaf_area_cm2)

#High summary 
summary(High$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.426  50.928 127.998 129.678 212.737 318.770  

#Medium summary 
summary(Medium$Total_leaf_area_cm2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.624  10.873  91.391 101.521 178.278 290.255  

#Low summary 
summary(Low$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.022   1.292   6.227  11.383  15.167  45.411 

#Root length 
group_by(Thesis_data_Aug13, Light_treatment) %>%
  summarise(
    mean = mean(Root_length_cm, na.rm = TRUE), 
    sd = sd(Root_length_cm, na.rm = TRUE)
  )
#Light_treatment    mean    sd
#1 High            17.8   7.44
#2 Medium          16.3   7.42
#3 Low              5.80  4.48

summary(Root_length_cm)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.    
# 1.200   4.975  14.700  13.807  21.700  29.300      

sd(Root_length_cm)

#High summary 
summary(High$Root_length_cm)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#3.10   14.60   21.20   17.76   22.20   29.30 

#Medium summary 
summary(Medium$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 2.60   10.60   20.20   16.26   21.85   26.50 

#Low summary 
summary(Low$Root_length_cm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#  1.200   2.550   4.525   5.803   7.425  22.000    

#Stem diameter
group_by(Thesis_data_Aug13, Light_treatment) %>%
  summarise(
    mean = mean(Stem_diameter_mm, na.rm = TRUE), 
    sd = sd(Stem_diameter_mm, na.rm = TRUE)
  )
#  Light_treatment  mean    sd
#1 High            2.23  1.03 
#2 Medium          1.87  0.974
#3 Low             0.766 0.257

summary(Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean    3rd Qu.    Max.   
#  0.2900  0.7375  1.4500  1.6836  2.5900  3.9500    

sd(Stem_diameter_mm)

#High summary 
summary(High$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
#  0.420   1.555   2.315   2.231   3.147   3.950       

#Medium summary 
summary(Medium$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.450   0.930   1.895   1.871   2.683   3.710  

#Low summary 
summary(Low$Stem_diameter_mm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.2900  0.6075  0.7500  0.7658  0.9150  1.4100 

#Sink to source
group_by(Thesis_data_Aug13, Light_treatment) %>%
  summarise(
    mean = mean(Sink_to_source_cm, na.rm = TRUE), 
    sd = sd(Sink_to_source_cm, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High             1.37 1.15 
#2 Medium           1.42 0.957
#3 Low              1.75 0.554

summary(Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean   3rd Qu.    Max.   
# 0.500   1.000   1.245   1.495   1.863   8.200    

sd(Sink_to_source_cm)

#High summary
summary(High$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.5000  0.8625  1.1000  1.3660  1.4875  8.2000 

#Medium summary 
summary(Medium$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 0.6000  0.9625  1.1500  1.4200  1.6000  7.2000   

#Low summary 
summary(Low$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#  0.750   1.285   1.875   1.748   2.100   3.000 


#SUMMARY FOR WEEK 8
#==================================================================================================
#Filtering the data
High <- Thesis_data_Aug13 %>%
  filter(Light_treatment == "High" & Age..J.H. == "57") 

Medium <- Thesis_data_Aug13 %>%
  filter(Light_treatment == "Medium" & Age..J.H. == "57") 

Low <- Thesis_data_Aug13 %>%
  filter(Light_treatment == "Low" & Age..J.H. == "57")

#Stem_length
group_by(Thesis_data_Aug13, Light_treatment) %>%
  filter(Age..J.H. == "57")  %>% 
  summarise(
    mean = mean(Stem_length_cm, na.rm = TRUE), 
    sd = sd(Stem_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean    sd

#1 High             18.5  3.04
#2 Medium           14.9  2.90

#High summary 
summary(High$Stem_length_cm)
# Min.   1st Qu.  Median    Mean    3rd Qu.    Max. 
# 12.70   17.18   18.60   18.52   19.30   24.20  
sd(High$Stem_length_cm)

#Medium summary 
summary(Medium$Stem_length_cm)
#Min. 1st Qu.  Median    Mean    3rd Qu.    Max.    
#11.30   12.38   15.35   14.94   17.02   19.90 
sd(Medium$Stem_length_cm)

#Total leaf area  
group_by(Thesis_data_Aug13, Light_treatment) %>%
  filter(Age..J.H. == "57")  %>% 
  summarise(
    mean = mean(Total_leaf_area_cm2, na.rm = TRUE), 
    sd = sd(Total_leaf_area_cm2, na.rm = TRUE)
  )
#Light_treatment     mean    sd
#1 High             231.  51.5
#2 Medium           196.  67.3


summary(Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.022   3.819  40.087  85.823 165.116 318.770 

sd(Total_leaf_area_cm2)

#High summary 
summary(High$Total_leaf_area_cm2)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 161.5   184.5   237.9   230.8   263.4   318.8  

#Medium summary 
summary(Medium$Total_leaf_area_cm2)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 90.65  144.26  195.25  196.49  250.12  290.25   

#Root length 
group_by(Thesis_data_Aug13, Light_treatment) %>%
  filter(Age..J.H. == "57")  %>% 
  summarise(
    mean = mean(Root_length_cm, na.rm = TRUE), 
    sd = sd(Root_length_cm, na.rm = TRUE)
  )
#Light_treatment  mean    sd
#1 High             21.6 0.750
#2 Medium           21.9 1.73 


#High summary 
summary(High$Root_length_cm)

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.     
# 20.10   21.20   21.85   21.61   22.10   22.50   
sd(High$Root_length_cm)

#Medium summary 
summary(Medium$Root_length_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 19.50   21.38   21.65   21.94   21.98   26.30  
sd(Medium$Root_length_cm)


#Stem diameter
group_by(Thesis_data_Aug13, Light_treatment) %>%
  filter(Age..J.H. == "57")  %>% 
  summarise(
    mean = mean(Stem_diameter_mm, na.rm = TRUE), 
    sd = sd(Stem_diameter_mm, na.rm = TRUE)
  )
#  Light_treatment  mean    sd
#1 High             3.34 0.347
#2 Medium           2.96 0.399

#High summary 
summary(High$Stem_diameter_mm)
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max.     
# 2.830   2.998   3.410   3.335   3.632   3.710 
sd(High$Stem_diameter_mm)

#Medium summary 
summary(Medium$Stem_diameter_mm)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 2.530   2.610   2.840   2.957   3.235   3.710   
sd(Medium$Stem_diameter_mm)


#Sink to source
group_by(Thesis_data_Aug13, Light_treatment) %>%
  filter(Age..J.H. == "57")  %>% 
  summarise(
    mean = mean(Sink_to_source_cm, na.rm = TRUE), 
    sd = sd(Sink_to_source_cm, na.rm = TRUE)
  )

#Light_treatment    mean    sd
#1 High             2.40  2.21
#2 Medium           2.12  1.82


#High summary
summary(High$Sink_to_source_cm)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 0.600   1.262   1.550   2.405   2.375   8.200 
sd(High$Sink_to_source_cm)

#Medium summary 
summary(Medium$Sink_to_source_cm)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.   
# 1.000   1.450   1.550   2.115   1.913   7.200
sd(Medium$Sink_to_source_cm)


#Photosynthesis 
group_by(Thesis_data_Aug13, Light_treatment) %>%
  filter(Age..J.H. == "57")  %>% 
  summarise(
    mean = mean(Photosynthesis, na.rm = TRUE), 
    sd = sd(Photosynthesis, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High            10.7   4.04
#2 Medium           9.98  3.41


#High summary
summary(High$Photosynthesis)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 5.103   7.676  11.345  10.676  12.778  17.764  
sd(High$Photosynthesis)

#Medium summary 
summary(Medium$Photosynthesis)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
# 1.449   9.489  11.057   9.981  12.224  12.975
sd(Medium$Photosynthesis)


#Respiration 
group_by(Thesis_data_Aug13, Light_treatment) %>%
  filter(Age..J.H. == "57")  %>% 
  summarise(
    mean = mean(Respiration, na.rm = TRUE), 
    sd = sd(Respiration, na.rm = TRUE)
  )

#Light_treatment  mean    sd
#1 High            -1.25 0.254
#2 Medium          -1.62 0.198


#High summary
summary(High$Respiration)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# -1.5752 -1.4617 -1.2248 -1.2521 -1.0792 -0.8663 
sd(High$Respiration)

#Medium summary 
summary(Medium$Respiration)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
#  -1.900  -1.753  -1.602  -1.620  -1.496  -1.307  
sd(Medium$Respiration)


#Height : Diameter
group_by(Thesis_data_Aug13, Light_treatment) %>%
  filter(Age..J.H. == "57")  %>% 
  summarise(
    mean = mean(H.D, na.rm = TRUE), 
    sd = sd(H.D, na.rm = TRUE)
  )

#Light_treatment    mean    sd
#1 High             55.5  6.87
#2 Medium           50.7  9.00


#High summary
summary(High$H.D)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
# 43.79   51.88   55.75   55.50   58.53   65.41 
sd(High$H.D)

#Medium summary 
summary(Medium$H.D)
#  Min. 1st Qu.  Median    Mean  3rd Qu.    Max.   
#  40.85   43.84   48.52   50.74   57.32   68.38  
sd(Medium$H.D)


#PLOTS
#==================================================================================================
#Order the plot

Thesis_data_Aug13 <- group_by(Thesis_data_Aug13, Light_treatment) %>%
  filter(Age..J.H. == "57")

#Stem length
jpeg("Stem_length2", width = 500, height = 500)
Stem_length2 <- ggplot(Thesis_data_Aug13, aes(x = Light_treatment, y = Stem_length_cm, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem Length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(26, 26))  
Stem_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Leaf area
jpeg("Leaf_area2", width = 500, height = 500)
Leaf_area2 <- ggplot(Thesis_data_Aug13, aes(x = Light_treatment, y = Total_leaf_area_cm2, 
                                           fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Leaf area (cm2)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15))  + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(350, 350))
Leaf_area2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Root length
jpeg("Root_length2", width = 500, height = 500)
Root_length2 <- ggplot(Thesis_data_Aug13, aes(x = Light_treatment, y = Root_length_cm, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Root length (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(27, 27)) 
Root_length2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Stem diameter
jpeg("Stem_diameter2", width = 500, height = 500)
Stem_diameter2 <- ggplot(Thesis_data_Aug13, aes(x = Light_treatment, y = Stem_diameter_mm, 
                                               fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Stem diameter (mm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) +
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(4.0, 4.0)) 
Stem_diameter2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Source to sink
jpeg("Sink_source2", width = 500, height = 500)
Sink_source2 <- ggplot(Thesis_data_Aug13, aes(x = Light_treatment, y = Sink_to_source_cm, 
                                             fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Distance from sink to source (cm)") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(8.4, 8.4))
Sink_source2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()

#Source to sink
jpeg("HD2", width = 500, height = 500)
HD2 <- ggplot(Thesis_data_Aug13, aes(x = Light_treatment, y = H.D, 
                                              fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Height : Diameter") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("Medium", "High")), map_signif_level = TRUE, 
              y_position = c(8.4, 8.4))
HD2 + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))
dev.off()


#Correlation plots
#Stem length 
ggscatter(Thesis_data_Aug13, x = "Stem_length_cm", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Root length (cm)")

#Not interesting

ggscatter(Thesis_data_Aug13, x = "Stem_length_cm", y = "Total_leaf_area_cm2", 
          add = "reg.line", conf.int = TRUE,
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Interesting, R = 0.49, p < 0.05

ggscatter(Thesis_data_Aug13, x = "Stem_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Stem diameter (cm)")
#Interesting, R = 0.7, p < 0.0001

ggscatter(Thesis_data_Aug13, x = "Stem_length_cm", y = "Leaf_number", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf number")
#Interesting, R = 0.94, p < 0.0001


ggscatter(Thesis_data_Aug13, x = "Stem_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Leaf area (cm2)")
#Not interesting

ggscatter(Thesis_data_Aug13, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Want to figure out to scale the colors to make it so that the color matches with the light
#R = 0.71, p < 0.001

ggscatter(High, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Close to interesting, R = 0.62, p = 0.056
ggscatter(Medium, x = "Stem_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))") + 
  geom_point(aes(shape = Light_treatment, fill = Light_treatment), size = 2.5, stroke = 2)
#Interesting, R = 0.82, p < 0.01


#Total leaf area 
ggscatter(Thesis_data_Aug13, x = "Total_leaf_area_cm2", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Root length (cm)")
#Not interesting

ggscatter(Thesis_data_Aug13, x = "Total_leaf_area_cm2", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Total leaf area (cm2)", ylab = "Stem diameter (mm)")
#Interesting R = 0.57, p < 0.01

ggscatter(Thesis_data_Aug13, x = "Total_leaf_area_cm2", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Leaf area (cm2)", ylab = "Sink to source (cm)")
#Not interesting

ggscatter(Thesis_data_Aug13, x = "Total_leaf_area_cm2", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting
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


#Root length 
ggscatter(Thesis_data_Aug13, x = "Root_length_cm", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Stem diameter (mm)")
#Not interesting


ggscatter(Thesis_data_Aug13, x = "Root_length_cm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Sink to source (cm)")
#Not interesting 

ggscatter(Thesis_data_Aug13, x = "Root_length_cm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting

ggscatter(Thesis_data_Aug13, x = "Root_length_cm", y = "Leaf_number", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Leaf number")
#Interesting, R = 0.84, p < 0.0001

#Stem diameter
ggscatter(Thesis_data_Aug13, x = "Stem_diameter_mm", y = "Sink_to_source_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "stem diameter (mm)", ylab = "Sink to source (cm)")
#Not interesting 

ggscatter(Thesis_data_Aug13, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem diameter (mm)", ylab = "Light Intensity (umol/(m2*s))")
#R = 0.46, p < 0.05

ggscatter(High, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting
ggscatter(Medium, x = "Stem_diameter_mm", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Stem length (cm)", ylab = "Light Intensity (umol/(m2*s))")
#Not interesting 

ggscatter(Thesis_data_Aug13, x = "Stem_diameter_mm", y = "Leaf_number", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Root length (cm)", ylab = "Leaf number")
#Interesting, R = 0.93, p < 0.0001

#Photosynthesis

#Light 
ggscatter(Thesis_data_Aug13, x = "Photosynthesis", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Not interesting
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


#Height
ggscatter(Thesis_data_Aug13, x = "Photosynthesis", y = "Stem_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem length (cm)")
#Not interesting

#Diameter 
ggscatter(Thesis_data_Aug13, x = "Photosynthesis", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem diameter (mm)")
#Not interesting

#Root length
ggscatter(Thesis_data_Aug13, x = "Photosynthesis", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Root length (cm)")
#Not interesting

#Respiration
#Light 
ggscatter(Thesis_data_Aug13, x = "Respiration", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Interesting, R = 0.51, p < 0.05
ggscatter(High, x = "Respiration", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#Not interesting 
ggscatter(Medium, x = "Respiration", y = "Light_level..umol.m2.s.", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Light Level")
#R = 0.66, p < 0.05


#Height
ggscatter(Thesis_data_Aug13, x = "Respiration", y = "Stem_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem length (cm)")
#Interesting, R = 0.53, p < 0.05

#Diameter 
ggscatter(Thesis_data_Aug13, x = "Respiration", y = "Stem_diameter_mm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Stem diameter (mm)")
#Not interesting

#Root length
ggscatter(Thesis_data_Aug13, x = "Respiration", y = "Root_length_cm", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", 
          xlab = "Photosynthesis", ylab = "Root length (cm)")
#Not interesting

#Total Leaf Area
ggscatter(Thesis_data_Aug13, x = "Respiration", y = "Total_leaf_area_cm2", 
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
hist(Thesis_data_Aug13$Stem_length_cm)
#Right-skewed

#Root length
hist(High$Root_length_cm)
#Normal? This plot is weird
hist(Medium$Root_length_cm)
#Normal?
hist(Low$Root_length_cm)
#Kind of right-skewed? 
hist(Thesis_data_Aug13$Root_length_cm)
#Right-skewed

#Stem diameter
hist(High$Stem_diameter_mm)
#Fairly normal
hist(Medium$Stem_diameter_mm)
#Fairly normal 
hist(Low$Stem_diameter_mm)
#Fairly normal 
hist(Thesis_data_Aug13$Stem_diameter_mm)
#Fairly normal

#Sink to source
hist(High$Sink_to_source_cm)
#Fairly normal
hist(Medium$Sink_to_source_cm)
#Fairly normal
hist(Low$Sink_to_source_cm)
#Left skew 
hist(Thesis_data_Aug13$Sink_to_source_cm)
#Fairly normal 

#STATISTICAL ANALYSES
#==================================================================================================

#Stem length
#ANOVA
stem.aov <- aov(Stem_length_cm ~ Light_treatment, data = Thesis_data_Aug13)
summary(stem.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment   2   1646   822.9   26.91 1.39e-10 ***
#Residuals       137   4190    30.6      

#Tukey multiple pairwise-comparisons
TukeyHSD(stem.aov)
#              diff        lwr       upr     p adj
#Medium-High -3.58 -6.368771 -0.7912289 0.0147455

#Leaf area
#ANOVA
leaf.aov <- aov(Total_leaf_area_cm2 ~ Light_treatment, data = Thesis_data_Aug13)
summary(leaf.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  1   5872    5872   1.634  0.217
#Residuals       18  64689    3594                     

#Tukey multiple pairwise-comparisons
TukeyHSD(leaf.aov)
#diff       lwr        upr     p adj
#Medium-High -34.2702 -90.59557 22.05517 0.2173922

#Root length
#ANOVA
root.aov <- aov(Root_length_cm ~ Light_treatment, data = Thesis_data_Aug13)
summary(root.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  1   0.54  0.5445   0.307  0.586
#Residuals       18  31.93  1.7741                

#Tukey multiple pairwise-comparisons
TukeyHSD(root.aov)
#diff       lwr        upr     p adj
#Medium-High 0.33 -0.9214357 1.581436 0.5863914

#Stem diameter
#ANOVA
diameter.aov <- aov(Stem_diameter_mm ~ Light_treatment, data = Thesis_data_Aug13)
summary(diameter.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  1 0.7144  0.7144    5.12 0.0363 *
#Residuals       18 2.5119  0.1395     

#Tukey multiple pairwise-comparisons
TukeyHSD(diameter.aov)
#          diff        lwr         upr     p adj
#Medium-High -0.378 -0.7289832 -0.02701676 0.0362619

#Sink to source
#ANOVA
sinktosource.aov <- aov(Sink_to_source_cm ~ Light_treatment, data = Thesis_data_Aug13)
summary(sinktosource.aov)
#                 Df Sum Sq Mean Sq F value   Pr(>F)    
#Light_treatment  1   0.42   0.421   0.102  0.753
#Residuals       18  73.85   4.103  

#Tukey multiple pairwise-comparisons
TukeyHSD(sinktosource.aov)
#        diff         lwr       upr     p adj
#Medium-High -0.29 -2.193078 1.613078 0.7525423

#Photosynthesis
#ANOVA
photo.aov <- aov(Photosynthesis ~ Light_treatment, data = Thesis_data_Aug13)
summary(photo.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  1   2.42   2.418   0.173  0.682
#Residuals       18 251.37  13.965  

#Tukey multiple pairwise-comparisons
TukeyHSD(photo.aov)
#         diff        lwr       upr     p adj
#Medium-High -0.6954551 -4.206591 2.815681 0.6822328

#Respiration
#ANOVA
resp.aov <- aov(Respiration ~ Light_treatment, data = Thesis_data_Aug13)
summary(resp.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Light_treatment  1 0.6422  0.6422   12.56 0.00249 **
#Residuals       17 0.8690  0.0511        

#Tukey multiple pairwise-comparisons
TukeyHSD(resp.aov)
#         diff        lwr       upr     p adj
#Medium-High -0.3682169 -0.5873838 -0.1490501 0.0024908


#COMPARING AGES
#==================================================================================================

#Have to re-download the csv
attach(Thesis_data_Aug13)

#Re-filter
High <- Thesis_data_Aug13 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_Aug13 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_Aug13 %>%
  filter(Light_treatment == "Low") 

#Made age a character to allow for plotting
High$Age..J.H. = as.character(High$Age..J.H.)
Medium$Age..J.H. = as.character(Medium$Age..J.H.)
Low$Age..J.H. = as.character(Low$Age..J.H.)

Thesis_data_Aug13$Photosynthesis = as.character(Thesis_data_Aug13$Photosynthesis)

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
#Order the plot 
Thesis_data_Aug13$Light_treatment <- factor (Thesis_data_Aug13$Light_treatment, levels = c("High", "Medium", "Low"))


ggplot(subset(Thesis_data_Aug13, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
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
ggplot(subset(Thesis_data_Aug13, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
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
ggplot(subset(Thesis_data_Aug13, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
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
ggplot(subset(Thesis_data_Aug13, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
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





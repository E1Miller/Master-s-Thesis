#Created by: Elise Miller
#Date started: 08/13/2021
#Date last edited: 08/13/2021
#Description: Total daily C assimilation for Cohort 1 seedlings

#Attach files
attach(Thesis_data_Aug31)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

#Order the plot 
Thesis_data_Aug31$Light_treatment <- factor (Thesis_data_Aug31$Light_treatment, levels = c("High", "Medium", "Low"))

#Filtering the data

High <- Thesis_data_Aug31 %>%
  filter(Light_treatment == "High") 

Medium <- Thesis_data_Aug31 %>%
  filter(Light_treatment == "Medium") 

Low <- Thesis_data_Aug31 %>%
  filter(Light_treatment == "Low") 

#Made age a character to allow for plotting
High$Age..J.H. = as.character(High$Age..J.H.)
Medium$Age..J.H. = as.character(Medium$Age..J.H.)
Low$Age..J.H. = as.character(Low$Age..J.H.)

#SUMMARY 
#==================================================================================================
#All weeks
group_by(Thesis_data_Aug31, Light_treatment) %>%
  summarise(
    mean = mean(Total_carbon_assimilation, na.rm = TRUE), 
    sd = sd(Total_carbon_assimilation, na.rm = TRUE)
  )
#Light_treatment     mean    sd

#1 High            9438. 5316.
#2 Medium          7642. 5020.
#3 Low              140.  210.

summary(Total_carbon_assimilation, is.na = FALSE)

#Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    
#-108.9  1737.8  7412.6  7346.4 11706.0 21130.3   

#High
  group_by(High, Age..J.H.) %>%
    summarise(
      mean = mean(Total_carbon_assimilation, na.rm = TRUE), 
      sd = sd(Total_carbon_assimilation, na.rm = TRUE)
    )
  #Age..J.H.   mean     sd
  #2 29        4300. 2505
  #3 43        9778. 5401.
  #4 50        11135. 5659.
  #5 57        12538. 3415..
  
#Medium
  group_by(Medium, Age..J.H.) %>%
    summarise(
      mean = mean(Total_carbon_assimilation, na.rm = TRUE), 
      sd = sd(Total_carbon_assimilation, na.rm = TRUE)
    )
  #Age..J.H.   mean     sd
  #2 29         2119.  805.
  #3 43        4424. 3926..
  #4 50        9847. 3824
  #5 57        10862. 4489.
  
#Low
  group_by(Low, Age..J.H.) %>%
    summarise(
      mean = mean(Total_carbon_assimilation, na.rm = TRUE), 
      sd = sd(Total_carbon_assimilation, na.rm = TRUE)
    )
  #Age..J.H.   mean     sd
  #3 43        -50.6  34.0
  #4 50         258.  182.


#PLOTS
#==================================================================================================
DailyC_all <- ggplot(Thesis_data_Aug31, aes(x = Light_treatment, y = Total_carbon_assimilation, 
                                              fill = Light_treatment)) + 
  geom_boxplot() + 
  labs(x = "Treatment", y = "Total Daily C Assimilation") + theme_classic() + theme(legend.position = "none", text = element_text(size = 15)) + 
  geom_signif(comparisons = list(c("High","Medium")), map_signif_level = TRUE, 
              y_position = c(24500, 24500)) + 
  geom_signif(comparisons = list(c("High","Low")), map_signif_level = TRUE, 
              y_position = c(22000, 22000)) +
  geom_signif(comparisons = list(c("Low","Medium")), map_signif_level = TRUE, 
              y_position = c(18000, 18000)) 
DailyC_all + scale_fill_manual(values = c("lightgoldenrod1", "darkgoldenrod2", "gray69"))

#COMPARING AGES
#==================================================================================================

ggplot(subset(Thesis_data_Aug31, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57")), 
       aes(x = Age..J.H., y = Total_carbon_assimilation, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Total Daily C Assimilation") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

#STATISTICAL ANALYSES
#==================================================================================================
#Comparing light treatments

  #ANOVA
  TotalC.aov <- aov(Total_carbon_assimilation ~ Light_treatment, data = Thesis_data_Aug31)
  summary(TotalC.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Light_treatment  2 8.531e+08 426569219   18.52 2.17e-07 ***
  #Residuals       84 1.934e+09  23028946  
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(TotalC.aov)
  #         diff        lwr       upr     p adj
  # Medium-High -1796.311  -4467.14   874.519 0.2492983
  # Low-High    -9298.339 -12953.75 -5642.930 0.0000001
  # Low-Medium  -7502.028 -11235.71 -3768.344 0.0000207
  
#Comparing across weeks
  
  #ANOVA for high
  TotalC_H.aov <- aov(Total_carbon_assimilation ~ Age..J.H., data = High)
  summary(TotalC_H.aov)
  #             Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3 390044309 130014770   6.571 0.00117 **
  #Residuals   36 712255120  19784864    
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(TotalC_H.aov)
  #         diff        lwr       upr     p adj
  # 43-29 5478.318   120.9123 10835.723 0.0434476
  # 50-29 6835.167  1477.7610 12192.572 0.0078388
  # 57-29 8237.870  2880.4648 13595.276 0.0010969
  # 50-43 1356.849 -4000.5569  6714.254 0.9032245
  # 57-43 2759.552 -2597.8531  8116.958 0.5152958
  # 57-50 1402.704 -3954.7018  6760.109 0.8943851
  
  #ANOVA for medium
  TotalC_M.aov <- aov(Total_carbon_assimilation ~ Age..J.H., data = Medium)
  summary(TotalC_M.aov)
  #             Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    3 377899034 125966345   8.329 0.000353 ***
  #Residuals   30 453703157  15123439         
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(TotalC_M.aov)
  #         diff        lwr       upr     p adj
  # 43-29 2305.584 -3950.2612  8561.429 0.7493350
  # 50-29 7728.676  1472.8308 13984.521 0.0109011
  # 57-29 8743.515  2487.6704 14999.360 0.0034945
  # 50-43 5423.092   694.1178 10152.066 0.0197530
  # 57-43 6437.932  1708.9574 11166.906 0.0045283
  # 57-50 1014.840 -3714.1346  5743.814 0.9362738
  
  #ANOVA for low
  TotalC_L.aov <- aov(Total_carbon_assimilation ~ Age..J.H., data = Low)
  summary(TotalC_L.aov)
  #                 Df Sum Sq Mean Sq F value Pr(>F)  
  #Age..J.H.    1 293533  293533   13.66 0.00352 **
  #Residuals   11 236293   21481     
  
  #Tukey multiple pairwise-comparisons
  TukeyHSD(TotalC_L.aov)
  #         diff        lwr       upr     p adj
  # 50-43 308.8662 124.9637 492.7687 0.0035225

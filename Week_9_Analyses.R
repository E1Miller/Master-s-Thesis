#Created by: Elise Miller
#Date started: 08/13/2021
#Date last edited: 08/13/2021
#Description: Growth data analyses for week 9 data

#Attach files
attach(Week_9_data)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

High <- Week_9_data %>%
  filter(Light_treatment == "High") 

Medium <- Week_9_data %>%
  filter(Light_treatment == "Medium") 

Low <- Week_9_data %>%
  filter(Light_treatment == "Low") 

#Made age a character to allow for plotting
High$Age..J.H. = as.character(High$Age..J.H.)
Medium$Age..J.H. = as.character(Medium$Age..J.H.)
Low$Age..J.H. = as.character(Low$Age..J.H.)

#Plots

ggplot(subset(Week_9_data, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57", "64")), 
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

ggplot(subset(Week_9_data, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57", "64", "63")), 
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

ggplot(subset(Week_9_data, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57", "64", "63")), 
       aes(x = Age..J.H., y = Total_leaf_area_cm2, group = Age..J.H., colour = Light_treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Light_treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Age (days)") + 
  ylab("Total Leaf Area") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

ggplot(subset(Week_9_data, Light_treatment %in% c("High", "Medium", "Low") & Age..J.H. %in% c("14", "29", "43", "50", "57", "64", "63")), 
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

#Stats
#Stem length
#ANOVA
StemLengthH.aov <- aov(Stem_length_cm ~ Age..J.H., data = High)
summary(StemLengthH.aov)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#Age..J.H.    1   2521  2521.3     152 <2e-16 ***
#Residuals   66   1095    16.6                     

#Tukey multiple pairwise-comparisons
TukeyHSD(StemLengthH.aov)
#              diff        lwr       upr     p adj
#43-29        -0.01116681 -0.3422198  0.31988617 0.9997248
#50-29        -0.39026165 -0.7213146 -0.05920867 0.0155794
#57-29        -0.41067652 -0.7508011 -0.07055191 0.0127920
#50-43        -0.37909484 -0.7101478 -0.04804186 0.0195913
#57-43        -0.39950971 -0.7396343 -0.05938510 0.0160393
#57-50        -0.02041487 -0.3605395  0.31970974 0.9984607


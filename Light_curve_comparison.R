#Created by: Elise Miller
#Date started: 08/18/2021
#Date last edited: 08/18/2021
#Description: Light curve analyses for cohort 1 
#Link: https://github.com/Tomeopaste/AQ_curves/blob/master/AQ_curve_function.R

#Attach files
attach(Light_curve_comparison)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

#Order the plot 
Light_curve_comparison <- group_by(Light_curve_comparison, Treatment)

#Summary 
  #Asat
    group_by(Light_curve_comparison, Treatment) %>%
    summarise(
    mean = mean(Asat, na.rm = TRUE), 
    sd = sd(Asat, na.rm = TRUE)
     )
    #1 High     10.5   1.66
    #2 Low      7.43  3.26
    #3 Medium   13.5   1.37
  #Phi
    group_by(Light_curve_comparison, Treatment) %>%
      summarise(
        mean = mean(Phi, na.rm = TRUE), 
        sd = sd(Phi, na.rm = TRUE)
      )
    #1 High      0.0456 0.00846
    #2 Low       0.0434 0.00805
    #3 Medium    0.0429 0.00599

    #Rd
    group_by(Light_curve_comparison, Treatment) %>%
      summarise(
        mean = mean(Rd, na.rm = TRUE), 
        sd = sd(Rd, na.rm = TRUE)
      )
    #1 High      1.13  0.352
    #2 Low       0.988 0.163
    #3 Medium    1.71  0.204
    
    #Theta
    group_by(Light_curve_comparison, Treatment) %>%
      summarise(
        mean = mean(Theta, na.rm = TRUE), 
        sd = sd(Theta, na.rm = TRUE)
      )
    #1 High      0.854 0.0743
    #2 Low       0.716 0.141 
    #3 Medium    0.771 0.0802
    
    #LCP
    group_by(Light_curve_comparison, Treatment) %>%
      summarise(
        mean = mean(LCP, na.rm = TRUE), 
        sd = sd(LCP, na.rm = TRUE)
      )
    #1 High       25.2  7.82
    #2 Low        24.6  4.54
    #3 Medium     42.2 10.1

#Light saturated net photosynthesis
Asat.aov <- aov(Asat ~ Treatment, data = Light_curve_comparison)
summary(Asat.aov)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#Treatment    2 107.16   53.58   12.19 0.000607 ***
#Residuals   16  70.31    4.39    

TukeyHSD(Asat.aov)
#diff       lwr        upr     p adj
#Low-High    -3.021819 -6.1890229 0.1453857 0.0626743
#Medium-High  3.008776  0.1175268 5.9000243 0.0408035
#Medium-Low   6.030594  2.8633898 9.1977985 0.0004336

#Quantum yield 
Phi.aov <- aov(Phi ~ Treatment, data = Light_curve_comparison)
summary(Phi.aov)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#Treatment    2 0.0000279 1.393e-05   0.247  0.784
#Residuals   16 0.0009036 5.647e-05          

TukeyHSD(Phi.aov)
#diff       lwr        upr     p adj
#Low-High    -0.0021741674 -0.01352823 0.009179891 0.8751106
#Medium-High -0.0026895429 -0.01305433 0.007675247 0.7841278
#Medium-Low  -0.0005153754 -0.01186943 0.010838683 0.9924691

#Mitochondrial respiration in the light 
Rd.aov <- aov(Rd ~ Treatment, data = Light_curve_comparison)
summary(Rd.aov)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#Treatment    2  1.895  0.9477   13.76 0.000334 ***
#Residuals   16  1.102  0.0689 

TukeyHSD(Rd.aov)
#diff       lwr        upr     p adj
#Low-High    -0.1187218 -0.5152202 0.2777766 0.7246314
#Medium-High  0.5981457  0.2361938 0.9600975 0.0016240
#Medium-Low   0.7168674  0.3203690 1.1133658 0.0007152

#The curvature/convexivity factor of the curve
Theta.aov <- aov(Theta ~ Treatment, data = Light_curve_comparison)
summary(Theta.aov)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#Treatment    2 0.05777 0.028887   3.055 0.0752 .
#Residuals   16 0.15126 0.009454  

TukeyHSD(Theta.aov)
#diff       lwr        upr     p adj
#Low-High    -0.13720294 -0.28410892 0.009703036 0.0691150
#Medium-High -0.08263741 -0.21674361 0.051468783 0.2783058
#Medium-Low   0.05456553 -0.09234045 0.201471507 0.6125767

#The light compensation point
LCP.aov <- aov(LCP ~ Treatment, data = Light_curve_comparison)
summary(LCP.aov)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#Treatment    2   1309   654.6   9.865 0.00162 **
#Residuals   16   1062    66.4     

TukeyHSD(LCP.aov)
              #diff       lwr        upr     p adj
#Low-High    -0.6690566 -12.977130 11.63902 0.9892194
#Medium-High 16.9213929   5.685711 28.15708 0.0035445
#Medium-Low  17.5904494   5.282376 29.89852 0.0053428





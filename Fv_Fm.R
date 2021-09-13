#Created by: Elise Miller
#Date started: 08/26/2021
#Date last edited: 08/26/2021
#Description: Comparison of Fv/Fm for Cohort 1 

#Attach files
attach(Fv_Fm)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

Fv_Fm <- Fv_Fm[-c(1),]
#Stats
Fv_Fm.aov <- aov(Fv.Fm ~ Treatment, data = Fv_Fm)
summary(Fv_Fm.aov)

TukeyHSD(Fv_Fm.aov)



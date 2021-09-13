#Created by: Elise Miller
#Date started: 08/18/2021
#Date last edited: 08/18/2021
#Description: Light curve analyses for cohort 1 

#Attach files
attach(Combined_light_curve_data)

#Attach dependencies 
library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)
library(ggsignif)
library(tibble)
library(readr)

#Order the plot 
Combined_light_curve_data <- group_by(Combined_light_curve_data, Treatment)


#Boxplot
ggplot(subset(Combined_light_curve_data, Treatment %in% c("High", "Medium", "Low") & Light_level %in% c("2000", "1800", "1500", "1200", "1000", 
                                                                                                        "800", "500", "240", "120", "60", 
                                                                                                        "30", "15", "0")), 
       aes(x = Light_level, y = A, group = Light_level, colour = Treatment)) + 
  geom_point(alpha = 0.3, position = "jitter") + 
  facet_wrap(~ Treatment) + 
  geom_boxplot(alpha = 0, colour = "black") + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  xlab("Light Level") + 
  ylab("Photosynthesis") + 
  scale_color_manual(breaks = c("High", "Medium", "Low"), 
                     values = c("darkorange", "darkgoldenrod2", "gray50")) + 
  scale_fill_manual(breaks = c("High", "Medium", "Low"), 
                    values = c("darkorange", "darkgoldenrod2", "gray50"))

#Stats
group_by(Combined_light_curve_data, Treatment) %>%
  summarise(
    mean = mean(A, na.rm = TRUE), 
    sd = sd(A, na.rm = TRUE)
  )

A.aov <- aov(A ~ Treatment, data = Combined_light_curve_data)
summary(A.aov)
#             Df Sum Sq Mean Sq F value  Pr(>F)   
#Treatment     2    271  135.31   6.747 0.00141 **
  #Residuals   243   4873   20.05    

TukeyHSD(A.aov)
              #diff       lwr        upr     p adj
#Low-High    -2.0279670 -3.801456 -0.2544783 0.0204190
#Medium-High  0.5219409 -1.063090  2.1069720 0.7177926
#Medium-Low   2.5499079  0.877109  4.2227069 0.0011413

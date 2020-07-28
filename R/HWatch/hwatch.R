library(tidyverse)
library(readxl)
library(vioplot)
library(reshape2)
setwd("~/R/Eurvalve") #set directory
HW <- read_xlsx("~/R/Eurvalve/allWatchData_TAVI.xlsx") #data per beat (so not per measuring point)
df <- tibble(HW)
#reshape database
df <- df %>%
  pivot_wider(names_from = when, values_from = c(age:sdActmin))
#convert slope to numbers
df$slope_pre <- as.numeric(as.character(df$slope_pre))
df$slope_post <- as.numeric(as.character(df$slope_post))

attach(df)
t.test(slope_pre, slope_post, paired = TRUE)
vioplot(slope_pre,slope_post)
summary(slope_pre)
summary(slope_post)
ImproveSlope <- slope_post - slope_pre
View(ImproveSlope)

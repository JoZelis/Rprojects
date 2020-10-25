# can we predict outcome (6mwt improve, QoL improve) from health watch parameters?
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

df$avrhr_pre <- as.numeric(as.character(df$avrhr_pre))
df$avrhr_post <- as.numeric(as.character(df$avrhr_post))
t.test(avrhr_pre, avrhr_post, paired = TRUE)

#numeric everything
i <- c(3, 154)                                  # Specify columns you want to change
df[ , i] <- apply(df[ , i], 2,            # Specify own function within apply
                    function(x) as.numeric(as.character(x)))
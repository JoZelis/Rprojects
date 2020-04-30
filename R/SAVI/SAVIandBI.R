library(tidyverse)
library(plotly) # for interactive plots
setwd("~/R/BuckbergIndex") #set directory
BI <- read_csv("~/R/BuckbergIndex/graosd180709.csv") #data per beat (so not per measuring point)
pt <- read_csv("~/R/BuckbergIndex/aos-stress_csv/aos01_pre.csv") #data per measuring point for patient # pre/post

# filter for one patient pre
BI %>% filter(input == "aos01_pre.csv") -> BI_1pre #filter Netherlands data

# plot LV pressure per beat
ggplot(BI_1pre, aes(x = beat, y = plv, group = input, colour = input)) + 
  geom_line() +
  theme_minimal()
# convert to long format
long_BI_1pre <- BI_1pre %>% gather(Measured, Pressure, plv:pao)
# plot LV and AO pressure per beat
ggplot(long_BI_1pre, aes(x = beat, y = Pressure, group = Measured, colour = Measured)) + 
  geom_line() +
  theme_minimal()

# add a row for ID/measured points with pressure wire
pt <- tibble::rowid_to_column(pt, "ID")
# transform to long
long_pt <- pt %>% gather(Measured, Pressure, Ao:LV)
# make a plot and make it interactive
p1pre <-ggplot(long_pt, aes(x= ID, y = Pressure, group = Measured, colour = Measured)) + 
  geom_line() +
  theme_minimal() +
  xlim(0,1000) # add limitations for part of the beats
ggplotly(p1pre)

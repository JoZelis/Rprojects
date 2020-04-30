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

# make a plot based on this long data with x lim
p1pre <-ggplot(long_pt, aes(x= ID, y = Pressure, group = Measured, colour = Measured)) + 
  geom_line() +
  theme_minimal() +
  xlim(0,1000) # add limitations for part of the beats

# or split dataset by number of rows
long_ptv2 <- long_pt %>%
  group_by(Measured) %>% 
  slice(1:1000) %>% # 1 to 1000 are the first 10 beats
  ungroup()

p1prev2 <-ggplot(long_ptv2, aes(x= ID, y = Pressure, group = Measured, colour = Measured)) + 
  geom_line() +
  theme_minimal()

# find intersections
# Find points where x1 (LV) is above x2 (Ao).
pt <- pt %>% slice(1:1000) # first 10 beats for visible example
x1 <- pt$LV
x2 <- pt$Ao
above <- x1 > x2

# Points always intersect when above=TRUE, then FALSE or reverse
intersect.points <- which(diff(above) != 0)

# Find the slopes for each line segment.
x1.slopes <- x1[intersect.points+1] - x1[intersect.points]
x2.slopes <- x2[intersect.points+1] - x2[intersect.points]

# Find the intersection for each segment.
x.points <- intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes-x2.slopes))
y.points <- x1[intersect.points] + (x1.slopes*(x.points-intersect.points))

# Joint points
joint.points <- which(x1 == x2)
x.points <- c(x.points, joint.points)
y.points <- c(y.points, x1[joint.points])

# Plot points
plot(x1,type='l', col='green')
lines(x2,type='l',col='red')
points(x.points,y.points,col='blue')

# Segment overlap
start.segment <- joint.points[-1][diff(joint.points) == 1] - 1
for (i in start.segment) lines(x = c(i, i+1), y = x1[c(i, i+1)], col = 'blue')



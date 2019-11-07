# Static graphs
library(dplyr)
library(ggplot2)
library(lubridate)

## Directly on data
dat %>% 
  ggplot(aes(Ostkoordinat, Nordkoordinat)) + geom_point() + coord_equal() +
  geom_density2d() +
  facet_wrap(month(dat$Startdatum))

## Using a map underlay
library(ggmap)


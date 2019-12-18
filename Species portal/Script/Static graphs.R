# Static graphs
library(dplyr)
library(ggplot2)
library(lubridate)

## Directly on data
g <- dat %>% 
  ggplot(aes(Ostkoordinat, Nordkoordinat)) + geom_point() + coord_equal() +
  geom_density2d() +
  facet_wrap(month(dat$Startdatum))
g
## Using a map underlay
g + geom_path(aes(X1, X2), data = dat_swe_border, inherit.aes = F)
ggplot(dat_swe_border, aes(X1, X2)) + geom_path()

ggplot() + 
  geom_point(aes(Ostkoordinat, Nordkoordinat), data = dat) +
  geom_path(aes(X1, X2), data = dat_swe_border) +
  theme_bw() + 
  coord_equal()

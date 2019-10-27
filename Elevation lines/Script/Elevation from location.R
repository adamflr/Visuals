# Elevation from location
library(raster)

dat_el <- raster("C:/Users/Adam/Downloads/srtm_39_01/srtm_39_01.tif", ncols = 100, nrows = 100)
#dat_el

dat <- matrix(getValues(dat_el, 1), 1)
for(i in seq(1, 6000, 100)){
  dat <- rbind(dat, getValues(dat_el, i))
}
#dim(dat)

library(dplyr)
library(tidyr)
library(ggplot2)

dat <- dat[, seq(1, 6000, 10)]
dat %>% 
  data.frame() %>% 
  mutate(y = dim(.)[1]:1) %>%
  pivot_longer(-y) %>% 
  mutate(x = as.numeric(substr(name, 2, 10)),
         value = ifelse(is.na(value), 0, value),
         n = y,
         value = value/max(value)) -> dat

ggplot(dat, aes(x, y, fill = value)) + geom_raster() + coord_equal()
  
  

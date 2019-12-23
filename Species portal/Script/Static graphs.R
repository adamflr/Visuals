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

# Swedish border
dat_swe_borders <- as.data.frame(dat_swe@polygons[[1]]@Polygons[[1]]@coords)
dat_swe_borders$id <- 1
dat_swe_borders$hole <- dat_swe@polygons[[1]]@Polygons[[1]]@hole
for(i in 2:36){
  dat <- as.data.frame(dat_swe@polygons[[1]]@Polygons[[i]]@coords)
  dat$id <- i
  dat$hole <- dat_swe@polygons[[1]]@Polygons[[i]]@hole
  dat_swe_borders <- rbind(dat_swe_borders, dat)
}

## Single map w. islands and lakes
ggplot(dat_swe_borders, aes(V1, V2, group = id, fill = hole)) + geom_polygon() + coord_equal()

## Standardized
dat_swe_borders %>% 
  mutate(V1 = (V1 - min(V1)) / (max(V2) - min(V2)),
         V2 = (V2 - min(V2)) / (max(V2) - min(V2))) -> dat_swe_borders

l <- dim(dat_swe_borders)[1]
dat_swe_borders[rep(1:l, 12),] %>% 
  mutate(month = rep(1:12, each = l),
         row = rep(c(1,1,1,2,2,2,3,3,3,4,4,4), each = l),
         col = rep(c(1,2,3,1,2,3,1,2,3,1,2,3), each = l)) -> dat_swe_borders

## Monthly map
ggplot(dat_swe_borders, aes(V1 + col/2, V2 + row, group = paste0(month, "_", id), fill = paste0(month, "_", hole))) +
  geom_polygon(col = "black") +
  coord_equal()

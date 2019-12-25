# Static graphs
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(cowplot)

## Directly on data
g <- dat_sm %>% 
  ggplot(aes(Ostkoordinat, Nordkoordinat)) + geom_point() + coord_equal() +
  geom_density2d() +
  facet_wrap(month(dat_sm$Startdatum))
g

## Using a map underlay
g + geom_path(aes(X1, X2), data = dat_swe_border, inherit.aes = F)

ggplot(dat_swe_border, aes(X1, X2)) + geom_path()

g <- ggplot() + 
  geom_point(aes(Ostkoordinat, Nordkoordinat), data = dat_sm) +
  geom_path(aes(V1, V2, group = id), data = dat_swe_borders) +
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
  mutate(x = (V1 - min(V1)) / (max(V2) - min(V2)),
         y = (V2 - min(V2)) / (max(V2) - min(V2))) -> dat_swe_borders

l <- dim(dat_swe_borders)[1]
r <- 1
c <- 12/r

map_pos <- data.frame(month = 1:12,
                      r = rep(r:1, each = c),
                      c = rep(1:c, r))

stand_swe <- function(x, direction = "east"){
  if (direction == "east"){
    (x - min(dat_swe_borders$V1)) / (max(dat_swe_borders$V2) - min(dat_swe_borders$V2))
  } else{
    (x - min(dat_swe_borders$V2)) / (max(dat_swe_borders$V2) - min(dat_swe_borders$V2))
  }
}

dat_swe_borders[rep(1:l, 12),] %>% 
  mutate(month = rep(1:12, each = l),
         row = rep(rep(1:r, each = c), each = l),
         col = rep(rep(1:c, r), each = l)) -> dat_swe_borders

## Monthly map
g <- ggplot(dat_swe_borders, aes(x + col/2, y + row, group = paste0(month, "_", id), fill = paste0(month, "_", hole))) +
  geom_polygon(col = "black") +
  coord_equal() +
  theme_nothing() + 
  theme(legend.position = "none")
g

## Monthly w. species
dat_sm %>% 
  select(Ostkoordinat, Nordkoordinat, Startdatum) %>% 
  mutate(Startdatum = ymd(Startdatum),
         month = month(Startdatum),
         x = stand_swe(Ostkoordinat, "east"),
         y = stand_swe(Nordkoordinat, "north")) %>% 
  left_join(map_pos, "month") -> dat_sm

g <- g + 
  geom_point(aes(x + c / 2, y + r), inherit.aes = F, data = dat_sm, size = 0.1, col = "red")
g

ggplotly(g)

## Observations per day, per week
obs_per_day <- data.frame(day = ymd("2019-01-01") + 0:364, day_no = 1:365)
dat <- dat_sm %>% count(Startdatum)
merge(obs_per_day, dat, by.x = "day", by.y = "Startdatum", all = T) %>% 
  mutate(n = ifelse(is.na(n), 0, n),
         cumsum = cumsum(n),
         day_no_stand = day_no / 365 - 1/365) -> obs_per_day
obs_per_day$ma_week <- c(rep(0,7), obs_per_day$cumsum[8:365] - obs_per_day$cumsum[1:358])

g <- g + 
  geom_path(aes(day_no_stand * 6 + 0.5, ma_week / (3 * max(ma_week)) + 0.65), inherit.aes = F, data = obs_per_day)
g

# Name of month
g +
  geom_text(aes(x, y, label = month), inherit.aes = F,
            data = data.frame(month = month.name, y = 2.1, x = 1:12 / 2 + 0.25), size = 2.5)

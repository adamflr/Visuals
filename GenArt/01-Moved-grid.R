# A grid which is 'moved' by some vectors
library(tidyverse)
library(patchwork)

# set.seed(20)

dat1 <- expand.grid(x = seq(0, 10, 0.15), y = seq(0, 10, 0.15))

n <- 5
dat_connection <- data.frame(x1 = sample(0:10, n, T),
                             y1 = sample(0:10, n, T))
dat_connection$x2 <- dat_connection$x1 + runif(n, -2, 2)
dat_connection$y2 <- dat_connection$y1 + runif(n, -1, 1)

# dat_connection <- tibble(x1 = c(5, 2, 8), y1 = c(0, 7, 7), x2 = c(5, 8, 2), y2 = c(7, 0, 0))

g1 <- dat1 %>% 
  ggplot(aes(x, y)) +
  geom_path(aes(group = as.character(x))) +
  geom_path(aes(group = as.character(y))) +
  coord_cartesian(xlim = c(-2, 12), ylim = c(-2, 12))

g2 <- ggplot(dat_connection, aes(x = x1, y = y1, xend = x2, yend = y2)) +
  geom_segment(arrow = arrow(angle = 25, type = "closed", length = unit(2.5, "mm"))) +
  geom_point() +
  coord_cartesian(xlim = c(-2, 12), ylim = c(-2, 12))

g1 + g2

# IDW
library(gstat)
library(sf)

dat_connection_sf <- st_as_sf(dat_connection[,1:2], coords = c("x1", "y1"))
dat_connection_sf <- cbind(dat_connection_sf, dat_connection)

dat2 <- st_as_sf(dat1, coords = c("x", "y"))

idp <- 5
dat_pred <- data.frame(x2 = idw(I(x2 - x1) ~ 1, dat_connection_sf, newdata = dat2, idp = idp)$var1.pred,
                       y2 = idw(I(y2 - y1) ~ 1, dat_connection_sf, newdata = dat2, idp = idp)$var1.pred)

dat_pred$x2 <- dat_pred$x2 + dat1$x
dat_pred$y2 <- dat_pred$y2 + dat1$y

dat <- data.frame(dat1, dat_pred)

g3 <- ggplot(dat, aes(x = x, y = y, xend = x2, yend = y2)) +
  geom_segment(arrow = arrow(angle = 25, type = "closed", length = unit(1.5, "mm"))) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2),
               arrow = arrow(angle = 25, type = "closed", length = unit(1.5, "mm")), 
               data = dat_connection, col = "red", size = 1) +
  geom_point(aes(x1, y1), data = dat_connection, col = "red") +
  coord_cartesian(xlim = c(0,11), ylim = c(0, 11))

g1 + g2 + g3

g4 <- dat %>% 
  ggplot(aes(x2, y2)) +
  geom_path(aes(group = as.character(x))) +
  geom_path(aes(group = as.character(y))) +
  coord_cartesian(xlim = c(-2, 12), ylim = c(-2, 12))

g1 + g2 + g4 & theme_void()

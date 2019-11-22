# Elevation lines, gganimate
dat <- data.frame(x = rep(1:10, 4), y = rep(1:4, each = 10), id = 1:20)

library(gganimate)
ggplot(dat, aes(x, y, group = id)) +
  geom_line()

dat$t <- (dat$y %in% c(1,2)) + 1
dat$t[dat$t == 2] <- 2 + seq(0, 1, length.out = sum(dat$t == 2))

ggplot(dat, aes(x, y)) +
  geom_point(group = seq_along(id)) +
  transition_time(t) +
  shadow_mark(past = T, future = F)

## Simplified example with a single row
dat <- data.frame(x = rep(1:5, 2), y = rep(0:1, each = 5))
dat$point_id <- dat$x
dat$t <- dat$y
dat$t[dat$t == 1] <- 1 + seq(0, 1, length.out = 5)

ggplot(dat, aes(x, y, col = as.character(point_id))) +
  geom_point() +
  transition_time(t)

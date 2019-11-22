# Elevation lines, gganimate
dat <- data.frame(x = rep(1:10, 4), y = rep(1:4, each = 10), id = 1:20)

library(dplyr)
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
dat$t[dat$t == 1] <- 1 + seq(-0.5, 0, length.out = 5)

g <- ggplot(dat, aes(x, y, col = as.character(point_id))) +
  geom_point() +
  transition_states(t) +
  shadow_wake(wake_length = 0.5)

animate(g, duration = 5, nframes = 20)

# Expand data frame
dat
t <- unique(dat$t)
dat2 <- expand.grid(x = 1:5, t)
dt <- dat[6:10,]
dat2 <- left_join(dat2, dt, by = "x") %>% 
  arrange(x)
dat2$y <- ifelse(dat2$Var2 >= dat2$t, 1, 0)

{ggplot(dat2, aes(x, y, col = as.character(point_id))) +
  geom_point() +
  transition_time(Var2)} %>% 
  animate(duration = 5, nframes = 40)
# Moving in the right direction. Each point moves independently. Cumbersome data construction tough.

{ggplot(dat2, aes(x, y)) +
    geom_line() +
    transition_time(Var2)} %>% 
  animate(duration = 5, nframes = 40)

# Moving points at particular times
dat <- data.frame(x = 1:3, y = rep(0:1, each = 3), t = c(0,0,0,0.5,1,2), point_id = 1:3)



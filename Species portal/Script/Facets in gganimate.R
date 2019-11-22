# Facets in gganimate
library(gganimate)

dat <- data.frame(x = 1:6, y = 0, class = letters[1:3], t = 1:2, id = 1:6)
g <- ggplot(dat, aes(x, y)) + 
  geom_point() + 
  facet_wrap(~ class)

g <- g +
  transition_time(t)

ggplot(dat, aes(x, y)) + 
  geom_point(aes(group = seq_along(id))) + 
  facet_wrap(~ class) +
  transition_time(t) +
  enter_fade() +
  exit_fade()
# Successfully fades from one to the other within the facets

dat <- data.frame(x = 1:3, y = 0, class = letters[1:3], t = 1:3, id = 1:3)

ggplot(dat, aes(x, y)) + 
  geom_point(aes(group = seq_along(id))) + 
  facet_wrap(~ class) +
  transition_time(t) +
  enter_fade() +
  exit_fade()
# Error

dat1 <- data.frame(x = 1:3, y = NA, class = letters[1:3], t = 0, id = 4:6)
dat2 <- rbind(dat, dat1)
dat2$show <- c(rep(T, 3), rep(F, 3))
dat2

ggplot(dat2, aes(x, y)) + 
  geom_point(aes(group = seq_along(id))) + 
  facet_wrap(~ class) +
  transition_time(t) +
  enter_fade() +
  exit_fade()

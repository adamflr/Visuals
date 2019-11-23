# Facets in gganimate
library(gganimate)
library(dplyr)

dat <- data.frame(x = 1:6, y = 0, class = letters[1:3], t = 1:2, id = 1:6)
g <- ggplot(dat, aes(x, y)) + 
  geom_point() + 
  facet_wrap(~ class)

g <- g +
  transition_time(t)
animate(g, nframes = 20)

{ggplot(dat, aes(x, y)) + 
  geom_point(aes(group = seq_along(id))) + 
  facet_wrap(~ class) +
  transition_time(t) +
  enter_fade() +
  exit_fade()} %>% 
  animate(20)
# Successfully fades from one to the other within the facets

dat$y[c(1,2,6)] <- NA # Re-run above. Removes one of the points

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

# Data with initial NAs?
dat <- data.frame(x = runif(90), y = runif(90), t = rep(1:3, each = 30) + runif(90), f = rep(1:3, each = 30))
dat$point_id <- 1:dim(dat)[1]

# Faux-facets. Strange behaviour with disappering points.
{ggplot(dat, aes(x + f, y)) + 
    geom_point() +
    geom_text(aes(x, y, label = label), data = data.frame(x = 1:3 + 0.5, y = 1.3, label = "textaara")) + 
    geom_vline(xintercept = 1:4) +
    transition_time(t) +
    enter_fade() +
    exit_fade() +
    shadow_mark(past = T) +
    theme_bw()} %>% 
  animate(300, duration = 10)

ggplot(dat, aes(x + t, y)) + 
  geom_point() + 
  transition_time(t) +
  shadow_mark(past = T)

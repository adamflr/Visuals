# Line elevation plots, first trials
source("Elevation lines/Script/Sample data.R")

library(ggplot2)
library(ggdark)

# Sample data ----
## Equal distance between lines, no depth
dat %>% 
  ggplot(aes(x, n + value, group = n)) + geom_line() +
  geom_line(aes(x, n + value, group = x))

## log-distance, no depth
dat %>% 
  ggplot(aes(x, log(n) + value, group = n)) + geom_line() +
  geom_line(aes(x, log(n) + value, group = x))

## Scale length for perspective
dat %>% 
  mutate(ln = -1.2^-n,
         xs = scale(x)/ln,
         value = (value + n)/ln) %>% 
  ggplot(aes(xs, ln + value, group = n)) + geom_line() +
  geom_line(aes(xs, ln + value, group = x)) -> g

g + dark_theme_void()

# Grid data ----
## Equal distance between lines, no depth
dat_grid %>% rename(value = y) %>% 
  ggplot(aes(x, n + value, group = n)) + 
  geom_line() +
  geom_line(aes(x, n + value, group = x)) +
  dark_theme_void()

## Scale length for perspective
dat_grid %>% rename(value = y) %>% 
  mutate(ln = - 1.2^-n,
         xs = scale(x)/ln,
         value = value/ln) %>% 
  ggplot(aes(xs, ln + value, group = n)) + geom_line() +
  geom_line(aes(xs, ln + value, group = x)) +
  dark_theme_void()

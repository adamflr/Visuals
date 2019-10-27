# Line elevation plots, first trials
source("Elevation lines/Script/Sample data.R")

library(ggplot2)
library(ggdark)
library(magrittr)

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
b <- 1.02
sc <- 10
dat %>% 
  mutate(ln = (1 - (1/b)^(n-1))/(1 - 1/b),
         xs = as.vector(scale(x)) / (b^(n - 1)),
         value = ln + sc * value / (b^(n - 1))) %>% 
  ggplot(aes(xs, value, group = n)) + 
  geom_line() +
#  geom_line(aes(group = x), data = . %>% filter(!(x %% 10) | x == 1)) +
#  geom_line(aes(xs, ln)) +
#  geom_line(aes(xs, ln, group = x)) +
  dark_theme_void() -> g
g

#ggsave("Elevation lines/Output/SouthernSweden.pdf", g, width = 10, height = 10)

# Grid data ----
## Equal distance between lines, no depth
dat_grid %>% rename(value = y) %>% 
  ggplot(aes(x, n + value, group = n)) + 
  geom_line() +
  geom_line(aes(x, n + value, group = x)) +
  dark_theme_void()

## Scale length for perspective
b <- 1.7
dat_grid %>% rename(value = y) %>% 
  mutate(ln = (1 - (1/b)^(n - 1))/(1 - 1/b),
         xs = scale(x) / (b^(n - 1)),
         value = ln) %>% 
  ggplot(aes(xs, value, group = n)) + geom_line() +
  geom_line(aes(xs, value, group = x)) +
  dark_theme_void()


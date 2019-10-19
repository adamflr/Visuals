# Sample data
xl <- 20
x <- seq(-1,1,length.out = xl)^2
n <- 10
dat <- matrix(rep(x, n), n, byrow = T)
dat <- dat + apply(matrix(rnorm(xl * n, sd = 0.05), n), 2, cumsum)
dat <- data.frame(dat)

dat$n <- 1:n

library(dplyr)
library(reshape2)
dat %>% 
  melt("n") %>% 
  mutate(x = as.numeric(as.factor(variable))) -> dat

# Simple grid
dat_grid <- expand.grid(x = 1:xl, n = 1:n)
dat_grid$y <- dat_grid$n

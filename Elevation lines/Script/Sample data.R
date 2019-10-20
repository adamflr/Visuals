# Sample data
set.seed(1901019)
xl <- 200
x <- cos(seq(-10, 10,length.out = xl))
n <- 10
dat <- matrix(rep(x, n), n, byrow = T)
dat <- dat + apply(matrix(rnorm(xl * n, sd = 0.0025), n), 2, cumsum)
#dat[,c(1, 2, xl - 1, xl)] <- 0
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

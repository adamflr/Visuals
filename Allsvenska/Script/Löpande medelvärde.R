# Löpande medelvärde
library(tidyverse)
library(ggrepel)
library(extrafont)

dat_long <- read_csv("Allsvenska/Data_out/Alls_matcher_long.csv",
                     col_types = "nnDccncccnn")

lagg <- 10
foo <- function(data, match_num, lag){
  sum(data$match_num > match_num - lagg & data$match_num <= match_num & data$lag == lag)
}

foo2 <- function(data, match_num, lag){
  dat <- data[data$match_num > match_num - lagg & data$match_num <= match_num & data$lag == lag, ]
  sum(dat$status == "seger")
}

dat_long <- dat_long %>% 
  mutate(sasong_num = 1924 + as.numeric(as.factor(sasong))) %>% 
  group_by(lag, sasong) %>% 
  mutate(omgång = 1:n(),
         match_num = sasong_num + omgång / max(omgång),
         match_num_lagg = match_num - lagg) %>% 
  ungroup() %>% 
  group_by(lag) %>% 
  mutate(spelade_matcher = cumsum(hemmamal != -1)) %>%
  mutate(spelade_matcher_år = map2_dbl(match_num, lag, function(x, y) foo(., x, y)),
         vunna_matcher_år = map2_dbl(match_num, lag, function(x, y) foo2(., x, y))) %>% 
  mutate(vunnen_andel = vunna_matcher_år / spelade_matcher_år)

dat_long2 <- dat_long %>% filter(spelade_matcher_år > 10)
ggplot(dat_long2, aes(match_num, vunnen_andel, group = lag)) +
  geom_line(alpha = 0.25) +
  geom_line(data = dat_long2 %>% filter(lag == "Helsingborg"), size = 3, col = "black") +
  geom_line(data = dat_long2 %>% filter(lag == "Helsingborg"), size = 2, col = "red") +
  geom_line(data = dat_long2 %>% filter(lag == "IFK Göteborg"), size = 3, col = "black") +
  geom_line(data = dat_long2 %>% filter(lag == "IFK Göteborg"), size = 2, col = "blue") +
  geom_line(data = dat_long2 %>% filter(lag == "Malmö FF"), size = 3, col = "black") +
  geom_line(data = dat_long2 %>% filter(lag == "Malmö FF"), size = 2, col = "lightblue")
  

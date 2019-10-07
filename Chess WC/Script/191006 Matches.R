# List of matches
library(ggplot2)
g <- ggplot(dat_matches, aes(y = -Year)) + 
  #geom_point() +
  geom_text(aes(-0.1, label = Champion2), hjust = 1) +
  geom_text(aes(0.1, label = `Runner-up`), hjust = 0) +
  geom_text(aes(0, label = Year), size = 1, col = "grey") +
  theme_bw() +
  xlim(-1,1)

library(cowplot)
g + theme_nothing()
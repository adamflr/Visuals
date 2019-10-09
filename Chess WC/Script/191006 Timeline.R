# Timelines
# Packages ----
library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)

dat_matches$Champion2NA <- dat_matches$Champion2
for(i in 2:dim(dat_matches)[1]){
  if (dat_matches$Champion2[i] == dat_matches$Champion2[i-1]) {dat_matches$Champion2NA[i] <- NA}
}

# Timeline with names
dat_matches %>% 
  filter(!is.na(Champion2NA), Status != "FIDE WCC") %>%  
  ggplot(aes(Year, 0)) +
  geom_line(aes(col = Champion2)) +
  geom_text(aes(label = Champion2NA), angle = 90)

# Timeline, wo FIDE 90s ----
g <- dat_matches %>% 
  filter(Status != "FIDE WCC") %>% 
  mutate(EndYear = c(Year[2:length(Year)], 2019),
         cycle = 1:length(Year)) %>% 
  select(Champion2, Year, EndYear, cycle, Status) %>% 
  melt(c("Champion2", "cycle", "Status")) %>% 
  mutate(Champion2 = factor(Champion2, levels = rev(unique(Champion2)), ordered = T)) %>% 
  ggplot(aes(value, Champion2, group = cycle, col = Status)) + 
    geom_line(size = 1) +
    xlab("") + ylab("") +
    theme_bw() +
    scale_color_discrete(breaks = c("Unofficial", "Official WCC",
                                    "Official FIDE WCC", "Classical WCC", "WCC")) +
    theme(legend.title = element_blank())
g
#ggsave("Chess WC/Output/Timeline.pdf", g)

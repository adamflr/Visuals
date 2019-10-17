# Timelines
# Packages ----
library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)
library(cowplot)
library(ggrepel)

dat_matches$Champion2 <- unlist(lapply(strsplit(dat_matches$Champion2, " "), function(x) x[length(x)]))
dat_matches$`Runner-up` <- unlist(lapply(strsplit(dat_matches$`Runner-up`, " "), function(x) x[length(x)]))

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

## Champions, timeline
dat_matches %>% 
  filter(Status != "FIDE WCC", Year > 1900) %>% 
  mutate(ChampionNA = ifelse(c(T, Champion2[-1] != Champion2[-length(Champion2)]),
                             Champion2, NA),
         ChampionOrder = cumsum(!is.na(ChampionNA)),
         ChampionHeight = 0.5 - 0.4 * ChampionOrder/max(ChampionOrder),
         `Runner-up` = ifelse(`Runner-up` == "players", "Tournament", `Runner-up`)) -> dat_temp
g <- ggplot(dat_temp, aes(Year, 0)) + 
  geom_text(aes(label = ChampionNA, y = ChampionHeight), angle = 0, hjust = 0, vjust = 0, nudge_y = 0.01) +
  geom_text(aes(label = `Runner-up`, x = Year + c(0,0,-0.5,0.5,rep(0, dim(dat_temp)[1] - 4))), angle = 90, hjust = 1, vjust = 0.3, nudge_y = -0.015) +
  geom_line(aes(x, y, group = x), data = data.frame(x = rep(dat_temp$Year[!is.na(dat_temp$ChampionNA)], 2),
                                                    y = c(dat_temp$ChampionHeight[!is.na(dat_temp$ChampionNA)], rep(0, 18)))) +
  geom_line(aes(x, y, group = gr), data = data.frame(x = c(dat_temp$Year[!is.na(dat_temp$ChampionNA)], dat_temp$Year[!is.na(dat_temp$ChampionNA)][-1] - 1, 2019), 
                                                     gr = 1:18,
                                                     y = dat_temp$ChampionHeight[!is.na(dat_temp$ChampionNA)])) +
  geom_point(aes(col = is.na(ChampionNA)), size = 5) +
  geom_text(aes(label = Year), size = 1.5, col = ifelse(is.na(dat_temp$ChampionNA), "white", "black")) + 
  ylim(-0.3, 0.6) +
  theme(legend.position = "none") +
  scale_color_manual(values = c("red", "black")) +
  theme_nothing()

ggsave("Chess WC/Output/Timeline2.pdf", g, width = 25, height = 10)

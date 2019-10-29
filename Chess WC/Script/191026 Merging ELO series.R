# Merging rating series
library(dplyr)
library(ggplot2)
library(plotly)

# Read and transform to similar tables ----
read.csv("Chess WC/Data/dat_elo_10.csv", stringsAsFactors = F) %>% 
  select(-X) %>% 
  mutate(Source = "chessmetrics",
         `Last name` = purrr::map_chr(Player, function(x) rev(strsplit(x, " ")[[1]])[1])) -> dat_cm

read.csv("Chess WC/Data/dat_elo_fide.csv", stringsAsFactors = F) %>% 
  select(-X) %>% 
  mutate(Source = "fide",
         `Last name` = purrr::map_chr(Player, function(x) strsplit(x, ",")[[1]][1])) %>% 
  filter(Ranking < 11) -> dat_fide

# Join and compare ----
dat <- full_join(dat_cm, dat_fide, by = c("Last name", "Year", "Month"))

dat %>% 
  filter(!is.na(Player.x) & !is.na(Player.y)) %>%
  ggplot(aes(Rating.y, Rating.x)) + 
  geom_point() + 
  geom_abline(aes(intercept = 0, slope = 1)) +
  xlab("FIDE ELO") + ylab("Chessmetrics")

dat %>% 
  filter(!is.na(Player.x) & !is.na(Player.y)) %>%
  lm(formula = Rating.x ~ Rating.y) %>% summary()

dat %>% 
  filter(!is.na(Player.x) & !is.na(Player.y)) %>%
  lm(formula = I(Rating.x - Rating.y) ~ 1) %>% summary() # FIDE rating about 15 points below chessmetrics

# Append datasets, FIDE from first available ranking (July 2000) ----
dat_cm %>% 
  filter(Year + (Month - 1) / 12 < 2000.5) %>% 
  rbind(dat_fide) %>% 
  arrange(desc(Year), desc(Month)) -> dat

ggplot(dat %>% filter(Ranking %in% c(1,5,10)), aes(Year + (Month - 1) / 12, Rating, group = Ranking, col = Source)) +
  geom_line()

# Identify World Champion per year and month ----
# Read dat_matches from script 191006 Data import.R
source("Chess WC/Script/191006 Data import.R")
dat_matches$Champion2 <- unlist(lapply(strsplit(dat_matches$Champion2, " "), function(x) x[length(x)]))
# dat_matches lacks dates, assumes July since middle of year
dat_champ <- dat_matches %>% 
  select(Year, Champion, Champion2, Status) %>% 
  filter(Status != "FIDE WCC") %>% 
  mutate(defended = c(F, Champion2[-1] == rev(rev(Champion2)[-1]))) %>% 
  filter(!defended) %>% 
  group_by(Champion2) %>% 
  mutate(`Returning champion` = cumsum(Champion2 == Champion2),
         `Unique reign` = paste(Champion2, `Returning champion`)) %>% 
  ungroup()

foo <- function(time){
  dat <- dat_champ %>% filter(Year <= time) %>% pull(`Unique reign`) %>% rev()
  dat[1]
}

dat %>% 
  mutate(Champion = purrr::map_chr(Year + (Month - 1) / 12, foo),
         `Champion status` = ifelse(`Last name` == substr(Champion, 1, nchar(Champion) - 2), "Champion", "Not champion")) -> dat

write.csv(dat, "Chess WC/Data/merged_elo_series.csv")

ggplot(dat) +
#  geom_vline(aes(xintercept = Year + 0.5), data = dat_champ %>% filter(Year > 1900), col = "grey") +
  geom_line(aes(Year + (Month - 1) / 12, Rating, group = Ranking), alpha = 0.2) +
  geom_smooth(aes(Year + (Month - 1) / 12, Rating, group = Champion), 
            data = dat %>% filter(`Champion status` == "Champion"), col = "red", se = F) +
#  xlim(1996,2006) +
#  scale_color_manual(values = c("Red", "Black")) +
  theme_bw() +
  geom_vline(xintercept = c(1975, 1972, 1985))

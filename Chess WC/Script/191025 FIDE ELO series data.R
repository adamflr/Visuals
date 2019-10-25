# Harvest for ELO series from fide.org. Gives ELO series from july 2000 to oct 2019
library(rvest)
library(dplyr)

dat_elo <- data.frame()

for(i in seq(1, 565, 4)){
  url <- paste0("https://ratings.fide.com/toparc.phtml?cod=", i)
  site <- read_html(url)
  header <- html_nodes(site, "table")[3] %>% html_table() %>% unlist()
  html_nodes(site, "table")[5] %>% 
    html_table(fill = T, header = T) %>% 
    data.frame() %>%
    mutate(Header = as.vector(header[1])) %>% 
    bind_rows(dat_elo) -> dat_elo
  print(i)
}

dat_elo_harvest <- dat_elo
#View(dat_elo)

dat_elo %>% 
  mutate(Year = as.numeric(substr(Header, nchar(Header) - 13, nchar(Header) - 10)),
         Month_text = purrr::map_chr(Header, function(x) strsplit(x, " ")[[1]][4]),
         Month = as.numeric(factor(Month_text, levels = month.name))) -> dat_elo

# Previous data has columns Ranking, Player, Rating, Year, Month
# Names where "First name" "Last name", rather than "Last name", "First name"
dat_elo %>% 
  rename(Player = Name, Ranking = Rank) %>% 
  select(Ranking, Player, Rating, Year, Month) -> dat_elo

write.csv(dat_elo, "Chess WC/Data/dat_elo_fide.csv")

library(ggplot2)
dat_elo %>% filter(Ranking < 11) %>% 
ggplot(aes(x = Year + (Month - 1) / 12, y = Rating, group = Ranking)) +
  geom_line(col = "grey80") + 
  geom_line(aes(group = Player), data = dat_elo %>% filter(Player == "Carlsen, Magnus"), size = 2) +
  theme(legend.position = "none") + 
  theme_bw()

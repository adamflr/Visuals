# Läsning av filer
library(rvest)
library(tidyverse)
library(purrr)

dat_temp <- read_csv("\n", c("hemma", "borta", "hemmamal", "bortamal", 
                 "publik", "domare", "datum", "sasong"))

fileslist <- list.files("Allsvenska/Data", full.names = T)
for(filename in fileslist){
html <- xml2::read_html(filename)

dat_sasong <- filename %>% 
  gsub(pattern = "Allsvenska/Data/Alls_", replacement = "") %>% 
  gsub(pattern = ".txt", replacement = "")

dat_dates <- html %>% 
  html_nodes(".everysport-date-label") %>% 
  html_text()

dat_n_games <- html %>% 
  html_nodes(".everysport-date") %>% 
  map(function(x) html_nodes(x, ".everysport-event")) %>% 
  map(function(x) length(x)) %>% 
  unlist()

dat_matcher <- html %>% 
  html_nodes(".everysport-event") %>% 
  map(clean_event_class) %>% 
  tibble() %>% 
  unnest(cols = c(.)) %>% 
  mutate(datum = rep(dat_dates, dat_n_games),
         sasong = dat_sasong)

dat_temp <- bind_rows(dat_temp, dat_matcher)

print(dat_sasong)
}

# write_csv(dat_temp, "Allsvenska/Data_out/Alls_matcher.csv")

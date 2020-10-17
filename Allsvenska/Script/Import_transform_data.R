# Läsning av filer
library(rvest)
library(tidyverse)
library(purrr)
library(stringi)

# Inläsning av nedladdade filer med matcher 1924-2016, exklusive 1933-34 ----
dat_temp <- read_csv("\n", c("hemma", "borta", "hemmamal", "bortamal", 
                 "publik", "domare", "datum", "sasong"))

fileslist <- list.files("Allsvenska/Data/old.allsvenskan/", full.names = T)
for(filename in fileslist){
html <- xml2::read_html(filename)

dat_sasong <- filename %>% 
  gsub(pattern = "Allsvenska/Data/old.allsvenskan/Alls_", replacement = "") %>% 
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

# Inläsning av 1933-34. Saknad i old.allsvenskan.se. Kopierad från någon sida
alls_33_34 <- readLines("Allsvenska/Data/Alls_1933_1934_raw.txt", encoding = "UTF-8")

alls_33_34 <- tibble(raw = alls_33_34) %>% 
  filter(substr(raw, 1, 4) != "Omgå") %>% 
  mutate(day_no = cumsum(substr(raw, 1, 5) != "00:00")) %>% 
  group_by(day_no) %>% 
  mutate(day = first(raw)) %>% 
  ungroup() %>% 
  filter(substr(raw, 1, 5) == "00:00") %>% 
  mutate(day = gsub(",.*", "", day),
         raw = gsub("00:00 ", "", raw),
         home = gsub(" -.*", "", raw),
         home_goals = stri_extract_first(raw, regex = "\\d+"),
         away_goals = stri_extract_last(raw, regex = "\\d+"),
         away = substr(raw, nchar(home) + 4, nchar(raw) - 5)) %>% 
  `names<-`(c("raw", "day_no", "datum", "hemma", "hemmamal", "bortamal", "borta"))

alls_33_34 <- alls_33_34 %>% 
  mutate(publik = NA, domare = NA, sasong = "1933_1934") %>% 
  select(hemma, borta, hemmamal, bortamal, publik, domare, datum, sasong)

dat_temp <- bind_rows(dat_temp, alls_33_34) %>% arrange(sasong)

write_csv(dat_temp, "Allsvenska/Data_out/Alls_matcher.csv")
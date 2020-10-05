# Skapa serietabell
library(tidyverse)

dat_match <- read_csv("Allsvenska/Data_out/Alls_matcher.csv")

mål_säsong <- 1968

dat_match %>% 
  select(-domare, -publik, datum) %>% 
  mutate(segrare = ifelse(hemmamal == bortamal, "lika", 
                         ifelse(hemmamal > bortamal, "hemma", "borta")),
         id = 1:n()) %>% 
  pivot_longer(-c(hemmamal, bortamal, datum, sasong, segrare, id), names_to = "plats", values_to = "lag") %>% 
  mutate(status = ifelse(plats == segrare, "seger",
                         ifelse(segrare == "lika", "lika", "förlust"))) %>% 
  filter(sasong == mål_säsong) %>% 
  count(lag, status) %>% 
  pivot_wider(names_from = status, values_from = n, values_fill = 0) %>% 
  mutate(poäng = lika + 2 * seger) %>% 
  arrange(-poäng) %>% 
  select(lag, seger, lika, förlust, poäng)

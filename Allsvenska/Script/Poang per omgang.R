# Poäng per lag och omgång
library(tidyverse)
library(ggrepel)
library(extrafont)

dat_match <- read_csv("Allsvenska/Data_out/Alls_matcher.csv",
                      col_types = "ccnnnccDc")

dat_long <- dat_match %>% 
  select(-domare, -publik, -arena) %>% 
  mutate(segrare = ifelse(hemmamal == bortamal, "lika", 
                          ifelse(hemmamal > bortamal, "hemma", "borta")),
         id = 1:n()) %>% 
  pivot_longer(-c(hemmamal, bortamal, datum, sasong, segrare, id), 
               names_to = "plats", values_to = "lag") %>% 
  mutate(status = ifelse(plats == segrare, "seger",
                         ifelse(segrare == "lika", "lika", "förlust")),
         gjorda_mål = ifelse(plats == "hemma", hemmamal, bortamal),
         insläppta_mål = ifelse(plats == "hemma", bortamal, hemmamal))

# Poäng per säsong och omgång
dat_long %>% 
  group_by(sasong, lag) %>% 
  mutate(omgång = 1:n(),
         poäng = cumsum(3 * (status == "seger") + (status == "lika")),
         poängsnitt = poäng / omgång,
         gjorda_mål_sum = cumsum(gjorda_mål),
         insläppta_mål_sum = cumsum(insläppta_mål),
         differens = gjorda_mål_sum - insläppta_mål_sum) %>% 
  ungroup() %>% 
  group_by(sasong, omgång) %>% 
  arrange(desc(poäng), desc(differens), desc(gjorda_mål_sum), desc(lag)) %>% 
  mutate(position = 1:n()) %>% 
  ungroup() %>% 
  group_by(sasong) %>% 
  filter(sasong >= 1990, omgång == max(omgång), position == 2) %>% 
  arrange(poängsnitt) %>% select(lag, omgång, position, poäng, poängsnitt) %>% print(n = 1000)
  

# Skapa serietabell
library(tidyverse)

dat_match <- read_csv("Allsvenska/Data_out/Alls_matcher.csv")

mål_säsong <- 2010

dat_match %>% 
  select(-domare, -publik, datum) %>% 
  mutate(segrare = ifelse(hemmamal == bortamal, "lika", 
                          ifelse(hemmamal > bortamal, "hemma", "borta")),
         id = 1:n()) %>% 
  pivot_longer(-c(hemmamal, bortamal, datum, sasong, segrare, id), names_to = "plats", values_to = "lag") %>% 
  mutate(status = ifelse(plats == segrare, "seger",
                         ifelse(segrare == "lika", "lika", "förlust")),
         gjorda_mål = ifelse(plats == "hemma", hemmamal, bortamal),
         insläppta_mål = ifelse(plats == "hemma", bortamal, hemmamal)) %>% 
  filter(sasong == mål_säsong) %>% 
  group_by(lag) %>% 
  summarise(vunna = sum(status == "seger"),
            oavgjorda = sum(status == "lika"),
            förluster = sum(status == "förlust"),
            gjorda_mål = sum(gjorda_mål),
            insläppta_mål = sum(insläppta_mål)) %>% 
  mutate(måldifferens = gjorda_mål - insläppta_mål,
         poäng = oavgjorda + 3 * vunna) %>% 
  arrange(-poäng, - måldifferens)

unique(dat_match$sasong)[c(1, unique(dat_match$sasong) %>% substr(1,4) %>% as.numeric() %>% diff()) != 1]

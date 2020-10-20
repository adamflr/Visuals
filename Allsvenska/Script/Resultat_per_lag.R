# Maratontabell
library(tidyverse)

dat_match <- read_csv("Allsvenska/Data_out/Alls_matcher.csv")

dat_long <- dat_match %>% 
  select(-domare, -publik) %>% 
  mutate(segrare = ifelse(hemmamal == bortamal, "lika", 
                          ifelse(hemmamal > bortamal, "hemma", "borta")),
         id = 1:n()) %>% 
  pivot_longer(-c(hemmamal, bortamal, datum, sasong, segrare, id), names_to = "plats", values_to = "lag") %>% 
  mutate(status = ifelse(plats == segrare, "seger",
                         ifelse(segrare == "lika", "lika", "förlust")),
         gjorda_mål = ifelse(plats == "hemma", hemmamal, bortamal),
         insläppta_mål = ifelse(plats == "hemma", bortamal, hemmamal)) %>% 
  arrange(datum) %>% 
  group_by(lag, sasong) %>% 
  mutate(vunna = cumsum(status == "seger"),
         oavgjorda = cumsum(status == "lika"),
         förluster = cumsum(status == "förlust"),
         gjorda_mål = cumsum(gjorda_mål),
         insläppta_mål = cumsum(insläppta_mål),
         omgång = 1:n()) %>% 
  mutate(måldifferens = gjorda_mål - insläppta_mål,
         poäng = oavgjorda + 2 * vunna) %>% 
  ungroup()

g1 <- dat_long %>% 
  ggplot(aes(omgång, poäng, group = lag)) +
  geom_line(col = "grey20") +
  geom_line(data = . %>% filter(lag == "Malmö"), col = "lightblue", size = 3) +
  facet_wrap(~ gsub("_", "-", sasong), scale = "free_x", strip.position = "left")
g1

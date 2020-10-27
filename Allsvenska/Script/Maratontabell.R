# Maratontabell
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
         insläppta_mål = ifelse(plats == "hemma", bortamal, hemmamal)) %>% 
  arrange(datum) %>% 
  group_by(lag) %>% 
  mutate(vunna = cumsum(status == "seger"),
         oavgjorda = cumsum(status == "lika"),
         förluster = cumsum(status == "förlust"),
         gjorda_mål = cumsum(gjorda_mål),
         insläppta_mål = cumsum(insläppta_mål),
         omgång = 1:n()) %>% 
  mutate(måldifferens = gjorda_mål - insläppta_mål,
         poäng = oavgjorda + 3 * vunna) %>% 
  ungroup() %>% 
  mutate(sasong_rank = as.numeric(as.factor(sasong))) %>% 
  group_by(sasong, lag) %>% 
  mutate(sasong_omgång = 0:(n() - 1) / n()) %>% 
  ungroup() %>% 
  mutate(sasong_omgång = sasong_rank + sasong_omgång) %>% 
  select(-sasong_rank)

g1 <- ggplot(dat_long, aes(sasong_omgång + 1924, poäng, group = lag)) +
  geom_line() +
  geom_line(data = dat_long %>% filter(lag == "Malmö"), col = "#0091d4", size = 2) +
  geom_point(data = dat_long %>% group_by(lag) %>% filter(sasong == 2016 & omgång == max(omgång))) +
  geom_text_repel(aes(label = lag),
                  data = dat_long %>% group_by(lag) %>% filter(sasong == 2016 & omgång == max(omgång)),
                  hjust = 0, direction = "y", nudge_x = 5, family = "Garamond") +
  theme_minimal() +
  theme(legend.position = "none", text = element_text(family = "Garamond")) +
  xlim(1925, 2030) +
  labs(x = "År", y = "Poäng")
g1

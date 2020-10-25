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
  group_by(lag, sasong) %>% 
  mutate(vunna = cumsum(status == "seger"),
         oavgjorda = cumsum(status == "lika"),
         förluster = cumsum(status == "förlust"),
         gjorda_mål = cumsum(gjorda_mål),
         insläppta_mål = cumsum(insläppta_mål),
         omgång = 1:n()) %>% 
  mutate(måldifferens = gjorda_mål - insläppta_mål,
         poäng = oavgjorda + 3 * vunna) %>% 
  ungroup()

# Samtliga säsonger och lag
g1 <- dat_long %>% 
  ggplot(aes(omgång, poäng, group = lag)) +
  geom_line(col = "grey20") +
  geom_line(data = . %>% filter(lag == "Malmö"), col = "lightblue", size = 3) +
  facet_wrap(~ gsub("_", "-", sasong), scale = "free_x", strip.position = "left")
# g1

# Säsongslinjediagram
dat_long <- dat_long %>% 
  left_join(dat_teams_col %>% rename("lag" = "Lag")) %>% 
  mutate(Color = ifelse(is.na(Color), "grey20", Color)) %>% 
  filter(sasong == 2010)

dat_long <- dat_long %>% 
  arrange(lag)

dat_long <- rbind(dat_long, dat_long %>% filter(omgång == 1) %>% 
                    mutate(omgång = 0, poäng = 0))

g2 <- ggplot(dat_long, aes(omgång, poäng, col = lag)) +
  # geom_smooth(size = 2, span = 0.2, se = F) +
  geom_line(size = 2, alpha = 0.75) +
  geom_point(data = dat_long) +
  geom_point(aes(x = omgång + 1), data = dat_long %>% filter(omgång == max(omgång))) +
  geom_text_repel(aes(x = omgång + 1, label = paste0(lag, ", ", poäng, "p")), 
                  data = dat_long %>% filter(omgång == max(omgång)), 
                  hjust = 0, direction = "y", nudge_x = 1, family = "Garamond") +
  scale_color_manual(values = dat_long %>% filter(omgång == 0) %>% pull(Color)) +
  facet_wrap(~ gsub("_", "-", sasong), scale = "free_x", strip.position = "top") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(family = "Garamond")) +
  coord_cartesian(xlim = c(0, 40)) +
  xlab("Omgång") + ylab("Poäng")
g2

# Justerat mot näst-bästa lag
mål_lag <- "Helsingborg"
dat_long <- dat_long %>% 
  group_by(omgång) %>% 
  mutate(poäng_just = poäng - max(poäng[lag != mål_lag])) %>% 
  ungroup()

g2 <- ggplot(dat_long, aes(omgång, poäng_just, col = lag)) +
  # geom_smooth(size = 2, span = 0.2, se = F) +
  geom_line(size = 2, alpha = 0.75) +
  geom_point(data = dat_long) +
  geom_point(aes(x = omgång + 1), data = dat_long %>% filter(omgång == max(omgång))) +
  geom_text_repel(aes(x = omgång + 1, label = paste0(lag, ", ", poäng, "p")), 
                  data = dat_long %>% filter(omgång == max(omgång)), 
                  hjust = 0, direction = "y", nudge_x = 1, family = "Garamond") +
  scale_color_manual(values = dat_long %>% filter(omgång == 0) %>% pull(Color)) +
  facet_wrap(~ gsub("_", "-", sasong), scale = "free_x", strip.position = "top") +
  theme_bw() +
  theme(legend.position = "none", text = element_text(family = "Garamond")) +
  coord_cartesian(xlim = c(0, 40))
g2
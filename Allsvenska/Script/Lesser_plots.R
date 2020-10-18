library(tidyverse)

dat_match <- read_csv("Allsvenska/Data_out/Alls_matcher.csv")

dat_match %>% 
  count(sasong) %>% 
  ggplot(aes(sasong, n)) + geom_point() + coord_flip()

# Resultat för enskilt lag
mållag <- "Malmö"

dat_temp <- dat_match %>% 
  filter(hemma == mållag | borta == mållag) %>% 
  mutate(utfall = ifelse(hemmamal > bortamal, "hemma", ifelse(hemmamal == bortamal, "lika", "borta")),
         mållag_hemma_borta = ifelse(hemma == mål_lag, "hemma", "borta"),
         mållag_utfall = ifelse(utfall == "lika", "lika", ifelse(mållag_hemma_borta == utfall, "vinst", "förlust")),
         mållag_utfall_num = as.numeric(factor(mållag_utfall, levels = c("förlust", "lika", "vinst"))) - 2,
         utfall_cumsum = cumsum(mållag_utfall_num),
         matchid = 1:n())

dat_temp %>%   
  ggplot(aes(datum, utfall_cumsum, color = sasong)) + 
  geom_line(size = 1) + 
  theme_minimal() +
  theme(legend.position = "none")

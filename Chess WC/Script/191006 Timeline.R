# Timelines
dat_matches$Champion2NA <- dat_matches$Champion2
for(i in 2:dim(dat_matches)[1]){
  if (dat_matches$Champion2[i] == dat_matches$Champion2[i-1]) {dat_matches$Champion2NA[i] <- NA}
}

library(dplyr)
dat_matches %>% 
  filter(!is.na(Champion2NA), Status != "FIDE WCC") %>%  
  ggplot(aes(Year, 0)) +
  geom_line(aes(col = Champion2)) +
  geom_text(aes(label = Champion2NA), angle = 90) 
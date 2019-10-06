# Data import ----
a <- readLines("Chess WC/Data/wc_matches.csv", encoding = "UTF-8")

dat_matches <- c(a[1], a[substr(a, 1, 1) %in% c(1:2)])
dat_matches <- read.csv(text = dat_matches, sep = ";", stringsAsFactors = F)

names(dat_matches) <- c("Year", "Host country", "Host city", "Champion", 
                        "Runner-up", "Won", "Lost", "Draw", "Format", "Comment 1", "Comments 2")

dat_matches$`Status` <- rep(c("Unofficial", "Official WCC", "Official FIDE WCC", "Classical WCC", "FIDE WCC", "WCC"),
                            c(9, 16, 19, 4, 8, 9))

dat_matches$Champion[40] <- "Anatoly Karpov"
dat_matches$`Runner-up`[40] <- "Garry Kasparov"

dat_matches$Champion2 <- ifelse(grepl("(", dat_matches$Champion, fixed = T), 
                                substr(dat_matches$Champion, 1, vapply(dat_matches$Champion, nchar, 1) - 4),
                                dat_matches$Champion)
#View(dat_matches)

# List of matches ----
library(ggplot2)
ggplot(dat_matches, aes(y = -Year)) + 
  #geom_point() +
  geom_text(aes(-0.1, label = Champion2), hjust = 1) +
  geom_text(aes(0.1, label = `Runner-up`), hjust = 0) +
  geom_text(aes(0, label = Year), size = 1, col = "grey") +
  theme_bw() +
  xlim(-1,1)

# Timeline of champion
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
  

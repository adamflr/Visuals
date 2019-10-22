# Harvest for ELO time series
library(rvest)
library(dplyr)

#year <- 1900
#month <- "01"
dat_elo <- data.frame()

for(year in 1900:2004){
  for(month in c(paste0("0", 1:9), "10", "11", "12")){
    url <- paste0("http://www.chessmetrics.com/cm/CM2/SingleMonth.asp?Params=190010SSSSS3S000000", 
                  year, month, "111000000000000010100")
    
    #url <- "http://www.chessmetrics.com/cm/CM2/SingleMonth.asp?Params=190010SSSSS3S000000190001111000000000000010100"
    site <- read_html(url)
    #str(site)
    html_nodes(site, "table")[4] %>% 
      html_table(fill = T) %>% 
      data.frame() %>%
      select(-X4, -X5) %>% 
      filter(X2 != "Player Name" & X2 != "") %>% 
      rename("Ranking" = X1, "Player" = X2, "Rating" = X3) %>% 
      mutate(Year = year, Month = month) %>% 
      bind_rows(dat_elo) -> dat_elo
  }
  print(year)
}

library(ggplot2)
library(stringr)
dat_elo %>% 
  mutate(Ranking = as.numeric(str_replace(Ranking, "#", ""))) %>% 
  mutate(Month = as.numeric(Month), Rating = as.numeric(Rating)) %>% 
  filter(Ranking <= 5) %>% 
  ggplot(aes(Year + (Month - 1)/12, Rating, col = as.character(Ranking))) + geom_point(alpha = 0.1) + geom_smooth(se = F)

write.csv(dat_elo, "Chess WC/Data/dat_elo.csv")

dat_elo %>% 
  mutate(Ranking = as.numeric(str_replace(Ranking, "#", ""))) %>% 
  mutate(Month = as.numeric(Month), Rating = as.numeric(Rating)) %>% 
  filter(Ranking <= 10) %>% 
  write.csv("Chess WC/Data/dat_elo_10.csv")

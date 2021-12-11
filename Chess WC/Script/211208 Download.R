path <- "http://ratings.fide.com/download/standard_may21frl.zip"

library(stringr)
library(tidyverse)
dat <- expand_grid(year = 19:21, month = lubridate::month(1:12, label = T, abbr = T)) %>% 
  mutate(month = as.character(month),
         month = ifelse(month == "maj", "may", month),
         month = ifelse(month == "okt", "oct", month),
         mo_ye = paste0(month, year),
         path = paste0("http://ratings.fide.com/download/standard_", mo_ye, "frl.zip"),
         month_no = rep(1:12, 3))

for(i in 9:36){
  download.file(dat$path[i], paste0("fide_", dat$mo_ye[i], ".zip"))
}

i <- dat$path[1]

# Extract data
dat2 <- tibble(zipfiles = paste0("Chess WC/Data/Download/", list.files("Chess WC/Data/Download/")),
               month_year = gsub("(fide_|\\.zip)", "", list.files("Chess WC/Data/Download/")),
               inner_file = paste0("standard_", month_year, "frl.txt")) %>% 
  mutate(data = map2(zipfiles, inner_file, ~ readLines(unz(.x, .y))))

# dat3 <- readLines(unz("Chess WC/Data/Download/fide_apr19.zip", "standard_apr19frl.txt"))

foo <- function(dat3){
  first_row <- dat3[1]
  first_row <- sub(" ", "_", first_row)
  
  startinglocal <- str_locate_all(first_row, " [[:alpha:]]")[[1]] %>% as.data.frame()
  colnames <- trimws(substring(first_row, c(1, startinglocal$end), c(startinglocal$start, nchar(first_row))))
  colnames[9] <- "ELO"
  
  dat4 <- tibble(text = dat3[-1]) %>% 
    mutate(split = map(text, ~ substring(.x, c(1, startinglocal$end), c(startinglocal$start, nchar(.x))))) %>% 
    select(-text) %>% 
    unnest(split)
  
  dat4 %>% 
    mutate(split = trimws(split),
           col = rep(colnames, n() / 13),
           id = rep(1:(n() / 13), each = 13)) %>% 
    pivot_wider(names_from = col, values_from = split) %>% 
    mutate_at(c(2, 10, 13), as.numeric) %>% 
    arrange(-ELO) %>% 
    slice(1:30) %>% 
    filter(!grepl("Kasparov", Name)) %>% 
    slice(1:10)
}

dat5 <- dat2 %>% 
  mutate(topten = map(data, foo)) %>% 
  select(-zipfiles, -inner_file, -data) %>% 
  unnest(topten)


dat5 <- dat5 %>% 
  left_join(dat %>% select(-path, -month) %>% rename("month_year" = "mo_ye"), by = "month_year")

dat6 <- dat5 %>% 
  select(Name, ELO, year, month_no) %>% 
  rename("Player" = "Name", "Rating" = "ELO", "Year" = "year", "Month" = "month_no") %>% 
  mutate(Year = paste0(20, Year),
         Source = "fide") %>% 
  group_by(Year, Month) %>% 
  mutate(Ranking = 1:n()) %>% 
  ungroup() %>% 
  mutate(`Last name` = gsub(",.*", "", Player),
         Champion = "Carlsen 1",
         `Champion status` = ifelse(Player == "Carlsen, Magnus", "Champion", "Not champion"))

ggplot(dat6, aes(as.numeric(Year) + (Month - 1)/12, Rating, group = Ranking)) +
  geom_line()

# write_csv(dat6, "Chess WC/Data/elo_update.csv")

# Merge new data and old data
dat_new <- read_csv("Chess WC/Data/elo_update.csv")
dat_old <- read_csv("Chess WC/Data/merged_elo_series.csv")

dat <- bind_rows(dat_new, dat_old) %>% 
  arrange(-Year, -Month) %>% 
  select(-...1)

write_csv(dat, "Chess WC/Data/merged_elo_series.csv")

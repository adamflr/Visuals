# Team info
library(tidyverse)
dat_match <- read_csv("Allsvenska/Data_out/Alls_matcher.csv")

dat_teams <- tibble(Lag = sort(unique(dat_match$hemma)))

dat_teams_col <- tribble(
  ~Lag, ~Color,
  "IFK Göteborg",     "244b9a",
  "Göteborg",         "244b9a",
  "Malmö",            "0091d4",
  "AIK",              "012e57",
  "Norrköping",       "004ea6",
  "Djurgården",       "db0c14",
  "Hammarby",         "00ab4d",
  "Häcken",           "241f1f",
  "Elfsborg",         "f7db15",
  "Kalmar",           "d9263b",
  "GAIS",             "006c2b",
  "Helsingborgs IF",  "d42719",
  "Helsingborg",      "d42719",
  "Örebro",           "000301",
  "Sirius",           "0159a1",
  "Östersund",        "f21c24",
  "Falkenberg",       "ecdc3f",
  "Mjällby",          "fde516",
  "Varberg",          "00a650",
  "Brommapojkarna",   "c54852",
  "Sundsvall",        "006cb7",
  "Halmstads BK",     "1c498d",
  "AFC Eskilstuna",   "a59361",
  "Trelleborg",       "0a74bb",
  "Örgryte",          "780807",
  "Syrianska",        "f20823",
  "Brage",            "037a39",
  "Öster",            "ae2008",
  "Jönköping",        "00633a"
)

dat_teams_col$Color <- paste0("#", dat_teams_col$Color)

dat_teams %>% left_join(dat_teams_col)

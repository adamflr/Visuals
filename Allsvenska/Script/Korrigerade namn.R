# Namn
library(tidyverse)

dat_match <- read_csv("Allsvenska/Data_out/Alls_matcher.csv",
                      col_types = "ccnnnccDc")

# cat(paste0(sort(unique(dat_match$hemma)), collapse = "\", ,\n\""))

# Koppling mellan ursprungliga namn och korrigerade namn
dat_teams_names <- tribble(
  ~Lag, ~Korrigerat_namn,
 "AFC Eskilstuna", NA,
"AIK", NA,
"Assyriska FF", "Assyriska",
"Billingsfors", NA,
"BK Häcken", "Häcken",
"Brage", NA,
"Brommapojkarna", NA,
"Brynäs", NA,
"City", NA,
"Dalkurd FF", "Dalkurd",
"Degerfors", NA,
"Derby", NA,
"Djurgården", NA,
"Djurgårdens IF", "Djurgården",
"Elfsborg", NA,
"Enköping SK", "Enköping",
"Falkenberg", NA,
"Falkenbergs FF", "Falkenberg",
"Gais", NA,
"GAIS", NA,
"Gefle", NA,
"Gefle IF FF", "Gefle",
"GIF Sundsvall", NA,
"Gårda", NA,
"Göteborg", "IFK Göteborg",
"Hallstahamm", NA,
"Halmia", NA,
"Halmstad", NA,
"Halmstads BK", "Halmstad",
"Hammarby", NA,
"Hammarby IF", "Hammarby",
"Helsingborg", NA,
"Helsingborgs IF", "Helsingborg",
"Helsingborgs IFs IF", "Helsingborg",
"Holmsund", NA,
"Häcken", NA,
"Högadal", NA,
"IF Brommapojkarna", "Brommapojkarna",
"IF Elfsborg", "Elfsborg",
"IFK Eskilstuna", "Eskilstuna",
"IFK Göteborg", "IFK Göteborg",
"IFK Luleå", "Luleå",
"IFK Malmö", NA,
"IFK Norrköping", NA,
"IFK Sundsvall", NA,
"IFK Uddevalla", NA,
"IK Sirius", "Sirius",
"IS Halmia", "Halmia",
"Jönköping", "Jönköpings Södra",
"Jönköpings Södra", NA,
"Kalmar", NA,
"Kalmar FF", "Kalmar",
"Landskrona", NA,
"Ljungskile", NA,
"Ludvika", NA,
"Malmö", "Malmö FF",
"Malmö FF", NA,
"Mjällby", NA,
"Motala", NA,
"Norrby", NA,
"Norrköping", "IFK Norrköping",
"Oddevold", NA,
"Redbergslid", NA,
"Reymersholm", NA,
"Råå", NA,
"Saab", NA,
"Sandviken", "Sandvikens IF",
"Sandviken AIK", NA,
"Sandvikens IF", NA,
"Sirius", NA,
"Sleipner", NA,
"Stattena", NA,
"Sundsvall", NA,
"Syrianska", NA,
"Trelleborg", "Trelleborg",
"Trelleborgs FF", NA,
"Umeå FC", NA,
"V Frölunda", "Västra Frölunda",
"Västerås IK", NA,
"Västerås SK", NA,
"Westermalm", NA,
"Åtvidaberg", NA,
"Örebro", NA,
"Örebro SK", "Örebro",
"Örgryte", NA,
"Örgryte IS", "Örgryte",
"Öster", NA,
"Östersund", NA,
"Östersunds FK", "Östersund")

dat_teams_names <- dat_teams_names %>% 
  mutate(Korrigerat_namn = ifelse(is.na(Korrigerat_namn), Lag, Korrigerat_namn))

# Korrigera namn i dat_match
correct_name <- function(x){
  ind <- which(dat_teams_names$Lag == x)
  unlist(dat_teams_names[ind, 2])
}

dat_match <- dat_match %>% 
  mutate(hemma = map_chr(hemma, correct_name),
         borta = map_chr(borta, correct_name))

# write_csv(dat_match, "Allsvenska/Data_out/Alls_matcher.csv")

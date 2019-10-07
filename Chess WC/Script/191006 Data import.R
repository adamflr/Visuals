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
# Import, 2016-2019. Från Wikipedia, 201028
library(rvest)
library(tidyverse)
library(purrr)

dat_match <- read_csv("Allsvenska/Data_out/Alls_matcher.csv",
                      col_types = "ccnnnccDc")

for(i in 2017:2019){
  url <- paste0("https://sv.wikipedia.org/wiki/Lista_%C3%B6ver_matchresultat_i_Fotbollsallsvenskan_", i)
  html <- read_html(url)
  
  dat <- html %>% 
    html_table(fill = T) %>% 
    tibble()
  
  dat[[8,1]]
  
  dat <- dat %>% 
    mutate(dim1 = map_dbl(., function(x) dim(x)[1]),
           dim2 = map_dbl(., function(x) dim(x)[2])) %>%
    filter(dim2 == 7) %>% 
    mutate(datum = map_chr(., function(x) x[1, 2]),
           hemma = map_chr(., function(x) x[1, 3]),
           borta = map_chr(., function(x) x[1, 5]),
           mal = map_chr(., function(x) x[1, 4]),
           arena = map_chr(., function(x) x[1, 6]),
           publik_domare = map_chr(., function(x) x[2, 6]))
  
  dat <- dat %>% 
    separate(mal, into = c("hemmamal", "bortamal"), sep = " – ", convert = T) %>% 
    separate(publik_domare, c("publik", "publik_domare"), sep = "Publik: ") %>% 
    separate(publik_domare, c("publik", "domare"), sep = "Domare: ") %>% 
    mutate(datum = lubridate::dmy(datum),
           sasong = i) %>% 
    select(hemma, borta, hemmamal, bortamal, publik, domare, arena, datum, sasong)
  
  a <- dat$publik[1]
  a <- substr(a, nchar(a) - 3, nchar(a) - 3)
  
  dat <- dat %>% 
    mutate(publik = gsub(a, "", publik))
    
    dat_match <- rbind(dat_match, dat)
}


# Export ----
# write_csv(dat_match, "Allsvenska/Data_out/Alls_matcher.csv")

dat_match <- read_csv("Allsvenska/Data_out/Alls_matcher.csv",
                      col_types = "ccnnnccDc")

dat_match %>% count(sasong) %>% print(n = 200)

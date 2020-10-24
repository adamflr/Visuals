check_all_spaces <- function(x){
  gsub(" ", "", x) == ""
}

remove_starting_spaces <- function(x){
  if (substr(x, 1, 1) == " "){
    b <- gregexpr(" *.", x)[[1]]
    x <- ifelse(b[1] == 1, substr(x, b[2] - 1, nchar(x)), x)
  }
  return(x)
}

clean_event_class <- function(x){
  match_data <- x %>% 
    html_text() %>% 
    gsub(pattern = "\t", replacement = "") %>% 
    strsplit("\n") %>% 
    unlist() %>% 
    subset(!check_all_spaces(.)) %>%
    purrr::map_chr(remove_starting_spaces)
  
  match_data_vect <- rep(NA, 7)
  match_data_vect[1:4] <- match_data[c(1,3,4,6)]
  
  match_data_vect[5] <- ifelse(any(match_data == "Publik:"), 
                          match_data[which(match_data == "Publik:") + 1],
                          NA)
  
  match_data_vect[6] <- ifelse(any(match_data == "Domare:"), 
                          match_data[which(match_data == "Domare:") + 1],
                          NA)
  
  match_data_vect[7] <- ifelse(any(match_data == "Arena:"), 
                          match_data[which(match_data == "Arena:") + 1],
                          NA)
  
  tab <- as_tibble(matrix(match_data_vect, 1, 7))
  names(tab) <- c("hemma", "borta", "hemmamal", "bortamal", "publik", "domare", "arena")
  tab
}

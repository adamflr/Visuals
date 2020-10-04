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
  
  match_data <- match_data[c(1,3,4,6,8,10)]
  
  tab <- as_tibble(matrix(match_data, 1, 6))
  names(tab) <- c("hemma", "borta", "hemmamal", "bortamal", "publik", "domare")
  tab
}

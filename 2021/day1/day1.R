library(magrittr)

# part 1 -----------------------------------------------------------------
fitter <- function(vector){
    
  tibble::tibble(
    beeps = as.integer(vector),
    previous_beeps = dplyr::lag(beeps)
    )
}

validator <- function(tb){
  
  tb %>% 
    dplyr::mutate(direction = beeps > previous_beeps) %>%
    dplyr::filter(direction) %>% 
    dplyr::summarise(n_increased = dplyr::n()) %>% 
    dplyr::pull(n_increased)
}
  
application <- function(){
  
  readr::read_lines("input.txt") %>% 
    fitter() %>% 
    validator()
}

application()

# parte 2 -----------------------------------------------------------------
fitter2 <- function(vector){
  
  tibble::tibble(
    beep = as.integer(vector),
    next_beep = dplyr::lead(beep),
    next_next_beep = dplyr::lead(next_beep)
  ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(sum_beeps = sum(beep, next_beep, next_next_beep)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(lag_sum_beeps = dplyr::lag(sum_beeps))
}

validator2 <- function(tb){
  tb %>% 
    dplyr::mutate(direction = sum_beeps > lag_sum_beeps) %>% 
    dplyr::filter(direction) %>% 
    dplyr::summarise(n_increased = dplyr::n()) %>% 
    dplyr::pull(n_increased)
}

application2 <- function(){
  
  readr::read_lines("input.txt") %>% 
    fitter2() %>% 
    validator2()
}

application2()

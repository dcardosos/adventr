library(magrittr)


# parte 1 -----------------------------------------------------------------
montadora <- function(vetor){
    
  tibble::tibble(
    beeps = as.integer(vetor),
    previous_beeps = dplyr::lag(beeps)
    )
}

validadora <- function(tb){
  
  tb %>% 
    dplyr::mutate(direction = beeps > previous_beeps) %>%
    dplyr::filter(direction) %>% 
    dplyr::summarise(q_increased = dplyr::n()) %>% 
    dplyr::pull(q_increased)
}
  
aplicacao <- function(){
  
  readr::read_lines("input.txt") %>% 
    montadora() %>% 
    validadora()
}

aplicacao()

# parte 2 -----------------------------------------------------------------
montadora2 <- function(vetor){
  
  tibble::tibble(
    beep = as.integer(vetor),
    next_beep = dplyr::lead(beep),
    next_next_beep = dplyr::lead(next_beep)
  ) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(sum_beeps = sum(beep, next_beep, next_next_beep)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(lag_sum_beeps = dplyr::lag(sum_beeps))
}

validadora2 <- function(tb){
  tb %>% 
    dplyr::mutate(direction = sum_beeps > lag_sum_beeps) %>% 
    dplyr::filter(direction) %>% 
    dplyr::summarise(q_increased = dplyr::n()) %>% 
    dplyr::pull(q_increased)
}

aplicacao2 <- function(){
  
  readr::read_lines("input.txt") %>% 
    montadora2() %>% 
    validadora2()
}

aplicacao2()

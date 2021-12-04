library(magrittr)

# part 1 ------------------------------------------------------------------
fitter <- function(vector){
  
  tibble::tibble(to_sep = vector,
                horizontal = 0,
                depth = 0) %>% 
    tidyr::separate(
      col = to_sep,
      into = c("command", "value"),
      sep = " ") %>% 
    dplyr::mutate(value = as.integer(value))
  
}

validator <- function(tb){
  
  tb %>% 
    dplyr::mutate(
      horizontal = dplyr::case_when(command == "forward" ~ value),
      depth = dplyr::case_when(
        command == "down" ~ value,
        command == "up" ~ -value)) %>% 
    dplyr::summarise(
      sum_horizontal = sum(horizontal, na.rm = TRUE),
      sum_depth = sum(depth, na.rm = TRUE),
      mult_together = sum_horizontal * sum_depth
    ) %>% 
    dplyr::pull(mult_together)
  
}

application <- function(){
  
  readr::read_lines("input.txt") %>% 
    fitter() %>% 
    validator()
}

application()

# part 2 ------------------------------------------------------------------
fitter2 <- function(vector){
  
  tibble::tibble(to_sep = vector,
                 horizontal = 0,
                 aim = 0,
                 depth = 0) %>% 
    tidyr::separate(
      col = to_sep,
      into = c("command", "value"),
      sep = " ") %>% 
    dplyr::mutate(value = as.integer(value))
  
}

validator2 <- function(tb){
  
  tb %>% 
    dplyr::mutate(
      horizontal = dplyr::case_when(command == "forward" ~ value),
      aim = dplyr::case_when(
        command == "down" ~ value,
        command == "up" ~ -value)) %>%
    tidyr::replace_na(list(horizontal = 0, aim = 0)) %>%
    dplyr::mutate(acc_aim = purrr::accumulate(aim, `+`)) %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(depth = horizontal * acc_aim) %>% 
    dplyr::ungroup() %>% 
    dplyr::summarise(mult_together = sum(horizontal) * sum(depth)) %>% 
    dplyr::pull(mult_together)

}

application2 <- function(){
  
  readr::read_lines("input.txt") %>% 
    fitter2() %>% 
    validator2()
}

application2()

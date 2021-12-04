library(magrittr)

# part 1 ------------------------------------------------------------------
## support function
prep_bits <- function(bits){
  
  bits %>% 
    stringr::str_split("") %>% 
    unlist() %>% 
    stringr::str_c(collapse = "_")
}

fitter <- function(str_bits, ids){
  
  tibble::tibble(
    bits = str_bits
  ) %>% 
    tidyr::separate(
      col = bits, 
      into = ids,
      sep = '_'
    ) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = "id_bit",
                        values_to = "bits") %>% 
    dplyr::mutate(id_bit = as.integer(id_bit))
}


validator <- function(tb){
  
  tb %>% 
    dplyr::group_by(id_bit) %>% 
    dplyr::summarise(media_bit = mean(as.integer(bits))) %>%
    dplyr::mutate(
      gamma_rate = dplyr::case_when(
        media_bit > 0.5 ~ 1,
        media_bit < 0.5 ~ 0
      ),
      epsilon_rate = abs(gamma_rate - 1)
    ) %>% 
    dplyr::summarise(
      gamma_rate = purrr::reduce(gamma_rate, paste, sep = ""),
      epsilon_rate = purrr::reduce(epsilon_rate, paste, sep = ""),
      mult_together_as_decimal = strtoi(gamma_rate, 2) * strtoi(epsilon_rate, 2)
    ) %>% 
    dplyr::pull(mult_together_as_decimal)
}

application <- function(){
  
  input <- readr::read_lines("input.txt")
  
  id_cols <- 1:stringr::str_length(input[1]) %>%  as.character()
  bake_bits <- purrr::map_chr(input, ~ prep_bits(.x))
  
  fitter(bake_bits, id_cols) %>% 
    validator()
  
}

application()

# part 2 ------------------------------------------------------------------

## support functions
oxygen_generator_decision <- function(binary_vec){
  
  dplyr::if_else(
    condition = mean(as.integer(binary_vec)) >= 0.5,
    true = 1,
    false = 0)
}

co2_scrubber_decision <- function(binary_vec){
  
  dplyr::if_else(
    condition = mean(as.integer(binary_vec)) < 0.5,
    true = 1,
    false = 0)
}

## oficial functions
fitter2 <- function(str_bits, ids){
  
  tibble::tibble(
    bits = str_bits
  ) %>% 
    tidyr::separate(
      col = bits, 
      into = ids,
      sep = '_'
    ) %>%
    janitor::clean_names()
}

get_rating <- function(df, .what){
  
  
  list(
    "oxygen-generator" = oxygen_generator_decision,
    "co2-scrubber" = co2_scrubber_decision) %>% 
    purrr::pluck(.what) ->
    decision_to_call
  
  for(col in names(df)){
    
    if(dim(df)[1] == 1) break
    
    df <- df %>% 
      dplyr::filter(!!rlang::sym(col) == decision_to_call(!!rlang::sym(col)))
    
  }
  
  df %>% 
    as.character() %>% 
    purrr::reduce(paste0, sep = "") %>% 
    strtoi(base = 2)
}

application2 <- function(){
  
  input <- readr::read_lines("input.txt")
  
  id_cols <- 1:stringr::str_length(input[1]) %>%  as.character()
  bake_bits <- purrr::map_chr(input, ~ prep_bits(.x))
  
  base <- fitter2(bake_bits, id_cols)
  
  tibble::tibble(rating = c("oxygen-generator", "co2-scrubber")) %>% 
    dplyr::mutate(value = purrr::map_int(rating, ~ get_rating(base, .x))) %>% 
    dplyr::pull(value) %>% 
    purrr::reduce(`*`)
}

application2()

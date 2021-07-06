library(tidyverse)

# part 1 - primeira forma

leitura <- function(){
  
  readr::read_lines('input') %>%
    as_tibble() %>%
    mutate(
      across(value, as.double))
}


processadora <- function(entrada){
  
  entrada %>% 
    mutate(
      reduzido = 2020 - purrr::pluck(value),
      validacao = value %in% reduzido
    ) %>%
    filter(validacao) %>%
    slice(1) %>%
    mutate(
      multiplicacao = value * reduzido
    ) %>%
    purrr::pluck('multiplicacao')
  
}

day1 <- function(){
 leitura() %>%
    processadora()
}

day1()

## part 1 - segundo jeito
processadorav2 <- function(entrada){
  
  entrada %>%
    purrr::pluck('value') ->
    vetor
  
  expand_grid(
    x = vetor,
    y = vetor) %>%
    mutate(
      soma = x + y) %>%
    filter(soma == 2020) %>%
    slice(1) %>%
    summarise(
      multiplicacao = x * y) %>%
    purrr::pluck('multiplicacao')
}

day1_v2 <- function(){
  leitura() %>%
    processadorav2()
}

day1_v2()

# part 2
## part 1 - segundo jeito
processadora_pt2 <- function(entrada){
  
  entrada %>%
    purrr::pluck('value') ->
    vetor
  
  expand_grid(
    x = vetor,
    y = vetor,
    z = vetor) %>%
    mutate(soma = x + y + z) %>%
    filter(soma == 2020) %>%
    slice(1) %>%
    summarise(
      multiplicacao = x * y * z) %>%
    purrr::pluck('multiplicacao')
}

day1_pt2 <- function(){
 leitura() %>%
    processadora_pt2()
}

day1_pt2()
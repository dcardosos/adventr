library(tidyverse)

# part 1
exemplo <- (c('1-3 a: abcde', '1-3 b: cdefg', '2-9 c: ccccccccc'))

limpadora <- function(vetor_string){
  vetor_string %>%
    stringr::str_replace_all('-', ' ') %>%
    stringr::str_replace_all(':', '') %>%
    stringr::str_replace_all(' ', ',') %>%
    readr::read_csv(col_names = c('lowest', 'highest', 'letter', 'password'))
}

validadora <- function(cleaned_string){
  cleaned_string %>%
    mutate(
      matchs = str_count(password, letter),
      validation = matchs >= lowest & matchs <= highest) %>%
    filter(
      validation) %>%
    summarise(
      quantidade = n()) %>%
    purrr::pluck('quantidade')
}

day2 <- function(){
  
  readr::read_lines('input') %>%
    limpadora() %>%
    validadora()
}

day2()


# part 1
construtora <- function(vetor_string){
  vetor_string %>%
    stringr::str_replace_all('-', ' ') %>%
    stringr::str_replace_all(':', '') %>%
    stringr::str_replace_all(' ', ',') %>%
    readr::read_csv(col_names = c('p1', 'p2', 'letter', 'password'))
}

validadora_extra <- function(constructed_string){
  
  constructed_string %>%
    mutate(
      check1 = stringr::str_sub(password, p1, p1),
      check2 = stringr::str_sub(password, p2, p2),
      validacao = (check1 == letter | check2 == letter) & (check1 != check2)) %>%
    filter(
      validacao) %>%
    summarise(
      quantidade = n()) %>%
    purrr::pluck('quantidade')
}


day2_p2 <- function(){
  
  readr::read_lines('input') %>%
    construtora() %>%
    validadora_extra()
}

day2_p2()

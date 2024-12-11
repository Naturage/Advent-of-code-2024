source(here::here("common functions.R"))

day <- 11

# Data input
input <- get_input(day)

input_clean <- input %>% 
  str_split_1(" ") %>% 
  as.numeric %>%
  tibble(number = .) %>%
  count(number)

# count indcidences and make sure to vectorise everything.

blink <- function(previous_set){
  previous_set %>%
    mutate(next_number_1 = case_when(
      number == 0 ~ 1,
      floor(log10(number)+1) %% 2 == 0 ~ number %/% 10^(floor(log10(number)+1) / 2),
      TRUE ~ number * 2024
    ),
    next_number_2 = case_when(
      number == 0 ~ -1,
      floor(log10(number)+1) %% 2 == 0 ~ number %% 10^(floor(log10(number)+1) / 2),
      TRUE ~ -1
    )) %>%
    select(-number) %>%
    pivot_longer(cols = c(next_number_1, next_number_2),
                 names_to = NULL,
                 values_to = "number") %>%
    filter(number != -1) %>%
    summarise(n = sum(n), .by = number)
}

curr_set <- input_clean
for (i in 1:25){
  curr_set <- blink(curr_set)
  print(i)
}

(ans_1 <- curr_set %>% summarise(ans = sum(n)) %>% pull(ans)) 

curr_set <- input_clean
for (i in 1:75){
  curr_set <- blink(curr_set)
  print(i)
}

(ans_2 <- curr_set %>% summarise(ans = sum(n)) %>% pull(ans))
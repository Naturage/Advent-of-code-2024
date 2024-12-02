source(here::here("common functions.R"))

day <- 2

# Data input
input <- get_input(day)

input_cleaner <- input %>% str_split(" ") %>% lapply(as.numeric)

is_line_clean <- function(line){
  line %>% diff %>% {all(. %in% 1:3) | all(. %in% -1:-3)}
}

# Pt1

(ans_1 <- input_cleaner %>% 
    sapply(is_line_clean) %>%
    sum)

# Pt2

(ans_2 <- input_cleaner %>% 
  sapply(function(line){
    any(sapply(1:length(line), 
               function(elem){is_line_clean(line[-elem])})) 
  }) %>%
  sum)


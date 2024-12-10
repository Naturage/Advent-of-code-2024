source(here::here("common functions.R"))

day <- 10

# Data input
input <- get_input(day)

input_clean <- input %>% 
  str_split("") %>% 
  sapply(as.numeric)

heights_data <- crossing(
  tibble(row = 1:nrow(input_clean)),
  tibble(col = 1:ncol(input_clean))
) %>%
  mutate(pos = row + col * 1i,
    height = sapply(pos, \(x){input_clean[Re(x),Im(x)]})) %>%
  select(pos, height)

current_paths <- heights_data %>%
  filter(height == 0) %>%
  select(start_pos = pos, curr_pos = pos, curr_height = height)

take_a_step <- function(current_paths){
  next_step <- current_paths %>%
    cross_join(heights_data %>% rename(next_height = height, next_pos = pos)) %>%
    filter((next_height == curr_height + 1) & (abs(next_pos - curr_pos) == 1)) %>%
    select(start_pos, curr_pos = next_pos, curr_height = next_height)
  return(next_step)
}

for (i in 1:9){
  current_paths <- take_a_step(current_paths)
}

# Pt1
(ans_1 <- current_paths %>% distinct(start_pos, curr_pos) %>% nrow)

# Pt2
(ans_2 <- current_paths %>% nrow)
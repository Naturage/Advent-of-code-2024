source(here::here("common functions.R"))

day <- 14

# Data input
input <- get_input(day)

input_clean <- input %>% 
  str_extract_all("(-?)\\d+") %>%
  tibble(pos_x = sapply(., \(x){x[1]}),
         pos_y = sapply(., \(x){x[2]}),
         vel_x = sapply(., \(x){x[3]}),
         vel_y = sapply(., \(x){x[4]})) %>%
  select(-1) %>%
  mutate(across(everything(), as.numeric))

room_x <- 101
room_y <- 103

# Pt1
(ans1 <- input_clean %>% 
  mutate(pos_end_x = (pos_x + 100 * vel_x) %% room_x,
         pos_end_y = (pos_y + 100 * vel_y) %% room_y) %>%
  mutate(quadrant = case_when(
    pos_end_x < 50 & pos_end_y < 51 ~ 1,
    pos_end_x < 50 & pos_end_y > 51 ~ 2,
    pos_end_x > 50 & pos_end_y < 51 ~ 3,
    pos_end_x > 50 & pos_end_y > 51 ~ 4,
    TRUE ~ 0
  )) %>%
  filter(quadrant != 0) %>%
  count(quadrant) %>%
  pull(n) %>%
  prod)

# Pt2
# Well... drat.
t <- 0
curr_pos <- input_clean

while(t <= room_x * room_y){
  t <- t+1
  curr_pos <- curr_pos %>%
    mutate(pos_x = (pos_x + vel_x) %% room_x,
           pos_y = (pos_y + vel_y) %% room_y)
  
  hopeful_test <- curr_pos %>%
    count(pos_x,pos_y) %>%
    filter(n > 1) %>%
    nrow
  
  if(hopeful_test == 0){
    picture <- matrix(" ", nrow = room_x, ncol = room_y)
    mapply(
      function(x,y){picture[x+1, y+1] <<- "#"},
      x = curr_pos$pos_x,
      y = curr_pos$pos_y
    ) %>%
      invisible
    
    out <- apply(picture, \(x){paste0(x, collapse = "")}, MARGIN = 1)
    print(out)
    print(t)
  }
}
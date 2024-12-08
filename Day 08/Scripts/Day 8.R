source(here::here("common functions.R"))

day <- 8

# Data input
input <- get_input(day)

size <- length(input)

input_clean <- input %>% 
  str_split("") %>% 
  unlist %>% 
  matrix(nrow = size, byrow = TRUE)

antenna_locs <- which(str_detect(input_clean, "[a-zA-Z0-9]")) %>%
  tibble(raw = .) %>%
  mutate(x = (raw -1)  %% size + 1,
         y = (raw -1) %/% size + 1) %>%
  mutate(antenna = mapply(\(x,y){input_clean[x,y]}, x,y, SIMPLIFY = TRUE)) %>%
  select(-raw)

resonant_pairs <- inner_join(
  antenna_locs %>% rename(x1 = x, y1 = y),
  antenna_locs %>% rename(x2 = x, y2 = y),
  by = join_by(antenna),
  relationship = "many-to-many") %>%
  filter(!(x1 == x2 & y1 == y2))

resonant_locs <- resonant_pairs %>%
  crossing(resonants = -size:size) %>%
  mutate(anti_x = x1 + resonants * (x1-x2),
         anti_y = y1 + resonants * (y1-y2)) %>%
  filter((anti_x %in% 1:size) & (anti_y %in% 1:size))

(ans_1 <- resonant_locs %>%
    # numbers - 0 and -1 return the source antennas, P1 resonant is 1 off them
    filter(resonants %in% c(-2,1)) %>%
    distinct(anti_x,anti_y) %>%
    nrow)

(ans_2 <- resonant_locs %>%
    distinct(anti_x,anti_y) %>%
    nrow)
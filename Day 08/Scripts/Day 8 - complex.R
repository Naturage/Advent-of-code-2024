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
  {((. -1)  %% size + 1) * 1L + ((. -1)  %/% size + 1) * 1i} %>%
  tibble(x = .)  %>%
  mutate(antenna = sapply(x, \(x){input_clean[Re(x),Im(x)]}))

resonant_locs <- inner_join(
  antenna_locs,
  antenna_locs %>% rename(y = x),
  by = join_by(antenna),
  relationship = "many-to-many") %>%
  crossing(resonants = -size:size) %>%
  filter(x != y) %>%
  mutate(anti = x + resonants * (x-y)) %>%
  filter((Re(anti) %in% 1:size) & (Im(anti) %in% 1:size))

(ans_1 <- resonant_locs %>%
    # numbers - 0 and -1 return the source antennas, P1 resonant is 1 off them
    filter(resonants %in% c(-2,1)) %>%
    distinct(anti) %>%
    nrow)

(ans_2 <- resonant_locs %>%
    distinct(anti) %>%
    nrow)
source(here::here("common functions.R"))

day <- 1

# Data input
input <- get_input(day)

input_cleaner <- input %>% str_split(" +")

list_1 <- sapply(input_cleaner, function(x){x[1]}) %>% as.numeric
list_2 <- sapply(input_cleaner, function(x){x[2]}) %>% as.numeric

# Pt1

(ans_1 <- (sort(list_1) - sort(list_2)) %>% abs %>% sum)

# Pt2

(ans_2 <- inner_join(tibble(list_1), tibble(list_2), by = join_by(list_1 == list_2)) %>% pull(1) %>% sum)
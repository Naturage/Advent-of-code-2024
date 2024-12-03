source(here::here("common functions.R"))

day <- 3

# Data input
input <- get_input(day)

# Pt1

(ans_1 <- input %>% 
   str_extract_all("mul\\(\\d+,\\d+\\)") %>% 
   unlist %>% 
   str_extract_all("\\d+") %>% 
   lapply(as.numeric) %>%
   sapply(prod) %>%
   sum)

# Pt2

parsed_with_dos <- input %>% 
  str_extract_all("mul\\(\\d+,\\d+\\)|don't|do") %>%
  unlist

only_do_dont <- case_when(parsed_with_dos == "do" ~ 1,
                          parsed_with_dos == "don't" ~ 0,
                          TRUE ~ NA)

last_seen <- 1
for (pos in 1:length(only_do_dont)){
  only_do_dont[pos] <- coalesce(only_do_dont[pos],last_seen)
  last_seen <- only_do_dont[pos]
}

(ans_2 <- parsed_with_dos %>% 
    str_extract_all("\\d+") %>% 
    lapply(as.numeric) %>%
    # annoyingly prod() is 1, so need to be explicit.
    sapply(function(numbers){if (length(numbers) == 0){0} else{prod(numbers)}}) %>%
    coalesce(0) %>%
    {. * only_do_dont} %>%
    sum)
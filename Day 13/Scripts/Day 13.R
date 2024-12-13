source(here::here("common functions.R"))

options(scipen=999)

day <- 13

# Data input
input <- get_input(day)

tol <- 10^-13

input_clean <- input %>% 
  str_extract_all("-?\\d+") %>%
  tibble(x = sapply(.,\(x)x[1]),
         y = sapply(.,\(x)x[2])) %>%
  mutate(arcade_id = cumsum(is.na(x)) + 1) %>%
  filter(!is.na(x)) %>%
  mutate(tmp = case_when(
    row_number() == 1 ~ "a",
    row_number() == 2 ~ "b",
    row_number() == 3 ~ "goal",
  ), .by = arcade_id) %>%
  pivot_wider(id_cols = arcade_id,
              values_from = c(x,y),
              names_from = tmp) %>%
  mutate(across(where(is.character), as.double))

# The equations
# A * x_a + B * x_b = x_goal
# A * y_a + B * y_b = y_goal
# have a solution of
# A = (x_goal - B*x_b)/x_a
# (x_goal - B*x_b) * y_a/x_a + B*y_b = y_goal
# x_goal * y_a/x_a - y_goal = B*(x_b*y_a/x_a- y_b)
# B = (x_goal * y_a/x_a - y_goal)/(x_b*y_a/x_a- y_b)
# A = (x_goal * y_b/x_b - y_goal)/(x_a*y_b/x_b- y_a)

# I'm having some issues with checking if integer, so using modular checks instead.
(pt_1 <- input_clean %>%
  mutate(click_a = (x_goal * y_b - y_goal * x_b)/(x_a*y_b- y_a*x_b),
         click_b = (x_goal * y_a - y_goal * x_a)/(x_b*y_a- y_b*x_a)) %>%
  filter(((click_a %% 1 < tol) | (-click_a %% 1 < tol)) & 
         ((click_b %% 1 < tol) | (-click_b %% 1 < tol))) %>%
  mutate(cost = 3*click_a + 1*click_b) %>%
  summarise(ans = sum(cost)) %>%
  pull(ans))

(pt_2 <- input_clean %>%
    mutate(x_goal = x_goal + 10^13,
           y_goal = y_goal + 10^13) %>%
    mutate(click_a = (x_goal * y_b - y_goal * x_b)/(x_a*y_b- y_a*x_b),
           click_b = (x_goal * y_a - y_goal * x_a)/(x_b*y_a- y_b*x_a)) %>%
    filter(((click_a %% 1 < tol) | (-click_a %% 1 < tol)) & 
           ((click_b %% 1 < tol) | (-click_b %% 1 < tol))) %>%
    mutate(cost = 3*click_a + 1*click_b) %>%
    summarise(ans = sum(cost)) %>%
    pull(ans))
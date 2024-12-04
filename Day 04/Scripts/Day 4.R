source(here::here("common functions.R"))

day <- 4

# Data input
input <- get_input(day)

input_cleaner <- input %>% 
  str_split("") %>% 
  unlist %>% 
  matrix(nrow = length(input)) %>%
# pad 3 chars from every side
  cbind(matrix(rep(".", nrow(.) * 3), ncol = 3),
        .,
        matrix(rep(".", nrow(.) * 3), ncol = 3)) %>%
  rbind(matrix(rep(".", ncol(.) * 3), nrow = 3),
        .,
        matrix(rep(".", ncol(.) * 3), nrow = 3))

# Pt1

check_if_xmas <- function(x, y, dir_x, dir_y, grid = input_cleaner){
  all(grid[x + 0 * dir_x, y + 0 * dir_y] == "X",
      grid[x + 1 * dir_x, y + 1 * dir_y] == "M",
      grid[x + 2 * dir_x, y + 2 * dir_y] == "A",
      grid[x + 3 * dir_x, y + 3 * dir_y] == "S")
}

check_all_xmas <- function(x,y){
  sum(check_if_xmas(x,y, 0, 1),
      check_if_xmas(x,y, 0,-1),
      check_if_xmas(x,y, 1, 0),
      check_if_xmas(x,y,-1, 0),
      check_if_xmas(x,y, 1, 1),
      check_if_xmas(x,y, 1,-1),
      check_if_xmas(x,y,-1, 1),
      check_if_xmas(x,y,-1,-1))
}

starting_pts <- which(input_cleaner == "X", arr.ind = T)

(ans_1 <- mapply(check_all_xmas,
                x = starting_pts[,1],
                y = starting_pts[,2]) %>%
  sum)

# Pt 2

# dir_x, dir_y here aren't cardinal directions but diagonals
check_if_crossmas <- function(x, y, dir_x, dir_y, grid = input_cleaner){
  all(grid[x, y] == "A",
      
      grid[x + 1 * dir_x, y + 1 * dir_x] == "M",
      grid[x - 1 * dir_x, y - 1 * dir_x] == "S",
      
      grid[x - 1 * dir_y, y + 1 * dir_y] == "M",
      grid[x + 1 * dir_y, y - 1 * dir_y] == "S")
}

check_all_crossmas <- function(x,y){
  max(check_if_crossmas(x,y, 1, 1),
      check_if_crossmas(x,y, 1,-1),
      check_if_crossmas(x,y,-1, 1),
      check_if_crossmas(x,y,-1,-1))
}

starting_pts <- which(input_cleaner == "A", arr.ind = T)

(ans_2 <- mapply(check_all_crossmas,
                 x = starting_pts[,1],
                 y = starting_pts[,2]) %>%
    sum)
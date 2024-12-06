source(here::here("common functions.R"))

day <- 6

# Data input
input <- get_input(day)

input_clean <- input %>% 
  str_split("") %>% 
  unlist %>% 
  matrix(nrow = length(input), byrow = TRUE) %>%
  cbind(matrix(rep("O", nrow(.)), ncol = 1),
        .,
        matrix(rep("O", nrow(.)), ncol = 1)) %>%
  rbind(matrix(rep("O", ncol(.)), nrow = 1),
        .,
        matrix(rep("O", ncol(.)), nrow = 1))

directions <- list("N" = c(-1, 0),
                   "E" = c( 0, 1),
                   "S" = c( 1, 0),
                   "W" = c( 0,-1))

turn_right <- list("N" = "E",
                   "E" = "S",
                   "S" = "W",
                   "W" = "N")

# Pt1

tiles_to_get_out <- function(curr_map, current_pos, current_dir){
  
  repeat{
    # Have we been here from same direction? If so, return looping error (-1)
    if (str_detect(curr_map[current_pos[1],current_pos[2]], current_dir)) {
      return(-1)
    } else {
      curr_map[current_pos[1],current_pos[2]] <- paste0(curr_map[current_pos[1],current_pos[2]],current_dir)
    }
    
    # Depending on next tile, exit, turn, or step forward.
    tile_next <- curr_map[current_pos[1] + directions[[current_dir]][1],current_pos[2] + directions[[current_dir]][2]]
    if (tile_next == "O"){
      return(curr_map[str_detect(curr_map,"[NESW]")] %>% length())
    } else if (tile_next == "#") {
      current_dir <- turn_right[[current_dir]]
    } else {
      current_pos <- current_pos + directions[[current_dir]]
    }
    
  }
  
}

(ans_1 <- tiles_to_get_out(curr_map = input_clean, 
                           current_pos = which(input_clean == "^", arr.ind = TRUE) %>% as.vector, 
                           current_dir = "N"))

# Pt2

curr_map <- input_clean
current_pos <- which(input_clean == "^", arr.ind = TRUE) %>% as.vector
current_dir <- "N"
ans_2 <- 0

repeat{
  
  curr_map[current_pos[1],current_pos[2]] <- paste0(curr_map[current_pos[1],current_pos[2]],current_dir)
  
  # Exit/turn/step forward.
  tile_next <- curr_map[current_pos[1] + directions[[current_dir]][1],current_pos[2] + directions[[current_dir]][2]]
  if (tile_next == "O"){
    break
  } else if (tile_next == "#") {
    current_dir <- turn_right[[current_dir]]
  } else {
    if (tile_next == "."){
      # If we haven't tried putting a box, check if that loops us.
      tmp_map <- curr_map
      tmp_map[current_pos[1] + directions[[current_dir]][1],current_pos[2] + directions[[current_dir]][2]] <- "#"
      ans_2 <- ans_2 + (tiles_to_get_out(tmp_map, current_pos, turn_right[[current_dir]]) == -1)
    }
    current_pos <- current_pos + directions[[current_dir]]
  }
}

(ans_2)

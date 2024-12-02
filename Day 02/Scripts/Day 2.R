source(here::here("common functions.R"))

day <- 2

# Data input
input <- get_input(day)

input_cleaner <- input %>% str_split(" ") %>% lapply(as.numeric)

differences <- input_cleaner %>% 
  lapply(function(line)
    {(line - lag(line)) %>% .[-1]})

(ans_1 <- differences %>% 
    sapply(function(line)
      {all(line %in% 1:3) | all(line %in% -1:-3)}) %>%
    sum)

# Pt2

(ans_2 <- differences %>% 
  sapply(function(line){
    
    ans <- FALSE
    
      # remove first or last elem
    if(all(line[-1] %in% 1:3) | all(line[-1] %in% -1:-3) | 
       all(line[-length(line)] %in% 1:3) | all(line[-length(line)] %in% -1:-3)){
      ans <- TRUE
    } else {
      
      # remove any other, i.e. collapse two sums into one.
      for (position in 1:(length(line) - 1)){
        line_temp <- line
        line_temp[position] <- line[position] + line[position + 1]
        line_temp <- line_temp[-(position + 1)]
        if(all(line_temp %in% 1:3) | all(line_temp %in% -1:-3)){
          ans <- TRUE
        }
      }
    }
    return(ans)
  }) %>%
  sum)
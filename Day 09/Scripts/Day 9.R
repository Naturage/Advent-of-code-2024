source(here::here("common functions.R"))

day <- 9

# Data input
input <- get_input(day)

#input <- c("2333133121414131402")

input_clean <- input %>% 
  str_split("") %>% 
  unlist %>% 
  as.numeric

pre_compression <- c()

for (i in 1:length(input_clean)){
  file_id <- i %/% 2
  file_real <- i %% 2
  
  if(file_real){
    pre_compression <- c(pre_compression, rep(file_id, input_clean[i]))
  } else {
    pre_compression <- c(pre_compression, rep(NA, input_clean[i]))
  }
}

# Pt 1

compressed <- pre_compression
i <- 1

repeat {
  if(is.na(compressed[i])){
    last_non_na <- which(!is.na(compressed)) %>% max
    compressed[i] <- compressed[last_non_na]
    compressed <- compressed[1:(last_non_na-1)]
  }
  i <- i+1
  if (i > length(compressed)){break}
}

(ans_1 <- (0:(length(compressed) - 1) * compressed) %>% sum(na.rm = T))

# Pt 2

compressed <- pre_compression

for (i in max(compressed, na.rm = T):1) {
  locs_to_move <- which(compressed == i)
  file_length <- length(locs_to_move)
  j <- 1
  repeat {
    if(all(is.na(compressed[j:(j+file_length-1)]))){
      break
    } else if (i+file_length-1 == length(compressed)){
      j <- -1
      break
    } else {
      j <- j + 1
    }
  }
  if (j < min(locs_to_move)){
    compressed[j:(j+file_length-1)] <- i
    compressed[locs_to_move] <- NA
    compressed <- compressed[1:(max(which(!is.na(compressed))))]
  }
  if(i %%10 == 0){print(i)}
}

(ans_2 <- (0:(length(compressed) - 1) * compressed) %>% sum(na.rm = T))
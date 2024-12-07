source(here::here("common functions.R"))

day <- 7

# Data input
input <- get_input(day)

input_clean <- input %>% str_split("(:)? ") %>% lapply(as.numeric)
tests <- sapply(input_clean, \(x){x[1]})
equations <- lapply(input_clean, \(x){x[-1]})

# Pt1

can_be_obtained <- function(result, inputs, part){
  
  # various terminating conditions
  if (length(inputs) == 1){
    return(inputs == result)
  }
  if (inputs[1] > result){
    return(FALSE)
  }
  
  sum_option    <- inputs[1] + inputs[2]
  mult_option   <- inputs[1] * inputs[2]
  concat_option <- paste0(inputs[1], inputs[2]) %>% as.numeric
  
  sum_works    <-     can_be_obtained(result, c(sum_option   , inputs[-1:-2]), part)
  mult_works   <-     can_be_obtained(result, c(mult_option  , inputs[-1:-2]), part)
  concat_works <- all(can_be_obtained(result, c(concat_option, inputs[-1:-2]), part),part == 2)
  
  return(any(mult_works,sum_works,concat_works))
  
}

(ans_1 <- (tests * 
    mapply(can_be_obtained,
           result = tests,
           inputs = equations,
           part = 1)) %>%
  sum)

# Pt 2
(ans_2 <- (tests * 
             mapply(can_be_obtained,
                    result = tests,
                    inputs = equations,
                    part = 2)) %>%
    sum)
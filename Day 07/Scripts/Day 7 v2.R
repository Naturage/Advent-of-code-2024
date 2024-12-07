source(here::here("common functions.R"))

day <- 7

# Data input
input <- get_input(day)

input_clean <- input %>% str_split("(:)? ") %>% lapply(as.numeric)
tests <- sapply(input_clean, \(x){x[1]})
equations <- lapply(input_clean, \(x){x[-1]})

can_be_obtained <- function(result, inputs, part){
  
  last <- length(inputs)
  digits_last <- floor(log10(inputs[last])) + 1
  
  # various terminating conditions
  if (last == 1){
    return(inputs == result)
  }
  if (inputs[last] > result){
    return(FALSE)
  }
  
  sum_works    <-     can_be_obtained(result - inputs[last] , inputs[-last], part)
  if (result %% inputs[last] == 0){
    mult_works   <-     can_be_obtained(result / inputs[last], inputs[-last], part)
  } else {
    mult_works <- FALSE
  }
  if ((result %% 10^digits_last == inputs[last]) & (part == 2)){
    concat_works <- can_be_obtained(result %/% 10^digits_last, inputs[-last], part)
  } else {
    concat_works <- FALSE
  }
  
  return(any(mult_works,sum_works,concat_works))
  
}

# Pt1
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
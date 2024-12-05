source(here::here("common functions.R"))

day <- 5

# Data input
input <- get_input(day)

rules <- input %>% 
  .[str_detect(.,"\\|")] %>% 
  tibble %>% 
  rename(raw = 1) %>%
  separate(raw, into = c("before","after"), sep = "\\|", convert = TRUE)

updates <- input %>% 
  .[str_detect(.,",")] %>%
  str_split(",") %>%
  lapply(as.numeric)

check_order <- function(update){
  
  midpoint <- (length(update) + 1)/2
  
  in_order <- rules %>%
    filter((before %in% update) & (after %in% update)) %>%
    mutate(before_when = sapply(before, function(x){which(update == x)}),
           after_when  = sapply(after , function(x){which(update == x)})) %>%
    mutate(fine = (before_when < after_when)) %>%
    pull(fine) %>%
    all
  
  if (in_order){
   return(update[midpoint])
  } else {
    return(0)
  }
}

# Pt1

(ans_1 <- sapply(updates, check_order) %>% sum)

# Pt2

# This needs to assume there's only one good ordering - I REALLY hope that's the case.
order_up <- function(update){
  
  midpoint <- (length(update) + 1)/2
  
  if(check_order(update)){
    return(0)
  } else{
    
    rules_to_follow <- rules %>%
      filter((before %in% update) & (after %in% update))
    
    ordered_pages <- c()
    
    while(nrow(rules_to_follow) > 0){
      # page that goes next is the one not seen in after column.
      next_page <- rules_to_follow %>% #
        anti_join(rules_to_follow, by = join_by(before == after)) %>%
        distinct(before) %>%
        pull(before)
      
      expect_true(length(next_page) == 1)
      
      rules_to_follow <- rules_to_follow %>% 
        filter((before != next_page) & (after != next_page))
      
      ordered_pages <- c(ordered_pages, next_page)
      
      # Don't fuss with the end where last few elements may have 0 rules and such. We only need half the job done.
      if (length(ordered_pages) > midpoint){
        break
      }
    }
    
    return(ordered_pages[midpoint])
  }
  
}

(ans_2 <- sapply(updates, order_up) %>% sum)
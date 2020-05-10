#Recoding for 3 values function
myrecode <- function(x, old, new){
  if(length(old)==2){ 
    recoded <- case_when(x==old[[1]]    ~ new[[1]],
                         x==old[[2]]    ~ new[[2]],
                         TRUE ~ x)
  }
  else if(length(old)==3){ 
    recoded <- case_when(x==old[[1]]    ~ new[[1]],
                         x==old[[2]]    ~ new[[2]],
                         x==old[[3]]    ~ new[[3]], 
                         TRUE ~ x)
  }
  else if(length(old)==4){ 
    recoded <- case_when(x==old[[1]]    ~ new[[1]],
                         x==old[[2]]    ~ new[[2]],
                         x==old[[3]]    ~ new[[3]], 
                         x==old[[4]]    ~ new[[4]], 
                         TRUE ~ x)
  }
  else if(length(old)==5){ 
    recoded <- case_when(x==old[[1]]    ~ new[[1]],
                         x==old[[2]]    ~ new[[2]],
                         x==old[[3]]    ~ new[[3]], 
                         x==old[[4]]    ~ new[[4]], 
                         x==old[[5]]    ~ new[[5]], 
                         TRUE ~ x)
  }
}
 


#Combining parent and child reports: 
#p1 = reporter 1 
#reporter2 = reporter 2
#scoreas = "higher", "higher2 "mean", "mean2", "total", "total.2" 
p1p2 <- function(p1, p2, scoreas = NULL){
  if(is.null(scoreas)){
    abort("Please provide an argument for scoreas (higher, higher2, mean, mean2, total)")
  }
  if (typeof(p1) == typeof(p2)){
    intp1p2 <- is.integer(p1)
    doublep1p2 <- is.double(p1)
    factorp1p2 <- is.factor(p1)
  } else {
    abort("Oops! Error: p1 and p2 are not the same type!\nPlease make the same type or things get funky...")
  }
  if(factorp1p2){
    warning("WARNING!! Variables for p1 and p2 are factors and will be coerced to numeric_double for function.\nMake sure you set your levels correctly!!")
  }
  if(intp1p2|factorp1p2){
    p1dbl <- as.double(p1)
    p2dbl <- as.double(p2)}
  if(scoreas == "higher") {        
    p1p2 <- case_when((is.na(p1) & is.na(p2))  ~ p1, 
                      (!is.na(p1) & is.na(p2)) ~ p1,
                      (is.na(p1) & !is.na(p2)) ~ p2,
                      p1>=p2 ~ p1, 
                      p2>=p1 ~ p2,
                      TRUE ~ NA_real_)
  }
  else if (scoreas == "higher2") {
    p1p2 <- case_when((is.na(p1) | is.na(p2)) ~ p1, 
                      p1>=p2 ~ p1, 
                      p2>=p1 ~ p2,
                      TRUE ~ NA_real_)
    
  }
  else if (scoreas == "mean") {
    p1p2 <- case_when((is.na(p1) & is.na(p2))  ~ p1, 
                      (!is.na(p1) & is.na(p2)) ~ p1,
                      (is.na(p1) & !is.na(p2)) ~ p2,
                      (!is.na(p1) & !is.na(p2)) ~ mean(p1,p2),
                      TRUE ~ NA_real_)
  }
  else if (scoreas == "mean2") {
    p1p2 <- case_when((is.na(p1) | is.na(p2))  ~ p1, 
                      (!is.na(p1) & !is.na(p2)) ~ mean(p1,p2),
                      TRUE ~ NA_real_)
  }
  else if (scoreas == "total") {
    p1p2 <- case_when((is.na(p1) | is.na(p2))  ~ p1, 
                      (!is.na(p1) & !is.na(p2)) ~ sum(p1,p2),
                      TRUE ~ NA_real_)
  }
}




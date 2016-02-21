fortify.ts <- function(x){
  time <- as.numeric(time(x))
  if(is.matrix(x)){
    df <- as.data.frame(x)
  }
  else {
    x <- as.numeric(x)
    df <- as.data.frame(x)
  }
  df$time <- time
  df
}

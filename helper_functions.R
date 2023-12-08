add_line <- function(p1, w, ...){
  # w = c(w[1], w[2], w[3]) = c(w0, w1, w2) 
  if(w[3]==0){
    if(w[2]!=0){
      p2 <- p1 + geom_vline(xintercept = -w[1]/w[2], ...)
    } else{
      p2 <- p1
    }
  } else{
    p2 <- p1 + geom_abline(intercept = -w[1]/w[3], slope = -w[2]/w[3], ...)
  }
  return(p2)
}

pplot <- function(d, sq){
  if(!sq){
    p1 <- ggplot(d, aes(x = b, y = c)) + 
      geom_point(aes(color = rødder), size = 4)
  } else{
    p1 <- ggplot(d, aes(x = b2, y = c)) + 
      geom_point(aes(color = rødder), size = 4) +
      labs(x = expression(b^2))
  }
  return(p1)
}

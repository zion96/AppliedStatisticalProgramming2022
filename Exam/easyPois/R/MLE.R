mle<-function(y){
  n<-length(y)
  #Getting n, again.

  the_mle<-sum(y)/n

  return(the_mle)
}


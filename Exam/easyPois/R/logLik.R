logLik<-function(y, lambda){
  n<-length(y)
  #Getting the n of the function

  the_ll<-(-n*lambda)-sum(log(factorial(y)))+(log(lambda)*sum(y))
  #Just using the definition of the function from the exam

  return(the_ll)
}

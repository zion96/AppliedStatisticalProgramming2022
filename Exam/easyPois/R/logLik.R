#'Log Likelihood for Poisson Model
#'
#'This calculates the log likelihood in a \code{PoisMLE} object. This is for internal use.
#'The following function calculates log likelihood for a vector of given data where rate parameter
#'\eqn{\lambda} is assumed. This function performs the log likelihood equation given by
#'\deqn{LL(\lambda)=-n\lambda-\sum^n_[i=1]\ln(y_i!)+\ln(y_i!)+\ln(\lambda)\sum^n_[i=1]y_i}.
#'The value that the below function gives is returned when an object in the class \code{PoisMLE} is initialized.
#'
#'@param y is a vector for which all entries are strictly non-negative reals
#'@param lambda is the assumed rate for a Poisson process or Poisson distribution
#'
#'@return The log likelihood value for objects in the class \code{PoisMLE}
#'
#'@example
#'y<-c(1:20)
#'lambda<-mle(y)
#'logLik(y=y, lambda=lambda)
#'
#'@author Zion Little
#'@seealso PoisMLE-class, MLE,StandardError
#'@rdname logLik
#'@aliases logLik
#'
#'@keywords internal
#'
#'@export

logLik<-function(y, lambda){
  n<-length(y)
  #Getting the n of the function

  the_ll<-(-n*lambda)-sum(log(factorial(y)))+(log(lambda)*sum(y))
  #Just using the definition of the function from the exam

  return(the_ll)
}

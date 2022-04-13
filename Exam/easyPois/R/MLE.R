#'MLE for \equn{\lambda} in object of class \code{PoisMLE}
#'
#'For internal use. The function below runs the MLE calculation for Poisson rate \eqn{\lambda}. The following
#'calculation is performed:
#'\deqn{\frac{\lambda^{y_{i} e(-\lambda)} }{y_{i}!}}, which defines \deqn{\P(Y_{i}=y)}.
#'The estimator is used and returned when an object in the class \code{PoisMLE} is initialized.
#'
#'@param y is a vector for which all entries are strictly non-negative reals
#'
#'@return MLE
#'
#'@example
#'y<-c(1:20)
#'y_mle<-mle(y)
#'
#'
#'@author Zion Little
#'@seealso PoisMLE-class, logLik,StandardError
#'@rdname MLE
#'@aliases MLE
#'
#'@keywords internal
#'
#'@export



mle<-function(y){
  n<-length(y)
  #Getting n, again.

  the_mle<-sum(y)/n

  return(the_mle)
}


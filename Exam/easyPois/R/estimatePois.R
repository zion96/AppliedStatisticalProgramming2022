#'Estimate the Poisson model
#'
#'For a vector of strictly non-negative entries, this returns an object in the class of
#'\code{PoisMLE} and \eqn{\hat \lambda}.
#'
#'@param y is a vector for which all entries are strictly non-negative reals
#'@param SEtype Standard error calculation method
#'@param B Is an integer that I have set to 20 as the default. This is not used when \code{"basic"} is selected.
#'
#'@return Object in \code{"PoisMLE"} with the following:
#'#'\itemize{
#'\item \code{y} is a vector for which all entries are strictly non-negative reals
#'\item \code{MLE} Poisson MLE for an entry of y
#'\item \code{LL} Log likelihood from data in y using Poisson MLE estimator
#'\item \code{SE} Standard error of Poisson MLE
#'\item \code{SEtype} Standard error calculation method
#'}
#'@author Zion Little
#'
#'@examples
#'\donttest{the_y<-c(1:20)
#'estimatePois(the_y, SEtype="basic", B=20)}
#'
#'@rdname estimatePois
#'@aliases estimatePois
#'
#'@export
setGeneric(
  name = "estimatePois",
  def = function(y, SEtype, B=20){
    standardGeneric("estimatePois")
  }
)

#'@rdname estimatePois
#'
#'@export
setMethod(
  "estimatePois",
  definition = function(y, SEtype, B=20){
    return(methods::new("PoisMLE",
                        #Initialize object in class "PoisMLE"
                        y = y,
                        MLE = mle(y),
                        LL = logLik(y, mle(y)),
                        SE = standardError(y, SEtype, B),
                        SEtype = SEtype
                        ))
  }
)

#Above slots are filled with the outputs of the other functions that are internal--see documentation for those.

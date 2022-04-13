#'MLE for Poisson
#'
#'Objects in class \code{PoisMLE} are created by the function \code{estimatePois}
#'
#'Objects of class \code{PoisMLE} have the slots"
#'\itemize{
#'\item \code{y} is a vector for which all entries are strictly non-negative reals
#'\item \code{MLE} Poisson MLE for an entry of y
#'\item \code{LL} Log likelihood from data in y using Poisson MLE estimator
#'\item \code{SE} Standard error of Poisson MLE
#'\item \code{SEtype} Standard error calculation method
#'}
#'
#'@author Zion Little: \email{l.zion@@wustl.edu}
#'@aliases estimatePois-class
#'@rdname PoisMLE
#'
#'@export

setClass(Class = "PoisMLE",
         representation=representation(
           y="numeric",
           MLE="numeric",
           LL="numeric",
           SE="numeric",
           SEType="character"
           #So far, I think this will be a character?
         ),
         prototype=prototype(
           y=0,
           MLE=0,
           LL=0,
           SE=0,
           SEType="basic"
         )
  )

setValidity("PoisMLE", function(object){

  if(any(y<0)){
    stop("Your vector of y contains negative numbers...TRY AGAIN")
    #Telling to stop if you feed in negatives. I want the message to be unnecessarily aggressive for this.
  }

  if(object@MLE != mle(object@y)){
    stop("INVALID MLE!!!!!!!!!!!")
    #Test whether MLE is invalid
  }

  if(object@LL != logLik(object@y, object@MLE)){
    stop("INVALID LL")
    #Test for valid LL
  }

  if(!(object@SEtype=="basic" | object@MLE=="bootstrap")){
    stop("Okay...it lists the options for the bootstrap type. USE THE RIGHT ONE. EITHER bootstrap OR basic")
    #Report the above VERY MEAN error message when the wrong SE type is used somehow
    }

}
)

#'@rdname PoisMLE
#'
#'@export

setMethod("initialize", "PoisMLE",
          function(.Object, ...){
            value= methods::callNextMethod()
            #Hellllooooo validator!! (Call validator)
            methods::validObject(value)
            return(value)
          }
  )

#'Calculate the standard error of \code{PoisMLE}
#'
#'For internal use. The function below finds the standard error of the MLE for object of the class \code{PoisMLE}.
#'The user selects an argument for which method to use in the calculation. The two methods are \code{"basic"} and \code{\"bootstrap}.
#'The following calculations are run, respectively:
#'\deqn{\sqrt(\frac{MLE}{n})} ; standard deviation of \code{B} bootstraps of \code{y}.
#'The output value is returned when object in class \code{PoisMLE} is initialized.
#'
#'@param y is a vector for which all entries are strictly non-negative reals
#'@param SEtype is entries of either \code{"basic"} or \code{"bootstrap"}. See description.
#'@param B Is an integer that I have set to 20 as the default. This is not used when \code{"basic"} is selected.
#'
#'@return the standard errors
#'
#'@example
#'y<-c(1:10)
#'basic<- standardError(y = y, SEtype= "basic", B=20)
#'bootstrap<-standardError(y = y, SEtype= "bootstrap", B=20)
#'
#'@author Zion Little
#'@seealso PoisMLE-class, logLik, MLE
#'@rdname StandardError
#'@aliases StandardError
#'
#'@keywords internal
#'
#'@export


standardError<-function(y, SEtype, B=20){
  #Run the if statement when SEtype is basic


  n<-length(y)
  #Getting n again

  if(SEtype=="basic"){

    the_se<-sqrt(mle(y)/n)
  }

  #Run the else if statement when SEtype is bootstrap

    else if (SEtype=="bootstrap"){
      the_samples<-matrix(unlist(lapply(c(1:B),
                                        function(x){sample(y, n, replace = TRUE)})),
                          nrow=n, ncol = B, byrow=FALSE)

    #Just using lappy to to get the samples as defined in the exam

    the_mle_samples<-apply(the_samples, MARGIN = 2, FUN = mle)
    #Create MLE samples as defined in the exam. This passes through the the_samples object

    mle_SD<-std(the_mle_samples)
    #Get mle std

    the_se<-mle_SD
  }


return(the_se)


}


#' integrateIt package
#'
#'This package uses simpson's and trapezoid rule to produce integral approximations
#'
#'@param f is a function over some bounded interval \code{[a,b]} to some real value.
#'@param n is the number of times we want to partition the domain. 
#'@param int is a vector that describes the interval of the domain. 
#'@param approxtype is what the user selects to either use \code{'trapezoid'} or \code{'simpson'} method for approximation. 
#'
#'@return object of one of the two classes--the same as the approximation methods. The return has three outputs:
#'\itemize{
#'\item{x} Subset of domain, partitioned
#'\item{y} Target of the function over the interval
#'\item{result} The numerical approximation 
#'}
#'
#'
#'@author Zion Little: \email{l.zion@@wustl.edu}
#'
#'@examples
#' 
#'integrateIt(exp, 10, c(5,15), "simpson")
#'
#'@seealso trapezoid-class, simpson-class
#'@rdname integrateIt
#'@aliases integrateIt, ANY-method
#'@rdname trapezoid
#'@export




#' @export

setGeneric(name="integrateIt", 
           def=function(f, n, int, approxtype){standardGeneric("integrateIt")}
)
           
#' @export
 
 setMethod(f="integrateIt", 
            definition=function(f, n, int, approxtype){     
             a<-int[1]
             b<-int[2]
             #Here, we're getting the upper and lower bounds of integration
             
             x<-seq(from=a, to=b, by=(b-a)/n)
             
             x_int<-x[a<x & x< b]
             #Interior values of x
             y_int<-unlist(lapply(x_int, f))
             #interior values of y 
             
             y_low<-unlist(lapply(a, f))
             y_up<-unlist(lapply(b, f))
             #get bounds of output with respect to interval
             
             h<-(b-a)/n
             #just h lol
             
             if(approxtype=="trapezoid"){
               result<-(h/2)*sum((2*y_int)+y_low+y_up)
             }
             #Here, we want to say that if user selects trapezoid as the type of approximation, then use the formula for the trapezoid rule 
             
             else if(rule=="simpson"){
               result<-(h/3)*(sum(4*y_int[c(TRUE, FALSE)])+sum(2*y_int)[c(FALSE, TRUE)]+y_low+y_up)
             } 
             #otherwise, if you select simpson, then it runs the simpson formula. 
             
             else{error("you just gave me something that wasn't an approimation type that I wanted")}
             #for whatever reason, if the function gets another approximation type, then return the above error. 

  
             
            return(new(rule, 
                       result=result, 
                       x=c(a, x_int, b), 
                       y=c(y_low, y_int, y_up))) 
             
             
      })




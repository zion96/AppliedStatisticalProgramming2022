#' Trapezoid rule output
#'
#'\code{integrateIt} function creates objects of class \code{"trapezoid"}
#'
#'
#'Objects of this class have the slots
#'\itemize{
#'\item \code{result} Is the numerical approx of integral over interval \code{[a,b]} using trapezoid rule 
#'\item \code{x} is a vector of values from partitions over \code{[a,b]}
#'\item \code{y} is the function f valued on the interval
#'}
#'
#'@author Zion Little: \email{l.zion@@wustl.edu}
#'@aliases trapezoid-class initialize, integrateIt-method
#'@rdname trapezoid
#'@export



setClass(Class="trapezoid", 
         representation = representation(
           result="numeric", 
           x="numeric", 
           y="numeric"),
         prototype = prototype(
           result=0,
           x=0, 
           y=0
         )
)

#simple class set up 

setValidity("trapezoid", function(object){
  
  test1<-any(is.infinite(object@x))
  test2<-any(is.infinite(object@y))
  #The above two tests check whether the x,y are finitely small
  
  test3<-any(is.na(object@x))
  test4<-any(is.na(object@y))
  #These two check that objects x,y are non-empty
  
  
  a<-object@x[1]
  b<-object@x[length(object@x)]
  #Get lower and upper x
  
  y_low<-object@y[1]
  y_up<-object@y[length(object@y)]
  #Get lower and upper y vals
  
  n<-length(object@x)
  #get n
  
  x_int<-object@x[a < object@x & object@x < b]
  y_int<-object@y[y_low < object@y & object@y < y_up]
  #define interior points
  
  h<-(b-a)/n
  #h
  
  the_res<-(h/2)*(sum(2*y_int)+y_low+y_up)
  #appropriate result of what we should get given inputs
  
  test5<-the_res==object@result
  
  if(any(test1, test2, test3, test4, test5)){return("@result invalid")}
}
)

#' @export
setMethod("initialize", "trapezoid", 
          function(.Object, ...){
            value=callNextMethod()
            validObject(value)
            return(value)
          })

#' @rdname trapezoid



#' @export


setMethod(f="print", 
          signature(x="trapezoid"), 
          definition = function(x){
            print(x@result)
          })


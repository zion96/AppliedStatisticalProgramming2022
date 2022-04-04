#' Simpsons rule output
#'
#'\code{integrateIt} function creates objects of class \code{"simpson}
#'
#'
#'Objects of this class have the slots
#'\itemize{
#'\item \code{result} Is the numerical approx of integral over interval \code{[a,b]} using simpson rule 
#'\item \code{x} is a vector of values from partitions over \code{[a,b]}
#'\item \code{y} is the function f valued on the interval
#'}
#'
#'@author Zion Little: \email{l.zion@@wustl.edu}
#'aliases simpson-class initialize, integrateIt-method
#'@rdname simpson
#'@export


setClass(Class = "simpson", 
         representation=representation(
           result="numeric", 
           x="numeric", 
           y="numeric"
         ), 
         prototype = prototype(
           result=0, 
           x=0, 
           y=0
         ))

setValidity("simpson", function(object){
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
  
  the_res<-(h/3)*(sum(4*y_int[c(TRUE, FALSE)])+sum(2*y_int[c(FALSE, TRUE)])+ y_low+y_up)
  test5<-the_res==object@result
  
  if(any(test1, test2, test3, test4, test5)){return("@result invalid")}

})

#' @export

setMethod("initialize", "simpson", 
          function(.Object, ...){
            value=callNextMethod()
            validObject(value)
            return(value)
          }
)

#' @rdname simpson

#' @export
setMethod(f="print", 
          signature(x="simpson"), 
          definition=function(x){
            print(x@result)
          })
















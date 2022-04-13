setClass(Class = "PoisMLE",
         representation=representation(
           y="numeric",
           MLE="numeric",
           LL="numeric",
           SE="numeric",
           SEType="character",
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

})

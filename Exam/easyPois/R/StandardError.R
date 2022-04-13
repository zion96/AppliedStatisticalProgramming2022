
standardError<-function(y, SEtype, B=20){
  #Run the if statement when SEtype is basic

  if(SEtype=="basic"){


    n<-length(y)
    #Getting n again

    the_se<-sqrt(mle(y)/n)
  }

  #Run the else if statement when SEtype is bootstrap

    else if (SEtype=="bootstrap"){
      the_samples<-matrix(unlist(lapply(c(1:B), FUN=sample(y, n , replace=TRUE))), nrow=n, ncol = B)

    #Just using lappy to to get the samples as defined in the exam

    the_mle_samples<-apply(the_samples, MARGIN = 2, FUN = mle)
    #Create MLE samples as defined in the exam. This passes through the the_samples object

    mle_SD<-sd(the_mle_samples)
    #Get mle std

    the_se<-mle_SD
  }


return(the_se)


}


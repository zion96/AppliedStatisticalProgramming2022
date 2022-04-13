
standardError<-function(y, SEtype, B=20){
    n<-length(y)
    #Getting n again

    the_se<-sqrt(mle(y)/n)
    #this is just the se assuming that I'm setting the method to pass the mle through here.
    #I'm not too sure about this yet.

    the_samples<-matrix(unlist(lapply(c(1:B), FUN=sample(y, n , replace=TRUE))), nrow=n, ncol = B)

    #Just using lappy to to get the samples as defined in the exam

    the_mle_samples<-apply(the_samples, MARGIN = 2, FUN = mle)
    #Create MLE samples as defined in the exam. This passes through the the_samples object

    mle_SD<-sd(the_mle_samples)
    #Get mle std

    the_se<-mle_SD





}


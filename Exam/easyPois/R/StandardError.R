
standardError<-function(y, SEtype, B=20){
    n<-length(y)
    #Getting n again

    the_se<-sqrt(mle(y)/n)
    #this is just the se assuming that I'm setting the method to pass the mle through here.
    #I'm not too sure about this yet.

    the_samples<-function(m){

    }

    #Okay now I need to think about whether I want another function in here.


}


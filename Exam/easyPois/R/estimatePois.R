setGeneric(
  name = "estimatePois",
  definition = function(y, SEtype, B=20){
    standardGeneric("estimatePois")
  }
)


setMethod(
  "estimatePois",
  definition = function(y, SEtype, B=20){
    return(methods::new("PoisMLE",
                        y = y,
                        MLE = mle(y),
                        LL = logLik(y, mle(y)),
                        SE = standardError(y, SEtype, B),
                        SEtype = SEtype
                        ))
  }
)


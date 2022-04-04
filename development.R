library(devtools)
library(roxygen2)
setwd("~/Downloads/GitHub/AppliedStatisticalProgramming2022") 


current.code <- as.package("integralIt")
load_all(current.code)
document(current.code)
install(current.code)

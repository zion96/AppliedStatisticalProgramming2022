library(devtools)
library(roxygen2)
setwd("~/Downloads/GitHub/AppliedStatisticalProgramming2022/Exam")


current.code <- as.package("easyPois")
load_all(current.code)
document(current.code)
install(current.code)

check(current.code)

y<-c(1:20)

estimatePois(y, SEtype="basic", B=20)

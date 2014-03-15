library(devtools)
library(roxygen2)
find_rtools()
current.code <- as.package("PolyregPack")
load_all(current.code)
document(current.code)
check(current.code) 
install(pkg=current.code, local=TRUE) 

# run demo
demo(reg)
demo(signi)

# check whether this package works well
?reg
?signi
x <- matrix(rnorm(240,0,2),60,4)
y <- 3+1.2*x[,1]+2.2*x[,2]+0.5*x[,3]+3.2*x[,4]+rnorm(60,0,3)
a <- reg(x,y)
b <- signi(x,y)
c <- signi(a)
plot(a)
plot(b)
plot(c)
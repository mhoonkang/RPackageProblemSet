#' A Poly Regression object 
#' 
#' Object of class \code{polyreg} are created by the \code{reg} functions
#'
#' 
#' An object of the class `polyreg' has the following slots:
#' \itemize{
#' \item{X}{The covariates matrix input}
#' \item{Y}{The dependent variable numeric vector input}
#' \item{coefficient}{A matrix of coefficients in each model}
#' \item{R.squared}{A vector of the value of R-sqaured for each model}
#' }
#'
#' @author Myunghoon Kang: \email{myunghoon@@wustl.edu}
#' @aliases polyreg-class initialize, polyreg-method 
#' @rdname polyreg
#' @export
setClass(Class="polyreg",  # set polyreg class
         representation=representation(
           X="matrix",
           Y="numeric",
           coefficient = "matrix",
           R.squared = "numeric"  
         ),
         prototype=prototype(
           X=matrix(),
           Y=numeric(),
           coefficient=matrix(),
           R.squared=numeric()
         )
)

#' @export
setMethod("initialize", "polyreg", function(.Object, ...){ # initialize polyreg class
  value=callNextMethod()
  validObject(value)
  return(value)
 }
)

#' @export
setGeneric("getX",  # set generic get method
          function(object="polyreg"){
            standardGeneric("getX")
          }
)

#' @export         
setMethod("getX", "polyreg", # set get method for polyreg class. this will get the matrix from slot X of polyreg class object
          function(object){
            return(object@X)
          }
)

#' @export         
setGeneric("getY",  # set generic get method
           function(object="polyreg"){
             standardGeneric("getY")
           }
)

#' @export         
setMethod("getY", "polyreg", # set get method for polyreg class. this will get the vector from slot Y of polyreg class object
          function(object){
            return(object@Y)
          }
)

#' @export         
setMethod("coef", "polyreg", # set coef method for polyreg class. this will get the matrix from slot coefficient of polyreg class object
          function(object){
            return(object@coefficient)
          }
)

#' @export        
setGeneric("getRsquared",  # set generic get method
           function(object="polyreg"){
             standardGeneric("getRsquared")
           }
)

#' @export        
setMethod("getRsquared", "polyreg", # set get method for polyreg class. this will get the Vector from slot R.squared of polyreg class object
          function(object){
            return(object@R.squared)
          }
)

#' @export        
setMethod("print","polyreg", # set print method for polyreg class.
          function(x, ...){
            for(i in 1:ncol(x@coefficient)){
              cat("coefficients (",colnames(x@coefficient)[i],"): \n",sep="")
              a <- x@coefficient[,i][!is.na(x@coefficient[,i])]
              names(a) <- rownames(x@coefficient)[!is.na(x@coefficient[,i])]
              print(round(a,4))
              cat("R-squared:", format(x@R.squared[i]),"\n\n")
            }
          }
)

#' @export 
setMethod("show","polyreg", # set show method for polyreg class. It will return the same things as print returns.
          function(object){
            print(object)
          })

#' @export
setMethod("plot","polyreg", #set plot method. it will plot the value of residual standard error and R-squared for each model.
          function(x,...){  # signi.polygen class object will inherit this method.
            X <- seq(1,length(x@R.squared), by=1)
            Y <- x@R.squared
            object <- signi(x@X, x@Y)
            RSE <- sqrt(object@ssr/object@df) # claculating Residual Standard Error
            par(mai=c(1.3,1.3,1.3,1.3))
            plot(X,Y,type='o',main="The value of residual standard error \n and R-squared for each model", xlab="Model", ylab="R-squared", ylim=c(0,1))
            par(new=TRUE)
            plot(X,RSE,type='o',xlab="", ylab="", axes=FALSE, pch=2, lty=2)
            axis(4,at=round(seq(floor(min(RSE)),ceiling(max(RSE)),length=10),1))
            mtext("Residual standard error", 4, line=2)
            legend("topright", pch=c(1,2), lty=c(1,2), legend=c("R-squared", "RSE"))
}
)

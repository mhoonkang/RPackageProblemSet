#' A Poly Regression object 
#' 
#' Object of class \code{polyreg} are created by the \code{reg} functions
#'
#' 
#' An object of the class `polyreg' has the following slots:
#' \itemize{
#' \item{X}{The covariates matrix input}
#' \item{Y}{The dependent variable numeric vector input}
#' \item{coefficient}{Coefficients for each covariate in each model}
#' \item{R.squared}{The value of R-sqaured for each model}
#' }
#'
#' @author Myunghoon Kang: \email{myunghoon@@wustl.edu}
#' @aliases polyreg-class initialize, polyreg-method 
#' @rdname polyreg
#' @export
setClass(Class="polyreg", 
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
setMethod("initialize", "polyreg", function(.Object, ...){
  value=callNextMethod()
  validObject(value)
  return(value)
 }
)

#' @export
setGeneric("getX", 
          function(object="polyreg"){
            standardGeneric("getX")
          }
)

#' @export
setMethod("getX", "polyreg",
          function(object){
            return(object@X)
          }
)

#' @export
setGeneric("getY", 
           function(object="polyreg"){
             standardGeneric("getY")
           }
)

#' @export
setMethod("getY", "polyreg",
          function(object){
            return(object@Y)
          }
)

#' @export
setMethod("coef", "polyreg",
          function(object){
            return(object@coefficient)
          }
)

#' @export
setGeneric("getRsquared", 
           function(object="polyreg"){
             standardGeneric("getRsquared")
           }
)

#' @export
setMethod("getRsquared", "polyreg",
          function(object){
            return(object@R.squared)
          }
)

#' @export
setMethod("print","polyreg",
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
setMethod("show","polyreg",
          function(object){
            print(object)
          })
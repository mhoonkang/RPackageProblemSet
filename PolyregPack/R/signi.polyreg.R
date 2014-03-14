#' A Significant Polyreg Object. 
#'
#' Object of class \code{signi.polyreg} as created by the \code{signi} function
#'
#' 
#' An object of the class \code{signi.polyreg} has the following slots:
#' \itemize{
#' \item{X}{The covariates matrix input}
#' \item{Y}{The dependent variable numeric vector input}
#' \item{coefficient}{A matrix of coefficients in each model}
#' \item{R.squared}{A vector of the value of R-sqaured for each model}
#' \item{t}{A list of t-stastics for the coefficients in each model}
#' \item{p}{A list of p-values for the coefficients in each model}
#' \item{std.error}{A list of standard errors for the coefficients in each model}
#' \item{df}{Degree of freedom}
#' \item{obs}{The number of observation}
#' \item{ssr}{The squared sum of residuals}
#' }
#' 
#' @author Myunghoon Kang: \email{myunghoon@@wustl.edu}
#' @aliases signi.polyreg-class initialize, signi.polyreg-method 
#' @rdname signi.polyreg
#' @export
setClass(Class="signi.polyreg",  # set signi.polyreg class as a subclass of polyreg
         contains="polyreg",
         representation=representation(
           t = "list",
           p = "list",
           std.error = "list",
           df = "numeric",
           obs = "numeric",
           ssr = "numeric"
         ),
         prototype=prototype(
           X=matrix(),
           Y=numeric(),
           coefficient=matrix(),
           R.squared=numeric(),
           t = list(),
           p = list(),
           std.error=list(),
           df = numeric(),
           obs = numeric(),
           ssr = numeric()
         )
)

#' @export
setMethod("initialize", "signi.polyreg", #initialize signi.polyreg class
          function(.Object, ...){
            value=callNextMethod()
            return(value)
          }
) 

#' @export
setAs(from="polyreg", to="signi.polyreg", #set the method of changing class from polyreg to signi.polyreg
      def=function(from){
        new("signi.polyreg",
            X=from@X,
            Y=from@Y,
            coefficient=from@coefficient,
            R.squared=from@R.squared
        )
      }
)

#' @export
setMethod("print","signi.polyreg", #set print method for signi.polyreg class
          function(x, ...){
            for(i in 1:ncol(x@coefficient)){
              cat("coefficients (",colnames(x@coefficient)[i],"): \n",sep="")
              coefficient <- x@coefficient[,i][!is.na(x@coefficient[,i])]
              table <- cbind(coefficient,x@std.error[[i]], x@t[[i]], x@p[[i]])
              colnames(table) <- c("beta.hat", "std.error", "t.value", 'p.value')
              print(round(table,4))
              cat("R-squared:", format(x@R.squared[i]),"\n")
              cat("Degree of freedom:", x@df[i],"\n")
              cat("The squared sum of residuals:", x@ssr[i],"\n")
              cat("The number of observations:", x@obs,"\n\n")
            }
          }
)

#' @export
setMethod("show","signi.polyreg", # set show method for signi.polyreg class.
          function(object){       # it will show the same thing as print shows. 
            print(object)
          })
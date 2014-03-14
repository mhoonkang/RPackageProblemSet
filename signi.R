#' Calculate P-value of Coefficients for All Models. 
#'
#' This function calculates P-value of coefficients for all models and provides detailed information.
#'
#' @param x A n by k matrix object
#' @param y A numeric vector object with the length of n.
#'
#' @return An object of class polyreg.R containing
#' \item{X}{The covariates matrix input}
#' \item{Y}{The dependent variable numeric vector input}
#' \item{coefficient}{Coefficients for each covariate in each model}
#' \item{R.squared}{The value of R-sqaured for each model}
#' \item{t}{t-stastics for the coefficients for each model}
#' \item{p}{p-value for the coefficients for each model}
#' @author Myunghoon Kang \email{myunghoon@@wustl.edu}
#' @note This function produces a new class of \code{signi.polyreg}.
#' @example
#' 
#' X <- matrix(rnorm(50,0,1),10,5)
#' Y <- rnorm(10,0,1)
#' signi(X,Y)
#' @seealso \code{\link{reg}}.
#' @rdname signi
#' @aliases signi,ANY-method
#' @export
setGeneric(name="signi",
           function(x,y, ...){
             standardGeneric("signi")
           }
)

#' @export
setMethod(f="signi",signature(x="matrix", y="numeric"),
          definition=function(x, y, ...){
            object <- reg(x,y)
            intercept <- rep(1,nrow(x))
            Y <- getY(object)
            X <- getX(object)
            X <- cbind(intercept,X)
            beta.hat <- coef(object)
            table <-lapply(1:ncol(beta.hat), function(i){
              b <- beta.hat[,i][!is.na(beta.hat[,i])]
              X.new <- X[,which(colnames(X)%in%names(b))]
              ee <- t(y-X.new%*%b)%*%(y-X.new%*%b)
              ee.m <- rep(ee, ncol(X.new))
              df <- nrow(X.new)-ncol(X.new)
              df.m <- rep(df,ncol(X.new))
              SS <- as.numeric(ee/df)
              var.b <- SS*solve(t(X.new)%*%X.new)
              Std.Error <- sqrt(diag(var.b))
              t.value <- b/Std.Error
              p.value <- 2*mapply(pt, q=-abs(t.value), df=df)
              result <- cbind(Std.Error,t.value,p.value,df.m,ee.m)
              })
            names(table) <- paste("Model",1:ncol(beta.hat))
            object <- as(object, "signi.polyreg")
            object@t <- lapply(table, function(x) x[,"t.value"])
            object@p <- lapply(table, function(x) x[,"p.value"])
            object@std.error <- lapply(table, function(x) x[,"Std.Error"])
            object@df <- sapply(table,function(x) x[1,"df.m"])
            object@obs <- nrow(X)
            object@ssr <- sapply(table,function(x) x[1,"ee.m"])
            return(object)
            }
)

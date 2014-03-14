#' Calculate P-value of Coefficients for All Models. 
#'
#' This function calculates P-value of coefficients for all models and provides detailed information.
#'
#' @param x A n by k matrix object
#' @param y A numeric vector object with the length of n
#'
#' @return An object of class \code{signi.polyreg} containing
#' \item{X}{The covariates matrix input}
#' \item{Y}{The dependent variable numeric vector input}
#' \item{coefficient}{Coefficients for each covariate in each model}
#' \item{R.squared}{The value of R-sqaured for each model}
#' \item{t}{t-stastics for the coefficients for each model}
#' \item{p}{p-value for the coefficients for each model}
#' \item{std.error}{A list of standard errors for the coefficients in each model}
#' \item{df}{Degree of freedom}
#' \item{obs}{The number of observation}
#' \item{ssr}{The squared sum of residuals}
#' @author Myunghoon Kang \email{myunghoon@@wustl.edu}
#' @note This function produces a new class of \code{signi.polyreg}.
#' @examples
#' 
#' X <- matrix(rnorm(50,0,1),10,5)
#' y <- 2+1.2*X[,1]+2.2*X[,2]+0.2*X[,3]+3.2*X[,4]+1.8*X[,5]+rnorm(10,0,3)
#' signi(X,y)
#' a <- reg(X,y)
#' signi(a)
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
setMethod(f="signi","polyreg", 
          definition=function(x,...){
            intercept <- rep(1,nrow(x@X)) 
            Y <- getY(x)  
            X <- getX(x)  
            X <- cbind(intercept,X) 
            beta.hat <- coef(x) 
            table <-lapply(1:ncol(beta.hat), function(i){ 
              b <- beta.hat[,i][!is.na(beta.hat[,i])] 
              X.new <- X[,which(colnames(X)%in%names(b))] 
              ee <- t(Y-X.new%*%b)%*%(Y-X.new%*%b) 
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
            object <- as(x, "signi.polyreg") 
            object@t <- lapply(table, function(x) x[,"t.value"]) 
            object@p <- lapply(table, function(x) x[,"p.value"])
            object@std.error <- lapply(table, function(x) x[,"Std.Error"])
            object@df <- sapply(table,function(x) x[1,"df.m"])
            object@obs <- nrow(X)
            object@ssr <- sapply(table,function(x) x[1,"ee.m"])
            return(object)
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
              ee <- t(Y-X.new%*%b)%*%(Y-X.new%*%b)
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
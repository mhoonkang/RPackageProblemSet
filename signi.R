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
setGeneric(name="signi",    # set generic signi function
           function(x,y, ...){
             standardGeneric("signi")
           }
)

#' @export
setMethod(f="signi","polyreg",  # signi function method in case that an input is polyreg class object
          definition=function(x,...){
            intercept <- rep(1,nrow(x@X)) # make one vector
            Y <- getY(x)                  # extract Y from input
            X <- getX(x)                  # extract X from input
            X <- cbind(intercept,X)       # make a covariates matrix including one vector
            beta.hat <- coef(x)           # extract coefficient from input
            table <-lapply(1:ncol(beta.hat), function(i){ # make a list of coefficients with standard error, t-statistic, p value in each model
              b <- beta.hat[,i][!is.na(beta.hat[,i])]  # extract coefficient of the covariates used in the model
              X.new <- X[,which(colnames(X)%in%names(b))] # extraact covariates used in the model
              ee <- t(Y-X.new%*%b)%*%(Y-X.new%*%b) # calculate squared sum of residuals 
              ee.m <- rep(ee, ncol(X.new))  # transform ee into a vector form with length of the covariates
              df <- nrow(X.new)-ncol(X.new) # calculate degree of freedom
              df.m <- rep(df,ncol(X.new))  # transform df into a vector form with length of the covariates
              SS <- as.numeric(ee/df) # calculate estimated sigma squared
              var.b <- SS*solve(t(X.new)%*%X.new)  # make a variance-covariance matrix of themodel
              Std.Error <- sqrt(diag(var.b)) # calculated standard error for each coefficient in the model
              t.value <- b/Std.Error # calculate t-statistic for each coefficient in the model
              p.value <- 2*mapply(pt, q=-abs(t.value), df=df) # calculate p-value for each coefficient in the model
              result <- cbind(Std.Error,t.value,p.value,df.m,ee.m) # make a matrix of standard error, t-statistics, p-values, degree of freedom, squared sum of residuals in the model
            })
            names(table) <- paste("Model",1:ncol(beta.hat)) 
            object <- as(x, "signi.polyreg")  # change class of input from polyreg to signi.polyreg
            object@t <- lapply(table, function(x) x[,"t.value"]) # from here, assign each value to designated slot in signi.polyreg class object
            object@p <- lapply(table, function(x) x[,"p.value"])
            object@std.error <- lapply(table, function(x) x[,"Std.Error"])
            object@df <- sapply(table,function(x) x[1,"df.m"])
            object@obs <- nrow(X)
            object@ssr <- sapply(table,function(x) x[1,"ee.m"])
            return(object)
          }
)
          
#' @export
setMethod(f="signi",signature(x="matrix", y="numeric"), # signi function method in case that an input is not polyreg class object
          definition=function(x, y, ...){ # x should be a matirx, y should be a numeric vector                 
            object <- reg(x,y) # by using reg function, create polyreg class object
            intercept <- rep(1,nrow(x)) # the following is the same as the above code.
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
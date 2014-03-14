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
setMethod(f="signi","polyreg", # in case for that an input is a class of polyreg.
          definition=function(x,...){
            intercept <- rep(1,nrow(x@X)) # make one vector to calculate intercept
            Y <- getY(x)  # extract Y from polyreg object
            X <- getX(x)  # extract X from polyreg object
            X <- cbind(intercept,X) # make covariate matrix including one vector
            beta.hat <- coef(x) # extract coefficient from polyreg object
            table <-lapply(1:ncol(beta.hat), function(i){ # calculate t-statistics, p-value and others for each model as a list
              b <- beta.hat[,i][!is.na(beta.hat[,i])] # refine beta corresponding to the covariates in the model
              X.new <- X[,which(colnames(X)%in%names(b))] # refine X corresponding to the covariates in the model
              ee <- t(Y-X.new%*%b)%*%(Y-X.new%*%b) # squared sum of residual
              ee.m <- rep(ee, ncol(X.new)) # make squared sum of residuals's length corresponding to the number of the covariates in order to combine with other statistics in a matrix form
              df <- nrow(X.new)-ncol(X.new) # degree of freedom
              df.m <- rep(df,ncol(X.new)) # this is same work as 'ee.m' above
              SS <- as.numeric(ee/df) # estimated sigma squared
              var.b <- SS*solve(t(X.new)%*%X.new) #estimated variance for each coefficient
              Std.Error <- sqrt(diag(var.b)) # standard error for each coefficient
              t.value <- b/Std.Error # t statistic for each coefficient
              p.value <- 2*mapply(pt, q=-abs(t.value), df=df) # p value for each coefficient
              result <- cbind(Std.Error,t.value,p.value,df.m,ee.m) # makes a matrix of the above statistics
            })
            names(table) <- paste("Model",1:ncol(beta.hat)) # give name for each item of list
            object <- as(x, "signi.polyreg") #change the class from polyreg to signi.polyreg
            object@t <- lapply(table, function(x) x[,"t.value"]) # from here, assign each value to appropriate slot of signi.polyreg class object
            object@p <- lapply(table, function(x) x[,"p.value"])
            object@std.error <- lapply(table, function(x) x[,"Std.Error"])
            object@df <- sapply(table,function(x) x[1,"df.m"])
            object@obs <- nrow(X)
            object@ssr <- sapply(table,function(x) x[1,"ee.m"])
            return(object)
          }
)
            

#' @export
setMethod(f="signi",signature(x="matrix", y="numeric"), #in case for the input is not polyreg class
          definition=function(x, y, ...){
            object <- reg(x,y) # by using reg function, obtain a polyreg class object
            intercept <- rep(1,nrow(x)) # the following is the same as the above
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

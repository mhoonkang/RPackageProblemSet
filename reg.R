#' Regression for All Possible Combinations of the Covariates
#' 
#' This function conducts regression for all possible combinations of the covariates.
#' 
#' @param x A n by k matrix object
#' @param y A numeric vector object with the length of n.
#' 
#' @return A class of "polyreg" with the elements
#' \item{X}{The covariates matrix input}
#' \item{Y}{The dependent variable numeric vector input}
#' \item{coefficient}{Coefficients for each covariate in each model}
#' \item{R.squared}{The value of R-sqaured for each model}
#' @author Myunghoon Kang \email{myunghoon@@wustl.edu}
#' @note This produces an object of a new class \code{polyreg}.
#' @examples
#' 
#' X <- matrix(rnorm(50,0,1),10,5)
#' y <- 2+1.2*X[,1]+2.2*X[,2]+0.2*X[,3]+3.2*X[,4]+1.8*X[,5]+rnorm(10,0,3)
#' reg(X,y)
#' @seealso \code{\link{signi}}.
#' @rdname reg
#' @aliases reg,ANY-method
#' @export
setGeneric(name="reg", # set generic method
           function(x,y, ...){
             standardGeneric("reg")
            }
           )

#' @export
setMethod(f="reg", signature(x="matrix", y="numeric"), # the first input is a matrix and the second input is a numeric vector.
          definition=function(x,y){
            if(is.null(colnames(x))) colnames(x) <- paste("x",1:ncol(x),sep="") # if the matrix does not have a variable name, then the function gives names of x1,x2,...,xk
            one <- rep(1,nrow(x)) # make one vector for calculating an intercept
            M <- one%*%solve(t(one)%*%one)%*%t(one) # matrix necessary for calculating R-squared 
            combi <- lapply(1:ncol(x), function(i) combn(1:ncol(x),i)) # calculating all possible combinations of the covrariates
            X <- cbind(one, x) # make a covariates matrix including one vector
            if(ncol(x)==1){ # if the number of the covariate is 1 then only one model is possible
              beta <- solve(t(X)%*%X)%*%t(X)%*%y # calculating coefficient
              R.squared <- (t(y)%*%X%*%solve(t(X)%*%X)%*%t(X)%*%y-t(y)%*%M%*%y)/(t(y)%*%y-t(y)%*%M%*%y) # calculating R-squared
              rownames(beta) <- c("intercept", colnames(x))
              colnames(beta) <- paste("Model",1:ncol(beta))
              R.squared <- as.numeric(R.squared)
              names(R.squared) <- paste("Model",1:ncol(beta))
              return(new("polyreg", X=x, Y=y, coefficient=beta, R.squared=R.squared)) # return polyreg class object
            }
            beta <- sapply(1:ncol(x), function(i){ # calculating coefficient (when the number of the covariates larger than 1)
              sapply(1:ncol(combi[[i]]), function(j){
                solve(t(X[,c(1,combi[[i]][,j]+1)])%*%X[,c(1,combi[[i]][,j]+1)])%*%t(X[,c(1,combi[[i]][,j]+1)])%*%y
              })
            })
            R.squared <- sapply(1:ncol(x), function(i){ # calculating R-squared (when the number of the covariates larger than 1)
              sapply(1:ncol(combi[[i]]), function(j){
                 (t(y)%*%X[,c(1,combi[[i]][,j]+1)]%*%solve(t(X[,c(1,combi[[i]][,j]+1)])%*%X[,c(1,combi[[i]][,j]+1)])%*%t(X[,c(1,combi[[i]][,j]+1)])%*%y-t(y)%*%M%*%y)/(t(y)%*%y-t(y)%*%M%*%y)
              })
            })
            intercept <- unlist(sapply(1:ncol(x), function(i) beta[[i]][1,])) # extract intercept from each model
            beta <- lapply(beta, function(x) x <- x[-1,,drop=FALSE]) # make a list of coefficients for each model
            combi <- sapply(1:ncol(x), function(i) matrix(as.vector(t(combi[[i]])), ncol(x), ncol(combi[[i]]), byrow=TRUE)) # make a list of a matrix of information for each model(show which covariate is included...)
            combi <- unlist(combi) # this and the next line make a matrix showing which covariates are included in each model
            combi <- matrix(combi, ncol(x))
            beta <- sapply(1:ncol(x), function(i) matrix(as.vector(t(beta[[i]])), ncol(x), ncol(beta[[i]]), byrow=TRUE)) # make a list of coefficient matrix for each model
            beta <- unlist(beta) # this and the next line make a k by the number of models matrix of coefficients
            beta <- matrix(beta, ncol(x))
            coeff.covariate <- NULL
            for(i in 1:ncol(x)){ # make a coefficient matrix. row is variable, column is each model
              for(j in 1:ncol(beta)){
                temporary <- unique(beta[,j]*(combi[,j]==i))
                coeff.covariate.new <- ifelse(length(temporary)>1, temporary[temporary!=0], temporary)
                coeff.covariate <- c(coeff.covariate, coeff.covariate.new)
              }
            }
            coeff.covariate <- matrix(coeff.covariate, ncol(x), ncol(beta), byrow=TRUE)
            beta <- rbind(intercept, coeff.covariate) # combine intercept with a coefficient matrix
            beta[beta==0]<-NA # assign NA to the covariates that are not used for each model
            rownames(beta) <- c("intercept", colnames(x))
            colnames(beta) <- paste("Model",1:ncol(beta))
            R.squared<-unlist(R.squared) # make R-squared vector
            names(R.squared) <- paste("Model",1:ncol(beta))
            return(new("polyreg", X=x, Y=y, coefficient=beta, R.squared=R.squared)) #return polyreg class object
          })
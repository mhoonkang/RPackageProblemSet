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
#' @note This produces an object of a new class "polyreg".
#' @example
#' 
#' X <- matrix(rnorm(50,0,1),10,5)
#' Y <- rnorm(10,0,1)
#' reg(X,Y)
#' @seealso \code{\link{mrs}} and \code{\link{signifi}}.
#' @rdname reg
#' @aliases reg,ANY-method
#' @export
setGeneric(name="reg",
           function(x,y, ...){
             standardGeneric("reg")
            }
           )

#' @export
setMethod(f="reg", signature(x="matrix", y="numeric"),
          definition=function(x,y){
            if(is.null(colnames(x))) colnames(x) <- paste("V",1:ncol(x),sep="")
            one <- rep(1,nrow(x))
            combi <- lapply(1:ncol(x), function(i) combn(1:ncol(x),i))
            X <- cbind(one, x)
            if(ncol(x)==1){
              beta <- solve(t(X)%*%X)%*%t(X)%*%y
              R.squared <- (t(y)%*%X%*%solve(t(X)%*%X)%*%t(X)%*%y)/(t(y)%*%y)
              rownames(beta) <- c("intercept", colnames(x))
              colnames(beta) <- paste("Model",1:ncol(beta))
              R.squared <- as.numeric(R.squared)
              names(R.squared) <- paste("Model",1:ncol(beta))
              return(new("polyreg", X=x, Y=y, coefficient=beta, R.squared=R.squared))
            }
            beta <- sapply(1:ncol(x), function(i){
              sapply(1:ncol(combi[[i]]), function(j){
                solve(t(X[,c(1,combi[[i]][,j]+1)])%*%X[,c(1,combi[[i]][,j]+1)])%*%t(X[,c(1,combi[[i]][,j]+1)])%*%y
              })
            })
            R.squared <- sapply(1:ncol(x), function(i){
              sapply(1:ncol(combi[[i]]), function(j){
                (t(y)%*%X[,c(1,combi[[i]][,j]+1)]%*%solve(t(X[,c(1,combi[[i]][,j]+1)])%*%X[,c(1,combi[[i]][,j]+1)])%*%t(X[,c(1,combi[[i]][,j]+1)])%*%y)/(t(y)%*%y)
              })
            })
            intercept <- unlist(sapply(1:ncol(x), function(i) beta[[i]][1,]))
            beta <- lapply(beta, function(x) x <- x[-1,,drop=FALSE])
            combi <- sapply(1:ncol(x), function(i) matrix(as.vector(t(combi[[i]])), ncol(x), ncol(combi[[i]]), byrow=TRUE))
            combi <- unlist(combi)
            combi <- matrix(combi, ncol(x))
            beta <- sapply(1:ncol(x), function(i) matrix(as.vector(t(beta[[i]])), ncol(x), ncol(beta[[i]]), byrow=TRUE))
            beta <- unlist(beta)
            beta <- matrix(beta, ncol(x))
            coeff.covariate <- NULL
            for(i in 1:ncol(x)){
              for(j in 1:ncol(beta)){
                temporary <- unique(beta[,j]*(combi[,j]==i))
                coeff.covariate.new <- ifelse(length(temporary)>1, temporary[temporary!=0], temporary)
                coeff.covariate <- c(coeff.covariate, coeff.covariate.new)
              }
            }
            coeff.covariate <- matrix(coeff.covariate, ncol(x), ncol(beta), byrow=TRUE)
            beta <- rbind(intercept, coeff.covariate)
            beta[beta==0]<-NA
            rownames(beta) <- c("intercept", colnames(x))
            colnames(beta) <- paste("Model",1:ncol(beta))
            R.squared<-unlist(R.squared)
            names(R.squared) <- paste("Model",1:ncol(beta))
            object <- new("polyreg", X=x, Y=y, coefficient=beta, R.squared=R.squared)
            return(new("polyreg", X=x, Y=y, coefficient=beta, R.squared=R.squared))
          })


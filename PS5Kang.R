#' Regression for All Possible Combinations of the Covariates
#' 
#' Regresses all possible combinations of the covariates
#' 
#' @param x A matrix object
#' @param y A numeric object with the same length of rows as \code{x}
#' 
#' @return A class of regression with the elements
#' \item{coefficient}{Coefficients for each covariate}
#' \item{R.squared}{The value of R-sqaured}
#' \item{X}{The covariates matrix input}
#' \item{Y}{The dependent variable numeric vector input}
#' @author Myunghoon Kang
#' @note This is an exercise for Topics in Applied Statistical Programming
#' @example
#' 
#' X <- matrix(rnorm(50,0,1),10,5)
#' Y <- rnorm(10,0,1)
#' colnames(x) <- c("v1","v2","v3","v4","v5")
#' reg(X,Y)
#' @rdname Regression
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

setValidity("polyreg", function(object){
  test1 <- length(object@Y)==nrow(object@X)
  if(!test1){return("Y and X length differs")}
})

setMethod("initialize", "polyreg", function(.Object, ...){
  value=callNextMethod()
  validObject(value)
  return(value)
})

setGeneric("reg",
           function(x,y){
             standardGeneric("reg")
})

setMethod("reg", signature(x="matrix", y="numeric"),
          function(x,y){
            if(is.null(colnames(x))) colnames(x) <- paste("V",1:ncol(x),sep="")
            object <- new("polyreg", X=x, Y=y)
            one <- rep(1,nrow(object@X))
            combi <- lapply(1:ncol(x), function(i) combn(1:ncol(x),i))
            X <- cbind(one, object@X)
            Y <- object@Y
            if(ncol(x)==1){
              beta <- solve(t(X)%*%X)%*%t(X)%*%Y
              R.squared <- (t(Y)%*%X%*%solve(t(X)%*%X)%*%t(X)%*%Y)/(t(Y)%*%Y)
              rownames(beta) <- c("intercept", colnames(x))
              colnames(beta) <- paste("Model",1:ncol(beta))
              object@coefficient <- beta
              object@R.squared <- as.numeric(R.squared)
              names(object@R.squared) <- paste("Model",1:ncol(beta))
              return(object)
            }
            beta <- sapply(1:ncol(x), function(i){
              sapply(1:ncol(combi[[i]]), function(j){
                solve(t(X[,c(1,combi[[i]][,j]+1)])%*%X[,c(1,combi[[i]][,j]+1)])%*%t(X[,c(1,combi[[i]][,j]+1)])%*%Y
              })
            })
            R.squared <- sapply(1:ncol(x), function(i){
              sapply(1:ncol(combi[[i]]), function(j){
                (t(Y)%*%X[,c(1,combi[[i]][,j]+1)]%*%solve(t(X[,c(1,combi[[i]][,j]+1)])%*%X[,c(1,combi[[i]][,j]+1)])%*%t(X[,c(1,combi[[i]][,j]+1)])%*%Y)/(t(Y)%*%Y)
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
            if(is.null(colnames(x))) colnames(x) <- paste("V",1:ncol(x),sep="")
            rownames(beta) <- c("intercept", colnames(x))
            colnames(beta) <- paste("Model",1:ncol(beta))
            object@coefficient <- beta
            object@R.squared<-unlist(R.squared)
            names(object@R.squared) <- paste("Model",1:ncol(beta))
            return(object)
          })


x <- matrix(rnorm(120,0,1),60,2)
y <- 2*x[,1]+0.5*x[,2]+3+rnorm(60,0,1)
reg(x,y)
x <- matrix(rnorm(120,0,1),120,1)
y <- 2*x[,1]+3+rnorm(120,0,1)
reg(x,y)
x <- matrix(rnorm(240,0,1),60,4)
y <- 2*x[,1]+0.5*x[,2]+0.1*x[,3]+1.2*x[,4]+3+rnorm(60,0,1)
colnames(x) <- paste("A",1:4,sep="")
reg(x,y)
x <- matrix(rnorm(320,0,1),40,8)
y <- 2*x[,1]+0.5*x[,2]+0.1*x[,3]+1.2*x[,4]+1.1*x[,5]+2.2*x[,6]+0.3*x[,7]+1.8*x[,8]+3+rnorm(40,0,1)
colnames(x) <- paste("A",1:8,sep="")
reg(x,y)


rm(list=ls())

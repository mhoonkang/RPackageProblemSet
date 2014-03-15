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
setGeneric(name="reg",     # set generic method for reg
           function(x,y, ...){
             standardGeneric("reg")
            }
           )

#' @export
setMethod(f="reg", signature(x="matrix", y="numeric"),  # reg has two inputs. x(covariates) should be a matrix, y(outcome) should be a numeric vector
          definition=function(x,y){
            if(is.null(colnames(x))) colnames(x) <- paste("x",1:ncol(x),sep="") # if covariates does not have a name, reg gives default name. (x1, x2, ...)
            one <- rep(1,nrow(x)) # make one vector for calculating an intercept
            M <- one%*%solve(t(one)%*%one)%*%t(one) # this matrix will be used when R-squared is calculated
            combi <- lapply(1:ncol(x), function(i) combn(1:ncol(x),i)) #a list of all possible combinations of the covariates
            X <- cbind(one, x) # covariate matrix combined with one vector
            if(ncol(x)==1){ # if the number of the covariate is one, then only one combination is possible.
              beta <- solve(t(X)%*%X)%*%t(X)%*%y  # calculate coefficient in the model
              R.squared <- (t(y)%*%X%*%solve(t(X)%*%X)%*%t(X)%*%y-t(y)%*%M%*%y)/(t(y)%*%y-t(y)%*%M%*%y) # calculate R-squared in the model
              rownames(beta) <- c("intercept", colnames(x)) # recode the name of covariates and intercept
              colnames(beta) <- paste("Model",1:ncol(beta)) # recode the name of the models
              R.squared <- as.numeric(R.squared) 
              names(R.squared) <- paste("Model",1:ncol(beta)) # recode the name of the model for each R-squared
              return(new("polyreg", X=x, Y=y, coefficient=beta, R.squared=R.squared)) # create a polyreg class object
            }
            beta <- sapply(1:ncol(x), function(i){ # when the number of covariates are larger than one,
              sapply(1:ncol(combi[[i]]), function(j){ # this makes a list consist of coefficient matrix for each model
                solve(t(X[,c(1,combi[[i]][,j]+1)])%*%X[,c(1,combi[[i]][,j]+1)])%*%t(X[,c(1,combi[[i]][,j]+1)])%*%y
              })
            })
            R.squared <- sapply(1:ncol(x), function(i){ # when the number of covariates are larger than one,
              sapply(1:ncol(combi[[i]]), function(j){ # this makes a vector consist of R-qaured for each model
                 (t(y)%*%X[,c(1,combi[[i]][,j]+1)]%*%solve(t(X[,c(1,combi[[i]][,j]+1)])%*%X[,c(1,combi[[i]][,j]+1)])%*%t(X[,c(1,combi[[i]][,j]+1)])%*%y-t(y)%*%M%*%y)/(t(y)%*%y-t(y)%*%M%*%y)
              })
            })
            intercept <- unlist(sapply(1:ncol(x), function(i) beta[[i]][1,])) # make a vector of intercept for each model
            beta <- lapply(beta, function(x) x <- x[-1,,drop=FALSE]) # delete intercept from the list of coefficient matrix
            combi <- sapply(1:ncol(x), function(i) matrix(as.vector(t(combi[[i]])), ncol(x), ncol(combi[[i]]), byrow=TRUE)) 
            combi <- unlist(combi) 
            combi <- matrix(combi, ncol(x)) # lines beginning with combi creates a information matrix showing that which covariates are used in each model
            beta <- sapply(1:ncol(x), function(i) matrix(as.vector(t(beta[[i]])), ncol(x), ncol(beta[[i]]), byrow=TRUE)) 
            beta <- unlist(beta) 
            beta <- matrix(beta, ncol(x)) # lines beginning with beta creates coefficient matrix. row:the number of all covariates plus intercept. column: the number of all possible combinations of the covariates
            coeff.covariate <- NULL
            for(i in 1:ncol(x)){  # this for loop eliminates duplicated coefficients in each column of the coefficient matrix
              for(j in 1:ncol(beta)){ # and return vector of coefficient
                temporary <- unique(beta[,j]*(combi[,j]==i))
                coeff.covariate.new <- ifelse(length(temporary)>1, temporary[temporary!=0], temporary)
                coeff.covariate <- c(coeff.covariate, coeff.covariate.new)
              }
            }
            coeff.covariate <- matrix(coeff.covariate, ncol(x), ncol(beta), byrow=TRUE) # make a coefficient matrix without any duplication
            beta <- rbind(intercept, coeff.covariate) # combine intercept vector into the covariate matrix
            beta[beta==0]<-NA # coefficient for the covariates not used in each model will be assigned the value of NA
            rownames(beta) <- c("intercept", colnames(x)) # giving corresponding names for each row and column
            colnames(beta) <- paste("Model",1:ncol(beta))
            R.squared<-unlist(R.squared) # create R-squared vector
            names(R.squared) <- paste("Model",1:ncol(beta)) # giving corresponding names for each element
            return(new("polyreg", X=x, Y=y, coefficient=beta, R.squared=R.squared)) #create polyreg class object
          })
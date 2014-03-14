\name{reg}
\alias{reg}
\alias{reg,ANY-method}
\title{Regression for All Possible Combinations of the Covariates}
\usage{
reg(x, y, ...)
}
\arguments{
  \item{x}{A n by k matrix object}

  \item{y}{A numeric vector object with the length of n.}
}
\value{
A class of "polyreg" with the elements \item{X}{The
covariates matrix input} \item{Y}{The dependent variable
numeric vector input} \item{coefficient}{Coefficients for
each covariate in each model} \item{R.squared}{The value of
R-sqaured for each model}
}
\description{
This function conducts regression for all possible
combinations of the covariates.
}
\note{
This produces an object of a new class \code{polyreg}.
}
\examples{
X <- matrix(rnorm(50,0,1),10,5)
y <- 2+1.2*X[,1]+2.2*X[,2]+0.2*X[,3]+3.2*X[,4]+1.8*X[,5]+rnorm(10,0,3)
reg(X,y)
}
\author{
Myunghoon Kang \email{myunghoon@wustl.edu}
}
\seealso{
\code{\link{signi}}.
}


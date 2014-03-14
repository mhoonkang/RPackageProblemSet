\name{signi}
\alias{signi}
\alias{signi,ANY-method}
\title{Calculate P-value of Coefficients for All Models.}
\usage{
signi(x, y, ...)
}
\arguments{
  \item{x}{A n by k matrix object}

  \item{y}{A numeric vector object with the length of n}
}
\value{
An object of class \code{signi.polyreg} containing
\item{X}{The covariates matrix input} \item{Y}{The
dependent variable numeric vector input}
\item{coefficient}{Coefficients for each covariate in each
model} \item{R.squared}{The value of R-sqaured for each
model} \item{t}{t-stastics for the coefficients for each
model} \item{p}{p-value for the coefficients for each
model} \item{std.error}{A list of standard errors for the
coefficients in each model} \item{df}{Degree of freedom}
\item{obs}{The number of observation} \item{ssr}{The
squared sum of residuals}
}
\description{
This function calculates P-value of coefficients for all
models and provides detailed information.
}
\note{
This function produces a new class of \code{signi.polyreg}.
}
\examples{
X <- matrix(rnorm(50,0,1),10,5)
y <- 2+1.2*X[,1]+2.2*X[,2]+0.2*X[,3]+3.2*X[,4]+1.8*X[,5]+rnorm(10,0,3)
signi(X,y)
a <- reg(X,y)
signi(a)
}
\author{
Myunghoon Kang \email{myunghoon@wustl.edu}
}
\seealso{
\code{\link{reg}}.
}


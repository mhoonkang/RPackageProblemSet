\docType{class}
\name{signi.polyreg-class}
\alias{initialize,}
\alias{signi.polyreg-class}
\alias{signi.polyreg-method}
\title{A Significant Polyreg Object.}
\description{
Object of class \code{signi.polyreg} as created by the
\code{signi} function
}
\details{
An object of the class \code{signi.polyreg} has the
following slots: \itemize{ \item{X}{The covariates matrix
input} \item{Y}{The dependent variable numeric vector
input} \item{coefficient}{A matrix of coefficients in each
model} \item{R.squared}{A vector of the value of R-sqaured
for each model} \item{t}{A list of t-stastics for the
coefficients in each model} \item{p}{A list of p-values for
the coefficients in each model} \item{std.error}{A list of
standard errors for the coefficients in each model}
\item{df}{Degree of freedom} \item{obs}{The number of
observation} \item{ssr}{The squared sum of residuals} }
}
\author{
Myunghoon Kang: \email{myunghoon@wustl.edu}
}


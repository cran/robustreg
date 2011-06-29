\name{robustRegH}
\alias{robustRegH}
\title{Robust Regression Function using Huber Psi Function}
\description{
Using iteratively reweighted least squares (IRLS), the function calculates the optimal weights to perform m-estimator or bounded influence regression.  Returns robust beta estimates and prints robust ANOVA table}
\usage{
robustRegH(y,X,tune=1.345,beta,m=TRUE,max.it=1000,tol=1e-10)}
\details{
M-estimates of beta should be used when evaluating least squares estimates of beta and diagnostics show outliers.  Least squares estimates of beta should be used as starting points to achieve convergence.

Bounded influence estimates of beta should be used when evaluating least squares estimates of beta and diagnostics show large values of the "Hat Matrix" diagonals and outliers.}
\arguments{
\item{y}{A vector of dependent variables}
\item{X}{A data frame or matrix of independent variables/regressors}
\item{tune}{Tuning Constant.  Default value of 1.345 is 95\% asymptotically efficient against outliers}
\item{beta}{Starting estimates of beta for algorithm}
\item{m}{If \code{TRUE}, calculates m estimates of beta.  If \code{FALSE}, calculates bounded influence estimates of beta}
\item{max.it}{Maximum number of iterations to achieve convergence in IRLS algorithm}
\item{tol}{Tolerance level in determining convergence}
}
                                                                                                                                            \author{Ian M. Johnson \email{ian@alpha-analysis.com}}
\note{\url{http://www.alpha-analysis.com/robustreg.html}}
\references{Huber, 
 
 Birch, Robust F-Test, 1983}
\seealso{robustRegBS()}
\examples{
data(stackloss)
X<-data.frame(stackloss$Air.Flow,stackloss$Water.Temp)
y<-stackloss$stack.loss

#calculate least squares estimates for starting point
m1<-lm(stack.loss~Air.Flow+Water.Temp,data=stackloss)$coefficients

robustRegH(y,X,beta=m1)

#If X matrix contained large values of H matrix (high influence points)
robustRegH(y,X,beta=m1,m=FALSE)
}
\keyword{regression}


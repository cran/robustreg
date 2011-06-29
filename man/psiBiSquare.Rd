\name{psiBiSquare}
\alias{psiBiSquare}
\title{Bisquare Psi Function}
\description{
Calculates residual from Bisquare psi function}
\usage{
psiBiSquare(r,c)}
\details{
Calculates residual from Bisquare psi function}
\arguments{
\item{r}{Value to be evaluated by function}
\item{c}{Tuning constant}
}
\value{
 r:	Value to be evaluated by function

 c:	Tuning constant
}                                                                                                                                             \author{Ian M. Johnson \email{ian@alpha-analysis.com}}
\note{\url{http://www.alpha-analysis.com/robustreg.html}}
\references{Tukey}
\seealso{psiHuber()}
\examples{
a<-rnorm(1000)
psi<-psiBiSquare(a,4.685)
plot(a,psi)
}
\keyword{regression}


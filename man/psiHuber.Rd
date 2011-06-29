\name{psiHuber}
\alias{psiHuber}
\title{Huber Psi Function}
\description{
Calculates residual from Huber psi function}
\usage{
psiHuber(r,c)}
\details{
Calculates residual from Huber psi function}
\arguments{
\item{r}{Value to be evaluated by function}
\item{c}{Tuning constant}
}
\value{
 r:	Value to be evaluated by function

 c:	Tuning constant
}                                                                                                                                             \author{Ian M. Johnson \email{ian@alpha-analysis.com}}
\note{\url{http://www.alpha-analysis.com/robustreg.html}}
\references{Huber}
\seealso{psiBiSquare()}
\examples{
a<-rnorm(1000)
psi<-psiHuber(a,1.345)
plot(a,psi)
}
\keyword{regression}


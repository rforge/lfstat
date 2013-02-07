\name{rfa}
\alias{rfa}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Regional Frequency Analysis
}
\description{
This function uses J.R.M. Hosking's package lmom to produce a L-moment diagram.
}
\usage{
rfa(lflist, n = 7, event = 100, dist =  c("wei","gev","ln3","gum","pe3"), plot = TRUE,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lflist}{A list of "lfobj"s}
  \item{n}{MAM-n is used (e.g. n=7 means MAM7)}
  \item{event}{A value for T, e.g. event = 100 means the 100 years
    extreme low flow event}
  \item{dist}{A vector of distribution to fit, the names are according to
  Hostking's in his "lmom" package. Can be an of
  "wei","gev","ln3","gum","pe3".}
  \item{plot}{logical, shoult a L-moment diagram be plotted?}
  \item{...}{is passed to Hosking's function \code{\link{lmrd}}.}
}


\references{
Manual on Low-flow Estimation and Prediction, Operational Hydrology
Report No. 50, Koblenz 2009

J. R. M. Hosking (2012). L-moments. R package, version 1.6. URL:
  http://CRAN.R-project.org/package=lmom.
}
\author{
Daniel Koffler and Gregor Laaha
}

\examples{
data(ngaruroro)

#Toy example to get some more "rivers"
seventies <- subset(ngaruroro, hyear \%in\% 1970:1979)
eighties <- subset(ngaruroro, hyear \%in\% 1980:1989)
nineties <- subset(ngaruroro, hyear \%in\% 1990:1999)

rfa(list(seventies,eighties,nineties), n=3,dist = "gev")
}
\keyword{Regional Frequency Analysis}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

\seealso{\code{\link{lmom}}}
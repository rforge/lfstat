\name{lfnacheck}
\alias{lfnacheck}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Low flow object check for missing values.
}
\description{
Looks for NAs in a lfobj.
}
\usage{
lfnacheck(lfobj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{An object of class "lfobj"
}
}

\value{
A list with the total number of NAs, the percentage, the NAs for every
year and the durations of NA-series.
}
\references{
Manual on Low-flow Estimation and Prediction, Operational Hydrology Report No. 50, Koblenz 2009
}

\seealso{\code{\link{bfplot}}}

\examples{
data(ngaruroro)
lfnacheck(ngaruroro)
}
\keyword{}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

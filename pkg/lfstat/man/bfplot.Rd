\name{bfplot}
\alias{bfplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Base Flow Plot
}
\description{
Visualises the hydrograph versus the base flow hydrograph.
}
\usage{
bfplot(lfobj,
       year = "any",
       col = "green",
       bfcol = "blue",
       ylog = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{
An object of class "lfobj"
}
  \item{year}{
The hydrological year for which the BFI should be computed. If "any" the
whole series is plotted.
}
\item{col}{Color of Flow}
\item{bfcol}{Color of Baseflow}
\item{ylog}{Log y-axis?}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\references{Gustard, A. & Demuth, S. (2009) (Eds) Manual on Low-flow Estimation and Prediction. Operational Hydrology Report No. 50, WMO-No. 1029, 136p.
}
\author{
Daniel Koffler and Gregor Laaha
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{BFI}}
}
\examples{
data(ngaruroro)
#Plot starts in December, as ngaruroro's hyearstart = 12
bfplot(ngaruroro, year = 1991) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{base flow}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

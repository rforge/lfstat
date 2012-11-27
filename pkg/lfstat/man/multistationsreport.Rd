\name{multistationsreport}
\alias{multistationsreport}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Report for several stations
}
\description{
Calculates indices for several stations at once.
}
\usage{
multistationsreport(...,indices = c("meanflow", "Q95", "MAM1", "MAM7", 
    "MAM10", "MAM30", "MAM90", "baseflowindex", "recession"), 
    recessionmethod = "MRC", recessionseglength = 7, recessionthreshold = 70, 
    recessiontrimIRS = 0.1) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{Objects of class "lfobj"}
  \item{indices}{A vector of indices to calculate}
  \item{recessionmethod}{See \code{\link{recession}}}
  \item{recessionseglength}{See \code{\link{recession}}}
  \item{recessionthreshold}{See \code{\link{recession}}}
  \item{recessiontrimIRS}{See \code{\link{recession}}}
}
\value{
A data.frame containing the calculated indices.
}

\references{
Manual on Low-flow Estimation and Prediction, Operational Hydrology Report No. 50, Koblenz 2009
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{\code{\link{meanflow}}, \code{\link{Q95}},\code{\link{MAM}},\code{\link{BFI}},\code{\link{recession}}}


\examples{
data(ngaruroro)
multistationsreport(ngaruroro, indices = c("meanflow", "MAM7"))

seventies <- subset(ngaruroro, hyear \%in\% 1970:1979)
eighties <- subset(ngaruroro, hyear \%in\% 1980:1989)
nineties <- subset(ngaruroro, hyear \%in\% 1990:1999)

multistationsreport(seventies, eighties, nineties)
}
\keyword{}
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

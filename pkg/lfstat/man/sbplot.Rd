\name{sbplot}
\alias{sbplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Seasonal Barchart
}
\description{
Plots a seasonal barchart for daily streamflow data
}
\usage{
sbplot(lfobj,hyearorder = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lfobj}{
A lfobj, as created with 'createlfobj'
}
\item{hyearorder}{
  logical, if TRUE the bars are plotted according to the hydrological
  year, if FALSE they start with January.
}}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
createlfobj
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (lfobj) 
{
    if (!inherits(lfobj, "lfobj")) {
        stop("This functions is designed for objects of the class 'lfobj', please use 'createlfobj' or see '?createlfobj' for more information")
    }
    mm <- aggregate(flow ~ month, data = lfobj, FUN = mean)
    mquan <- aggregate(flow ~ month, data = lfobj, FUN = quantile, 
        probs = 0.1)
    mmplot <- barchart(flow ~ reorder(month, monthreorder), data = mm, 
        box.ratio = 1, horizontal = FALSE, xlab = "Month", ylab = "Streamflow") + 
        layer(panel.barchart(x = reorder(month, monthreorder), 
            y = flow, box.ratio = 0.3, horizontal = FALSE, col = "black"), 
            data = mquan)
    mmplot
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

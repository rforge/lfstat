\name{createlfobj}
\alias{createlfobj}
\alias{createlfobj.default}
\alias{createlfobj.data.frame}
\alias{createlfobj.ts}
\alias{createlfobj.lfobj}

\title{
Create an lfobj for further Low Flow Analysis
}
\description{
Generic function for creating a low flow object (lfobj). Low flow objects can be
created out of a time series of daily flow, a data.frame with columns
"flow", "day", "month" and "year". 
}
\usage{
createlfobj(x, ...)


\method{createlfobj}{data.frame}(x, hyearstart = 1, baseflow = TRUE,...)

\method{createlfobj}{ts}(x,
            startdate,
            dateformat = "\%d/\%m/\%Y",
            ...)
\method{createlfobj}{lfobj}(x, hyearstart = NULL, baseflow = NULL,...)
}
\arguments{
  \item{x}{
An object out of which a lfobj should be created
}
%\item{flow}{The daily runoff data}
%\item{day}{A vector of "day" as part of date}
%\item{month}{A vector of "month" as part of date}
%\item{year}{A vector of "year" as part of date}
  \item{hyearstart}{
integer, which month should the hydrological year start?
}
\item{baseflow}{
  logical, should the baseflow curve be calculated? Needed, if you want
  to apply 'bfplot' or 'BFI' later on.
}
\item{startdate}{Startdate of the time-series}
\item{dateformat}{Format of the startdate}
\item{...}{Additional arguments depending on the method used.}
}
\details{
'hyearstart' defines the starting month of the hydrological year. If
'hyearstart' is greater then 6.5, the hydrological year starts earlier
then the actual date, e.g. hyearstart = 10, then the 1st of October 2011
is part of the hydrological year 2012. If hyearstart = 4, then the 31st
of March 2011 ist part of the hydrological year 2010.
}
\value{
An object of class 'lfobj'.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Daniel Koffler
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{readlfdata}}
}
\examples{
#Creating a lfobj from a timeseries
#Some sample data:

somevalues <- rexp(365)
#Convert to time series:
time <- ts(somevalues)

#Lets say our data contains values from one hydrological year (Oct-Sep)
#starting on 1. Oct. 1992:
myriver <- createlfobj(time, startdate = "01/10/1992",hyearstart = 10)
#Done
hydrograph(myriver)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ lfobj }


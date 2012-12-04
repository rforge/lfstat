\name{lfstat-package}
\alias{lfstat-package}
\alias{lfstat}
\docType{package}
\title{
Calculates Low Flow Statistics for daily stream flow data
}
\description{
The "Manual on Low-flow Estimation and Prediction", published by the World Meteorological Organisation (WMO), gives an idea how to analyse stream flow data focusing on low-flow issues. This packages gives functions to compute the described statistics and produce plots similar to the one in the manual.
}
\details{

###################################
# Create lfobj (Low-Flow-Objects) #
###################################

The package calculates induces and makes graphics for low flow
analysis. It brings its own class "lfobj", a special data.frame format
with columns "day", "month", "year", "flow", "hyear" and possibly "baseflow".

"day", "month" and "year" refer to the date, "flow" is the measured
runoff (unit-independent), "baseflow" the calculated baseflow.

"hyear" refers to the hydrological year. When creating the "lfobj" you
define the month where the stations hydrological year starts. If annual indices are
calculated or single years are plotted, the "hyear" is taken.

Basically there are to options to create an lfobj:

If you have special data format, e.g. GRDC, you can use the function
'readlfdata', see '?readlfdata' to see which formats are currently
supported.
Otherwise you can use 'createlfobj'. You can apply it for new data in
one of two ways:
1) You create a data.frame with columns: "day", "month", "year" and
"flow".
2) You create a time-series (ts) from "flow" and give the start date of
the series when calling 'createlfobj'.

##############################
# Preparation                #
##############################

lfstat does not need to know the unit of the flow, but you might want it
to appear in your plots. You can use 'setlfunit' to define how units are
labeled in your graphics. Examples are given in '?setlfunit'.

Please check for NA-values using 'lfnacheck', indices and plots are made
as if series were complete. See the manual on how to deal with missing
values and, if reasonable, use 'lfnainterpolate'.

###############################
# Indices                     #
###############################
Functions available (see ?function for more information)
'meanflow', 'Q95', 'MAM' (mean annual minima), 'BFI', 'recession' (recession constant),
'streamdef' (Streamflow deficit), tyears (Extreme value - T-years
 event), 'seasratio', 'seasindex' and 'multistationsreport'.
################################
# Plots                        #
################################
'hydrograph'
'recessionplot' (Diagnosis for recession)
'fdc' (Flow-duration-curve)
'sbplot' (seasonal barchart)
'seglenplot' (select recession length for 'recession')
'streamdefplot' (Streamflow deficit)
'rfa' (Regional frequency analysis)
'dmcurve' (Double mass curve)


}
\author{
Daniel Koffler <daniel.koffler@boku.ac.at>
}
\references{
Manual on Low-flow Estimation and Prediction, Operational Hydrology Report No. 50, Koblenz 2009
}

\keyword{low flow, WMO}
\seealso{
}
\examples{
}

#Different methods to create a lfobj:
#Data.frame with named columns
#ts + start date (does vector work?)

createlfobj <- function(x,...){UseMethod("createlfobj")}

createlfobj.lfobj <- function(x, hyearstart = NULL, baseflow = NULL,...){
  if(is.null(baseflow)){
   baseflow <- "baseflow" %in% names(x)}
  if(is.null(hyearstart)){
    ii <- subset(x, year != hyear, month)
    if(nrow(ii)==0){hyearstart <-1} else if(max(ii) <5.5){hyearstart <- max(ii)+1}else{hyearstart <- min(ii)}
  }

  bf <- baseflow
  hy <- hyearstart

  dat <- createlfobj.data.frame(x=x,hyearstart = hy, baseflow = bf)
  dat
  }


#Create a lfobj from a vector of daily flow data and the startdate
createlfobj.ts <- function(x, startdate, dateformat = "%d/%m/%Y", ...){
    y <- x
    day <- NULL;month <- NULL;year <- NULL
    start <- as.Date(startdate,dateformat)
    for(ii in seq_along(y)){
      actual <-  start +ii-1
      day[ii] <- as.numeric(format(actual,"%d"))
      month[ii] <- as.numeric(format(actual,"%m"))
      year[ii] <- as.numeric(format(actual,"%Y"))}
    dat <- createlfobj(x = data.frame(
                               day = day,
                               month = month,
                               year = year,
                               flow = as.vector(y)),
                               ...)
    dat}


#Create a lfobj from a data frame with cols named "flow", "day", "month", "year"
createlfobj.data.frame <- function(x, hyearstart = 1,baseflow = TRUE,...){

if(prod(c("flow", "day", "month", "year") %in% names(x))!= 1)
  {stop("Your data.frame must contain colums named 'flow', 'day, 'month' and 'year'! Please look at the help files for more information")}

if(!(hyearstart %in% 1:12)){
    stop("hyearstart must be whole number between 1 and 12")}

 dat <- data.frame(day = x$day,
                   month = x$month,
                   year = x$year,
                   flow = x$flow)

 dates <- as.Date(paste(dat$year,dat$month,dat$day,sep = "-"))

#Sorting data.frame
dat <- dat[order(dates),]
differ <- diff(dates)
#Missing dates -> filling with NA!
if(sum(differ>1)>0){
  warning("There were missing dates! These dates where filled using flow = NA")
  for(ii in rev(seq_along(differ))){
    if(differ[ii] > 1){
      a <- differ[ii]
         for(jj in 1:(a-1)){
           date <- dates[ii]+jj
           dat <- rbind(dat, c(as.numeric(format(date,"%d")),as.numeric(format(date,"%m")),as.numeric(format(date,"%Y")),NA))
         }}}
  datesnew <- as.Date(paste(dat$year,dat$month,dat$day,sep = "-"))
  dat <- dat[order(datesnew),]
}

rownames(dat) <- seq(length=nrow(dat))
dat <- hyear(dat, startmonth = hyearstart)
if(baseflow){
dat <- baseflow(dat)}
class(dat) <- c("lfobj","data.frame")
dat}

lfcheck <- function(lfobj){
if(!inherits(lfobj,"lfobj")){stop("This functions is designed for objects of the class 'lfobj', please use 'createlfobj' or see '?createlfobj' for more information")}}

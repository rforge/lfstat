#Reads GRDC data sheets like 9104020.day

readlfdata <- function(file, type = c("GRDC","HZB","LFU","TU"),lfobj = TRUE, hyearstart = 1,baseflow = TRUE){
  style <-match.arg(type)

#Read GRDC sheet  
if(style == "GRDC"){
  a <- read.table(file,header = T, sep = ";")
  a[,1] <- as.Date(a[,1])
  }

#Read HZB sheet  
if(style == "HZB"){
  lines <- readLines(file, n=50,encoding = "latin1")
  wert <- grep("Werte:",lines)
  a <- read.table(file,header = F,skip = wert,na.strings = iconv("L\374cke",from = "latin1",to = "latin1"),encoding = "latin1")
  a[,1] <- as.Date(a[,1],"%d.%m.%Y")
}
#Read LfU-Bayern sheet new
if(style == "LFU"){
  a <- read.table(file, header = F)
  a[,1] <- as.Date(as.character(a$V1),"%Y%m%d%H%M")
  a[,3] <- a[,2]
}

#TU
if(style == "TU"){
  b <- read.table(file, header = F)
  a1 <- as.Date(paste(b[,3],b[,2],b[,1],sep = "/"),"%Y/%m/%d")
  a2 <- b[,4]
  a3 <- a2
  a3[a2 == -999] <- NA
  a <- data.frame(a1,a2,a3)
}
    
dat <- data.frame(day =  as.numeric(format(a[,1], "%d")),
                    month =  as.numeric(format(a[,1], "%m")),
                    year =  as.numeric(format(a[,1], "%Y")),
                    flow = a[,3])
if(lfobj){
  lfobj<-createlfobj(dat,hyearstart = hyearstart, baseflow = baseflow)
  return(lfobj)} else {
  return(dat)}  
}





#readGRDC <- function(file){
#  a <- read.table(file,header = T, sep = ";")
#  a[,1] <- as.Date(a[,1])
#  dat <- data.frame(day =  as.numeric(format(a[,1], "%d")),
#                    month =  as.numeric(format(a[,1], "%m")),
#                    year =  as.numeric(format(a[,1], "%Y")),
#                    flow = a[,3])
#  dat}

#readHZB <- function(file){
#  lines <- readLines(file, n=50)
#  wert <- grep("Werte:",lines)
#  a <- read.table(file,header = F,skip = wert)
#  a[,1] <- as.Date(a[,1],"%d.%m.%Y")
#  dat <- data.frame(day =  as.numeric(format(a[,1], "%d")),
#                    month =  as.numeric(format(a[,1], "%m")),
#                    year =  as.numeric(format(a[,1], "%Y")),
#                    flow = a[,3])
#  dat}

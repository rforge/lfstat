activelf <- function(){
  activeDataSetP() && inherits(justDoIt(activeDataSet()),'lfobj')
} 

  

activelfandbf <- function(){
  if( activelf()) {"baseflow" %in% Variables()}else(FALSE)
}

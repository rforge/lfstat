#########################
# T-YEARS               #
#########################

tyearscalc <- function(){
initializeDialog(title=gettextRcmdr("T-years event"))
optionsFrame <-  tkframe(top)
textFrame <- tkframe(top)

MAdays <- tclVar(gettextRcmdr(getlfopt("extn")))
MAentryframe <- ttkentry(textFrame, width="2", textvariable=MAdays)
tyears <- tclVar(gettextRcmdr(getlfopt("extyears")))
tyearsentryframe <- ttkentry(textFrame, width="4", textvariable=tyears)
choice <- c("Weibull", "GEV", "Lognormal","Gumbel","Pearson Type 3")
init <-NULL

for(ii in seq_along(getlfopt("extdist"))){
  init[ii] <- which(getlfopt("extdist")[ii] == choice)-1
}

distBox <- variableListBox(top, choice,
		selectmode="multiple", title=gettextRcmdr("Select distributions: "),
                           initialSelection = init,
                           listHeight = 5)

onOK <- function(){
  dist <- getSelection(distBox)
          options("RcmdrPlugin.lfstat" =
                        modifyList(getOption("RcmdrPlugin.lfstat"),list(extdist = dist)))
  closeDialog()
  distname <- NULL
  names <- c("wei","gev","ln3","gum","pe3")
  for(ii in seq_along(dist)){
    distname[ii] <-names[which(dist[ii] == choice)]
    }
  distname2 <- NULL
  for (ii in seq_along(distname)){
    distname2 <- paste(distname2,distname[ii],sep =if(ii == 1){"\""}else{"\",\""})
  }

  event <- tclvalue(tyears)
   options("RcmdrPlugin.lfstat" =
                        modifyList(getOption("RcmdrPlugin.lfstat"),list(extyears = event)))
  n <- tclvalue(MAdays)
   options("RcmdrPlugin.lfstat" =
                        modifyList(getOption("RcmdrPlugin.lfstat"),list(extn = n)))
  command <- paste('tyears(lfobj = ',ActiveDataSet(),', event = ', event,', n = ',n,',dist = c(',distname2,'"))',sep="")
  doItAndPrint(command)
  tkfocus(CommanderWindow())
}#end onOK

OKCancelHelp(helpSubject="tyears")
	tkgrid(textFrame,sticky = "w")
        tkgrid(labelRcmdr(textFrame, text = gettextRcmdr("Annual n-day minima, n:")),MAentryframe,
               sticky = "w")
        tkgrid(labelRcmdr(textFrame, text = gettextRcmdr("T-Years:")),tyearsentryframe,
               sticky = "w")
        tkgrid(getFrame(distBox), sticky="nw")
       
        tkgrid(buttonsFrame, sticky = "w")
dialogSuffix(rows=4, columns=2)
}


listlfobj <- function(envir=.GlobalEnv, ...) {
	objects <- ls(envir=envir, ...)
	if (length(objects) == 0) NULL
	else objects[sapply(objects,
		function(.x) "lfobj" == (class(get(.x, envir=envir))[1]))]
}

#########################
#Regional frequency     #
#########################


rfacalc <- function(){
	initializeDialog(title=gettextRcmdr("Regional frequency analysis"))
        optionsFrame <-  tkframe(top)
	lfobjBox <- variableListBox(top, listlfobj(),
		selectmode="multiple", title=gettextRcmdr("Select low flow objects: "))
        Madays <- tclVar(gettextRcmdr("1"))
        MAentryframe <- ttkentry(optionsFrame, width = "2", textvariable =Madays)
                  
onOK <-  function(){
  lfobjs <- getSelection(lfobjBox)
  if (length(lfobjs) == 0) {
     errorCondition(recall=Recode, message=gettextRcmdr("You must select an object."))
			return()
		}
  a<- NULL
  for(ii in lfobjs){
    if(is.null(a)){a <- ii} else{
    a <- paste(a,ii,sep = ",")}
  }
  closeDialog()
  n <- tclvalue(Madays)
  command <- paste("rfa(lfobj = list(",a,"),n =",n,")",sep = "")
  doItAndPrint(command)  
} #End on OK


OKCancelHelp(helpSubject="rfa")
	tkgrid(getFrame(lfobjBox), sticky="nw")
        tkgrid(labelRcmdr(optionsFrame, text = gettextRcmdr("Annual n-day minima, n:")),MAentryframe,
               sticky = "w")
        tkgrid(optionsFrame, sticky = "w")
        tkgrid(buttonsFrame, sticky = "w")
        dialogSuffix(rows=3, columns=2)
      }
  

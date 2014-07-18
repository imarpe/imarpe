
.getLandingsData = function(file=file, ...) {
  
  out = read.csv(file=file, na.strings="", stringsAsFactors = FALSE, ...)
  
  ports = out[c(4:length(colnames(out)))]
  namesPorts = names(ports)
  
  info = list(file=file, records=nrow(out), 
              months = length(rle(out$month)$values),
              years  = length(unique(out$year)),
              ports  = length(namesPorts))
  
  output = list(data=out, info=info)
  class(output) = c("landings")
  return(output)
  
}

print.landings = function(x,...) {
  
  cat("Landings data from ", sQuote(x$info$file), "\n", sep="")
  RowNames=c("Number of records","Number of months of data",
             "Number of years of data","Number of ports")
  #W=rep(':',4)
  #Z=data.frame(W,Value=as.numeric(x$info[-1]),row.names=RowNames)
  #colnames(Z)=c(" "," ")
  #print(Z)
  
  RowNames=RefineChar4Table(RowNames)
  FrameInfo=data.frame(Value=as.numeric(x$info[-1]),row.names=RowNames)
  colnames(FrameInfo)=c(" ")
  print(FrameInfo,...)

  #cat("Number of records: ", x$info$records, "\n", sep="")
  #cat("Number of months of data: ", x$info$months, "\n", sep="")
  #cat("Number of years of data: ", x$info$years, "\n", sep="")
  #cat("Number of ports: ", x$info$ports, "\n", sep="")
  
  return(invisible())
}

summary.landings = function(object,...) {
  
  output = list()
  
  output$info = object$info
  
  output$sumPorts = .getSumPorts.landings(object)
  output$ports    = .getPorts.landings(object)
  output$months   = .getMonth.landings(object)
  output$years    = .getYear.landing(object)
  
  class(output) = "summary.landings"
  return(output)
  
}

print.summary.landings = function(x, ...) {

  cat("Landings data from ", sQuote(x$info$file), "\n", sep="")
  RowNames=c("Number of records","Number of months of data",
             "Number of years of data","Number of ports")
  RowNames=RefineChar4Table(RowNames)
  FrameInfo=data.frame(Value=as.numeric(x$info[-1]),row.names=RowNames)
  colnames(FrameInfo)=c(" ")
  print(FrameInfo,...)
#   cat("Number of records: ", x$info$records, "\n", sep="")
#   cat("Number of months of data: ", x$info$months, "\n", sep="")
#   cat("Number of years of data: ", x$info$years, "\n", sep="")
#   cat("Number of ports: ", x$info$ports, "\n", sep="")
  
#   cat("\nDaily landing:\n\n")
#   print(x$sumPorts)
  
  cat("\nLandings by ports (non-zero only):\n\n")
#   PortNames=rownames(x$ports)
#   PortNames[x$ports$Landings>0]=RefineChar4Table(PortNames[x$ports$Landings>0])
#   rownames(x$ports)=PortNames
  print(x$ports[x$ports$Landings>0, ,drop=FALSE],...)
  
  cat("\nMonthly landing:\n\n")
  print(t(x$months),...)
  
  cat("\nAnnual landing:\n\n")
  print(x$years,...)
  
  return(invisible(x))
  
}

plot.landings = function(x, time=NULL, ...) {
  
#  if(!is.null(y) & is.null(time)) time = y
#  if( is.null(y) & is.null(time)) time = "day"
   if(is.null(time)) time="day"
  switch(time,
         day   = .plotDays.landings(x=x, ...),
         month = .plotMonths.landings(x=x, ...),
         year  = .plotYears.landings(x=x, ...),
  )
  
  return(invisible())
}
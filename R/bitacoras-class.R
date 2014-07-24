
.getBitacorasData = function(file=file, ...) {
  
  out = read.csv(file=file, na.strings="", stringsAsFactors = FALSE, ...)
  
  id = c("anchoveta", "sardina", "jurel", "caballa", "otros", "observador")
  check =  id %in% tolower(colnames(out)) 
  if(!all(check)) stop("File does not seem to by of type 'bitacoras'.")
  
  ports = unique(out$puerto.salida)
  
  date = out$dia.salida
  date = date[date != "" & !is.na(date)]
  dataDate = strptime(date, format="%d-%m-%Y %H:%M")
  years = as.numeric(format(dataDate, "%Y"))
  
  info = list(file=file,
              records=length(unique(out$dia.salida)), 
              observers=length(unique(out$observador)),
              ports = length(ports[!is.na(ports)]),
              years = unique(years))
  output = list(data=out, info=info)
  class(output) = c("bitacoras")
  return(output)
  
}

print.bitacoras = function(x, ...) {
  
  cat("Bitacoras data from ", sQuote(x$info$file), "\n", sep="")
  x$info$years=as.character(paste(x$info$years,collapse=", "))
  RowNames=c("Number of records","Number of observers",
             "Number of ports","Years")
  RowNames=.RefineChar4Table(RowNames)
  FrameInfo=data.frame(Value=as.character(x$info[-1]),row.names=RowNames)
  colnames(FrameInfo)=c(" ")
  print(FrameInfo,...)
#   cat("Number of records: ", x$info$records, "\n", sep="")
#   cat("Number of observers: ", x$info$observers,"\n", sep="")
#   cat("Number of ports: ", x$info$ports,"\n", sep="")
#   cat("Years: ", x$info$years, "\n", sep="")
  
  return(invisible())
  
}

summary.bitacoras = function(object,...) {
  
  output = list()
  
  output$info = object$info
  output$composition = .getSpeciesComposition.bitacoras(object)
  output$effort      = .getEffort.bitacoras(object) 
  output$observer    = .getNumberObserver.bitacoras(object)
  output$set         = .getNumberSet.bitacoras(object)#number of coves by latitude
  output$depth       = .getDepth.bitacoras(object)#depth by latitude
  
  class(output) = "summary.bitacoras"
  return(output)
  
}

print.summary.bitacoras = function(x,...) {
  
    x2=x; class(x2)='bitacoras'
    print(x2,...)
    
  cat("\nSpecies composition:\n\n")
  print(x$composition,...)
  
  cat("\nSampling effort:\n\n")
  print(x$effort,...)
  
  cat("\nNumber of observers:\n\n")
  print(x$observer,...)
  
  cat("\nNumber of sets by latitudinal degree:\n\n")
  print(t(x$set),...)
  
  cat("\nDepth by latitudinal degree:\n\n")
  print(t(x$depth),...)
  
  return(invisible(x))
}

plot.bitacoras = function(x, type, detailed=FALSE,...) {
  
  switch(type,
         effort = .plotEffort.bitacoras(x=x, ...),
         depth = .plotDepth.bitacoras(object=x,detailed,...),
         stop("Plot type not defined."))
  
  return(invisible())
}

.getBitacorasData = function(file=file, ...) {
  
  out = read.csv(file=file, na.strings="", stringsAsFactors = FALSE, ...)
  
  id = c("anchoveta", "sardina", "jurel", "caballa", "otros", "observador")
  check =  id %in% tolower(colnames(out)) 
  if(!all(check)) stop("File does not seem to by of type 'bitacoras'.")
  
  ports = unique(out$puerto.salida)
    
  info = list(file=file, records=nrow(out), 
              observers=length(unique(out$observador)),
              ports = length(ports[!is.na(ports)]))
  output = list(data=out, info=info)
  class(output) = c("bitacoras")
  return(output)
  
}

print.bitacoras = function(x, ...) {
  
  cat("Bitacoras data from ", sQuote(x$info$file), "\n", sep="")
  cat("Number of records: ", x$info$records, "\n", sep="")
  cat("Number of observers: ", x$info$observers,"\n", sep="")
  cat("Ports: ", x$info$ports,"\n", sep="")
  
  return(invisible())
  
}

summary.bitacoras = function(object, ...) {
  
  output = list()
  
  output$info = object$info
  output$composition = .getSpeciesComposition.bitacoras(object)
  output$effort      = .getEffort.bitacoras(object) 
  output$observer    = .getNumberObserver.bitacoras(object)
  output$set         = .getNumberSet.bitacoras(object)
  output$depth       = .getDepth.bitacoras(object) 
  
  class(output) = "summary.bitacoras"
  return(output)
  
}

print.summary.bitacoras = function(x, ...) {
  
  cat("Bitacoras data from ", sQuote(x$info$file), "\n", sep="")
  cat("Number of records: ", x$info$records, "\n", sep="")
  cat("Number of observers: ", x$info$observers,"\n", sep="")
  cat("Ports: ", x$info$ports,"\n", sep="")
  
  cat("\nSpecies composition:\n\n")
  print(x$composition)
  
  cat("\nSampling effort:\n\n")
  print(x$effort)
  
  cat("\nNumber of observers:\n\n")
  print(x$observer)
  
  cat("\nNumber of sets by latitudinal degree:\n\n")
  print(x$set)
  
  cat("\nDepth by latitudinal degree:\n\n")
  print(x$depth)
  
  return(invisible(x))
}


plot.bitacoras = function(x, total=FALSE, ...) {
  if(isTRUE(total))
    x$data = data.frame(total=apply(x$data, 1, sum, na.rm=TRUE))
  
  opar = par(no.readonly=TRUE)
  on.exit(par(opar))
  
  n = ncol(x$data)
  par(mfrow = c(n,1), oma=c(2,2,2,2), mar=c(0,0,0,0))
  ylim = c(0, 1.2*max(as.numeric(x$data), na.rm=TRUE))
  
  for(i in seq_len(n)) {
    plot(x=x$time$time, y=x$data[,i], axes=FALSE, ylim=ylim, ...)
    margin = 2*(1+ (i%%2 != 1))
    axis(margin)
  }
  
  axis(1)
  box()
  
}

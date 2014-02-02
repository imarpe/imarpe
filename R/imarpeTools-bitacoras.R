
.getBitacorasData = function(file=file, ...) {
  
  out = read.csv(file=file, na.strings="", stringsAsFactors = FALSE, ...)
  
  id = c("anchoveta", "sardina", "jurel", "caballa", "otros", "observador")
  check =  id %in% tolower(colnames(out)) 
  if(!all(check)) stop("File does not seem to by of type 'bitacoras'.")
  
  info = list(file=file, records=nrow(out), 
              observers=length(unique(out$observador)))
  output = list(data=out, info=info)
  class(output) = c("bitacoras")
  return(output)
  
}

print.bitacoras = function(x, ...) {
  
  cat("Bitacoras data from ", sQuote(x$info$file), "\n", sep="")
  cat("Number of records: ", x$info$records, "\n", sep="")
  cat("Number of observers: ", x$info$observers,"\n", sep="")
  
  return(invisible())
  
}

summary.bitacoras = function(object, ...) {
  
  output = list()
  
  output$info = object$info
  output$composition = .getSpeciesComposition.bitacoras(object)
  output$effort      = .getEffort.bitacoras(object) 
  
  class(output) = "summary.bitacoras"
  return(output)
  
}

print.summary.bitacoras = function(x, ...) {
  
  cat("Bitacoras data from ", sQuote(x$info$file), "\n", sep="")
  cat("Number of records: ", x$info$records, "\n", sep="")
  cat("Number of observers: ", x$info$observers,"\n", sep="")
  
  cat("\nSpecies composition:\n\n")
  print(x$composition)
  
  cat("\nSampling effort:\n\n")
  print(x$effort)
  
  return(invisible(x))
}


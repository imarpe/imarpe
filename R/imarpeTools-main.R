# To define generic classes and methods for the package

getData = function(file, type, ...) {
  
  output = switch(type,
                  bitacoras = .getBitacorasData(file=file, ...),
                  landings = .getLandingsData(file=file, ...),
                  read.csv(file=file, ...)
  )
  return(output)
}
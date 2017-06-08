
print.bitacora = function(x, language="spanish") {

  if(language == "english") {
    cat("Data from: ", sQuote(x$info$file), "\n", sep="")
    cat("Number of trips: ", x$info$trips, "\n", sep="")
    cat("Number of ports of data: ", x$info$ports, "\n", sep="")
    cat("Number of months of data: ", x$info$months, "\n", sep="")
    cat("Number of years of data: ", x$info$years, "\n", sep="")
    cat("Types of fleet:", x$info$fleets, "\n", sep="  ") }

  if(language == "spanish") {
    cat("Datos de : ", sQuote(x$info$file), "\n", sep="")
    cat("Numero de viajes: ", x$info$trips, "\n", sep="")
    cat("Numero de puertos: ", x$info$ports, "\n", sep="")
    cat("Numero de meses de la data: ", x$info$months, "\n", sep="")
    cat("Numero de ahnos de la data: ", x$info$years, "\n", sep="")
    cat("Tipos de flota:", x$info$fleets , "\n", sep="  ") }

  return(invisible())
}


summary.bitacora = function(object, language = "spanish") {

  output = list()
  output$observedTrip = .observedTrip.bitacora(object = object, language = language)

  class(output) = "summary.bitacora"

  return(output)
}


print.summary.bitacora = function(x, language = "spanish") {

  x2 = x
  class(x2) = 'bitacora'

  if(language == "english"){
    cat("\nObserved trips by port:\n\n") ; print(x$observedTrip)
  }

  if(language == "spanish"){
    cat("\nViajes observados por puerto:\n\n") ; print(x$observedTrip)
  }


  return(invisible())
}


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


summary.bitacora = function(object, language = "spanish", latByPort = FALSE) {

  output = list()
  output$observedTrip = .observedTrip.bitacora(object = object, language = language)
  output$fishingHaul   = .fishingHaul.bitacora(object = object, language = language, latByPort = latByPort)
  class(output) = "summary.bitacora"

  return(output)
}


print.summary.bitacora = function(x, language = "spanish") {

  x2 = x
  class(x2) = 'bitacora'

  if(language == "english"){
    cat("\nObserved trips by port:\n\n") ; print(x$observedTrip)
    cat("\nFishing haul by latitude:\n\n") ; print(x$fishingHaul)
  }

  if(language == "spanish"){
    cat("\nViajes observados por puerto:\n\n") ; print(x$observedTrip)
    cat("\nLances pesqueros por latitud:\n\n") ; print(x$fishingHaul)
  }

  return(invisible())
}


report.bitacora = function(x, format = "latex", tangle=FALSE, output = NULL) {

  if(is.null(output)) output = getwd()
  outputName = deparse(substitute(x))

  skeleton = system.file("reports", "bitacora-report.Rmd", package = "imarpe")

  if(isTRUE(tangle)) {
    knit(skeleton, tangle=TRUE)
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from=basename(f1), to=paste0(outputName, ".R"))
  }

  outputFile = paste0(outputName, "_output.pdf")
  render(skeleton, c("pdf_document"), output_file=outputFile, output_dir=output)

  if(isTRUE(open)) shell.exec(outputFile)

  return(invisible(file.path(output, outputFile)))

}


plot.bitacora = function(x, language, ploType = NULL, dataType, ...) {

  if(is.null(ploType)) ploType = "plotFishingPoints"
  if(ploType %in% "plotFishingPoints") {dataFishingPoints = .fishingPoints.bitacora(x)}

  switch(ploType,
         plotFishingPoints = .plotFishingPoints.bitacora(x=dataFishingPoints, language=language, dataType = dataType, ...))

  return(invisible())

}

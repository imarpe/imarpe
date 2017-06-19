#' @title Print method for bitacora objects
#' @description Shows main information from bitacora objects, like the origen of the
#' information, the number of trips, the number of data ports, the number of months
#' of data, the number of years of data, the types of fleets on the data.
#' @param x Object of class \code{bitacora}.
#' @param language The select language to print the outputs.
#' @export
#' @method print bitacora
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

#' @title Summary method for bitacora objects
#' @description Get summary information of bitacora object.
#' @param object Object of class \code{bitacora}.
#' @param language The select language to print the summary of bitacora objects.
#' @param latByPort \code{logical}. By default (\code{FALSE}) to get the
#' latitude accoding the log book information, and to get the latitude
#' according the port (\code{TRUE}).
#' @return A \code{list} of summary.bitacora class. This contains:
#' \itemize{
#'   \item observedTrip The observed trips by port.
#'   \item fishingHaul The fishing haul by latitude.
#' }
#' @export
#' @method summary bitacora
summary.bitacora = function(object, language = "spanish", latByPort = FALSE) {

  output = list()
  output$observedTrip  = .observedTrip.bitacora(object = object, language = language)
  output$fishingHaul   = .fishingHaul.bitacora(object = object, language = language, latByPort = latByPort)
  class(output) = "summary.bitacora"

  return(output)
}

#' @title Print method for summary.bitacora
#' @description Shows main information from \code{summary.bitacora} objects.
#' @param x Object of class \code{summary.bitacora}.
#' @param language The select language to print the summary of bitacora objects.
#' @return Each element of \code{summary.bitacora} method.
#' @export
#' @method print summary.bitacora
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

#' @title Plot method for bitacora objects
#' @description This method takes a \code{bitacora} object and make useful plots.
#' @param x Object of \code{bitacora} class.
#' @param language \code{character}. Define the language of text labels in plots.
#' It could be \code{"spanish"} or \code{"english"}.
#' @param ploType What type of plot should be draw. By default is \code{plotFishingPoints}
#' but the possible types are:
#' \itemize{
#'   \item plotFishingPoints for a graphic of fishing points
#'   \item plotFishingPresence for a graphic of non-target species presence by taxonomic group
#'   \item plotSpeciesComposition for a graphic of species in the catch
#' }
#' @param dataType Indicates what species should be plot on plotFishingPoints graphic type. It could
#' be:
#' \itemize{
#'   \item "dataAnch" to graphic fishing points of anchovy
#'   \item "dataSar" to graphic fishing points of sardine
#'   \item "dataJur" to graphic fishing points of jack mackerel
#'   \item "dataCab" to graphic fishing points of chub mackere
#'   \item "dataBon" to graphic fishing points of bonito
#' }
#' @param group Indicates what taxonomic group should be plot on plotFishingPresence graphic type. It
#' could be: "neritico", "transzonal", "costero", "oceanico", "demersal", "bentonico", "mesopelagico",
#' "depredador", "elasmobranquio", "cefalopodo", "medusa", "crustaceo", "tunicado", "alga", "copepodo",
#' "eufausido", "molusco", "equinodermo", "invertebrado", "dinogelado".
#' @param threshold Parameter of plotSpeciesComposition graphic type. Logical, by default is \code{TRUE}
#' when the species composition on the catches will be graphic according to a thershold
#' specified in \code{minPercentage}.
#' @param minPercentage Parameter of plotSpeciesComposition graphic type. Numeric value,
#' indicating the minimum percentage that catches must have to be graphic. By default is
#' 0.2.
#' @param ... Extra arguments.
#' @return A graph of the specified type in \code{ploType}.
#' @export
#' @method plot bitacora
plot.bitacora = function(x, language = "spanish", ploType = NULL, dataType, group, threshold = TRUE, minPercentage = 0.2, ...) {

  if(is.null(ploType)) ploType = "plotFishingPoints"
  if(ploType %in% c("plotFishingPoints", "plotFishingPresence")) {dataFishingPoints = .fishingPoints.bitacora(x)}
  if(ploType %in% "plotSpeciesComposition") {dataSpeciesComposition = .speciesComposition.bitacora(object = x, language = language)}

  switch(ploType,
         plotFishingPoints      = .plotFishingPoints.bitacora(x = dataFishingPoints, language = language, dataType = dataType, ...),
         plotFishingPresence    = .plotFishingPresence.bitacora(x = dataFishingPoints, group = group, ...),
         plotSpeciesComposition = .plotSpeciesComposition.bitacora(x = dataSpeciesComposition, threshold = threshold, minPercentage = minPercentage, ...))

  return(invisible())
}

report.bitacora = function(x, format = "latex", tangle=FALSE, output = NULL) {

  if(is.null(output)) output = getwd()
  outputName = deparse(substitute(x))

  skeleton = system.file("reports", "bitacora-report.Rmd", package = "imarpe")

  if(isTRUE(tangle)) {
    knit(skeleton, tangle=TRUE, encoding = "latin1")
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from=basename(f1), to=paste0(outputName, ".R"))
  }

  outputFile = paste0(outputName, "_output.pdf")
  render(skeleton, c("pdf_document"), output_file=outputFile, output_dir=output,
         encoding = "latin1")

  if(isTRUE(open)) shell.exec(outputFile)

  return(invisible(file.path(output, outputFile)))

}

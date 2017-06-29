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


#' @title Plot for fishing points
#' @description This method takes a \code{bitacora} object and plots fishing points.
#' @param x Object of \code{bitacora} class.
#' @param language \code{character}. Define the language of text labels in plots, by default is \code{"spanish"}.
#' @param dataType To indicate the fishing points of the species to be graphed. This could be:
#' \itemize{
#'   \item "dataAnch" to graphic fishing points of anchovy
#'   \item "dataSar" to graphic fishing points of sardine
#'   \item "dataJur" to graphic fishing points of jack mackerel
#'   \item "dataCab" to graphic fishing points of chub mackere
#'   \item "dataBon" to graphic fishing points of bonito
#'   \item "dataGroups" to graphic fishing points for other species differences to the five mentioned
#'   \item "dataTotal" to graphic fishing points of all species
#' }
#' @param colMap Assigns the color to the land domain. By default is \code{khaki1}.
#' @param colFleet A vector. Colouts assigned to each fleet type.
#' \itemize{
#'   \item "red" for artisanal type fleet (boats with storage capacity between 0 and 10 tons).
#'   \item "blue" for smaller scale type fleet (boats with storage capacity between 10 and 32.5 tons).
#'   \item "green" for industrial wood fleet (boats with storage capacity between 32.5 and 110 tons).
#'   \item "black" for industrial fleet (boats with storage capacity greather than 110 tons).
#' }
#' @param cexPointCatch \code{logical}. To plot the size of the fishing points according the catch size
#' \code{TRUE} or to plot the fishing points without considering the size of the catch \code{FALSE}
#' (by default).
#' @param cexPoint The size of the fishing points on the map. By default is 0.8.
#' @param cex.axis The size of the axis on the map. By default is 1.2.
#' @param cexPorts The size of the port names on the map. By default is 0.9.
#' @param ... Extra arguments.
#'
#' @return A map for fishing points of the data type selected on \code{dataType}.
#' @export
plotFishingPoints.bitacora = function(x, language = "spanish", dataType,
                                      colMap = "khaki1", colFleet = c("red", "blue", "green", "black"),
                                      cexPointCatch = FALSE, cexPoint = 0.8, cex.axis = 1.2, cexPorts = 0.9, ...) {

  #get data to plot
  dataToPlot = .fishingPoints.bitacora(x)

  #use internal function to plot the fishing point
  .plotFishingPoints.bitacora(x = dataToPlot, language = language, dataType = dataType,
                              colMap = colMap, colFleet = colFleet,
                              cexPointCatch = cexPointCatch, cexPoint = cexPoint, cex.axis = cex.axis, cexPorts = cexPorts, ...)

  return(invisible())
}


#' @title Plot for fishing presence points
#' @description This method takes a \code{bitacora} object and plot on a map the presence of the
#' non-tarjet species for the fishery.
#' @param x Object of \code{bitacora} class.
#' @param byGroup \code{logical}. To indicate if the fishing presence points will be plotted
#' by taxonomic group (\code{TRUE}), but to
#' @param group Indicates what taxonomic group should be graph on the map of fishing presence.
#' By defaul is \code{NULL} when \code{byGroup = FALSE}, but when \code{byGroup = TRUE} this parameter receives
#' the name of the taxonomic group. These might be: "neritico", "transzonal", "costero", "oceanico", "demersal",
#' "bentonico", "mesopelagico", "depredador", "elasmobranquio", "cefalopodo", "medusa", "crustaceo", "tunicado",
#'  "alga", "copepodo", "eufausido", "molusco", "equinodermo", "invertebrado", "dinogelado".
#' @param colMap Assigns the color to the land domain. By default is \code{khaki1}.
#' @param colSpecies A vector with colours that represent the non-tarjet species. By defaul is \code{NULL} and
#' use a internal \code{\link{vectorColours}}.
#' @param colLegend By default (\code{colLegend = NULL}) receives the vector with colours of \code{colSpecies}.
#' But this parameter could be changed including a vector of colour by each species.
#' @param cexPoint The size of the fishing points on the map. By default is 1.
#' @param cex.axis The size of the axis on the map. By default is 1.2.
#' @param cexPorts The size of the port names on the map. By default is 0.9.
#' @param cexLegend The size of the common name of species.
#' @param ... Extra arguments.
#'
#' @return A map for fishing presence points by taxonomic group or in general (for all species if the data).
#' @export
plotFishingPresence.bitacora = function(x, byGroup = TRUE, group = NULL,
                                        colMap = "khaki1", colSpecies = NULL, colLegend = NULL,
                                        cexPoint = 1, cex.axis = 1.2, cexPorts = 0.9, cexLegend = 1, ...) {

  #get data to plot
  dataToPlot = .fishingPoints.bitacora(x)

  #use internal function to plot the fishing presence
  .plotFishingPresence.bitacora(x = dataToPlot, byGroup = byGroup, group = group,
                                colMap = colMap, colSpecies = colSpecies, colLegend = colLegend,
                                cexPoint = cexPoint, cex.axis = cex.axis, cexPorts = cexPorts, cexLegend = cexLegend, ...)

  return(invisible())
}


#' @title Plot for species composition of catch
#' @description This method takes a \code{bitacora} object and do a pie graph.
#' @param x Object of \code{bitacora} class.
#' @param threshold Logical, by default is \code{TRUE} when the species composition on the
#' catches will be graphic according to a thershold specified in \code{minPercentage}.
#' When is \code{FALSE}, the pie graph uses all the species catches which are greather than zero.
#' @param minPercentage  Numeric value indicating the minimum percentage that catches must have
#'  to be graphic. By default is 0.2.
#' @param ... Extra arguments.
#'
#' @return A pie graph.
#' @export
plotSpeciesComposition.bitacora = function(x, threshold = TRUE, minPercentage = 0.2, ...) {

  #get data to plot
  dataToPlot = .speciesComposition.bitacora(object = x, language = "spanish")

  #use internal function to plot the species composition
  .plotSpeciesComposition.bitacora(x = dataToPlot,
                                   threshold = threshold, minPercentage = minPercentage, ...)

  return(invisible())
}


#' @title Report method for bitacora objects
#' @description Export a report of bitacora.
#' @param x Object of \code{bitacora} class.
#' @export
#' @method report bitacora
report.bitacora = function(x, format = "latex", tangle=FALSE, output = NULL) {

  if(is.null(output)) output = getwd()

  cacheDirs = list.dirs(path = ".", recursive = FALSE, full.names = TRUE)
  unlink(x = cacheDirs[grepl(x = basename(cacheDirs), pattern = "(_cache) | (_files)")], recursive = TRUE)

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


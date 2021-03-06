#' @title Print method for cpue objects
#' @description Shows main information from objects of the catch per unit effort
#' (cpue) class, like number of records data, the time period of dates
#' (years and months), the number of ports, the analyzed species and the
#' type of effort used on the cpue.
#' @param x Object of \code{cpue} class.
#' @param language The select language to print the outputs.
#' @export
#' @examples
#' # Read a example of a data base
#' fisheryData = system.file("extdata", "fisheryData.csv", package = "imarpe")
#'
#' # Produce a object of cpue class
#' cpue  = getFishingData(file = fisheryData, type = "cpue", varType = "cpue", sp = "caballa", efforType = "capacidad_bodega")
#' class(cpue)
#'
#' # Show main information of the cpue class object
#' print(cpue)
#' print(cpue, language = "english")
#'
#' @method print cpue
print.cpue = function(x, language="spanish"){

  if(language == "english"){
    cat("Data from: ", sQuote(x$info$file), "\n", sep="")
    cat("Number of records: ", x$info$records, "\n", sep="")
    cat("Number of months of data: ", x$info$months, "\n", sep="")
    cat("Number of years of data: ", x$info$years, "\n", sep="")
    cat("Number of ports of data: ", x$info$ports, "\n", sep="")
    cat("Analyzed species: ", x$info$sp, "\n", sep="")
    cat("Effort type: ", x$info$efforType, "\n", sep="")
  } else {
    cat("Datos de : ", sQuote(x$info$file), "\n", sep="")
    cat("Numero de registros: ", x$info$records, "\n", sep="")
    cat("Numero de meses de la data: ", x$info$months, "\n", sep="")
    cat("Numero de ahnos de la data: ", x$info$years, "\n", sep="")
    cat("Numero de puertos: ", x$info$ports, "\n", sep="")
    cat("Especie analizada: ", x$info$sp, "\n", sep="")
    cat("Tipo de esfuerzo: ", x$info$efforType, "\n", sep="")
  }

  return(invisible())
}

#' @title Summary method for cpue objects
#' @description Get summary information included on objects of catch
#' per unit effort (cpue) class.
#' @param object Object of \code{cpue} class.
#' @param language The select language to print the summary of cpue objects.
#' It could be \code{"spanish"} by default or \code{"english"}.
#' @return A \code{list} of summary.cpue class. This contains:
#' \itemize{
#'   \item effort The type of the effort type that has been to analyze.
#'   \item portDay A data frame with the cpue by day and port.
#'   \item day A data frame with the cpue by day.
#'   \item port A data frame with the cpue by ports.
#'   \item months A data frame with the cpue by months.
#'   \item years A data frame with the cpue by years.
#' }
#' @export
#' @examples
#' # Read a example of a data base
#' fisheryData = system.file("extdata", "fisheryData.csv", package = "imarpe")
#'
#' # Produce a object of cpue class
#' cpue  = getFishingData(file = fisheryData, type = "cpue", varType = "cpue", sp = "caballa", efforType = "capacidad_bodega")
#' class(cpue)
#'
#' # Produce the summary of the cpue class object
#' summary(cpue)
#'
#' @method summary cpue
summary.cpue = function(object, language = "spanish"){

  object2 = object
  if(language=="spanish"){
    object2$data$month = engToSpa(object2$data$month)
    colnames(object2$data) = c("anho", "mes", "dia", "cpue")

    object2$dataPortDay$month = engToSpa(object2$dataPortDay$month)
    colnames(object2$dataPortDay)[1:3] = c("anho", "mes", "dia")

  } else{
    object2$data$month = object2$data$month
    colnames(object2$data) = c("year", "month", "day", "cpue")
  }

  output = list()
  output$effort    =  object2$info$efforType
  output$portDay   =  object2$dataPortDay
  output$day       =  object2$data
  output$port      =  object2$dataPort
  output$months    =  .getMonth.cpue(object = object, language = language)
  output$years     =  .getYear.cpue(object = object)

  class(output) = "summary.cpue"
  return(output)
}

#' @title Print method for summary.cpue
#' @description Shows main information from \code{summary.cpue} objects.
#' @param x Object of \code{summary.cpue} class.
#' @param language The select language to print the summary of cpue objects.
#' It could be \code{"spanish"} by default or \code{"english"}.
#' @return Each element of \code{summary.cpue} method.
#' @export
#' @examples
#' # Read a example of a data base
#' fisheryData = system.file("extdata", "fisheryData.csv", package = "imarpe")
#'
#' # Produce a object of cpue class
#' cpue  = getFishingData(file = fisheryData, type = "cpue", varType = "cpue", sp = "caballa", efforType = "capacidad_bodega")
#' class(cpue)
#'
#' # Print the summary of the cpue class object
#' sumCpue = summary(effort)
#' print(sumCpue)
#'
#' sumCpue = summary(effort, language = "english")
#' print(sumCpue, language = "english")
#'
#' @method print summary.cpue
print.summary.cpue = function(x, language = "spanish"){

  x2 = x
  class(x2) = 'cpue'

  if(language == "english"){
    cat("\nCPUE by port and day (non-zero only):\n\n") ; print(x$portDay[x$portDay[,1]>0, ,drop=FALSE])
    cat("\nCPUE by day:\n\n") ; print(x$day)
    cat("\nCPUE by port (non-zero only):\n\n") ; print(x$port[x$port[,1]>0, ,drop=FALSE])
    cat("\nMonthly CPUE:\n\n") ; print(t(x$months))
    cat("\nAnnual CPUE:\n\n") ; print(x$years)}
  else {
    cat("\nCPUE por puerto y dia (solo positivos):\n\n") ; print(x$portDay[x$portDay[,1]>0, ,drop=FALSE])
    cat("\nCPUE por dia:\n\n") ; print(x$day)
    cat("\nCPUE por puerto (non-zero only):\n\n") ; print(x$port[x$port[,1]>0, ,drop=FALSE])
    cat("\nCPUE mensual:\n\n") ; print(t(x$months))
    cat("\nCPUE anual:\n\n") ; print(x$years)}

  return(invisible())
}

#' @title Plot method for cpue objects
#' @description This method takes a \code{cpue} object and make useful plots.
#' The plots can be daily, monthly, yearly, over the peruvian region, and
#' for north-central and south peruvian region.
#' @param x Object of \code{cpue} class.
#' @param language A \code{character}. Define the language of text labels in plots.
#' It could be \code{"spanish"} or \code{"english"}.
#' @param ploType What type of plot should be draw. Possible types are:
#' \itemize{
#'    \item plotDaily for daily plot
#'   \item plotMonthly for monthly plot
#'   \item plotYearly for yearly plot
#'   \item plotPeru for all the peruvian region
#'   \item plotNC to graph the north-central region
#'   \item plotS to graph the south region
#' }
#' @param daysToPlot If is a daily plot by default the x-axis show the first day of a week
#'  (1, 8, 15, 22) but could be change to show a specific day or to show the all days of the
#'  data with \code{all}. This is including in a vector form.
#' @param textAxis2 The text of the x axis.
#' @param textAxis4 The text of the y axis.
#' @param ... Extra arguments.
#' @return A graph of the specified type in \code{ploType}.
#' @export
#' @examples
#' # Read a example of a data base
#' fisheryData = system.file("extdata", "fisheryData.csv", package = "imarpe")
#'
#' # Produce a object of cpue class
#' cpue  = getFishingData(file = fisheryData, type = "cpue", varType = "cpue", sp = "caballa", efforType = "capacidad_bodega")
#' class(cpue)
#'
#' plot(cpue)
#' plot(cpue, daysToPlot = "1", colBar = "red")
#' plot(cpue, language= "english")
#' plot(cpue, ploType = "plotMonthly")
#' plot(cpue, ploType = "plotMonthly", language= "english", colBar = "green")
#' plot(cpue, ploType = "plotYearly")
#' plot(cpue, ploType = "plotYearly", colBar = "orange")
#' plot(cpue, ploType = "plotPERU")
#' plot(cpue, ploType = "plotNC")
#' plot(cpue, ploType = "plotS")
#'
#' @method plot cpue
plot.cpue = function(x, language = "spanish", ploType = NULL, daysToPlot = c(1,8,15,22),
                     textAxis2 = NULL, textAxis4 = NULL, colBar = "gray", ...){

  if(is.null(ploType)) ploType = "plotDaily"
  if(ploType %in% c("plotPERU", "plotNC", "plotS")){
    x2 = list()
    x2$data = x$dataPortDay
    x2$info$varType = x$info$varType
    x2$info$efforType = x$info$efforType

    dataRegion = .getRegionData(x = x2)}

  switch(ploType,
         plotDaily   = .plotDays.cpue(x=x, language=language, daysToPlot = daysToPlot, colBar = colBar, ...),

         plotMonthly = .plotMonths.cpue(x=x, language=language, colBar = colBar, ...),

         plotYearly  = .plotYears.cpue(x=x, language=language, colBar = colBar, ...),

         plotPERU    = .plotRegion(x = dataRegion, region = "PERU", daysToPlot = daysToPlot,
                                   textAxis2 = textAxis2, textAxis4 = textAxis4, ...),

         plotNC      = .plotRegion(x = dataRegion, region = "NC", daysToPlot = daysToPlot,
                                   textAxis2 = textAxis2, textAxis4 = textAxis4, ...),

         plotS       = .plotRegion(x = dataRegion, region = "S", daysToPlot = daysToPlot,
                                   textAxis2 = textAxis2, textAxis4 = textAxis4, ...))

  return(invisible())
}

#' @title Report method for cpue objects
#' @description Export a report of catch per unit effort (cpue) class.
#' @param x Object of \code{cpue} class.
#' @param daysToPlot If is a daily plot by default the x-axis show the first day of a week
#'  (1, 8, 15, 22) but could be change to show a specific day or to show the all days of the
#'  data with \code{all}. This is including in a vector form.
#' @param textAxis2 The text of the x axis.
#' @param textAxis4 The text of the y axis.
#' @export
#' @examples
#' # Read a example of a data base
#' fisheryData = system.file("extdata", "fisheryData.csv", package = "imarpe")
#'
#' # Produce a object of cpue class
#' cpue  = getFishingData(file = fisheryData, type = "cpue", varType = "cpue", sp = "caballa", efforType = "capacidad_bodega")
#' class(cpue)
#'
#' #Produce the report showing all days on the x-axis (by default)
#' report(cpue)
#'
#' #Produce the report showing only the day 15 on the x-axis
#' report(cpue, daysToPlot = "15")
#'
#' @method report cpue
report.cpue = function(x, format = "latex", tangle=FALSE, output = NULL, open = TRUE,
                       daysToPlot = c(1,8,15,22), textAxis2 = NULL, textAxis4 = NULL){

  if(is.null(output)) output = getwd()

  outputName = deparse(substitute(x))

  skeleton = system.file("reports", "cpue-report.Rmd", package = "imarpe")

  if(isTRUE(tangle)) {
    knit(skeleton, tangle=TRUE, encoding = "latin1")
    f1 = gsub(pattern = ".Rmd", replacement = "\\.R", skeleton)
    file.rename(from=basename(f1), to=paste0(outputName, ".R"))
  }

  outputFile = paste0(outputName, "_output.pdf")
  render(skeleton, c("pdf_document"), output_file=outputFile, output_dir=output, encoding = "latin1")

  if(isTRUE(open)) shell.exec(outputFile)

  return(invisible(file.path(output, outputFile)))

}

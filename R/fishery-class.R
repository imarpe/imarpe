#' @title Print method for fishery objects
#' @description Shows main information from fishery objects, like number of records data,
#' the time period of date (years and months), the number of ports, the analyzed species and
#' the analyzed variable type.
#' @param x Object of class \code{fishery}.
#' @param language The select language to print the outputs.
#' @export
#' @method print fishery
print.fishery = function(x, language="spanish") {

  if(language == "english"){
    cat("Data from: ", sQuote(x$info$file), "\n", sep="")
    cat("Number of records: ", x$info$records, "\n", sep="")
    cat("Number of months of data: ", x$info$months, "\n", sep="")
    cat("Number of years of data: ", x$info$years, "\n", sep="")
    cat("Number of ports of data: ", x$info$ports, "\n", sep="")
    cat("Analyzed species: ", x$info$sp, "\n", sep="")
    cat("Analyzed variable: ", x$info$varType, "\n", sep="")
  } else {
    cat("Datos de : ", sQuote(x$info$file), "\n", sep="")
    cat("Numero de registros: ", x$info$records, "\n", sep="")
    cat("Numero de meses de la data: ", x$info$months, "\n", sep="")
    cat("Numero de ahnos de la data: ", x$info$years, "\n", sep="")
    cat("Numero de puertos: ", x$info$ports, "\n", sep="")
    cat("Especie analizada: ", x$info$sp, "\n", sep="")
    cat("Variable analizada: ", x$info$varType, "\n", sep="")
  }

  return(invisible())
}

#' @title Summary method for fishery objects
#' @description Get summary information of landing and fishing effort included on
#'  \code{fishery} objects.
#' @param object Object of class \code{fishery}.
#' @param language The select language to print the summary of fishery objects.
#' It could be \code{"spanish"} by default or \code{"english"}.
#' @return A \code{list} of summary.fishery class. This contains:
#' \itemize{
#'   \item var The type of the variable that has been to analyze. This can be lading or effort.
#'   \item portDay A data frame with the information of the variable analyzed by day and port.
#'   \item day A data frame with the information of the variable by day.
#'   \item port A data frame with the information of the variable by ports.
#'   \item months A data frame with the information of the variable by months.
#'   \item years A data frame with the information of the variable by years.
#' }
#' @export
#' @method summary fishery
summary.fishery =  function(object, language = "spanish") {

  object2 = object
  if(language=="spanish"){
    object2$data$month = engToSpa(object2$data$month)
    colnames(object2$data)[1:3] = c("anho", "mes", "dia")

  } else{
    object2$data$month = object2$data$month
    colnames(object2$data)[1:3] = c("year", "month", "day")
  }

  output = list()

  output$var      = object$info$varType
  output$portDay =  object2$data
  output$day     =  .getSumPorts.fishery(object=object, language=language)
  output$port    =  .getPorts.fishery(object=object, language=language)
  output$months   = .getMonth.fishery(object=object, language=language)
  output$years    = .getYear.fishery(object=object, language=language)

  class(output) = "summary.fishery"
  return(output)
}

#' @title Print method for summary.fishery
#' @description Shows main information from \code{summary.fishery} objects.
#' @param x Object of class \code{summary.fishery}.
#' @param language The select language to print the summary of fishery objects.
#' It could be \code{"spanish"} by default or \code{"english"}.
#' @return Each element of \code{summary.fishery} method.
#' @export
#' @method print summary.fishery
print.summary.fishery = function(x, language = "spanish") {

  x2 = x
  class(x2) = 'fishery'

  if(x$var == "landing"){
    if(language == "english"){
      cat("\nLanding by port and day (non-zero only):\n\n") ; print(x$portDay[x$portDay[,1]>0, ,drop=FALSE])
      cat("\nLanding by day:\n\n") ; print(x$day)
      cat("\nLanding by port (non-zero only):\n\n") ; print(x$port[x$port[,1]>0, ,drop=FALSE])
      cat("\nMonthly landing:\n\n") ; print(t(x$months))
      cat("\nAnnual landing:\n\n") ; print(x$years)}
    else {
      cat("\nDesembarque por puerto y por dia (solo positivos):\n\n") ; print(x$portDay[x$portDay[,1]>0, ,drop=FALSE])
      cat("\nDesembarque por dia:\n\n") ; print(x$day)
      cat("\nDesembarque por puerto (solo positivos):\n\n") ; print(x$port[x$port[,1]>0, ,drop=FALSE])
      cat("\nDesembarque mensual:\n\n") ; print(t(x$months))
      cat("\nDesembarque anual:\n\n") ; print(x$years)}

  } else {
    if(language == "english"){
      cat("\nEffort by port and day (non-zero only):\n\n") ; print(x$portDay[x$portDay[,1]>0, ,drop=FALSE])
      cat("\nEffort by day:\n\n") ; print(x$day)
      cat("\nEffort by port (non-zero only):\n\n") ; print(x$port[x$port[,1]>0, ,drop=FALSE])
      cat("\nMonthly effort:\n\n") ; print(t(x$months))
      cat("\nAnnual effort:\n\n") ; print(x$years)}
    else {
      cat("\nEsfuerzo por puerto y por dia (solo positivos):\n\n") ; print(x$portDay[x$portDay[,1]>0, ,drop=FALSE])
      cat("\nEsfuerzo por dia:\n\n") ; print(x$day)
      cat("\nEsfuerzo por puerto (solo positivos):\n\n") ; print(x$port[x$port[,1]>0, ,drop=FALSE])
      cat("\nEsfuerzo mensual:\n\n") ; print(t(x$months))
      cat("\nEsfuerzo anual:\n\n") ; print(x$years)}
  }

  return(invisible())
}

#' @title Plot method for fishery objects
#' @description This method takes a \code{fishery} object and make useful plots for
#' each variables (lading and fishing effort). The plots can be daily, monthly, yearly or
#' for north-central and south peruvian region.
#' @param x Object of \code{fishing} class.
#' @param language \code{character}. Define the language of text labels in plots.
#' It could be \code{"spanish"} or \code{"english"}.
#' @param ploType What type of plot should be draw. Possible types are:
#' \itemize{
#'   \item plotDaily for daily plot
#'   \item plotMonthly for monthly plot
#'   \item plotYearly for yearly plot
#'   \item plotNC to plot the north-central region
#'   \item plotS to plot the south region
#' }
#' @param daysToPlot If is a daily plot by default the x axis plot the first day of the month
#'  (1, 8, 15, 22). This is including in a vector form.
#' @param textAxis2 The text of the x axis.
#' @param textAxis4 The text of the y axis.
#' @param ... Extra arguments.
#' @return A graph of the specified type in \code{ploType}.
#' @export
#' @method plot fishery
plot.fishery = function(x, language, ploType = NULL, daysToPlot = c(1,8,15,22),
                        textAxis2 = NULL, textAxis4 = NULL, ...) {

  if(is.null(ploType)) ploType = "plotDaily"
  if(ploType %in% c("plotNC", "plotS")){dataRegion = .getRegionData(x = x)}

  switch(ploType,
         plotDaily   = .plotDays.fishery(x=x, language=language, daysToPlot = daysToPlot,
                                         textAxis2 = textAxis2, textAxis4 = textAxis4, ...),

         plotMonthly = .plotMonths.fishery(x=x, language=language, ...),

         plotYearly  = .plotYears.fishery(x=x, language=language, ...),

         plotNC      = .plotRegion(x = dataRegion, region = "NC", daysToPlot = daysToPlot,
                                   textAxis2 = textAxis2, textAxis4 = textAxis4, ...),

         plotS       = .plotRegion(x = dataRegion, region = "S", daysToPlot = daysToPlot,
                                   textAxis2 = textAxis2, textAxis4 = textAxis4, ...))
  return(invisible())

}

#' @title Report method for fishery objects
#' @description Export a report of landing or fishing effort.
#' @param x Object of class \code{fishery}.
#' @param daysToPlot If is a daily plot by default the x axis plot the first day of the month
#'  (1, 8, 15, 22). This is including in a vector form.
#' @param textAxis2 The text of the x axis.
#' @param textAxis4 The text of the y axis.
#' @export
#' @method report fishery
report.fishery = function(x, format="latex", tangle=FALSE, output = NULL,
                          daysToPlot = c(1,8,15,22), textAxis2 = NULL, textAxis4 = NULL){

  if(is.null(output)) output = getwd()
  outputName = deparse(substitute(x))

  varType = x$info$varType
  if(varType == "landing"){skeleton = system.file("reports", "fishery-report_landing.Rmd", package = "imarpe")
  } else {skeleton = system.file("reports", "fishery-report_effort.Rmd", package = "imarpe")}

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

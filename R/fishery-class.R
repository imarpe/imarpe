
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


report = function(x, ...) {
  UseMethod("report")
}


report.fishery = function(x, format = "latex", tangle=FALSE, output = NULL, daysToPlot = c(1,8,15,22),
                          textAxis2 = NULL, textAxis4 = NULL){

  if(is.null(output)) output = getwd()
  outputName = deparse(substitute(x))

  varType = x$info$varType
  if(varType == "landing"){skeleton = system.file("reports", "fishery-report_landing.Rmd", package = "imarpe")
  } else {skeleton = system.file("reports", "fishery-report_effort.Rmd", package = "imarpe")}

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

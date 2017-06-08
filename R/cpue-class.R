# plot.fishery

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
  output$day     =  object2$data
  output$portDay =  object2$dataPortDay
  output$port    =  object2$dataPort
  output$months  =  .getMonth.cpue(object = object, language = language)
  output$years   =  .getYear.cpue(object = object)
  
  class(output) = "summary.cpue"
  return(output)
}


print.summary.cpue = function(x, language = "spanish"){
  
  x2 = x
  class(x2) = 'cpue'
  
  if(language == "english"){
    cat("\nCPUE by day:\n\n") ; print(x$day)
    cat("\nCPUE by port and day (non-zero only):\n\n") ; print(x$portDay[x$portDay[,1]>0, ,drop=FALSE])
    cat("\nCPUE by port (non-zero only):\n\n") ; print(x$port[x$port[,1]>0, ,drop=FALSE])
    cat("\nMonthly CPUE:\n\n") ; print(t(x$months))
    cat("\nAnnual CPUE:\n\n") ; print(x$years)}
  else {
    cat("\nCPUE por dia:\n\n") ; print(x$day)
    cat("\nCPUE por puertos y dias (solo positivos):\n\n") ; print(x$portDay[x$portDay[,1]>0, ,drop=FALSE])
    cat("\nCPUE por puertos (non-zero only):\n\n") ; print(x$port[x$port[,1]>0, ,drop=FALSE])
    cat("\nCPUE mensual:\n\n") ; print(t(x$months))
    cat("\nCPUE anual:\n\n") ; print(x$years)}
    
  return(invisible())
}


plot.cpue = function(x, language="spanish", time = NULL, ...){
  
  if(is.null(time)) time = "day"
  switch(time,
         day   = .plotDays.cpue(x=x, language=language,...),
         month = .plotMonths.cpue(x=x, language=language, ...),
         year  = .plotYears.cpue(x=x, language=language, ...))
  
  return(invisible())
}

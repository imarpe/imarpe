
.getSurveyData = function(file=file, fSp = fSp,
                          lSp = lSp, ...) {
  
  out = read.csv(file=file, na.strings="", stringsAsFactors = FALSE, ...)
  
  id = c("nombre_cruc", "fecha", "latitud", "barco")
  check =  id %in% tolower(colnames(out)) 
  if(!all(check)) stop("File does not seem to by of type 'survey'.")
  
  out$fecha = out$fecha[out$fecha != "" & !is.na(out$fecha)]
  out$fecha = strptime(out$fecha, format = "%d/%m/%Y")
  
  agno = unique(as.numeric(format(out$fecha, format = "%Y")))
  surveyName = unique(as.character(out$nombre_cruc))
  
  maxdateTotal = max(out$fecha, na.rm = TRUE) #
  mindateTotal = min(out$fecha, na.rm = TRUE) #

  nBarcos = length(unique(as.character(out$barco)))
  nCalas = nrow(out)

  info = list(file = file,
              agno_crucero = agno,
              nombre_crucero = surveyName,
              inicio_crucero = mindateTotal, 
              fin_crucero = maxdateTotal,
              number_trawl = nCalas
              )
  
  output = list(data=out, info=info, firstSp = fSp, 
                lastSp = lSp)
  class(output) = c("survey")
  return(output)
  
}

#' Print General Description of Survey Objects
#' @description
#' Prints the main features of the \dQuote{survey} database such as file name, dates, trawl numbers, latitudes 
#' and year, and returns them invisibly (via \code{\link{invisible}}).
#' @param x An object of the class \dQuote{survey}.
#' @param \dots Further arguments accepted by \code{\link{print}}.
#' @details It is not useful to print all the content of large databases as \dQuote{survey} object. 
#' For this reason, this function is useful to print only metadata of \dQuote{survey} object.
#' @return None (invisible NULL).
#' @author Giancarlo Moron, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{plot.survey}}, \code{\link{summary.survey}}
#' @examples
#' ## Loading the database of the class 'survey'
#' data(Survey)
#' 
#' ## Verifying the class of the data
#' class(Survey)
#' 
#' ## S3 method for class 'survey'
#' print(Survey)
print.survey = function(x, ...) {
  
  cat("Survey data from ", sQuote(x$info$file), "\n", sep="")
  x$info$agno_crucero = x$info$agno_crucero
  RowNames=c("Agno","Survey Name",
             "Start of the Survey","End of the Survey",
             "Number of trawls")
  RowNames=.RefineChar4Table(RowNames)
  FrameInfo=data.frame(Value=as.character(x$info[-1]),row.names=RowNames)
  colnames(FrameInfo)=c(" ")
  print(FrameInfo, ...)
  
  return(invisible())
  
}

#' Create a Summary.survey Object
#' @description
#' Creates a new object of class \dQuote{summary.survey}. 
#' This class has summary information about a \dQuote{survey} object, which includes the metadata, Boat names, 
#' trawls by latitude, sample composition, diversity.
#' @param object An object of class \dQuote{survey}.
#' @param \dots Further arguments.
#' @return Returns an object of class \dQuote{summary.survey} containing the following features:
#' \tabular{ll}{
#' \code{info} \tab General overview, metadata, of the \dQuote{survey} database (year, survey name, start and end of the survey).\cr
#' \code{boats} \tab Dataframe indicating trawls and latitudes by boat.\cr
#' \code{trawl} \tab Dataframe indicating the number of trawls by latitude.\cr
#' \code{composition} \tab Dataframe indicating the most abundant species.\cr
#' \code{diversity} \tab Dataframe indicating four different diversity indices.
#' }
#' @author Giancarlo Moron, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{plot.survey}}, \code{\link{print.survey}}
#' @examples
#' ## Loading the database of the class 'survey'
#' data(Survey)
#' 
#' ## Verifying the class of the data
#' class(Survey)
#' 
#' ## S3 method for class 'survey'
#' SummarySurvey = summary(Survey)
summary.survey = function(object,...) {
  
  output = list()
  
  output$info        =  object$info
  output$boats       = .getBoats.survey(object)
  output$trawl       = .getNumberTrawl.survey(object) 
  output$composition = .getComposition.survey(object)
  output$diversity   = .getDiversity.survey(object)
  
  class(output) = "summary.survey"
  return(output)
  
}

#' Print General Description of Summary.survey Objects
#' @description
#' Prints summary information about a \dQuote{survey} object, which includes the metadata, Boat names, 
#' trawls by latitude, sample composition, diversity.
#' @param x An object of class \dQuote{summary.survey}.
#' @param \dots Further arguments accepted by \code{\link{print}}
#' @details
#' \tabular{ll}{
#' \code{info} \tab General overview of the survey database (year, survey name, start and end of the survey). \cr
#' \code{Boats information} \tab Dataframe indicating trawls and latitudes by boat.\cr
#' \code{Trawls by latitude} \tab Table indicating the number of trawls by latitude.\cr
#' \code{Species composition} \tab Dataframe indicating the most abundant species.\cr
#' \code{Diversity} \tab Dataframe indicating four different diversity indices.
#' }
#' @return None (invisible NULL).
#' @author Giancarlo Moron, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{summary.bitacoras}}, \code{\link{print.bitacoras}}, \code{\link{plot.bitacoras}}
#' @examples
#' ## Loading the database of the class 'bitacoras'
#' data(Survey)
#' 
#' ## Printing the summary
#' summary(Survey)
print.summary.bitacoras = function(x,...) {
  
    x2=x; class(x2)='survey'
    print(x2,...)
    
  cat("\nBoats information:\n\n")
  print(x$boats,...)
  
  cat("\nTrawls by latitude:\n\n")
  print(x$trawls,...)
  
  cat("\nSpecies composition:\n\n")
  print(t(x$composition),...)
  
  cat("\nDiversity:\n\n")
  print(t(x$diversity),...)
  
  return(invisible(x))
}

#' Plot a Bitacoras Object
#' @description
#' The \code{plot} method for objects of class \dQuote{bitacoras}. 
#' This function plots two kind of graphs, which are defined by \dQuote{type} argument.
#' @param x Object of class \dQuote{bitacoras}.
#' @param type Type of graph to plot. Possible types are:
#' \itemize{
#'   \item \dQuote{effort} for plotting a barplot of average travel time with a line of average coves by port.
#'   \item \dQuote{depth} for plotting a boxplot of depths by ranges of latitudes.
#' }
#' @param detailed Logical attribute for \dQuote{depth} \code{type}. 
#' If it is TRUE, points will be added in the boxplot (FALSE by defautl).
#' @param \dots Other graphical parameters accepted by \code{\link{plot}}.
#' @details
#' In \dQuote{effort} graph, plotted statistics like travel time, number of coves are 
#' average data of the gathered profiles by port.
#' @return None (invisible NULL).
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{print.bitacoras}}, \code{\link{summary.bitacoras}}
#' @examples
#' ## Loading the data
#' data(Bitacoras)
#' 
#' ## Plotting by effort
#' plot(Bitacoras,type="effort")
#' 
#' ## Plotting by depth
#' plot(Bitacoras,type="depth")
plot.bitacoras = function(x, type, detailed=FALSE,...) {
  
  switch(type,
         effort = .plotEffort.bitacoras(x=x, ...),
         depth = .plotDepth.bitacoras(object=x,detailed,...),
         sets = .plotSets.bitacoras(object=x,detailed,...),
         stop("Plot type not defined."))
  
  return(invisible())
}
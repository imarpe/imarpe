
.getBitacorasData = function(file=file, ...) {
  
  out = read.csv(file=file, na.strings="", stringsAsFactors = FALSE, ...)
  
  id = c("anchoveta", "sardina", "jurel", "caballa", "otros", "observador")
  check =  id %in% tolower(colnames(out)) 
  if(!all(check)) stop("File does not seem to by of type 'bitacoras'.")
  
  ports = unique(out$puerto.salida)
  
  date = out$dia.salida
  date = date[date != "" & !is.na(date)]
  dataDate = strptime(date, format="%d-%m-%Y %H:%M")
  years = as.numeric(format(dataDate, "%Y"))
  
  info = list(file=file,
              records=length(unique(out$dia.salida)), 
              observers=length(unique(out$observador)),
              ports = length(ports[!is.na(ports)]),
              years = unique(years))
  output = list(data=out, info=info)
  class(output) = c("bitacoras")
  return(output)
  
}

#' Print General Description of Bitacoras Objects
#' @description
#' Prints the main features of the \dQuote{bitacoras} database such as file name, number of records, observers, ports 
#' and involved years, and returns them invisibly (via \code{\link{invisible}}).
#' @param x An object of the class \dQuote{bitacoras}.
#' @param \dots Further arguments accepted by \code{\link{print}}.
#' @details It is not useful to print all the content of large databases as \dQuote{bitacoras} object. 
#' For this reason, this function is useful to print only metadata of \dQuote{bitacoras} object.
#' @return None (invisible NULL).
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{plot.bitacoras}}, \code{\link{summary.bitacoras}}
#' @examples
#' ## Loading the database of the class 'bitacoras'
#' data(Bitacoras)
#' 
#' ## Verifying the class of the data
#' class(Bitacoras)
#' 
#' ## S3 method for class 'bitacoras'
#' print(Bitacoras)
print.bitacoras = function(x, ...) {
  
  cat("Bitacoras data from ", sQuote(x$info$file), "\n", sep="")
  x$info$years=as.character(paste(x$info$years,collapse=", "))
  RowNames=c("Number of records","Number of observers",
             "Number of ports","Years")
  RowNames=.RefineChar4Table(RowNames)
  FrameInfo=data.frame(Value=as.character(x$info[-1]),row.names=RowNames)
  colnames(FrameInfo)=c(" ")
  print(FrameInfo,...)
#   cat("Number of records: ", x$info$records, "\n", sep="")
#   cat("Number of observers: ", x$info$observers,"\n", sep="")
#   cat("Number of ports: ", x$info$ports,"\n", sep="")
#   cat("Years: ", x$info$years, "\n", sep="")
  
  return(invisible())
  
}

#' Create a Summary.bitacoras Object
#' @description
#' Creates a new object of class \dQuote{summary.bitacoras}. 
#' This class has summary information about a \dQuote{bitacoras} object, which includes the metadata, captures by specie, 
#' travel duration and number of coves, observers by port, coves by latitude and depth by latitude.
#' @param object An object of class \dQuote{bitacoras}.
#' @param \dots Further arguments.
#' @return Returns an object of class \dQuote{summary.bitacoras} containing the following features:
#' \tabular{ll}{
#' \code{info} \tab General overview, metadata, of the \dQuote{bitacoras} database (number of records, observers, ports and involved years).\cr
#' \code{composition} \tab Dataframe indicating the number of captures by fish species.\cr
#' \code{effort} \tab Dataframe indicating the travel duration and the number of coves by port.\cr
#' \code{observer} \tab Dataframe indicating the number of observers by port.\cr
#' \code{set} \tab Dataframe indicating the number of coves by latitude.\cr
#' \code{depth} \tab Dataframe indicating the upper limit, lower limit and average depth by latitude.
#' }
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{plot.bitacoras}}, \code{\link{print.bitacoras}}
#' @examples
#' ## Loading the database of the class 'bitacoras'
#' data(Bitacoras)
#' 
#' ## Verifying the class of the data
#' class(Bitacoras)
#' 
#' ## S3 method for class 'bitacoras'
#' SummaryBitacoras = summary(Bitacoras)
summary.bitacoras = function(object,...) {
  
  output = list()
  
  output$info = object$info
  output$composition = .getSpeciesComposition.bitacoras(object)
  output$effort      = .getEffort.bitacoras(object) 
  output$observer    = .getNumberObserver.bitacoras(object)
  output$set         = .getNumberSet.bitacoras(object)#number of coves by latitude
  output$depth       = .getDepth.bitacoras(object)#depth by latitude
  
  class(output) = "summary.bitacoras"
  return(output)
  
}

#' Print General Description of Summary.bitacoras Objects
#' @description
#' Prints summary information about a \dQuote{bitacoras} object, which includes  the metadata, captures by specie, 
#' travel duration and number of coves, observers by port, coves by latitude and depth by latitude.
#' @param x An object of class \dQuote{summary.bitacoras}.
#' @param \dots Further arguments accepted by \code{\link{print}}
#' @details
#' \tabular{ll}{
#' \code{info} \tab General overview of the bitacoras database (number of records, observers, ports and involved years). \cr
#' \code{Species composition} \tab Table indicating the number and percentage of captures by fish species.\cr
#' \code{Sampling effort} \tab Table indicating the travel duration and the average number of coves by port.\cr
#' \code{Number of observers} \tab Table indicating the number of observers by port.\cr
#' \code{Number of sets by latitude} \tab Table indicating the number of coves by latitude.\cr
#' \code{Depth by latitude} \tab Table indicating the upper limit, lower limit and average depth by latitude.
#' }
#' @return None (invisible NULL).
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{summary.bitacoras}}, \code{\link{print.bitacoras}}, \code{\link{plot.bitacoras}}
#' @examples
#' ## Loading the database of the class 'bitacoras'
#' data(Bitacoras)
#' 
#' ## Printing the summary
#' summary(Bitacoras)
print.summary.bitacoras = function(x,...) {
  
    x2=x; class(x2)='bitacoras'
    print(x2,...)
    
  cat("\nSpecies composition:\n\n")
  print(x$composition,...)
  
  cat("\nSampling effort:\n\n")
  print(x$effort,...)
  
  cat("\nNumber of observers:\n\n")
  print(x$observer,...)
  
  cat("\nNumber of sets by latitudinal degree:\n\n")
  print(t(x$set),...)
  
  cat("\nDepth by latitudinal degree:\n\n")
  print(t(x$depth),...)
  
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

.getLandingsData = function(file=file, ...) {
  
  out = read.csv(file=file, na.strings="", stringsAsFactors = FALSE, ...)
  
  ports = out[c(4:length(colnames(out)))]
  namesPorts = names(ports)
  
  info = list(file=file, records=nrow(out), 
              months = length(rle(out$month)$values),
              years  = length(unique(out$year)),
              ports  = length(namesPorts))
  
  output = list(data=out, info=info)
  class(output) = c("landings")
  return(output)
  
}

#' Print General Description of Landings Objects
#' @description
#' Prints the main features of the \dQuote{landings} database such as the number of records, months, years and ports, 
#' and returns them invisibly (via \code{\link{invisible}}).
#' @param x An object of the class \dQuote{landings}.
#' @param \dots Further arguments accepted by \code{\link{print}}
#' @details It is not useful to print all the content of large databases as \dQuote{landings} objects. 
#' For this reason, this function is useful to print only metadata of \dQuote{landings} objects.
#' @return None (invisible NULL).
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{plot.landings}}, \code{\link{summary.landings}}
#' @examples
#' ## Loading the database of the class 'landings'
#' data(Landings)
#' 
#' ## Verifying the class of data
#' class(Landings)
#' 
#' ## S3 method for class 'landings'
#' print(Landings)
print.landings = function(x,...) {
  
  cat("Landings data from ", sQuote(x$info$file), "\n", sep="")
  RowNames=c("Number of records","Number of months of data",
             "Number of years of data","Number of ports")
  #W=rep(':',4)
  #Z=data.frame(W,Value=as.numeric(x$info[-1]),row.names=RowNames)
  #colnames(Z)=c(" "," ")
  #print(Z)
  
  RowNames=.RefineChar4Table(RowNames)
  FrameInfo=data.frame(Value=as.numeric(x$info[-1]),row.names=RowNames)
  colnames(FrameInfo)=c(" ")
  print(FrameInfo,...)

  #cat("Number of records: ", x$info$records, "\n", sep="")
  #cat("Number of months of data: ", x$info$months, "\n", sep="")
  #cat("Number of years of data: ", x$info$years, "\n", sep="")
  #cat("Number of ports: ", x$info$ports, "\n", sep="")
  
  return(invisible())
}

#' Create a Summary.landings Object
#' @description
#' Creates a new object of class \dQuote{summary.landings}. This class has summary information about a \dQuote{landings} object, which includes the metadata, 
#' captures by specie, number of ports by day, landings by port, landings by month and landings by year.
#' @param object An object of class \dQuote{landings}.
#' @param \dots Further arguments
#' @return Returns an object of class \dQuote{summary.landings} containing the following features:
#' \tabular{ll}{
#' \code{info} \tab General overview, metadata, of the landings database (number of records, months, years and ports).\cr
#' \code{sumPorts} \tab Dataframe containing the number of ports by day.\cr
#' \code{ports} \tab Dataframe indicating the number of landings by port.\cr
#' \code{months} \tab Dataframe indicating the number of landings by month.\cr
#' \code{years} \tab Dataframe indicating the number of landings by year.
#' }
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{plot.landings}}, \code{\link{summary.landings}}
#' @examples
#' ## Loading the database of the class 'landings'
#' data(Landings)
#' 
#' ## Verifying the class of the data
#' class(Landings)
#' 
#' ## S3 method for class 'landings'
#' SummaryLandings = summary(Landings)
summary.landings = function(object,...) {
  
  output = list()
  
  output$info = object$info
  
  output$sumPorts = .getSumPorts.landings(object)
  output$ports    = .getPorts.landings(object)
  output$months   = .getMonth.landings(object)
  output$years    = .getYear.landing(object)
  
  class(output) = "summary.landings"
  return(output)
  
}

#' Print General Description of Summary.landings Objects
#' @description
#' Prints summary information about a \dQuote{landings} object, which includes  the metadata, landings by ports, monthly landings and annual landings.
#' @param x An object of class \dQuote{summary.landings}.
#' @param \dots Further arguments accepted by \code{\link{print}}
#' @details
#' \tabular{ll}{
#' \code{info} \tab General overview of the landings database (number of records, months, years and ports). \cr
#' \code{ports} \tab Table indicating the number of landings by port.\cr
#' \code{months} \tab Table indicating the number of landings by month.\cr
#' \code{years} \tab Table indicating the number of landings by year.
#' }
#' @return None (invisible NULL).
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{summary.landings}}, \code{\link{print.landings}}, \code{\link{plot.landings}}
#' @examples
#' ## Loading the database of the class 'landings'
#' data(Landings)
#' 
#' ## Printing the summary
#' summary(Landings)
print.summary.landings = function(x, ...) {
  
  x2=x; class(x2)='landings'
  print(x2,...)
    
#   cat("\nDaily landing:\n\n")
#   print(x$sumPorts)
  
  cat("\nLandings by ports (non-zero only):\n\n")
#   PortNames=rownames(x$ports)
#   PortNames[x$ports$Landings>0]=RefineChar4Table(PortNames[x$ports$Landings>0])
#   rownames(x$ports)=PortNames
  print(x$ports[x$ports$Landings>0, ,drop=FALSE],...)
  
  cat("\nMonthly landing:\n\n")
  print(t(x$months),...)
  
  cat("\nAnnual landing:\n\n")
  print(x$years,...)
  
  return(invisible(x))
  
}

#' Plot a Landings Object
#' @description
#' The \code{plot} method for objects of class \dQuote{landings}. 
#' This function plots three kinds of graphs, which are defined by \dQuote{time} argument.
#' @param x Object of class "landings".
#' @param time Required period of time. Possible types are
#' \itemize{
#'   \item "day" for plotting by days,
#'   \item "month" for plotting by months,
#'   \item "year" for plotting by years.
#' }
#' @param \dots Other graphical parameters accepted by \code{\link{plot}}.
#' @details In \dQuote{day} graph, the user can define the extent of the days that he wants to plot by adding the start and end date parameters as a character class.
#' @return None (invisible NULL).
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso \code{\link{getData}}, \code{\link{print.landings}}, \code{\link{summary.landings}}
#' @examples
#' ## Loading the data
#' data(Landings)
#' 
#' ## Plotting by day
#' # to plot the whole range of days 
#' plot(Landings,time="day") 
#' # to plot a subset of the days 
#' plot(Landings,time="day",start="2013-02-14",end="2013-06-16") 
#' 
#' ## Plotting by month
#' plot(Landings,time="month")
#' 
#' ## Plotting by year
#' plot(Landings,time="year")
plot.landings = function(x, time=NULL, ...) {
  
#  if(!is.null(y) & is.null(time)) time = y
#  if( is.null(y) & is.null(time)) time = "day"
   if(is.null(time)) time="day"
  switch(time,
         day   = .plotDays.landings(x=x, ...),
         month = .plotMonths.landings(x=x, ...),
         year  = .plotYears.landings(x=x, ...),
  )
  
  return(invisible())
}
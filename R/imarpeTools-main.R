# To define generic classes and methods for the package
#' Data Input
#' @description
#' Reads a file and creates a database of the desired type, which includes a data frame and a 
#' list containing the main features of the data frame. 
#' The included data frame object has observations corresponding to rows and variables to columns. 
#' @param file The name of the file from which the data is read. Each row of the table appears as one line of the file. 
#' If it does not contain an absolute path, the file name is relative to the current working directory, \code{\link{getwd}}. 
#' @param type Type of the database to be read. The following types are accepted: \dQuote{bitacoras},\dQuote{landings}.
#' @param \dots Further arguments allowed by \code{\link{read.csv}} function.
#' @details
#' This function is the principal function of reading data into R for the \pkg{imarpe} package.
#' @return 
#' \tabular{ll}{
#' \code{data} \tab A data frame containing a representation of the data in the file.\cr
#' \code{info} \tab A list containing the main features of the data.
#' }
#' @note If type is not explicitly defined, the function will be equivalent to use \code{\link{read.csv}}
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @seealso After reading a database with \code{\link{getData}} function, you can use other functions to 
#' summary or plot the obtained data using functions like \code{\link{plot.bitacoras}}, \code{\link{plot.landings}}, 
#' \code{\link{summary.bitacoras}}, \code{\link{summary.landings}}.
#' @examples
#' ## Creating the database in a temporal file
#' data(Bitacoras)
#' Data = Bitacoras$data
#' tf = tempfile('Bitacoras.csv')
#' write.csv(Data,tf,na="")
#' 
#' ## Importing the data with the function
#' ImpData=getData(tf,type = 'bitacoras')
#' unlink(tf)
#' 
#' ## Verifying the class of the data
#' class(data)
getData = function(file, type, ...) {
  
  output = switch(type,
                  bitacoras = .getBitacorasData(file=file, ...),
                  landings = .getLandingsData(file=file, ...),
                  read.csv(file=file, ...)
  )
  return(output)
}
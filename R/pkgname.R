#' \pkg{imarpe}: Automation of Graphs, Tables and Reports of the Instituto del Mar del Peru (IMARPE).
#'
#' This package provides analytics tools to make graphs and tables that are often used in the reports produced by the Instituto del Mar del Peru (IMARPE). 
#' The \pkg{imarpe} package has been designed to work with generic functions (e.g. \code{\link{plot}}, \code{\link{summary}}, \code{\link{print}}) without caring the kind of data to be handled. 
#' Hence, it is easy to use and makes feasible the application of the package for beginners in \code{R} programming. 
#'
#' @details 
#' The environment of the package works basically with five different classes of objects such as bitacoras, landing, length, composition and spatialDistribution.
#' @section Additional Information:
#' \tabular{ll}{
#' Package: \tab imarpe\cr
#' Type: \tab Package\cr
#' Version: \tab 0.1\cr
#' Date: \tab 2014-03-11\cr
#' License: \tab TBD
#' }
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' 
#' @seealso The following list shows the functions that can be used to analyze the different kinds of classes after 
#' reading the dataset from a file with \code{\link{getData}} function.
#' \describe{
#'   \item{\bold{Bitacoras}}{\code{\link{print.bitacoras}},\code{\link{summary.bitacoras}}, \code{\link{plot.bitacoras}}}
#'   \item{\bold{Landings}}{\code{\link{print.landings}},\code{\link{summary.landings}}, \code{\link{plot.landings}}}
#' }
#' @keywords graphs tables reports R
#' @concept IMARPE
#' @examples
#' ## For bitacoras class
#' ## Loading the data
#' data = Bitacoras
#' ## Plotting by effort
#' plot(data,type="effort")
#' ## Plotting by depth
#' plot(data,type="depth")
#' 
#' ## For landings class
#' ## Loading the data
#' data = Landings
#' ## Plotting by month
#' plot(data,time="month")
#' ## Plotting by year
#' plot(data,time="year")
#' 
#' @docType package
#' @name imarpe
NULL
%\inputencoding{utf8}
\name{imarpe-package}
\alias{imarpe-package}
\alias{imarpe}
\docType{package}
\title{
\pkg{imarpe}: Automation of Graphs, Tables and Reports of the Instituto del Mar del Perú (IMARPE).
%imarpe: automatizaci?n de gr?ficas, tablas y reportes del Instituto del Mar del Per?.
}
\description{
This package provides analytics tools to make graphs and tables that are often used in the  reports produced by the Instituto del Mar del Peru (IMARPE). The \pkg{imarpe} package has been designed to work with generic functions (e.g. \code{\link{plot}}, \code{\link{summary}}, \code{\link{print}}) without caring the kind of data to be handled. Hence, it is easy to use and makes feasible the application of the package for beginners in \code{R} programming. 
%Este paquete proporciona herramientas para la elaboraci?n de gr?ficas y tablas que se realizan de manera rutinaria en los informes producidos por el Instituto del Mar del Per? (IMARPE). El paquete est? dise?ado para trabajar con funciones gen?ricas (e.g. \code{plot}, \code{summary}, \code{print}) sin importar el tipo de datos utilizado, facilitando su uso y permitiendo su utilizaci?n incluso con conocimientos m?nimos de \code{R}. 
}
\details{
The environment of the package works basically with five different classes of objects such as bitacoras, landing, length, composition and spatialDistribution.
}
\section{Additional Information}{
\tabular{ll}{
Package: \tab imarpe\cr
Type: \tab Package\cr
Version: \tab 0.1\cr
Date: \tab 2014-03-11\cr
License: \tab TBD\cr
}
}
\author{
Criscely Lujan Paredes, Ricardo Oliveros-Ramos, Erick Chacon Montalvan, Vilma Romero Romero.

Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@imarpe.gob.pe}>
}
\references{
Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
}
\keyword{graphs}
\keyword{tables}
\keyword{reportes}
\keyword{R}
\concept{IMARPE}
%\keyword{figuras, tablas, reportes, imarpe, R. }
\seealso{
The following list shows the functions that can be used to analyze the different kinds of classes after reading the dataset from a file with \code{\link{getData}} function.
\subsection{Bitacoras}{\code{\link{print.bitacoras}},\code{\link{summary.bitacoras}}, \code{\link{plot.bitacoras}}}
\subsection{Landings}{\code{\link{print.landings}},\code{\link{summary.landings}}, \code{\link{plot.landings}}}
}
\examples{
## For bitacoras class
## Loading the data
data = getData("matrizPBP.csv", type="bitacoras")
## Plotting by effort
plot(data,type="effort")
## Plotting by depth
plot(data,type="depth")

## For landings class
## Loading the data
data = getData("desembarques_data3.csv", type="landings")
## Plotting by month
plot(data,time="month")
## Plotting by year
plot(data,time="year")
}
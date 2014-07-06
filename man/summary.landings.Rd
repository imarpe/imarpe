\name{summary.landings}
\alias{summary.landings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Class summary.landings
%%  ~~function to do ... ~~
}
\description{
Creates a new class called "summary.landings".
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
summary.landings(object, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{object}{An object of class "landings".
%%     ~~Describe \code{object} here~~
}
  \item{\dots}{Further arguments
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
Returns an object from class "summary.landings" containing the following features:
\item{info}{General overview of the landings database (number of records, months, years and ports).}
\item{sumPorts}{Dataframe containing the complete dataset of interest.}
\item{ports}{Dataframe indicating the number of landings by port.}
\item{months}{Dataframe indicating the number of landings by month.}
\item{years}{Dataframe indicating the number of landings by year.}
}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
}
\author{
Criscely Lujan Paredes, Ricardo Oliveros-Ramos, Erick Chacon Montalvan, Vilma Romero Romero.

Maintainer: Ricardo Oliveros-Ramos <roliveros@imarpe.gob.pe>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function (object, ...) 
%{
%    output = list()
%    output$info = object$info
%    output$sumPorts = .getSumPorts.landings(object)
%    output$ports = .getPorts.landings(object)
%    output$months = .getMonth.landings(object)
%    output$years = .getYear.landing(object)
%    class(output) = "summary.landings"
%    return(output)
%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

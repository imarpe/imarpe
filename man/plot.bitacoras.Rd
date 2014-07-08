\name{plot.bitacoras}
\alias{plot.bitacoras}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a Bitacoras Object
%%  ~~function to do ... ~~
}
\description{
The \code{plot} method for objects of class \dQuote{bitacoras}. This function plots two kind of graphs, which are defined by \dQuote{type} argument.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{plot}{bitacoras}(x, type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object of class "bitacoras".
%%     ~~Describe \code{x} here~~
}
  \item{type}{ Type of graph to plot. Possible types are:
  \itemize{
  \item \dQuote{effort} for plotting a barplot of average travel time with a line of average coves by port.
  \item \dQuote{depth} for plotting a boxplot of depths by ranges of latitudes.
  }
%%     ~~Describe \code{type} here~~
}
  \item{\dots}{Other graphical parameters accepted by \code{\link{plot()}}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
In \dQuote{effort} graph, plotted statistics like travel time, number of coves are average data of the gathered profiles by port.

%%  ~~ If necessary, more details than the description above ~~
}
\value{None (invisible NULL).
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\author{
Criscely Lujan Paredes, Ricardo Oliveros-Ramos, Erick Chacon Montalvan, Vilma Romero Romero.

Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@imarpe.gob.pe}>
}

\references{
Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.

%Introducci?n al paquete \code{imarpe} para la automatizaci?n de gr?ficos, tablas y reportes usando \code{R}.
}

\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getData}}, \code{\link{print.bitacoras}}, \code{\link{summary.bitacoras}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Loading the data
data = getData("matrizPBP.csv", type="bitacoras")

## Plotting by effort
plot(data,type="effort")

## Plotting by depth
plot(data,type="depth")
}
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function (x, type, ...) 
%{
%    switch(type, effort = .plotEffort.bitacoras(x = x, ...), 
%        depth = .plotDepth.bitacoras(object = x, ...), error("Plot type not defined."))
%    return(invisible())
%  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

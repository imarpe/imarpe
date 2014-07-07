\name{plot.landings}
\alias{plot.landings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot a landings object
%%  ~~function to do ... ~~
}
\description{
Plot method for objects of class \dQuote{landings}.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{method}{landings}(x, time = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Object of class "landings".
%%     ~~Describe \code{x} here~~
}
%  \item{y}{
%%     ~~Describe \code{y} here~~
%}
  \item{time}{
Required period of time. Possible types are
  \itemize{
  \item "day" for plotting by days,
  \item "month" for plotting by months,
  \item "year" for plotting by years.
  }
%%     ~~Describe \code{time} here~~
}
  \item{\dots}{Other graphical parameters accepted by \code{\link{plot()}}.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
## Loading the data
data <- getData("desembarques_data3.csv", type="landings")

## Plotting by month
plot(data,time="month")

## Plotting by year
plot(data,time="year")

%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function (x, y = NULL, time = NULL, ...) 
%{
%    if (!is.null(y) & is.null(time)) 
%        time = y
%    if (is.null(y) & is.null(time)) 
%        time = "day"
%    switch(time, day = .plotDays.landings(x = x, ...), month = .plotMonths.landings(x = x, 
%        ...), year = .plotYears.landings(x = x, ...), )
%    return(invisible())
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

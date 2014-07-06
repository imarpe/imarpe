\name{print.landings}
\alias{print.landings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print Landings General Description
}
\description{
Prints the main features of the landings database such as the number of records, months, years and ports, and returns them invisibly (via \code{\link{invisible}}).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.landings(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of the class landings.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{Further arguments
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

## Printing
print.landings(data)

%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function (x, ...) 
%{
%    cat("Landings data from", sQuote(x$info$file), "\n", sep = "")
%    cat("Number of records:", x$info$records, "\n", sep = "")
%    cat("Number of months of data:", x$info$months, "\n", sep = "")
%    cat("Number of years of data:", x$info$years, "\n", sep = "")
%    cat("Number of ports:", x$info$ports, "\n", sep = "")
%    return(invisible())
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

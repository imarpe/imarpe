\name{print.summary.landings}
\alias{print.summary.landings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print General Description of Summary.landings Objects
%%  ~~function to do ... ~~
}
\description{
Prints summary information about a \dQuote{landings} object, which includes  the metadata, landings by ports, monthly landings and annual landings.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{print}{summary.landings}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \dQuote{summary.landings}.
%%     ~~Describe \code{x} here~~
}
%  \item{\dots}{Further arguments
%%     ~~Describe \code{\dots} here~~
%}
}

\details{
%%  ~~ If necessary, more details than the description above ~~
  \tabular{ll}{
    \code{info} \tab General overview of the landings database (number of records, months, years and ports). \cr
    \code{ports} \tab Table indicating the number of landings by port.\cr
    \code{months} \tab Table indicating the number of landings by month.\cr
    \code{years} \tab Table indicating the number of landings by year.
  }
}

\value{
None (invisible NULL).
}
\references{
Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
}
\author{
Criscely Lujan Paredes, Ricardo Oliveros-Ramos, Erick Chacon Montalvan, Vilma Romero Romero.

Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@imarpe.gob.pe}>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getData}}, \code{\link{summary.landings}}, \code{\link{print.landings}}, \code{\link{plot.landings}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Loading the data
data = getData("desembarques_data3.csv", type="landings")

## Printing the summary
summary(data)
}
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
%    cat("\nLandings by ports (non-zero only):\n\n")
%    print(x$ports[x$ports$Landings > 0, , drop = FALSE])
%    cat("\nMonthly landing:\n\n")
%    print(t(x$months))
%    cat("\nAnnual landing:\n\n")
%    print(x$years)
%    return(invisible(x))
%  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

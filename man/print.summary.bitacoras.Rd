\name{print.summary.bitacoras}
\alias{print.summary.bitacoras}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print General Description of Summary.bitacoras Objects
%%  ~~function to do ... ~~
}
\description{
Prints summary information about a \dQuote{bitacoras} object, which includes  the metadata, captures by specie, travel duration and number of coves, observers by port, coves by latitude and depth by latitude.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{print}{summary.bitacoras}(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class \dQuote{summary.bitacoras}.
%%     ~~Describe \code{x} here~~
}
%  \item{\dots}{Further arguments
%%     ~~Describe \code{\dots} here~~
%}
}
\details{
  \tabular{ll}{
    \code{info} \tab General overview of the bitacoras database (number of records, observers, ports and involved years). \cr
    \code{Species composition} \tab Table indicating the number and percentage of captures by fish species.\cr
    \code{Sampling effort} \tab Table indicating the travel duration and the average number of coves by port.\cr
    \code{Number of observers} \tab Table indicating the number of observers by port.\cr
    \code{Number of sets by latitude} \tab Table indicating the number of coves by latitude.\cr
    \code{Depth by latitude} \tab Table indicating the upper limit, lower limit and average depth by latitude.
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

Maintainer: Ricardo Oliveros-Ramos <roliveros@imarpe.gob.pe>
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getData}}, \code{\link{summary.bitacoras}}, \code{\link{print.bitacoras}, \code{\link{plot.bitacoras}
}}}
\examples{
## Reading a database of the 'bitacoras' class 
data <- getData("matrizPBP.csv", type="bitacoras")

## Printing the summary
summary(data)

%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function (x, ...) 
%{
%    cat("Bitacoras data from ", sQuote(x$info$file), "\n", sep = "")
%    cat("Number of records: ", x$info$records, "\n", sep = "")
%    cat("Number of observers: ", x$info$observers, "\n", sep = "")
%    cat("Ports: ", x$info$ports, "\n", sep = "")
%    cat("Years: ", x$info$years, "\n", sep = "")
%    cat("\nSpecies composition:\n\n")
%    print(x$composition)
%    cat("\nSampling effort:\n\n")
%    print(x$effort)
%    cat("\nNumber of observers:\n\n")
%    print(x$observer)
%    cat("\nNumber of sets by latitudinal degree:\n\n")
%    print(t(x$set))
%    cat("\nDepth by latitudinal degree:\n\n")
%    print(t(x$depth))
%    return(invisible(x))
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

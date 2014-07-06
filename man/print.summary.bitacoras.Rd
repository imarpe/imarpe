\name{print.summary.bitacoras}
\alias{print.summary.bitacoras}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
print.summary.bitacoras
%%  ~~function to do ... ~~
}
\description{
Shows a summary for objects of class "bitacoras".
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
print.summary.bitacoras(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of class "bitacoras".
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
\item{info}{General overview of the bitacoras database (number of records, observers, ports and the unique years).}
\item{composition}{Dataframe indicating the number of captures by fish species.}
\item{effort}{Dataframe indicating the travel duration and the number of coves by port.}
\item{observer}{Dataframe indicating the number of observers by port.}
\item{set}{Dataframe indicating the number of coves by latitude.}
\item{depth}{Dataframe indicating the upper limit, lower limit and average depth by latitude.}
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

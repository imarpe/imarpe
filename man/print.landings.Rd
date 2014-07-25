\name{print.landings}
\alias{print.landings}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Print General Description of Landings Objects
}
\description{
Prints the main features of the \dQuote{landings} database such as the number of records, months, years and ports, and returns them invisibly (via \code{\link{invisible}}).
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
\method{print}{landings}(x,\dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{An object of the class \dQuote{landings}.
%%     ~~Describe \code{x} here~~
}
  \item{\dots}{Further arguments accepted by \code{\link{print}}
%%     ~~Describe \code{\dots} here~~
}
}
\details{It is not useful to print all the content of large databases as \dQuote{landings} objects. For this reason, this function is useful to print only metadata of \dQuote{landings} objects.
%%  ~~ If necessary, more details than the description above ~~
}
\value{None (invisible NULL).
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

Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@imarpe.gob.pe}>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{getData}}, \code{\link{plot.landings}}, \code{\link{summary.landings}}
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## Loading the database of the class 'landings'
data(Landings)

## Verifying the class of data
class(Landings)

## S3 method for class 'landings'
print(Landings)
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
%    return(invisible())
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

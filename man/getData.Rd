\name{getData}
\alias{getData}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data Input
%%  ~~function to do ... ~~
}
\description{Reads a file and creates a database of the desired type, which includes a data frame and a list containing the main features of the data frame. The included data frame object has observations corresponding to rows and variables to columns.   
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getData(file, type, \dots)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The name of the file from which the data is read. Each row of the table appears as one line of the file. If it does not contain an absolute path, the file name is relative to the current working directory, \code{\link{getwd()}}.
%%     ~~Describe \code{file} here~~
}
  \item{type}{Type of the database to be read. The following types are accepted: \dQuote{bitacoras},\dQuote{landings}. 
%%     ~~Describe \code{type} here~~
}
  \item{\dots}{Further arguments allowed by \code{\link{read.csv()}} function.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
This function is the principal function of reading data into R for the \pkg{imarpe} package.
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{data}{A data frame containing a representation of the data in the file.}
\item{info}{A list containing the main features of the data.}
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
\note{If type is not explicitly defined, the function will be equivalent to use \code{\link{read.csv()}}
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
After reading a database with \code{\link{getData}} function, you can use other functions to summary or plot the obtained data using functions like \code{\link{plot.bitacoras}}, \code{\link{plot.landings}}, \code{\link{summary.bitacoras}}, \code{\link{summary.landings}}.
}
\examples{
# Reading a database of the 'landings' class.
data = getData("desembarques_data3.csv", type="landings")

# Reading a database of the 'bitacoras' class with encoding of windows computer.
data = getData("matrizPBP.csv", type="bitacoras",fileEncoding="iso-8859-1")

# Reading a database without an specific class.
data = getData("matrizPBP.csv")
%##---- Should be DIRECTLY executable !! ----
%##-- ==>  Define data, use random,
%##--	or do  help(data=index)  for the standard data sets.

%## The function is currently defined as
%function (file, type, ...) 
%{
%    output = switch(type, bitacoras = .getBitacorasData(file = file, 
%        ...), landings = .getLandingsData(file = file, ...), 
%        read.csv(file = file, ...))
%    return(output)
%  }
%}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

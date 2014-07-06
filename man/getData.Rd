\name{getData}
\alias{getData}
\docType{data}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Data Input
%%  ~~function to do ... ~~
}
\description{Reads a file and creates a data frame from it with observations corresponding to lines and variables to fields in the file, and a list containing the main features of the data frame.   
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getData(file, type, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{The name of the file from which the data is read.
%%     ~~Describe \code{file} here~~
}
  \item{type}{Type of the database to be read.  
%%     ~~Describe \code{type} here~~
}
  \item{\dots}{Further arguments.
%%     ~~Describe \code{\dots} here~~
}
}
\details{
This function is the principal main of reading data into R for the \pkg{imarpe} package.
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
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
data <- getData("desembarques_data3.csv", type="landings")

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

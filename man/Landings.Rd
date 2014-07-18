\name{Landings}
\alias{Landings}
\docType{data}
\title{Landings Data
%%   ~~ data name/kind ... ~~
}
\description{ This data set gives the numbers of landings at different peruvian ports from january 2013 to may 2014.  
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data(Landings)}
\format{
An object of the \code{\link{class}} \dQuote{landings}, which is list with 2 components:
  \describe{
    \item{\code{data}}{a dataframe containing 488 observations and 16 variables}
    \item{\code{info}}{a list containing the metadata}
  }
}
\details{
The first three variables refer to the year, month and day, while the rest variables refer to 13 peruvian ports.
%%  ~~ If necessary, more details than the __description__ above ~~
}
%\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
%}
\references{
Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
%%  ~~ possibly secondary sources and usages ~~
}
\seealso{
\code{\link{plot.landings}} some examples where it is used \code{Landings}.
}
\examples{
data(Landings)
%## maybe str(Landings) ; plot(Landings) ...
}
\keyword{datasets}

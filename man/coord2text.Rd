\name{coord2text}
\alias{coord2text}
\title{Convert Latitude and Longitude Coordinates to Character
}
\description{
Converts latitude and longitude coordinates, which are \dQuote{numeric} vectors or matrices, to character vectors. Mainly, it adds cardinal directions depending of the sign of the coordinates values and the kind of coordinate (Latitude or Longitude).
}
\usage{
coord2text(coord, type)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{coord}{ It is \dQuote{numeric} single value, vector or matrix that contain coordinates values.
}
  \item{type}{Kind of coordinate to be converted. Possible values are c(\dQuote{lat},\dQuote{lon}).
}
}
\details{
\itemize{
\item In \dQuote{lat} coordinate, negative values represent north (\strong{N}) direction and positive values represent south (\strong{S}) direction.
\item In \dQuote{lon} coordinate, negative values represent west (\strong{W}) direction and positive values represent east (\strong{E}) direction.
}
}
\value{
A vector character of the input coordinates.
}
\references{
Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
}
\author{
Criscely Lujan Paredes, Ricardo Oliveros-Ramos, Erick Chacon Montalvan, Vilma Romero Romero.

Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@imarpe.gob.pe}>
}
\note{The function accepts any numeric value as input, but you should take into consideration that \dQuote{lat} coordinate goes from -90 to 90, while \dQuote{lon} coordinate goes from -180 to 180.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
## Defining a vector of latitude coordinate
Coord = c(-6,-8,-10,-15,-16,-17)

## Converting to text
CoordText = coord2text(Coord,'lat')

## Printing CoordText
CoordText
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line

# auxiliary functions
#' Convert Latitude and Longitude Coordinates to Character
#' @description 
#' Converts latitude and longitude coordinates, which are \dQuote{numeric}
#' vectors or matrices, to character vectors. Mainly, it adds cardinal directions
#' depending of the sign of the coordinates values and the kind of coordinate (Latitude or Longitude).
#' @param coord It is \dQuote{numeric} single value, vector or matrix that contain coordinates values.
#' @param type Kind of coordinate to be converted. Possible values are c(\dQuote{lat},\dQuote{lon}).
#' @details
#' \itemize{
#' \item In \dQuote{lat} coordinate, negative values represent north (\strong{N}) direction and positive values represent south (\strong{S}) direction.
#' \item In \dQuote{lon} coordinate, negative values represent west (\strong{W}) direction and positive values represent east (\strong{E}) direction.
#' }
#' @return A vector character of the input coordinates.
#' @note The function accepts any numeric value as input, but you should take into consideration that “lat” 
#' coordinate goes from -90 to 90, while “lon” coordinate goes from -180 to 180.
#' @author Erick Chacon Montalvan, Vilma Romero Romero, Criscely Lujan Paredes, Ricardo Oliveros-Ramos.
#' 
#' Maintainer: Ricardo Oliveros-Ramos <\email{roliveros@@imarpe.gob.pe}>
#' @references Lujan Paredes C., Oliveros-Ramos R., Chacon Montalvan E., Romero Romero V., 2014. 
#' Introduction to \pkg{imarpe} package for the automation of graphs, charts and reports using \code{R}.
#' @examples
#' ## Defining a vector of latitude coordinate
#' Coord = c(-6,-8,-10,-15,-16,-17)
#' 
#' ## Converting to text
#' CoordText = coord2text(Coord,'lat')
#' 
#' ## Printing CoordText
#' CoordText
coord2text = function(coord, type) {
  if(!is.character(type)) type=deparse(substitute(type))
  out = sapply(coord, FUN=.coord2text, type=type)
  return(out)
}


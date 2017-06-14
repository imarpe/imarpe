#' @import reshape2
#' @import plyr
#' @import rmarkdown
#' @import maps
#' @import fields
#' @import sp
#' @importFrom plotrix pie3D
#' @importFrom Hmisc monthDays yearDays

#' @title imarpe: R package for the the automation of graphs, tables and reports of the Instituto del Mar del Peru
#' @author Criscely Lujan-Paredes \email{criscelylujan@gmail.com}, Luis Lau-Medrano \email{luis.laum@gmail.com}.
#' @name imarpe
#' @description This packages provides tools for the elaboration of graphs and tables that are
#' routinely performed in reports produced by Instituto del Mar del Peru. The package imarpe
#' is designed to work with generic functions regardless of the type of data used.
#' @docType package
#' @keywords automatization, reports, fishery, imarpe.
NULL

#' @title Species catch in the database of PBP.
#' @name catchSpecies
#' @description Data with a list of species and taxonomic groups finding on bitacoras catches.
#' @aliases catchSpecies
#' @docType data
#' @usage catchSpecies
#' @format A \code{data.frame} with three columns of: name of spcies catch, taxonomic group, and the common species name.
#' @references imarpe package vignette.
NULL

#' @title Isoparalitoral area.
#' @name AIPShapefile
#' @description The peruvian shape isoparalitoral area.
#' @aliases AIPShapefile
#' @docType data
#' @usage AIPShapefile
#' @format A \code{shapefile} with the information of the isoparalitoral area: longitude, latitude, area and perimeter.
#' @references imarpe package vignette.
NULL

#' @title Colours for the maps.
#' @name imarpeColours
#' @description Vector of colours to use in the package.
#' @aliases imarpeColours
#' @docType data
#' @usage imarpeColours
#' @references imarpe package vignette.
NULL

#' @title Abbreviations of the months.
#' @name month.abb_spanish
#' @description Vector with the months names abbreviations on spanish.
#' @aliases month.abb_spanish
#' @docType data
#' @usage month.abb_spanish
#' @format A \code{vector} containing the abbreviations of the months of the year.
#' @references imarpe package vignette.
NULL

#' @title Peruvian port data.
#' @name portData
#' @description Data with a list of ports around peruvian coast.
#' @aliases portData
#' @docType data
#' @usage portData
#' @format A \code{data.frame} with six columns: the name of the port, the pattern to find the name
#'  of the port, the longitude and latitude of their names, the area (north, south and central) of
#'   ports and the importance of the port according the history of the fishery.
#' @references imarpe package vignette.
NULL

#' @title Peruvian port data.
#' @name seasonAnchovyNC
#' @description Data with a list of ports around peruvian coast.
#' @aliases seasonAnchovyNC
#' @docType data
#' @usage seasonAnchovyNC
#' @format A \code{data.frame} with six columns: the name of the port, the pattern to find the name
#'  of the port, the longitude and latitude of their names, the area (north, south and central) of
#'   ports and the importance of the port according the history of the fishery.
#' @references imarpe package vignette.
NULL

#' @title Peruvian port data.
#' @name seasonAnchovyS
#' @description Data with a list of ports around peruvian coast.
#' @aliases seasonAnchovyS
#' @docType data
#' @usage seasonAnchovyS
#' @format A \code{data.frame} with six columns: the name of the port, the pattern to find the name
#'  of the port, the longitude and latitude of their names, the area (north, south and central) of
#'   ports and the importance of the port according the history of the fishery.
#' @references imarpe package vignette.
NULL


#' @title Obtener blabla
#'
#' @param file
#' @param type
#' @param varType
#' @param toTons
#' @param sp
#' @param start
#' @param end
#' @param port
#' @param efforType
#' @param fleeType
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
getFishingData =  function(file, type, varType, toTons=TRUE, sp, start=NULL, end = NULL, port = NULL,
                           efforType = "viaje", fleeType=NULL, ...){

  dataBase = .convertBase(file=file, sp=sp, fleeType=fleeType, efforType=efforType)
  fleet    = .fleetData(file = file, varType = varType, toTons = toTons, sp = sp, efforType=efforType,
                        fleeType = fleeType, start = start, end = end, port = port, ...)

  output = switch(tolower(type),
                  fisheryinfo = .getFisheryData(x=dataBase, fileName = file, fleet = fleet, varType=varType,
                                                toTons = toTons, sp=sp, start=start, end=end, port=port, efforType = efforType),
                  cpue        = .getCPUEData(x=dataBase, fileName = file, fleet = fleet, varType=varType,
                                             toTons = toTons, sp=sp, start=start, end=end, port=port, efforType = efforType),
                  read.csv(file = dataBase, stringsAsFactors = FALSE, ...))

  return(output)
}

#' Title
#'
#' @param file
#' @param colTrip
#' @param colPort
#' @param colDateStart
#' @param colDateOut
#' @param colSearchTime
#' @param colStorageCapacity
#' @param colLat
#' @param colLon
#' @param colHaul
#' @param colHaulTotal
#' @param colCatchHaul
#' @param capAnch
#' @param capSar
#' @param capJur
#' @param capCab
#' @param capBon
#'
#' @return
#' @export
#'
#' @examples
getBitacoraData = function(file, colTrip = "CODIGO_VIAJE", colPort = "PUERTO_SALIDA",
                           colDateStart = "DIA_SALIDA", colDateOut = "DIA_ARRIBO",
                           colSearchTime = "DURACION_BUSQUEDA", colStorageCapacity = "CAPACIDAD_BODEGA_REGISTRADA",
                           colLat = "LATITUD_INICIAL", colLon = "LONGITUD_INICIAL",
                           colHaul = "NUMERO_CALA", colHaulTotal = "TOTAL_CALAS", colCatchHaul = "CAPTURA_CALA",
                           capAnch = "CAPTURA_ANCHOVETA", capSar = "CAPTURA_SARDINA",
                           capJur = "CAPTURA_JUREL", capCab = "CAPTURA_CABALLA", capBon = "CAPTURA_BONITO"){

  dataBase = .getBitacoraData(file = file, colTrip = colTrip, colPort = colPort,
                              colDateOut = colDateOut, colDateStart = colDateStart, colSearchTime = colSearchTime,
                              colStorageCapacity = colStorageCapacity, colLat = colLat, colLon = colLon,
                              colHaul = colHaul, colHaulTotal = colHaulTotal, colCatchHaul = colCatchHaul,
                              capAnch = capAnch, capSar = capSar, capJur = capJur, capCab = capCab, capBon = capBon)
  return(dataBase)
}

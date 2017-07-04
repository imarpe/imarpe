#' @import reshape2
#' @import plyr
#' @import rmarkdown
#' @import maps
#' @import fields
#' @import sp
#' @importFrom plotrix pie3D
#' @importFrom Hmisc monthDays yearDays
#' @importFrom RCurl url.exists
#' @importFrom XLConnect loadWorkbook readWorksheet

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


#' @title Colours for the package.
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


#' @title Anchovy season for the north-central region.
#' @name seasonAnchovyNC
#' @description Historical records of the opening and closing of anchovy fishing
#' seasons in the north-central region of the Peruvian coast.
#' @aliases seasonAnchovyNC
#' @docType data
#' @usage seasonAnchovyNC
#' @format A \code{data.frame} with four columns: year, the start date of the season for the
#' year, the end date of the season, and a code for the season.
#' @references imarpe package vignette.
NULL


#' @title Anchovy season for the south region.
#' @name seasonAnchovyS
#' @description Historical records of the opening and closing of anchovy fishing
#' seasons in the south region of the Peruvian coast.
#' @aliases seasonAnchovyS
#' @docType data
#' @usage seasonAnchovyS
#' @format A \code{data.frame} with four columns: year, the start date of the season for the
#' year, the end date of the season, and a code for the season.
#' @references imarpe package vignette.
NULL


#' @title Lista de puertos
#' @name harborData
NULL


#' @title Lista de especies
#' @name species
NULL


#' @title Get data from fishery class
#'
#' @description This function read the fishing information (landings, effort and catch per unit effor) to
#'  create a data base which includes a data frame and a list with the main features of the data frame.
#'  The included data frame object has observations corresponding to rows and variables to columns.
#'
#' @param file The name of the file with fishery data. If it does not contain an absolute path,
#'  the file name is relative to the current working directory, \code{\link{getwd}}.
#' @param type The type of the information: fisheryInfo with two variables (landing and fishing effort) and
#' the cpue (catch per unith effort).
#' @param varType The type of the variable that has been to analyze. This can be: lading, effort and cpue.
#' @param toTons A logical parameter. \code{TRUE} (dafault) it assume that the fishery information is in kilograms
#' and converts it into tonnes (divided by 1000). \code{FALSE} it assume the the information is in tons and don't convert the data.
#' @param sp The name of the species.
#' @param start The date to start the analysis. By dafault is \code{NULL}.
#' @param end The date to end the analysis. By dafault is \code{NULL}.
#' @param port The specific port to analyze. By dafault is \code{NULL} and analyze all ports.
#' @param efforType If the varType is effort or cpue (catch per unit effort) this parameter indicate what
#' type of effort will be used.
#' \itemize{
#'   \item "viajes" for the travels number.
#'   \item "capacidad_bodega" for storage capacity.
#'   \item "anzuelos" for the hook number.
#'   \item "embarcaciones" for the number of boats.
#' }
#' @param fleeType The fleet type to analyze. By default is \code{NULL}.
#' @param ... Further arguments passed to \code{.fleetData} function.
#'
#' @return
#' \tabular{ll}{
#' \code{data} \tab A data frame containing a representation of the data in the file.\cr
#' \code{info} \tab A list containing the main features of the data. \cr
#' \code{fleeTable} \tab A data frame with the fishery information by years and months by
#' each type of fleet.
#' }
#' @note If type is not explicitly defined, the function will be equivalent to use \code{\link{read.csv}}.
#' @examples
#' # Read a data base of fishery class
#' file.landing = "FisheryData.csv"
#'
#' # For landing information assuming the analyzed species is 'anchovy', put the name of the species on \code{sp}:
#' landing  = getFishingData(file = file.landing, type = "fisheryinfo", varType = "landing", sp = "anchovy")
#'
#' # If you can analyzed a period of time you have to use \code{start} and \code{end} parameters:
#' landing  = getFishingData(file = file.name, type = "fisheryinfo", varType = "landing", sp = "anchovy",
#' start = "2009-04-10", end = "2009-08-30")
#'
#' # If you can analyzed a specific port put the name of the port on \code{port}:
#' landing  = getFishingData(file = file.name, type = "fisheryinfo", varType = "landing", sp = "anchovy",
#' start = "2009-04-10", end = "2009-08-30", port = "PAITA")
#'
#' # Check the class of the landing object, it would be 'fishery'.
#' class(landing)
#'
#' # To get the data of the landing object.
#' data = landing$data
#'
#' # To get the main features of the landing object.
#' info = landing$info
#'
#' # To get the data frame with the landings by each type of fleet.
#' fleet = landing$fleeTable
#'
#' @export
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


#' @title Get data from bitacora class
#'
#' @description This function read the information from the Programme of Fishery Logbooks (in spanish: Programa de
#' Bitacoras de Pesca - PBP) and create  a data base which includes a data frame and a list with the main features
#' of the data frame. The included data frame object has observations corresponding to rows and variables to columns.
#' @param file The name of the file with data of the bitacora class. If it does not contain an absolute path,
#'  the file name is relative to the current working directory, \code{\link{getwd}}.
#' @param colTrip Name of travel code column. By default is "CODIGO_VIAJE".
#' @param colPort Name of the port column. By default is "PUERTO_SALIDA".
#' @param colDateStart Name of data column with the start date of the travel. By default is "DIA_SALIDA".
#' @param colDateOut Name of data column with the end date of the travel. By default is "DIA_ARRIBO".
#' @param colSearchTime Name of the data column with the search time. By default is "DURACION_BUSQUEDA".
#' @param colStorageCapacity Name of the data column with the storage capacity. By default is "CAPACIDAD_BODEGA_REGISTRADA".
#' @param colLat Name of the data column with the latitude. By default is "LATITUD_INICIAL".
#' @param colLon Name of the data column with the longitude. By default is "LONGITUD_INICIAL".
#' @param colHaul Name of the data column with the fishing haul number. By default is "NUMERO_CALA".
#' @param colHaulTotal Name of the data column with the total number of fishing hauls by travel. By default is "TOTAL_CALAS".
#' @param colCatchHaul Name of the data column with the catch by fishing haul. By default is "CAPTURA_CALA".
#' @param capAnch Name of the data column with the anchovy catches. By default is "CAPTURA_ANCHOVETA".
#' @param capSar Name of the data column with the sardine catches. By default is "CAPTURA_SARDINA".
#' @param capJur Name of the data column with the jack mackerel catches. By default is "CAPTURA_JUREL".
#' @param capCab Name of the data column with the chub mackerel catches. By default is "CAPTURA_CABALLA".
#' @param capBon Name of the data column with the bonito catches. By default is "CAPTURA_BONITO".
#'
#' @return
#' \tabular{ll}{
#' \code{data} \tab A data frame containing a representation of the data in the file.\cr
#' \code{info} \tab A list containing the main features of the data.
#' }
#' @note If type is not explicitly defined, the function will be equivalent to use \code{\link{read.csv}}.
#' @examples
#' # Read a data base of bitacora class
#' file.name = "bitacoraData.csv"
#' dataBase  = getBitacoraData(file = file.name)
#'
#' # Check the class of the data
#' class(dataBase)
#'
#' # To get the data of the bitacora class
#' data = dataBase$data
#'
#' # To get the main features of the data
#' info = dataBase$info
#'
#' @export
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


#' @title Report method
#' @description This function built a report for each class including on imarpe package.
#' @param object Object of class \code{fishery}, \code{cpue} and \code{bitacora}.
#' @param format The format to export the report.
#' @param output The report.
#' @param ... Extra arguments passed to \code{\link{report}} function.
#' @return A report on specific format.
#' @export
report = function(object, format, output, ...) {
  UseMethod("report")
}


#' @title Get main results from bitacora
#' @description Principal function of bitacora class objects to get principal results.
#' @param object Object of \code{bitacora} class.
#' @param observedTrip Parameter to indicate whether the observed trip will be estimated.
#' By default is \code{NULL} but if it will be estimated receive the logical value of \code{TRUE}.
#' @param fishingHaul Parameter to indicate whether the fishing haul sampled will be
#' estimated. By default is \code{NULL} but if it will be estimated receive the
#' logical value of \code{TRUE}.
#' @param fishingPoints Parameter to indicate whether the fishing poins will be estimated.
#' By default is \code{NULL} but if it will be estimated received the logical value of \code{TRUE}.
#' @param speciesComposition Parameter to indicate whether the species composition will be
#' estimated. By default is \code{NULL} but if it will estimated received the logical value of \code{TRUE}.
#' @param distributionCatch Parameter to indicate whether the distribution of catches will be
#' estimated. By default is \code{NULL} but if it will estimated received the logical value of \code{TRUE}.
#' @param effortData Parameter to indicate whether the effort data will be
#' estimated. By default is \code{NULL} but if it will estimated received the logical value of \code{TRUE}.
#' @param language The select language to print the results. By default is "spanish".
#' @param latByPort \code{logical}. Parameter of fishingHaul.bitacora function. By
#' default is \code{FALSE} indicating that latitude information is not get from
#' port information. When is \code{TRUE}, the latitude is get from port information.
#' @param specie \code{character}. Parameter of distributionCatch.bitacora function.
#' Receives the name of the species that is calculated the distribution of the catch.
#' Default value is "anchoveta" but this could be:
#' \itemize{
#'   \item "anchoveta" to estimated the anchovy catches distribution.
#'   \item "sardina" to estimated the sardine catches distribution.
#'   \item "jurel" to estimated the jack mackerel catches distribution.
#'   \item "caballa" to estimated the chub mackerel catches distribution.
#'   \item "bonito" to estimated the bonito catches distribution.
#' }
#' @return A list of \code{bitacora_mainResults} class. The \code{length} of the list is six,
#' one by each parameter (observedTrip, fishingHaul, fishingPoints, speciesComposition,
#' distributionCatch, and effortData).
#' @details If one of the parameter (observedTrip, fishingHaul, fishingPoints, speciesComposition,
#' distributionCatch, and effortData) is \code{NULL} on \code{getMainResults.bitacora} function
#' the output of this parameter on the list produced by the function is \code{NULL} too.
#' @export
getMainResults.bitacora = function(object, observedTrip = NULL, fishingHaul = NULL, fishingPoints = NULL,
                                   speciesComposition = NULL, distributionCatch = NULL, effortData = NULL,
                                   language = "spanish", latByPort = FALSE, specie = "anchoveta") {

  if(is.null(observedTrip) & is.null(fishingHaul) & is.null(fishingPoints) &
     is.null(speciesComposition) & is.null(distributionCatch) & is.null(effortData)) {
    message("There is not output to return")
    return(invisible())
  }

  #Check the output to return
  if(isTRUE(observedTrip)) {observedTrip = .observedTrip.bitacora(object, language) }

  if(isTRUE(fishingHaul)) {fishingHaul = .fishingHaul.bitacora(object, language, latByPort)}

  if(isTRUE(fishingPoints)) {fishingPoints = .fishingPoints.bitacora(object)}

  if(isTRUE(speciesComposition)) {speciesComposition = .speciesComposition.bitacora(object, language)}

  if(isTRUE(distributionCatch)) {distributionCatch = .distributionCatch.bitacora(object, language, specie)}

  if(isTRUE(effortData)) {effortData = .effortData.bitacora(object) }

  output = list(observedTrip       = observedTrip,
                fishingHaul        = fishingHaul,
                fishingPoints      = fishingPoints,
                speciesComposition = speciesComposition,
                distributionCatch  = distributionCatch,
                effortData         = effortData)

  class(output) = "bitacora_mainResults"

  return(output)
}


#' @title PlotFishignPoints method
#' @description Method for plotFishingPoins.bitacora function.
#' @param x Object of \code{bitacora} class.
#' @param ... Extra arguments passed to \code{plotFishingPoints.bitacora} function.
#' @details For more details read the help of \code{\link{plotFishingPoints.bitacora}}.
#' @export
plotFishingPoints = function(x, laguage, dataType, ...) {
  UseMethod(generic = "plotFishingPoints", object = x)
}


#' @title PlotFishingPresence method
#' @description Method for plotFishingPresence.bitacora function.
#' @param x Object of \code{bitacora} class.
#' @param ... Extra arguments passed to \code{plotFishingPresence.bitacora} function.
#' @details For more details read the help of \code{\link{plotFishingPresence.bitacora}}.
#' @export
plotFishingPresence = function(x, ...) {
  UseMethod(generic = "plotFishingPresence", object = x)
}


#' @title PlotSpeciesComposition method
#' @description Method for plotSpeciesComposition.bitacora function.
#' @param x Object of \code{bitacora} class.
#' @param ... Extra arguments passed to \code{plotSpeciesComposition.bitacora} function.
#' @details For more details read the help of \code{\link{plotSpeciesComposition.bitacora}}.
#' @export
plotSpeciesComposition = function(x, ...) {
  UseMethod(generic = "plotSpeciesComposition", object = x)
}


#' @title PlotEffort method
#' @description Method for plotEffort.numeric function.
#' @param x Object of \code{bitacora} class.
#' @param ... Extra arguments passed to \code{plotEffort.numeric} function.
#' @details For more details read the help of \code{\link{plotEffort.numeric}}.
#' @export
plotEffort = function(effort1, effort2, ...) {
  UseMethod(generic = "plotEffort", object = c(effort1, effort2))
}


#' Title
#'
#' @param directorio
#' @param URLporcentas
#' @param fechaInicio
#' @param fechaFinal
#' @param fechaInicio_exploratoria
#' @param fechaFinal_exploratoria
#' @param axisParamsCuota
#' @param tallasSimple
#' @param datosCrucero
#' @param umbral
#' @param especie
#' @param a
#' @param b
#' @param officialBiomass
#' @param k
#' @param Linf
#' @param sizeM
#' @param vectorM
#' @param catchFactor
#'
#' @return
#' @export
getDailyReport = function(directorio, URLporcentas, fechaInicio, fechaFinal,
                          fechaInicio_exploratoria, fechaFinal_exploratoria, axisParamsCuota,
                          tallasSimple, datosCrucero, umbral, especie, a, b,
                          officialBiomass, k, Linf, sizeM, vectorM, catchFactor){

  # COMPILAR PORCENTAS
  cat("\n-------COMPILING DAILY REPORTS-------\n")

  if(is.null(directorio) || !dir.exists(directorio)){
    directorio <- tempdir()
    dir.create(path = directorio, showWarnings = FALSE)
  }

  # Downloading daily reports
  # descarga los porcenta
  DownloadPorcenta(directorio = directorio, dirUrl = URLporcentas, inicio = fechaInicio, fin = fechaFinal)

  # Leer porcentaes
  porcentasSalida <- ReadPorcenta(directorio = directorio, inicio = fechaInicio, fin = fechaFinal)

  # Escribir tabla compilada
  porcentasArchivo <- paste0("data/desembarque_UpTo", format(as.Date(fechaFinal, "%Y-%m-%d"), "%d%m%Y"),".csv")
  write.csv(x = porcentasSalida$desembarque, file = porcentasArchivo, row.names = FALSE)


  # PONDERAR DATOS DE FRECUENCIAS SIMPLES Y DESEMBARQUES
  cat("\n-------WEIGHTING SIMPLE FREQUENCY DATA AND LANDINGS-------\n")

  # Leer frecuencias simples
  datosPonderacion <- leerData(muestreo = tallasSimple, desembarque = porcentasArchivo)

  # Hacer ponderaciones
  surveyData <- get(load(datosCrucero))

  # Si el objeto proviene de TBE, obtener valores de a y b
  if(!is.null(object$info$a_b)){
    a <- object$info$a_b$a
    b <- object$info$a_b$b
  }

  DatosPonderados <- LC_ponderada(data = datosPonderacion, tallas = seq(5, 20, 0.5), especie = especie,
                                  umbral = umbral, a = a, b = b)

  # Guardar datos ponderados
  ponderadosArchivo <- paste0("data/ponderados_UpTo", format(as.Date(fechaFinal, "%Y-%m-%d"), "%d%m%Y"),".csv")
  guardarPonderacion(data = DatosPonderados, filename = ponderadosArchivo)


  # GENERAR DATOS PARA REPORTE
  cat("\n-------GENERATE DATA FOR REPORTING-------\n")

  sp <- tolower(especie)
  allMarks <- seq(2, 20, 0.5)

  catchData <- readAtLength(file = ponderadosArchivo, sp = sp, check.names = FALSE)

  index <- as.Date(colnames(catchData))
  index <- index >= as.Date(fechaInicio) & index <= as.Date(fechaFinal)
  catchData <- catchData[,index]

  if(is.null(officialBiomass)){
    officialBiomass <- surveyData$results$nc$biomass$total
  }

  surveyVector <- as.numeric(surveyData$results$nc$biomass$length)
  surveyVector <- surveyVector/sum(surveyVector)*officialBiomass
  surveyVector <- surveyVector/(a*allMarks^b)

  allDates <- as.Date(colnames(catchData),
                      format = ifelse(grepl(pattern = "-", x = colnames(catchData)[1]), "%Y-%m-%d", "%d/%m/%Y"))


  # BY WEEK
  index <- c(1, grep(x = weekdays(allDates), pattern = "lunes|monday"), length(allDates) + 1)
  index <- index[!duplicated(index)]
  index <- do.call(c, mapply(rep, seq(length(index) - 1), diff(index)))

  catchVector <- t(aggregate(t(catchData), by = list(index), FUN = sum))[-1,]

  output <- as.matrix(surveyVector)
  for(i in seq(ncol(catchVector))){

    tempOutput <- projectPOPE(N = cbind(output[,i], output[,i]), catch = catchVector[,i]*catchFactor,
                              a = a, b = b, k = k, Linf = Linf, sizeM = sizeM, vectorM = vectorM,
                              freq = 52, sp = sp, Ts = 1)

    output <- cbind(output, tempOutput$N[2,])
  }

  colnames(output) <- c("Crucero", paste("Semana", seq(1, ncol(output) - 1)))
  colnames(catchVector) <- paste("Semana", 1:ncol(catchVector))

  outputByWeek <- cbind(allMarks, output)


  # BY DAY (LAST WEEK)
  index <- c(1, grep(x = weekdays(allDates), pattern = "lunes|monday"), length(allDates) + 1)
  index <- index[!duplicated(index)]
  index <- do.call(c, mapply(rep, seq(length(index) - 1), diff(index)))

  index <- index == max(index)

  catchVector <- as.matrix(catchData[,index])

  output <- as.matrix(outputByWeek[,ncol(outputByWeek) - 1])
  for(i in seq(sum(index))){

    tempOutput <- projectPOPE(N = cbind(output[,i], output[,i]), catch = catchVector[,i]*catchFactor,
                              a = a, b = b, k = k, Linf = Linf, sizeM = sizeM, vectorM = vectorM,
                              freq = 365, sp = sp, Ts = 1)

    output <- cbind(output, tempOutput$N[2,])
  }

  colnames(output) <- c(ifelse(ncol(outputByWeek) > 3, paste("Semana", ncol(outputByWeek) - 3), "Crucero"),
                        paste("Día", seq_len(ncol(output) - 1)))
  colnames(catchVector) <- paste("Día", 1:ncol(catchVector))

  ylimList <- list(c(0, 3e+05, 5e4), c(0, 20e3, 5e3), c(0, 3e+05, 5e4))
  ylimList2 <- list(c(0, 1e+06, 2e5), c(0, 5e5, 1e5), c(0, 1e+06, 2e5))

  outputByDay <- cbind(allMarks, output)
  outputByDay <- outputByDay[,-2]


  # BY DAY (all season)
  outputByDayAll <- as.matrix(surveyVector)
  for(i in seq(ncol(catchData))){

    tempOutput <- projectPOPE(N = cbind(outputByDayAll[,i], outputByDayAll[,i]), catch = catchData[,i],
                              a = a, b = b, k = k, Linf = Linf, sizeM = sizeM, vectorM = vectorM,
                              freq = 365, sp = sp, Ts = 1)

    outputByDayAll <- cbind(outputByDayAll, tempOutput$N[2,])
  }

  dimnames(outputByDayAll) <- list(allMarks, c("crucero", as.character(allDates)))

  updatedTo <- allDates[match(as.Date(fechaFinal, format = "%Y-%m-%d"), as.Date(colnames(catchData)))]

  directorioTrabajo <- getwd()

  archivoSalida <- "datosIntegrados.RData"
  save(directorioTrabajo, catchData, allMarks, a, b, updatedTo, surveyVector, outputByWeek, outputByDay,
       getInfo, ylimList, ylimList2, axisParamsCuota, outputByDayAll, fechaInicio, fechaFinal, fechaInicio_exploratoria,
       fechaFinal_exploratoria, fechaInicio_temporada, fechaFinal_temporada, allDates,
       file = archivoSalida)


  return(archivoSalida)
}

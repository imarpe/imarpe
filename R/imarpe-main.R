#' @title imarpe: R package for the the automation of graphs, tables and reports of the Instituto del Mar del Peru
#' @author Criscely Lujan-Paredes \email{criscelylujan@gmail.com}, Luis Lau-Medrano \email{luis.laum@gmail.com}.
#' @name imarpe
#' @description This packages provides tools for the elaboration of graphs and tables that are
#' routinely performed in reports produced by Instituto del Mar del Peru. The package imarpe
#' is designed to work with generic functions regardless of the type of data used.
#' @docType package
#' @keywords automatization, reports, fishery, imarpe.
NULL


#' @title Get data from fishery class
#'
#' @description This function read the fishing information (landings, effort and catch per unit effor) to
#'  create a data base which includes a data frame and a list with the main features of the data frame.
#'  The included data frame object has observations corresponding to rows and variables to columns.
#'
#' @param file The name of a file with a comma separated values format (extension of this files are .csv).
#'  This file have fishing information related to the landing and effort. If it does not contain an absolute path,
#'  the file name is relative to the current working directory, \code{\link{getwd}}.
#' @param type The type of the information: fisheryInfo with two variables (landing and fishing effort) and
#' the cpue (catch per unith effort).
#' @param varType The type of the variable that has been to analyze. This can be: lading, effort and cpue.
#' @param toTons A logical parameter. \code{TRUE} (dafault) it assume that the fishery information is
#' in kilograms and converts it into tonnes (divided by 1000). \code{FALSE} it assume the the information
#' is in tons and don't convert the data.
#' @param sp The name of the species.
#' @param start The date to start the analysis. By dafault is \code{NULL}.
#' @param end The date to end the analysis. By dafault is \code{NULL}.
#' @param port The specific port to analyze. By dafault is \code{NULL} and analyze all ports.
#' @param efforType If the varType is effort or cpue (catch per unit effort) this parameter indicate what
#' type of effort will be used.
#' \itemize{
#'   \item "viaje" for the travel number.
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
#' @author Criscely Lujan-Paredes, \email{criscelylujan@gmail.com}.
#' @examples
#' # Read a example of a data base
#' fisheryData = system.file("extdata", "fisheryData.csv", package = "imarpe")
#'
#' # For landing information assuming the analyzed species is 'caballa', put the name of the species on \code{sp}
#' landing  = getFishingData(file = fisheryData, type = "fisheryinfo", varType = "landing", sp = "caballa")
#'
#' # If you can analyzed a period of time you have to use \code{start} and \code{end} parameters
#' landing  = getFishingData(file = fisheryData, type = "fisheryinfo", varType = "landing", sp = "caballa",
#' start = "2009-04-10", end = "2009-08-30")
#'
#' # If you can analyzed a specific port put the name of the port on \code{port}
#' landing = getFishingData(file = fisheryData, type = "fisheryinfo", varType = "landing", sp = "caballa",
#'                          start = "2009-04-10", end = "2009-08-30", port = "PAITA")
#'
#' # Check the class of the landing object, it would be 'fishery'
#' class(landing)
#'
#' # To get the data of the landing object.
#' dataBase = landing$data
#'
#' # To get the main features of the landing object.
#' info = landing$info
#'
#' # To get the data frame with the landings by each type of fleet.
#' fleet = landing$fleeTable
#'
#'
#' # To analyze the effort information on the same data, use the parameter varType and
#' # specify the type of effort in the parameter efforType
#' effort = getFishingData(file = fisheryData, type = "fisheryinfo", varType = "effort",
#'                         sp = "caballa", efforType = "capacidad_bodega")
#'
#' # To analyze the cpue information on the same data change the parameter type, specify
#' the varType and specify the type of effort to calculate the catch per unit effort (cpue)
#' cpue = getFishingData(file = fisheryData, type = "cpue", varType = "cpue",
#'                       sp = "caballa", efforType = "capacidad_bodega")
#'
#' @export
getFishingData =  function(file, type, varType, toTons=TRUE, sp, start=NULL, end = NULL, port = NULL,
                           efforType = "viaje", fleeType=NULL, ...){
  
  dataBase = .convertBase(file=file, sp=sp, fleeType=fleeType, efforType=efforType)
  fleet    = .fleetData(file = file, varType = varType, toTons = toTons, sp = sp, efforType=efforType,
                        fleeType = fleeType, start = start, end = end, port = port, ...)
  
  output = switch(tolower(type),
                  fisheryinfo = .getFisheryData(x = dataBase, fileName = file, fleet = fleet, varType = varType,
                                                toTons = toTons, sp = sp, start = start, end = end, 
                                                port = port, efforType = efforType),
                  cpue        = .getCPUEData(x = dataBase, fileName = file, fleet = fleet, varType = varType,
                                             toTons = toTons, sp = sp, start = start, end = end, 
                                             port = port, efforType = efforType),
                  
                  read.csv(file = dataBase, stringsAsFactors = FALSE, ...))
  
  return(output)
}


#' @title Get data from bitacora class
#'
#' @description This function read the information from the Programme of Fishery Logbooks (in spanish: Programa de
#' Bitacoras de Pesca - PBP) and create a data base which includes a data frame and a list with the main features
#' of the data frame. The included data frame object has observations corresponding to rows and variables to columns.
#' @param file The name of the file. If it does not contain an absolute path, the file name is relative to
#' the current working directory, \code{\link{getwd}}.
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
#' @author Criscely Lujan-Paredes, \email{criscelylujan@gmail.com}.
#' @examples
#' # Read a example of data base
#' bitacoraData = system.file("extdata", "bitacoraData.csv", package = "imarpe")
#'
#' # Create a object of bitacora class
#' bitacoraObject = getBitacoraData(file = bitacoraData)
#'
#' # Check the class of bitacoraObject
#' class(bitacoraObject)
#'
#' # To get the data of bitacora class
#' dataBase = bitacoraObject$data
#'
#' # To get the main features of the data
#' info = bitacoraObject$info
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
#' @param output Folder where the report will be saved.
#' @param ... Extra arguments passed to \code{\link{report}} function.
#' @return A report on specific format.
#' @author Criscely Lujan-Paredes, \email{criscelylujan@gmail.com}.
#' @export
report = function(x, format, output, ...) {
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
#' @author Criscely Lujan-Paredes, \email{criscelylujan@gmail.com}.
#' @examples
#' # Read a example of data base
#' bitacoraData = system.file("extdata", "bitacoraData.csv", package = "imarpe")
#'
#' # Create a object of bitacora class
#' bitacoraObject = getBitacoraData(file = bitacoraData)
#' class(bitacoraObject)
#'
#' # Get the results
#' mainBitacoraData = getMainResults.bitacora(object = bitacoraObject, language = "spanish",
#' specie = "anchoveta", observedTrip = TRUE, fishingHaul = TRUE, distributionCatch = TRUE)
#'
#' # See the principal results bitacora class object
#' mainBitacoraData$observedTrip
#' mainBitacoraData$fishingHaul
#' mainBitacoraData$distributionCatch
#'
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
#' @author Criscely Lujan-Paredes, \email{criscelylujan@gmail.com}.
#' @export
plotFishingPoints = function(x, laguage, dataType, ...) {
  UseMethod(generic = "plotFishingPoints", object = x)
}


#' @title PlotFishingPresence method
#' @description Method for plotFishingPresence.bitacora function.
#' @param x Object of \code{bitacora} class.
#' @param ... Extra arguments passed to \code{plotFishingPresence.bitacora} function.
#' @details For more details read the help of \code{\link{plotFishingPresence.bitacora}}.
#' @author Criscely Lujan-Paredes, \email{criscelylujan@gmail.com}.
#' @export
plotFishingPresence = function(x, ...) {
  UseMethod(generic = "plotFishingPresence", object = x)
}


#' @title PlotSpeciesComposition method
#' @description Method for plotSpeciesComposition.bitacora function.
#' @param x Object of \code{bitacora} class.
#' @param ... Extra arguments passed to \code{plotSpeciesComposition.bitacora} function.
#' @details For more details read the help of \code{\link{plotSpeciesComposition.bitacora}}.
#' @author Criscely Lujan-Paredes, \email{criscelylujan@gmail.com}.
#' @export
plotSpeciesComposition = function(x, ...) {
  UseMethod(generic = "plotSpeciesComposition", object = x)
}


#' @title PlotEffort method
#' @description Method for plotEffort.numeric function.
#' @param x Object of \code{bitacora} class.
#' @param ... Extra arguments passed to \code{plotEffort.numeric} function.
#' @details For more details read the help of \code{\link{plotEffort.numeric}}.
#' @author Criscely Lujan-Paredes, \email{criscelylujan@gmail.com}.
#' @export
plotEffort = function(effort1, effort2, ...) {
  UseMethod(generic = "plotEffort", object = c(effort1, effort2))
}


#' @title Get daily report of fishing monitoring
#' @description This function download anchovy landing information from an official
#' repository of the IMARPE to compare this landings with the biomass estimated.
#'
#' @param directory Directory where the anchovy landing reports are stored. By default it
#' is \code{NULL}, temporarily saved and then deleted. If you want to keep this parameter
#' must be changed.
#' @param datesList A \code{list} of six dates (format: YEAR-month-day) with the names:
#'  \itemize{
#'   \item surveyDate: finish date of reference survey.
#'   \item startDate: start date to download the landing files.
#'   \item endDate: end date to download the landing files.
#'   \item endSeasonDate: end date of fishing season.
#' }
#' @param simpleFreqSizes A comma delimited file (.csv) with simple frequency data per size.
#' @param dataCruise A RData with the outputs of the cruise.
#' @param officialBiomass A official value of biomass.
#' @param addEnmalle \code{logical} which indicates whether to add or not (default) 'enmalle' influence to catches.
#' @param enmalleParams If \code{addEnmalle = TRUE}, a \code{list} with main parameters to describe 'enmalle':
#'  \itemize{
#'   \item mean: the meanfor 'enmallados' length.
#'   \item sd: the SD for 'enmallados' length.
#'   \item maxProportion: factor of 'enmallados'.
#' }
#' @param savePorcentas If \code{TRUE}, save download and save porcentas on a directory called '/porcentas_xls_files'. 
#' By default (\code{FALSE}) the function saves porcentas as tempory files.
#' @param urlFishingMonitoring The web address (url - Uniform Resource Locator) for downloading
#' the landings. By default it is \url{http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_rpelag_porfinal}.
#' @param threshold Threshold for considering the number of individuals. By default \code{threshold = 30}.
#' @param species The species that is going to be analyzed. By default is \code{species = "Anchoveta"}.
#' @param a Length-weight ratio parameter. By default, it is \code{NULL}, check Details.
#' @param b Length-weight ratio parameter. By default, it is \code{NULL}, check Details.
#' @param growthParameters A \code{list} of growthParamters. By default this are:
#' \itemize{
#'  \item k = 0.83
#'  \item Linf = 19.21
#'  \item sizeM = c(0, 8, 12)
#'  \item vectorM = rep(0.8, 3)
#'  \item catchFactor = 1
#'  \item scenario = "neutro"
#' }
#' 
#' 
#' @details Allometric growth parameters \code{a} and \code{b} are completely necesary for calculations, if they
#' are \code{NULL} (default), the users must be sure that the \code{dataCruise} file has them inside (that is true if 
#' \code{dataCruise} comes from \code{TBE} package).
#' 
#' @return A object of fishingMonitoring class. It is saved on the working directory.
#' @author Wencheng Lau-Medrano, \email{luis.laum@gmail.com}, Josymar Torrejon and Pablo Marin.
#' @export
getDailyReport <- function(directory = NULL, datesList, simpleFreqSizes, dataCruise, officialBiomass, 
                           addEnmalle = TRUE, enmalleParams = list(mean = 11, sd = 5.5, maxProportion = 0.20*0.05),
                           savePorcentas = FALSE, 
                           urlFishingMonitoring = "http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_rpelag_porfinal",
                           threshold = 30, species = "Anchoveta",
                           a = NULL, b = NULL, 
                           growthParameters = list(k = 0.83, Linf = 19.21, sizeM = c(0, 8, 12), vectorM = c(1.29, 0.92, 0.83), 
                                                   catchFactor = 1, scenario = "neutro")){
  
  # COMPILAR PORCENTAS
  cat("\n-------COMPILING DAILY REPORTS-------\n")
  
  if(is.null(directory) || !dir.exists(directory)){
    directory <- tempdir()
    dir.create(path = directory, showWarnings = FALSE)
  }
  
  datesList <- lapply(datesList, function(x) as.Date(x, format = ifelse(grepl(pattern = "-", x = x), "%Y-%m-%d", "%d/%m/%Y")))
  
  catchFactor <- ifelse(is.null(growthParameters$catchFactor), 1, growthParameters$catchFactor[1])
  if(!is.null(growthParameters$scenario)){
    
    growthParameters <- switch(growthParameters$scenario,
                               favorable    = list(k = 0.95, Linf = 19.98, t0 = -0.13, sizeM = c(0, 8, 12), vectorM = c(1.29, 0.92, 0.83)),
                               neutro       = list(k = 0.83, Linf = 19.21, t0 = -0.21, sizeM = c(0, 8, 12), vectorM = c(1.29, 0.92, 0.83)),
                               desfavorable = list(k = 0.64, Linf = 18.60, t0 = -0.30, sizeM = c(0, 8, 12), vectorM = c(1.28, 1.08, 1.00)),
                               paste("Invalid value for 'growthParameters$scenario'.", 
                                     "If you prefer to specify the Growth parameters, set growthParameters$scenario = NULL"))
  }
  growthParameters$catchFactor <- catchFactor
  
  # Downloading daily reports
  # descarga los porcenta
  if(isTRUE(savePorcentas)){
    dirPorcentas <- paste0(directory, "/porcentas_xls_files")
  }else{
    dirPorcentas <- tempdir()
  }
  
  dir.create(path = dirPorcentas, showWarnings = FALSE)
  
  DownloadPorcenta(directorio = dirPorcentas, dirUrl = urlFishingMonitoring, 
                   inicio = datesList$startDate, fin = datesList$endDate)
  
  # Leer porcentaes
  porcentasSalida <- ReadPorcenta(directorio = dirPorcentas, inicio = datesList$startDate, fin = datesList$endDate)
  
  # Escribir tabla compilada
  porcentasArchivo <- paste0(directory, "desembarque_UpTo", 
                             format(datesList$endDate, "%d%m%Y"),".csv")
  write.csv(x = porcentasSalida$desembarque, file = porcentasArchivo, row.names = FALSE)
  
  
  # PONDERAR DATOS DE FRECUENCIAS SIMPLES Y DESEMBARQUES
  cat("\n-------WEIGHTING SIMPLE FREQUENCY DATA AND LANDINGS-------\n")
  
  # Leer frecuencias simples
  datosPonderacion <- leerData(muestreo = simpleFreqSizes, desembarque = porcentasArchivo)
  
  # Hacer ponderaciones
  surveyData <- get(load(dataCruise))
  
  # Si el objeto proviene de TBE, obtener valores de a y b
  if(is.null(a) | is.null(b)){
    if(!is.null(object$info$a_b)){
      a <- object$info$a_b$a
      b <- object$info$a_b$b
    }else{
      stop("'a' and 'b' are missing.")
    }
  }
  
  # Get weighted data  
  DatosPonderados <- LC_ponderada(data = datosPonderacion, tallas = seq(5, 20, 0.5), especie = species,
                                  umbral = threshold, a = a, b = b)
  
  # Guardar datos ponderados
  ponderadosArchivo <- paste0("data/ponderados_UpTo", format(datesList$endDate, "%d%m%Y"),".csv")
  guardarPonderacion(data = DatosPonderados, filename = ponderadosArchivo)
  
  
  # GENERAR DATOS PARA REPORTE
  cat("\n-------GENERATE DATA FOR REPORTING-------\n")
  
  sp <- tolower(species)
  allMarks <- seq(2, 20, 0.5)
  
  # Read catch data
  catchData <- readAtLength(file = ponderadosArchivo, sp = sp, check.names = FALSE)
  rownames(catchData) <- allMarks
  
  # Create an empty matrix of catches
  allDates <- seq(from = datesList$startDate, to = datesList$endDate, by = "day")
  newCatch <- matrix(data = 0, nrow = length(allMarks), ncol = length(allDates), 
                     dimnames = list(allMarks, as.character(allDates)))
  
  # Replace read catchData over the pattern
  newCatch[,match(colnames(catchData), as.character(allDates))] <- catchData
  catchData <- newCatch
  
  # Weight to official biomass
  officialBiomass <- ifelse(is.null(officialBiomass), surveyData$results$nc$biomass$total, officialBiomass)
  surveyVector <- as.numeric(surveyData$results$nc$biomass$length)
  surveyVector <- surveyVector/sum(surveyVector)*officialBiomass
  surveyVector <- surveyVector/(a*allMarks^b)
  
  # Get Enmalle info
  if(isTRUE(addEnmalle)){
    # Get enmalle info
    enmalleInfo <- getEnmallamiento(imarsisData = datosPonderacion$baseMuestreo, enmalleParams = enmalleParams, 
                                    a = a, b = b)
    
    # Add enmalle info to catchData
    enmalleInfo <- aggregate(x = enmalleInfo$enmalleMatrix, by = list(rownames(enmalleInfo$enmalleMatrix)), 
                             FUN = sum, na.rm = TRUE)
    enmalleNames <- enmalleInfo[,1]
    
    # Get Factoor by day
    index <- which(grepl(x = colnames(datosPonderacion$baseMuestreo), pattern = "[[:digit:]^]", perl = TRUE))
    catchMuestreo <- aggregate(x = datosPonderacion$baseMuestreo[,index], 
                               by = list(datosPonderacion$baseMuestreo$date), sum, na.rm = TRUE)
    
    index <- match(enmalleInfo[,1], colnames(catchData))
    weightFactor <- colSums(catchData[,index])/rowSums(as.matrix(catchMuestreo[,-1]), na.rm = TRUE)
    weightFactor[is.infinite(weightFactor)] <- 0
    
    # Weighting values of catch
    enmalleInfo <- sweep(t(enmalleInfo[,-1])*1e-6, 2, weightFactor, "*")
    colnames(enmalleInfo) <- enmalleNames
    
    # Add enmalle info to catchData matrix
    for(i in seq(ncol(enmalleInfo))){
      indexCol <- colnames(catchData) == colnames(enmalleInfo)[i]
      indexRow <- match(rownames(enmalleInfo), rownames(catchData))
      catchData[indexRow, indexCol] <- rowSums(cbind(catchData[indexRow, indexCol], enmalleInfo[,i]), na.rm = TRUE)
    }  
  }
  
  # MAKE PROJECTIONS
  # Projection from end of survey to the start of season
  index <- seq(from = datesList$surveyDate, to = datesList$startDate - 1, by = "day")
  catchVector <- matrix(data = 0, nrow = length(allMarks), ncol = length(index), 
                        dimnames = list(allMarks, as.character(index)))
  
  preSeasonProj <- as.matrix(surveyVector)
  for(i in seq_along(index)){
    tempOutput <- projectPOPE(N = cbind(preSeasonProj[,i], preSeasonProj[,i]), 
                              catch = catchVector[,i],
                              a = a, b = b, k = growthParameters$k, Linf = growthParameters$Linf, 
                              sizeM = growthParameters$sizeM, vectorM = growthParameters$vectorM,
                              freq = 365, sp = sp, Ts = 1)
    
    preSeasonProj <- cbind(preSeasonProj, tempOutput$N[2,])
  }
  
  # BY WEEK
  # Get index on starting days on weeks
  weekIndex <- cumsum(grepl(x = weekdays(allDates), pattern = "lunes|monday"))
  weekIndex <- weekIndex - ifelse(weekIndex[1] == 1, 1, 0)
  
  # Start week matrix with survey data
  outputByWeek <- as.matrix(preSeasonProj[,ncol(preSeasonProj)])
  dimnames(outputByWeek) <- list(allMarks, "Crucero")
  
  # Make an index of the number of days in each week
  repWeekIndex <- table(weekIndex)
  
  # If the 1st day is not Monday, then make a daily projection until the nearest one
  if(repWeekIndex[1] < 7){
    index <- weekIndex == 0
    catchVector <- as.matrix(catchData[,index])
    
    output <- outputByWeek
    for(i in seq(sum(index))){
      tempOutput <- projectPOPE(N = cbind(output[,i], output[,i]), 
                                catch = catchVector[,i]*growthParameters$catchFactor,
                                a = a, b = b, k = growthParameters$k, Linf = growthParameters$Linf, 
                                sizeM = growthParameters$sizeM, vectorM = growthParameters$vectorM,
                                freq = 365, sp = sp, Ts = 1)
      
      output <- cbind(output, tempOutput$N[2,])
    }
    
    outputByWeek <- cbind(outputByWeek, output[,ncol(output)])
    colnames(outputByWeek)[-1] <- paste(format(allDates[which(index)[c(1, sum(index))]], "%d/%m"), collapse = " - ")
  }
  
  # If there is (at least) a Monday with more than 6 days, then make a weekly projection
  index7days <- repWeekIndex == 7
  if(sum(index7days) > 0){
    catchVector <- aggregate(t(catchData), list(weekIndex), sum, na.rm = TRUE)
    index <- is.element(catchVector[,1], names(repWeekIndex)[index7days])
    catchVector <- as.matrix(t(catchVector[index,])[-1,])
    
    output <- as.matrix(outputByWeek[,ncol(outputByWeek)])
    for(i in seq(ncol(catchVector))){
      tempOutput <- projectPOPE(N = cbind(output[,i], output[,i]), 
                                catch = catchVector[,i]*growthParameters$catchFactor,
                                a = a, b = b, k = growthParameters$k, Linf = growthParameters$Linf, 
                                sizeM = growthParameters$sizeM, vectorM = growthParameters$vectorM,
                                freq = 52, sp = sp, Ts = 1)
      
      output <- cbind(output, tempOutput$N[2,])
    }
    
    index <- !duplicated(weekIndex) & is.element(weekIndex, names(repWeekIndex[repWeekIndex == 7]))
    namesColumns <- allDates[index]
    namesColumns <- data.frame(namesColumns, namesColumns + 6)
    namesColumns <- apply(namesColumns, 1, function(x) paste(format(as.Date(x), "%d/%m"), collapse = " - "))
    
    output <- as.matrix(output[,-1])
    colnames(output) <- namesColumns
    outputByWeek <- cbind(outputByWeek, output)
  }
  
  # If the last week has less than 7 days, then make a daily projection until the end
  if(length(repWeekIndex) > 1 && tail(repWeekIndex, 1) < 7){
    
    index <- weekIndex == tail(names(repWeekIndex), 1)
    catchVector <- as.matrix(catchData[,index])
    
    output <- as.matrix(outputByWeek[,ncol(outputByWeek)])
    for(i in seq(sum(index))){
      tempOutput <- projectPOPE(N = cbind(output[,i], output[,i]), 
                                catch = catchVector[,i]*growthParameters$catchFactor,
                                a = a, b = b, k = growthParameters$k, Linf = growthParameters$Linf, 
                                sizeM = growthParameters$sizeM, vectorM = growthParameters$vectorM,
                                freq = 365, sp = sp, Ts = 1)
      
      output <- cbind(output, tempOutput$N[2,])
    }
    
    output <- as.matrix(output[,-1])
    dimnames(output) <- list(allMarks, as.character(allDates[index]))
    
    output <- as.matrix(output[,ncol(output)])
    colnames(output) <- paste(format(range(allDates[index]), "%d/%m"), collapse = " - ")
    outputByWeek <- cbind(outputByWeek, output)
  }
  
  # BY DAY (all season)
  outputByDayAll <- as.matrix(preSeasonProj[,ncol(preSeasonProj)])
  dimnames(outputByDayAll) <- list(allMarks, "Crucero")
  for(i in seq(ncol(catchData))){
    
    tempOutput <- projectPOPE(N = cbind(outputByDayAll[,i], outputByDayAll[,i]), 
                              catch = catchData[,i]*growthParameters$catchFactor,
                              a = a, b = b, k = growthParameters$k, Linf = growthParameters$Linf, 
                              sizeM = growthParameters$sizeM, vectorM = growthParameters$vectorM,
                              freq = 365, sp = sp, Ts = 1)
    
    outputByDayAll <- cbind(outputByDayAll, tempOutput$N[2,])
  }
  
  colnames(outputByDayAll)[-1] <- as.character(allDates)
  
  # Build catch matrix by weeks
  catchByWeek <- aggregate(x = t(catchData), by = list(weekIndex), FUN = sum)
  catchByWeek <- cbind(surveyVector, t(catchByWeek[,-1]), catchData[,ncol(catchData)])
  
  rownames(catchByWeek) <- allMarks
  colnames(catchByWeek) <- c("Crucero",
                             tapply(allDates, weekIndex, function(x) paste(format(range(x), format = "%d/%m"), collapse = " - ")),
                             colnames(catchData)[ncol(catchData)])
  
  # Concatenate object as a list
  output <- list(allMarks         = allMarks,
                 a                = a,
                 b                = b,
                 surveyVector     = surveyVector,
                 catchByDay       = catchData,
                 catchByWeek      = catchByWeek,
                 projPreSeason    = preSeasonProj,
                 projByWeek       = outputByWeek,
                 projByDay        = outputByDayAll,
                 weekIndex        = weekIndex,
                 getInfo          = getInfo,
                 startDate        = datesList$startDate,
                 endDate          = datesList$endDate,
                 endExploringDate = datesList$endExploringDate,
                 endSeasonDate    = datesList$endSeasonDate,
                 allDates         = allDates)
  
  class(output) = "fishingMonitoring"
  save(output, file = "output.RData")
  
  return(output)
}

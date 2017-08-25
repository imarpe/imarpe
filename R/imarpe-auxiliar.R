
#' Months in words to months in numbers
#' @description Function to convert abbreviations of months in words to months in number.
#' @param x A vector with the abbreviation of months in words.
#' @return A vector with the months in numbers. Where January is number one
#' and increasing until December that is the number twelve.
#' @examples
#' # The 'months' vector have the twelve abbreviation months
#' months = month.abb
#' word2month(x = months)
#'
#' @export
word2month = function(x){
  x = seq_along(month.abb)[match(tolower(x), tolower(month.abb))]

  return(x)
}

#' Months in numbers to months in words
#' @description Function to convert months in number to abbreviations of months in words.
#' @param x A vector with months in numbers.
#' @return A vector with the abbreviations of months in words. Where the number one
#' is "Jan" (January) and increasing until the number twelve that is "Dec" (December).
#' @examples
#' # The 'months' vector have the numbers between one and twelve.
#' months = seq(from = 1, to = 12, by = 1)
#' month2word(x = months)
#'
#' @export
month2word = function(x){
  x = month.abb[match(as.numeric(x), 1:12)]

  return(x)
}

#' Months in english to months in spanish
#' @description Function to convert abbreviation of months in english to abbreviation of
#' months in spanish.
#' @param x A vector with abbreviation of months in english.
#' @return A vector with abbreviation of months in spanish.
#' @examples
#' # The 'months' vector contain abbreviation of months in english.
#' months = month.abb
#' engToSpa(x = months)
#'
#' @export
engToSpa = function(x){
  index = match(x, month.abb)
  return(month.abb_spanish[index])
}

#' Get calendar
#' @description Function to get a data base of dates according a specific year.
#' @param year The specific year to get the data base of dates.
#' @param language The language for columns names in the data frame that is returned from the function. By default
#' is in spanish, but this can be english.
#' @return A data frame with three columns, the years, months and days.
#' @examples
#' #To get the a data base of dates to the year 2001 in spanish.
#' getCalendar(year = 2001)
#'
#' #To get the same data base of dates but with the colnames in englis, change the \code{language} parameter to 'english'.
#' getCalendar(year = 2001, language = "english")
#'
#' @export
getCalendar = function(year, language = "spanish") {

  ndays   = NULL
  nmonths = NULL
  nyears  = NULL

  for(j in seq_along(year)) {

    for(i in 1:12) {

      dates   = paste(year[j], "-", i, "-01", sep = "")
      xdays   = 1:monthDays(as.Date(dates))
      ndays   = c(ndays, xdays)
      xmonth  = monthDays(as.Date(dates))
      ymonth  = rep(i, xmonth)
      nmonths = c(nmonths, ymonth)
    }

    xyears = yearDays(as.Date(dates))
    yyears = rep(year[j], xyears)
    nyears = c(nyears, yyears)
  }

  dataBase = data.frame(anho = nyears, mes = nmonths, dia = ndays)

  if(language == "spanish"){dataBase = dataBase}
  if(language == "english"){colnames(dataBase) = c("year", "month", "day")}

  return(dataBase)
}

#' Capitalize first letter
#' @description Function that capitalize only the first letter of a object and convert the
#' rest of the object on lower case.
#' @param x A R object with elements to the character class.
#' @return A object with the first letter of each element on capital letter.
#' @examples
#' capitalizeFirstLetter(x = c("the LAsT BOOK", "my favoURIte MOVIe"))
#'
#' @export
capitalizeFirstLetter = function(x) {
  x = paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, 9999)))
  return(x)
}

#' Get port information from name
#' @description This function uses a data base with the list of ports around Peruvian
#' coast (portData) and according to names of the ports that is given, the function get information
#' about: the name of the port, the pattern to find the name of the port, the
#' longitude and latitude of their names, the area (north, south and central) of
#' ports and the importance of the port according the history of the fishery.
#' @param myPorts A vector of the names of ports.
#' @return A list, with a list of the information about the ports and a vector with the
#' positions of the ports given to the function.
#' @examples
#' # The function receive a vector with port names:
#' ports = getPort(c("T. de mora", "Paita"))
#' ports$data
#' ports$position
#'
#' @export
getPort = function(myPorts){

  myPorts = tolower(myPorts)

  portPosition = NULL
  for(i in seq_along(myPorts)){
    evalPort = myPorts[i]
    posPort  = which(sapply(portData$pattern, grepl, x = evalPort))

    if(length(posPort) > 1){
      warning(paste(evalPort, "combinado con mas de un patron en la posicion =", i,
                    "\nLa funcion tomara el primer valor encontrado:", portData$name[posPort[1]]))}

    portPosition = c(portPosition, posPort[1])
  }

  output = list(data     = as.list(portData[portPosition,]),
                position = portPosition)

  return(output)
}

#' Vector colours
#' @description Function to use an additional vector of colours. This function use the
#' the function \code{tim.colors} of the fields packages.
#' @param n Number of color levels.
#' @return A vector giving the code of colours.
#' @examples
#' vectorColours(n = 5)
#'
#' @export
vectorColours = function (n){
  return(colorRampPalette(colors = tim.colors(64))(n))
}

#' Title Get the isoparalitoral area code
#' @description Using information of the geographical coordinates calculate the
#' isoparalitoral area code (AIP).
#' @param dataPoints \code{data.frame} which has longitude and latitude information.
#' @param colLon Name of column which contains longitude information.
#' @param colLat Name of column which contains latitude information.
#' @return A numeric vector indicating the AIP values for each geographical coordinates.
#' @examples
#' # Create a data frame with information of geographical coordinates.
#' exampleCoords = data.frame(lon = runif(n = 10, min = -80, max = -78),
#'                             lat = runif(n = 10, min = -14, max = -12))
#'
#' isopArea.assigner(dataPoints = exampleCoords)
#'
#' @export
isopArea.assigner = function(dataPoints, colLon = "lon", colLat = "lat"){

  referenceShapefile = AIPShapefile

  dataPoints = switch(class(dataPoints),
                      "data.frame" = as.data.frame(dataPoints[,c(colLon, colLat)]),
                      "numeric" = data.frame(lon = dataPoints[1], lat = dataPoints[2], stringsAsFactors = FALSE))

  output = rep(NA, nrow(dataPoints))

  index = complete.cases(dataPoints)
  dataPoints = dataPoints[index,]

  coordinates(dataPoints) = dataPoints

  proj4string(dataPoints) = proj4string(referenceShapefile)

  dataPoints = over(x = dataPoints, y = referenceShapefile)

  output[index] = dataPoints$code

  return(output)
}

#' Title Extract information from isoparalitoral area code
#' @description The isoparalitoral area (AIP) code must be written in format DDLLPP,
#' where DD is value of distance to coast (mn/10), LL are values of latitude (as integer) and
#' PP is position (up or down).
#' @param aipVector A vector indicating the AIP code.
#' @return A \code{data.frame} with variables distance to the coast (dc), latitude (lat)
#' and position (upDown) in the columns for each AIP code in rows.
#' @examples
#' getAIPInfo(c(30073, 1020, 2010))
#'
#' @export
getAIPInfo = function(aipVector){

  ncharAip = nchar(aipVector)

  dc = as.numeric(substr(aipVector, 1, ifelse(ncharAip == 4, 1, 2)))*10
  lat = as.numeric(substr(aipVector, ifelse(ncharAip == 4, 2, 3), ncharAip - 1))
  upDown = as.numeric(substr(aipVector, ncharAip, ncharAip))

  return(data.frame(dc, lat, upDown, stringsAsFactors = FALSE))
}

#' Title Assing anchovy season
#' @description Using the historical records of the opening and closing of
#' anchovy fishing by regions (north-central and south), this function can identify the
#' anchovy season.
#' @param x A vector of date (with day, month and year) in the format \code{Date}.
#' @param region A object indicating if is the north-central region ('norte-centro') or if is
#' the south region ('sur').
#' @return A object with the season code.
#' @examples
#' # Construct a vector of dates
#'
#' exampleDates = as.Date(c("20/01/2013", "30/01/1999"), format = "%d/%m/%Y")
#'
#' assignAnchovy_season(exampleDates, region = "norte-centro")
#'
#' @export
assignAnchovy_season = function(x, region){

  if(region == "norte-centro") { seasonData = seasonAnchovyNC }
  if(region == "sur") { seasonData = seasonAnchovyS }

  seasonData$start = as.Date(seasonData$start, format ="%d/%m/%Y")
  seasonData$end   = as.Date(seasonData$end, format ="%d/%m/%Y")

  seasonVector = rep(NA, length(x))

  for(i in seq_along(seasonData$year)){

    seasonVector[x >= seasonData$start[i] & x <= seasonData$end[i]] = seasonData$season[i]
  }
  return(seasonVector)
}


#' Download daily fishing monitoring files
#' @description Function to download landing and effort anchovy information from an official
#' repository of the IMARPE. The unique effort of this data base is number of boats.
#' @param directory Directory where the landing and effort information are stored. By default it
#' is \code{NULL}, temporarily saved and then deleted. If you want to keep this parameter must be changed.
#' @param urlFishingMonitoring The web address (url - Uniform Resource Locator) for downloading
#' the landings. By default it is \url{http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_rpelag_porfinal}.
#' @param startDate Start date to download the files.
#' @param endDate End date to download the files.
#' @param saveFile A logical parameter. By default is \code{TRUE} to save a data frame
#' with the fishery information that has been downloaded. The name to save the file is
#' "dailyFishing" with the start date (without "-") and the end date (without "-"), this
#' characters saparated by "_".
#' @return A data frame where the columns are the variables in spanish:
#' \itemize{
#'  \item anho: for years
#'  \item mes: for months
#'  \item dia: for days
#'  \item especie: for species
#'  \item tipo_flota: for type of fleet
#'  \item puerto: for port
#'  \item captura: for the catch or landing of the species
#'  \item embarcaciones: for the effort
#' }
#' @export
#' @examples
#' # Download fishing information, by default it is saved.
#' fishingInfo = downloadDailyFishing(startDate = "2017-4-20", endDate = "2017-6-19")
#' View(fishingInfo)
#'
#' #Use saveFile = FALSE to do not save the information.
#' fishingInfo = downloadDailyFishing(startDate = "2017-4-20", endDate = "2017-6-19", saveFile = FALSE)
#' View(fishingInfo)
#'
downloadDailyFishing = function(directory = NULL,
                                urlFishingMonitoring = "http://www.imarpe.pe/imarpe/archivos/reportes/imarpe_rpelag_porfinal",
                                startDate, endDate, saveFile = TRUE) {

  # working directory
  if(is.null(directory) || !dir.exists(directory)) {
    directory = tempdir()
    dir.create(path = directory, showWarnings = FALSE)
  }

  # download the daily data
  DownloadPorcenta(directorio = directory, dirUrl = urlFishingMonitoring, inicio = startDate, fin = endDate)

  # Read the daily data
  outputData = ReadPorcenta(directorio = directory, inicio = startDate, fin = endDate)

  #Get the data base
  dataLanding = outputData$desembarque
  dataEffort  = outputData$n.embarcaciones

  #sort data
  dataLanding$puerto = as.character(dataLanding$puerto)
  dataLanding = rbind(colnames(dataLanding), dataLanding)
  colnames(dataLanding) = ""

  #dates
  vectorDates = seq(from = as.Date(startDate, format = "%Y-%m-%d"), to = as.Date(endDate, format = "%Y-%m-%d"), by = "day")
  years  = as.numeric(format(vectorDates, "%Y"))
  months = as.numeric(format(vectorDates, "%m"))
  days   = as.numeric(format(vectorDates, "%d"))

  vectorPorts = (dim(dataLanding)[2] - 1)

  dailyFishing = data.frame(anho          = rep(years, each = vectorPorts),
                            mes           = rep(months, each = vectorPorts),
                            dia           = rep(days, each = vectorPorts),
                            especie       = rep("anchoveta", vectorPorts),
                            tipo_flota    = rep(c("industrial", "industrial_madera"), times = vectorPorts/2 * length(vectorDates)),
                            puerto        = rep(as.character(dataLanding[1, 2:(dim(dataLanding)[2])]), times =  length(vectorDates)),
                            captura       = as.numeric(as.vector(t(dataLanding[c(3:dim(dataLanding)[1]), c(2:dim(dataLanding)[2])]))),
                            embarcaciones = as.numeric(as.vector(t(dataEffort[c(2:dim(dataEffort)[1]), c(2:dim(dataEffort)[2])]))))

  #delete temporary files
  unlink(x = list.files(path = directory, pattern = ".xlsx", full.names = TRUE), force = TRUE, recursive = TRUE)

  #save the new data base
  if(isTRUE(saveFile)){
    write.csv(x = dailyFishing,
              file = paste0("dailyFishing", "_", gsub("-", "", startDate), "_", gsub("-", "", endDate), ".csv"),
              row.names = FALSE)
  }
  return(dailyFishing)
}

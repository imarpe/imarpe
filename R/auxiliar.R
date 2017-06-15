
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
#' @export
#'
#' @examples
#' #To get the a data base of dates to the year 2001 in spanish.
#' getCalendar(year = 2001)
#'
#' #To get the same data base of dates but with the colnames in englis, change the \code{language} parameter to 'english'.
#' getCalendar(year = 2001, language = "english")
#'
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
#'
#' @return A object with the first letter of each element on capital letter.
#' @export
#'
#' @examples
#' capitalizeFirstLetter(x = c("the LAsT BOOK", "my favoURIte MOVIe"))
#'
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
#' @export
#'
#' @examples
#' # The function receive a vector with port names:
#' ports = getPort(c("T. de mora", "Paita"))
#' ports$data
#' ports$position
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

#Function to use an additional vector of colours (tim.colors(1e3))
vectorColours = function (n){
  return(colorRampPalette(colors = tim.colors(64))(n))
}

#Function to get the aip
isopArea.assigner <- function(dataPoints, colLon = "lon", colLat = "lat"){

  referenceShapefile <- AIPShapefile

  dataPoints <- switch(class(dataPoints),
                       "data.frame" = as.data.frame(dataPoints[,c(colLon, colLat)]),
                       "numeric" = data.frame(lon = dataPoints[1], lat = dataPoints[2], stringsAsFactors = FALSE))

  output <- rep(NA, nrow(dataPoints))

  index <- complete.cases(dataPoints)
  dataPoints <- dataPoints[index,]

  coordinates(dataPoints) <- dataPoints

  proj4string(dataPoints) <- proj4string(referenceShapefile)

  dataPoints <- over(x = dataPoints, y = referenceShapefile)

  output[index] <- dataPoints$code

  return(output)
}

#Function to get the distance to the coast
getAIPInfo <- function(aipVector){
  ncharAip <- nchar(aipVector)

  dc <- as.numeric(substr(aipVector, 1, ifelse(ncharAip == 4, 1, 2)))*10
  lat <- as.numeric(substr(aipVector, ifelse(ncharAip == 4, 2, 3), ncharAip - 1))
  upDown <- as.numeric(substr(aipVector, ncharAip, ncharAip))

  # if(any(!is.element(upDown, c(0, 3)))){
  #   warning(paste("Values #", paste(which(!is.element(upDown, c(0, 3))), collapse = ", "), "have wrong values for up-down info."))
  # }

  return(data.frame(dc, lat, upDown, stringsAsFactors = FALSE))
}

#Function to assign anchovy season
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


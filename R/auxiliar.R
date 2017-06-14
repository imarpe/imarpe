
# Auxiliar functions to use in the package imarpe

# Fishery -----------------------------------------------------------------

#Functions to include in fishery section

# Landing Function -------------------------------------------------------

# Function to convert months in words to months in numbers
word2month = function(x){
  x = seq_along(month.abb)[match(tolower(x), tolower(month.abb))]

  return(x)
}

# Function to convert months in numbers to months in words
month2word = function(x){
  x = month.abb[match(as.numeric(x), 1:12)]

  return(x)
}

# Function to convert months in english to months in spanish
engToSpa = function(x){
  index = match(x, month.abb)
  return(month.abb_spanish[index])
}

# Function to get the calendar according to a year (number of days, months by a specific year)
.getCalendar = function(year = NULL, ...){
  ndays   = NULL
  nmonths = NULL
  nyears  = NULL
  for(j in seq_along(year)){
    for(i in 1:12){
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
  return(data.frame(anho = nyears, mes = nmonths, dia = ndays))
}

# Function to capitalize only the first letters
capitalizeFirstLetter = function(x) {
  x = paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, 9999)))
  return(x)
}

# Function to order the port names in relation to the latitude
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


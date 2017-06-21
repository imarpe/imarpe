

# Internal function for the class bitacora --------------------------------

#Funcion para obtener la data del programa de bitacoras (PBP)
.getBitacoraData = function(file, colTrip, colPort, colDateOut, colDateStart,
                            colSearchTime, colStorageCapacity,
                            colLat, colLon, colHaul, colHaulTotal, colCatchHaul,
                            capAnch, capSar, capJur, capCab, capBon, ...) {

  dataBase = read.csv(file = file, header = TRUE, na.strings = "", stringsAsFactors = FALSE, ...)

  #Fishing trip
  tripVector = dataBase[, colTrip]
  if(sum(is.na(tripVector)) != 0) {warning("Existen viajes sin el registro del codigo de viaje")}
  tripVector = tripVector[tripVector != 0 & tripVector != ""]
  tripVector = tripVector[!duplicated(tripVector)]

  #Ports (seguir poniendo a prueba el vector de puertos)
  portsVector = dataBase[, colPort]
  if(sum(is.na(portsVector)) != 0) {warning("Existen Viajes sin el registro del puerto")}

  portsVector = gsub(pattern = "\\([[:print:]]+\\)", replacement = "", x = portsVector, perl = TRUE)
  portList    = getPort(myPorts = portsVector)
  dataBase$puerto = portList$data$name
  dataBase$latitudAux = portList$data$lat

  #Dates
  datesVector = dataBase[, colDateOut]
  if(sum(is.na(datesVector)) != 0) {warning("Existen viajes sin el registro de la fecha")}
  datesVector = datesVector[datesVector != "" & !is.na(datesVector)]
  newDatesVector = as.Date(datesVector, format="%d/%m/%Y %H:%M:%S")

  yearVector  = as.numeric(format(newDatesVector, "%Y"))
  monthVector = as.numeric(format(newDatesVector, "%m"))
  dayVector   = as.numeric(format(newDatesVector, "%d"))

  #Fleet
  fleetVector = dataBase[, colStorageCapacity]
  if(sum(is.na(fleetVector)) != 0) {warning("Existen viajes sin el registro del tipo de flota")}

  dataBase$flota = cut(x  = fleetVector, breaks = c(0, 10, 32.5, 110, Inf),
                       labels = c("Artesanal", "Menor escala", "Industrial madera", "Industrial"))

  info = list(file   =  file,
              trips  =  length(unique(tripVector)),
              ports  =  length(unique(dataBase$puerto[!is.na(dataBase$puerto)])),
              years  =  unique(yearVector),
              months =  length(rle(monthVector)$values),
              fleets =  as.vector(unique(dataBase$flota[!is.na(dataBase$flota)])),
              colTrip = colTrip, colDateOut = colDateOut, colDateStart = colDateStart,
              colSearchTime = colSearchTime, colStorageCapacity = colStorageCapacity,
              colLat  = colLat, colLon  = colLon,
              colHaul = colHaul, colHaulTotal = colHaulTotal, colCatchHaul = colCatchHaul,
              capAnch = capAnch, capSar = capSar, capJur = capJur, capCab = capCab, capBon  = capBon)

  output = list(data = dataBase, info = info)
  class(output) = c("bitacora")
  return(output)

}

#Funcion para obtener los viajes observados
.observedTrip.bitacora = function(object, language) {

  dataBase = object$data
  dataBase = dataBase[, c(object$info$colTrip, "puerto", "flota", "latitudAux") ]
  colnames(dataBase) = c("codeTrip", "puerto", "flota", "latitudAux")

  dataBase = dataBase[!is.na(dataBase$codeTrip) & dataBase$codeTrip != 0 & dataBase$codeTrip != "" &
                        !is.na(dataBase$puerto) & dataBase$puerto != 0 & dataBase$puerto != "" &
                        !is.na(dataBase$flota) & dataBase$flota != 0 & dataBase$flota != "" , ]

  #to get the trips
  dataBase = dataBase[!duplicated(dataBase), ]

  dataTable = table(dataBase$puerto, dataBase$flota)
  dataTable = data.frame(dataTable)

  dataTable = reshape(dataTable, idvar = "Var1", timevar = "Var2", direction = "wide")
  namesData = unlist(strsplit(colnames(dataTable), "\\."))
  namesData = namesData[!namesData == "Freq"]
  colnames(dataTable) = c("Puerto", namesData[-1])

  #Order by colnames
  fleetNames = c("Puerto", "Artesanal", "Menor escala", "Industrial madera", "Industrial")
  sortFleet  = sort(match(colnames(dataTable), fleetNames), decreasing = FALSE)
  dataTable  = dataTable[sortFleet]
  dataTable$Total = rowSums(dataTable[, seq(from = 2, to = dim(dataTable)[2], by = 1)])
  dataTable$Puerto = as.character(dataTable$Puerto)

  #Order by ports
  dataTable$lat = dataBase$latitudAux[match(dataTable$Puerto, dataBase$puerto)]
  dataTable = dataTable[with(dataTable, order(dataTable$lat, decreasing = TRUE)), ]
  dataTable$lat = NULL
  rownames(dataTable) = NULL

  dataTable = rbind(dataTable, c("Total", as.vector(colSums(dataTable[, seq(from = 2, to = dim(dataTable)[2], by = 1)]))))

  #Language
  if(language == "english"){
    vectorNames = match(colnames(dataTable), c("Puerto", "Artesanal", "Menor escala", "Industrial madera", "Industrial", "Total"))
    colnames(dataTable) = cut(x = vectorNames, breaks = c(0,1,2,3,4,5,6),
                              labels = c("Port", "Artisanal", "Small scale", "Industrial wood", "Industrial", "Total"))}

  if(language == "spanish"){colnames(dataTable) = colnames(dataTable)}

  return(dataTable)
}

#Funcion para obtener los lances pesqueros muestreados
.fishingHaul.bitacora = function(object, language, latByPort) {

  dataBase = object$data
  dataBase = dataBase[, c(object$info$colLat, "latitudAux", object$info$colHaul, "flota") ]
  dataBase[, 1] = -abs(dataBase[, 1])
  colnames(dataBase) = c("lat", "latAux", "haul", "fleet")

  dataBase = dataBase[!is.na(dataBase$lat) & dataBase$lat != 0 & dataBase$lat != "" &
                        !is.na(dataBase$haul) & dataBase$haul != 0 & dataBase$haul != "" &
                        !is.na(dataBase$fleet) & dataBase$fleet != 0 & dataBase$fleet != "", ]

  if(isTRUE(latByPort)) {
    indexLat = which(is.na(dataBase$lat))
    dataBase$lat[indexLat] = dataBase$latAux[indexLat]
  }

  dataBase$latAux = NULL
  rownames(dataBase) = NULL

  dataBase$lat = cut(dataBase$lat,
                     breaks = seq(from = -19, to = -3, by = 1),
                     labels = seq(from = -18, to = -3, by = 1))

  dataTable = table(dataBase$lat, dataBase$fleet)
  dataTable = data.frame(dataTable)
  dataTable = reshape(dataTable, idvar = "Var1", timevar = "Var2", direction = "wide")
  namesData = unlist(strsplit(colnames(dataTable), "\\."))
  namesData = namesData[!namesData == "Freq"]
  colnames(dataTable) =  c("Latitud", namesData[-1])
  dataTable = dataTable[sort(dataTable$Latitud, decreasing = TRUE),]

  dataTable$Latitud   =  as.character(dataTable$Latitud)
  dataTable$Latitud   =  paste0(substring(dataTable$Latitud, 2), "S")
  #dataTable$Latitud  =  paste0(substring(dataTable$Latitud, 2), "\U00B0", "S")

  #Order by colnames
  fleetNames = c("Latitud", "Artesanal", "Menor escala", "Industrial madera", "Industrial")
  sortFleet  = sort(match(colnames(dataTable), fleetNames), decreasing = FALSE)
  dataTable  = dataTable[sortFleet]
  dataTable$Total = rowSums(dataTable[, seq(from = 2, to = dim(dataTable)[2], by = 1)])

  dataTable = rbind(dataTable, c("Total", as.vector(colSums(dataTable[, seq(from = 2, to = dim(dataTable)[2], by = 1)]))))

  #Language
  if(language == "english"){
    vectorNames = match(colnames(dataTable), c("Latitud", "Artesanal", "Menor escala", "Industrial madera", "Industrial", "Total"))
    colnames(dataTable) = cut(x = vectorNames, breaks = c(0,1,2,3,4,5,6),
                              labels = c("Latitude", "Artisanal", "Small scale", "Industrial wood", "Industrial", "Total"))}
  if(language == "spanish"){colnames(dataTable) = colnames(dataTable)}
  rownames(dataTable) = NULL

  return(dataTable)

}

#Funcion para obtener los puntos de pesca (por especie y grupo taxonomico)
.fishingPoints.bitacora = function(object){

  dataBase = object$data
  dataBase = dataBase[, c(object$info$colLat, object$info$colLon, "flota", object$info$colHaul,
                          object$info$colCatchHaul, catchSpecies$catch) ]
  colnames(dataBase)[1:5] = c("lat", "lon", "fleet", "num_haul", "catch_haul")

  #remove null catched
  dataBase = dataBase[!is.na(dataBase$num_haul) & dataBase$num_haul != 0 & dataBase$num_haul != "", ]
  #remove null fleet type
  dataBase = dataBase[!is.na(dataBase$fleet) & dataBase$fleet != 0, ]

  #removing null lat and lon
  dataBase = dataBase[!is.na(dataBase$lat) & dataBase$lat != "", ]
  dataBase = dataBase[!is.na(dataBase$lon) & dataBase$lon != "", ]
  rownames(dataBase) = NULL

  dataBase$lat = -abs(dataBase$lat)
  dataBase$lon = -abs(dataBase$lon)

  #data for plot the 5 species
  dataAnch = dataBase[, c("lat", "lon", "fleet", "num_haul", "catch_haul", object$info$capAnch)]
  dataSar  = dataBase[, c("lat", "lon", "fleet", "num_haul", "catch_haul", object$info$capSar)]
  dataJur  = dataBase[, c("lat", "lon", "fleet", "num_haul", "catch_haul", object$info$capJur)]
  dataCab  = dataBase[, c("lat", "lon", "fleet", "num_haul", "catch_haul", object$info$capCab)]
  dataBon  = dataBase[, c("lat", "lon", "fleet", "num_haul", "catch_haul", object$info$capBon)]

  colnames(dataAnch) = c("lat", "lon", "fleet", "num_haul", "catch_haul", "catchSpecies")
  dataAnch = dataAnch[!is.na(dataAnch$catchSpecies) & dataAnch$catchSpecies !=0, ]
  rownames(dataAnch) = NULL

  colnames(dataSar) = c("lat", "lon", "fleet", "num_haul", "catch_haul", "catchSpecies")
  dataSar  = dataSar[!is.na(dataSar$catchSpecies) & dataSar$catchSpecies !=0, ]
  rownames(dataSar) = NULL

  colnames(dataJur) = c("lat", "lon", "fleet", "num_haul", "catch_haul", "catchSpecies")
  dataJur  = dataJur[!is.na(dataJur$catchSpecies) & dataJur$catchSpecies !=0, ]
  rownames(dataJur) = NULL

  colnames(dataCab) = c("lat", "lon", "fleet", "num_haul", "catch_haul", "catchSpecies")
  dataCab  = dataCab[!is.na(dataCab$catchSpecies) & dataCab$catchSpecies !=0, ]
  rownames(dataCab) = NULL

  colnames(dataBon) = c("lat", "lon", "fleet", "num_haul", "catch_haul", "catchSpecies")
  dataBon  = dataBon[!is.na(dataBon$catchSpecies) & dataBon$catchSpecies !=0, ]
  rownames(dataBon) = NULL

  #presence for other species
  dataGroups = dataBase[, -which(names(dataBase) %in%
                                   c(object$info$capAnch, object$info$capSar, object$info$capJur, object$info$capCab, object$info$capBon))]
  dataGroups = melt(dataGroups, id.vars=c("lat", "lon", "fleet", "num_haul", "catch_haul"))
  indexGroup = match(dataGroups$variable, catchSpecies$catch)
  dataGroups$group = catchSpecies$group[indexGroup]
  dataGroups$species = catchSpecies$species[indexGroup]
  dataGroups = dataGroups[!is.na(dataGroups$value),]
  rownames(dataGroups) = NULL

  output = list(dataAnch = dataAnch, dataSar = dataSar, dataJur = dataJur, dataCab = dataCab,
                dataBon = dataBon, dataGroups = dataGroups, dataTotal = dataBase)

  return(output)

}

#Funcion para plotear los mapas con puntos de pesca
.plotFishingPoints.bitacora = function(x, language, dataType,
                                       colMap = colMap, cexPointCatch = cexPointCatch, cexPoint = cexPoint,
                                       colFleet = colFleet, cex.axis = cex.axis, cexPorts = cexPorts, ...){

  dataBase = x[[dataType]]
  ports    = portData[portData$importance == 1, ]

  #Colours by fleet
  colVector = match(dataBase$fleet, c("Artesanal", "Menor escala", "Industrial madera", "Industrial"))

  #Legend colours
  if(language == "spanish"){fleetName = c("AR", "ME", "IM", "IN")[sort(unique(colVector), decreasing = FALSE)]}
  if(language == "english"){fleetName = c("AR", "SS", "IW", "IN")[sort(unique(colVector), decreasing = FALSE)]}
  colLegend = colFleet[sort(unique(colVector), decreasing = FALSE)]

  colVector = cut(colVector, breaks = c(-Inf, 1, 2, 3, 4), labels = colFleet)

  #Size according the catch
  sizePoints = cut(x = dataBase$catchSpecies, breaks = pretty(dataBase$catchSpecies),
                   labels = seq(from = 1, by = 1, length.out = length(pretty(dataBase$catchSpecies))-1))
  if(isTRUE(cexPointCatch)){cexPoints = as.numeric(as.character(sizePoints))} else {cexPoints = cexPoint}

  #plot
  par(mar = c(3.5, 3.5, 1, 1))
  plot(x = dataBase$lon, y = dataBase$lat, xlim = c(-85, -70), ylim = c(-20,-2),
       xlab = "", ylab = "", col = as.character(colVector), axes = FALSE, type = "p", pch = 16,
       cex = cexPoints, ...)
  map(add = TRUE, fill = TRUE, col = colMap)
  box()

  axis(2, at = seq(from = -20, to = -2, by = 2), las = 2, cex.axis = cex.axis,
       labels = paste(seq(from = 20, to = 2, by = -2), "S", sep = " "))

  axis(1, at = seq(from = -82, to = -70, by = 2), cex.axis = cex.axis,
       labels = paste(seq(from = 82, to = 70, by = -2), "W", sep = " "))

  text(x = ports$lon, y = ports$lat, labels = ports$name, adj = -0.1, cex = cexPorts)

  legend("bottomleft", legend = fleetName, col = colLegend,
         pch = 20, horiz = FALSE, bty = "n", cex = 1)

  return(invisible())

}

#Funcion para plotear la presencia de otras especies por grupo taxonomico
.plotFishingPresence.bitacora = function(x, byGroup = TRUE, group = NULL,
                                         cexPoint = 1, colMap = "khaki1",
                                         cex.axis = 1.2, cexPorts = 0.9,
                                         colSpecies = NULL, colLegend = NULL, cexLegend = 1, ...){

  dataBase = x$dataGroups

  if(isTRUE(byGroup)){
    dataBase = dataBase[dataBase$group %in% group, ]
    specieVector = match(dataBase$species, unique(dataBase$species))

    if(is.null(colSpecies)){
      colPlot   = vectorColours(length(dataBase$species))[rank(dataBase$species)]
      colLegend = unique(vectorColours(length(dataBase$species))[rank(dataBase$species)])
    } else {
      colPlot   = colSpecies
      colLegend = colLegend
    }

    namesLegend = capitalizeFirstLetter(unique(dataBase$species))

  } else {
    dataBase = dataBase[!dataBase$group %in% c("neritico", "transzonal", "oceanico", "demersal"),]
    dataBase = dataBase[!dataBase$group == "", ]
    specieVector = match(dataBase$group, unique(dataBase$group))

    if(is.null(colSpecies)){
      colPlot   = vectorColours(length(dataBase$group))[rank(dataBase$group)]
      colLegend = unique(vectorColours(length(dataBase$group))[rank(dataBase$group)])
    } else {
      colPlot   = colSpecies
      colLegend = colLegend
    }

    namesLegend = capitalizeFirstLetter(unique(dataBase$group))
  }

  ports    = portData[portData$importance == 1, ]

  #plot
  par(mar = c(3.5, 3.5, 1,1))
  plot(x = dataBase$lon, y = dataBase$lat, xlim = c(-85, -70), ylim = c(-20,-2),
       xlab = "", ylab = "", col = colPlot, axes = FALSE, type = "p", pch = 16, cex = cexPoint, ...)
  map(add = TRUE, fill = TRUE, col = colMap)
  box()

  axis(2, at = seq(from = -20, to = -2, by = 2), las = 2, cex.axis = cex.axis,
       labels = paste(seq(from = 20, to = 2, by = -2), "S", sep = " "))

  axis(1, at = seq(from = -82, to = -70, by = 2), cex.axis = cex.axis,
       labels = paste(seq(from = 82, to = 70, by = -2), "W", sep = " "))

  text(x = ports$lon, y = ports$lat, labels = ports$name, adj = -0.1, cex = cexPorts)

  legend("bottomleft", legend = namesLegend, col = colLegend,
         pch = 20, horiz = FALSE, bty = "n", cex = cexLegend)

  return(invisible())

}

#Funcion para obtener la composicion de especies de las capturas
.speciesComposition.bitacora = function(object, language) {

  #cleaning data
  dataBase = object$data
  dataBase = dataBase[, c(object$info$colCatchHaul, catchSpecies$catch) ]
  dataBase[, 1] = apply(dataBase[2:dim(dataBase)[2]], 1, sum, na.rm = TRUE)
  dataBase = dataBase[!dataBase[, 1] == 0, ]
  dataBase[, 1] = NULL

  #remove species with catch = 0
  index = apply(dataBase, 2, sum, na.rm = TRUE)
  index = which(index != 0); names(index) = NULL
  dataBase = dataBase[, index]

  #data.frame in a new format (species, catch, porcentaje)
  dataTable = apply(dataBase, 2, sum, na.rm = TRUE)
  dataTable = as.data.frame(dataTable)
  dataTable$species = capitalizeFirstLetter(catchSpecies$species[match(rownames(dataTable), catchSpecies$catch)])
  dataTable = dataTable[order(dataTable$dataTable, decreasing = TRUE), ]

  #remove Unidentified species
  dataTable = dataTable[!dataTable$species == "", ]

  dataTable = data.frame(Species    = dataTable$species,
                         Catch      = round(dataTable$dataTable, 4),
                         Percentage = round((dataTable$dataTable * 100) / sum(dataTable$dataTable), 4) )
  dataTable$Species = as.character(dataTable$Species)

  dataTable = rbind(dataTable, c("Total", round(sum(dataTable$Catch),2), round(sum(dataTable$Percentage), 2)))
  dataTable$Catch   = as.numeric(dataTable$Catch)
  dataTable$Percentage = as.numeric(dataTable$Percentage)

  #Language
  if(language == "english") {colnames(dataTable) = colnames(dataTable)}
  if(language == "spanish") {colnames(dataTable) = c("Especie", "Captura", "Porcentaje")}

  return(dataTable)
}

#Funcion para plotear el pie de composicion de especies
.plotSpeciesComposition.bitacora = function(x, threshold = TRUE, minPercentage = 0.2, ...) {

  dataBase = x
  dataBase = dataBase[- dim(dataBase)[1],]

  if(isTRUE(threshold)){
    dataBase = dataBase[dataBase[, 3] >= minPercentage, ]
    dataBase$Porcentaje = dataBase$Captura * 100 / sum(dataBase$Captura)
  } else {
    dataBase = dataBase
  }

  #plot
  par(oma = c(1,1,1,1))
  pie3D(dataBase[,3], labels = dataBase[, 1], col = rainbow(dim(dataBase)[1]),
        radius = 1, height = 0.15, theta = 1.1, start = 1,
        labelcex = 1.2, labelrad = 1.3, ...)

  return(invisible())
}

#Funcion para obtener la distribucion espacial de las capturas
.distributionCatch.bitacora = function(object, language, specie) {

  dataBase = object$data
  dataBase = dataBase[, c(object$info$colLat, object$info$colLon, catchSpecies$catch) ]
  colnames(dataBase)[c(1,2)] = c("lat", "lon")
  dataBase$lat = -abs(dataBase$lat)
  dataBase$lon = -abs(dataBase$lon)

  dataBase = dataBase[!is.na(dataBase$lat) & !is.na(dataBase$lon), ]

  aip = isopArea.assigner(dataBase)
  getAipInfo = getAIPInfo(aipVector = aip)

  dataBase$lon = getAipInfo$dc
  colnames(dataBase)[2] = "dc"
  colnames(dataBase)[3:dim(dataBase)[2]] = catchSpecies$species[match(colnames(dataBase[3:dim(dataBase)[2]]), catchSpecies$catch)]

  #Indexing for name specie
  dataTable = dataBase[, c(1, 2)]
  dataTable$specie = round(dataBase[, specie], 2)

  dataTable$lat = cut(dataTable$lat, breaks = seq(from = -19, to = -3, by = 1), labels = seq(from = -18, to = -3, by = 1))
  dataTable     = aggregate(specie ~ lat + dc, data = dataTable, FUN = sum, na.rm = TRUE)
  dataTable     = dcast(dataTable, lat ~ dc, value.var = "specie")

  #Order by latitude
  dataTable$lat  = as.numeric(as.character(dataTable$lat))
  dataTable      = dataTable[order(dataTable$lat, decreasing = TRUE), ]
  dataTable[is.na(dataTable)] = 0
  rownames(dataTable) = NULL

  #Latitud
  dataTable$lat = as.character(dataTable$lat)
  dataTable$lat =  paste0(substring(dataTable$lat, 2), "S")

  #Total
  dataTable$Total = rowSums(dataTable[, seq(from = 2, to = dim(dataTable)[2], by = 1)])
  dataTable = rbind(dataTable, c("Total", as.vector(colSums(dataTable[, seq(from = 2, to = dim(dataTable)[2], by = 1)]))))

  #Language
  if(language == "spanish"){colnames(dataTable)[1] = "Latitud"}
  if(language == "spanish"){colnames(dataTable)[1] = "Latitude"}

  return(dataTable)

}

#Funcion para obtener la data para analizar el esfuerzo y el cpue
.effortData.bitacora = function(object) {

  dataBase = object$data
  dataBase = dataBase[, c(object$info$colTrip, object$info$colStorageCapacity, "flota", "puerto",
                          object$info$colDateStart, object$info$colDateOut,
                          object$info$colLat, object$info$colLon, object$info$colSearchTime,
                          object$info$colHaul, object$info$colHaulTotal, object$info$colCatchHaul,
                          object$info$capAnch, object$info$capSar, object$info$capJur, object$info$capCab, object$info$capBon)]

  colnames(dataBase) = c("codeTrip", "storageCapacity", "fleet", "port", "dateStart", "dateOut",
                         "lat", "lon", "searchTime", "haulNumber", "haulTotal","haulCatch",
                         "anchoveta", "sardina", "jurel", "caballa", "bonito")

  #Obtencion de variables  para el esfuerzo
  #travel time (duracion viaje)
  dataBase = dataBase[!is.na(dataBase$dateStart) & !is.na(dataBase$dateOut), ]
  dataBase$dateStart  = as.POSIXct(strptime(dataBase$dateStart, format = "%d/%m/%Y %H:%M:%S"))
  dataBase$dateOut    = as.POSIXct(strptime(dataBase$dateOut, format = "%d/%m/%Y %H:%M:%S"))
  dataBase$travelTime = as.numeric(dataBase$dateOut - dataBase$dateStart) / 60

  #search time (tiempo busqueda)
  hours = suppressWarnings(as.numeric(substring(gsub("^\\s+|\\s+$", "", dataBase$searchTime), 1, 2)))
  mins  = suppressWarnings(as.numeric(substring(gsub("^\\s+|\\s+$", "", dataBase$searchTime), 4, 5)))
  dataBase$searchTime = hours + mins/60

  # variables time
  dataBase$year  = as.numeric(format(dataBase$dateStart, "%Y"))
  dataBase$month = as.numeric(format(dataBase$dateStart, "%m"))
  dataBase$day   = as.numeric(format(dataBase$dateStart, "%d"))
  dataBase$dates = as.Date(paste(dataBase$day, dataBase$month, dataBase$year, sep = "/"),
                           format = "%d/%m/%Y")
  # code trip haul
  dataBase = dataBase[!is.na(dataBase$codeTrip) & !is.na(dataBase$haulTotal), ]
  dataBase$codeTripHaul = paste(dataBase$codeTrip, dataBase$haulTotal, sep = "-")

  return(dataBase)
}

#Funcion para extraer informacion de la especies y la temporada
#' Get species effort data
#' @description Receive an object of bitacora class, indexes the data by species,
#' estimates the season by species, for anchovy the season depend of region
#' (north-central and south region) but for other species
#' (sardine, jack mackerel, chub mackerel and bonito) the season is annual.
#' @param data A \code{data.frame} obtained from effortData.bitacora function (internal function
#' of bitacora class). This data frame could be obtained from
#' getMainResults.bitacora function too.
#' @param species A \code{character} with the common name of anchovy ("anchoveta"),
#' sardine ("sardina"), jack mackerel ("jurel"), chub mackerel ("caballa") and bonito ("bonito").
#' @param region If the species is anchovy (\code{anchoveta}) is necessary
#' indicate for each region the effort data is going to be calculate. By default is \code{NULL}.
#' @return A \code{data.frame} with effort data for a given species.
#' @export
effortSpeciesData.bitacora = function(data, species, region = NULL){

  dataBase = data

  nameSpecies = c("anchoveta", "sardina", "jurel", "caballa", "bonito")
  nameSpecies = nameSpecies[!nameSpecies %in% species]
  dataBase = dataBase[, -which(colnames(dataBase) %in% nameSpecies)]
  dataBase = dataBase[!is.na(dataBase[species]), ]

  catchSpecies = tapply(dataBase[, species], dataBase[, "codeTripHaul"], sum)
  dataBase = subset(dataBase, !duplicated(subset(dataBase, select=c(codeTripHaul))))
  dataBase[, species] = catchSpecies[match(dataBase$codeTripHaul, names(catchSpecies)) ]

  #building season by species
  if(species %in% c("sardina", "jurel", "caballa", "bonito")) {
    dataBase$season = dataBase$year}

  if(species == "anchoveta") {

    if(!is.null(region)){
      if(region == "norte-centro") {
        dataBase = dataBase[!is.na(dataBase$lat), ]
        dataBase = dataBase[which(dataBase$lat > 2 & dataBase$lat <= 15.99), ]
        dataBase$season = assignAnchovy_season(x = dataBase$dates, region = region) }

      if(region == "sur") {
        dataBase = dataBase[!is.na(dataBase$lat), ]
        dataBase = dataBase[which(dataBase$lat >= 16 & dataBase$lat < 18.9), ]
        dataBase$season = assignAnchovy_season(x = dataBase$dates, region = region) }
    } else {
      dataBase$season = dataBase$year
    }

  }
  colnames(dataBase)[13] = "catch"
  rownames(dataBase) = NULL

  return(data = dataBase)
}

#Funcion para obtener el esfuerzo
#' Get fishing effort
#' @description Generical function to get the fishing effort. The type of fishing effort could be four types:
#' \itemize{
#'   \item travelTime for the effort as a function of the travel time.
#'   \item haulTotal for the effort as a function of the total number of fishing haul.
#'   \item storageCapacity for the effort as a function of the storage capacity.
#'   \item searchTime for the effort as a function of the search time to catch the species.
#' }
#' @param data A \code{data.frame} with information to estimated the fishing effort (in columns).
#' This data base have to contain columns of the time (days, months, years, season), ports and the
#' fleet type.
#' @param efforType A \code{character}. It could be one of the fishing effort type ("travelTime",
#' "haulTotal", "storageCapacity", "searchTime").
#' @param effortBy Parameter to indicate whether the effort will be estimated by
#' time (\code{effortBy = time}) or by port (\code{effortBy = port}).
#' @param timeBy If the effort is estimated over the time, it could be by days
#'  (\code{timeBy = days}), months (\code{timeBy = months}), years (\code{timeBy = years}) or
#'  by seasons (\code{timeBy = seasons}).
#' @param fleeType The effort could be estimated for the whole data given or for a data by fleeType.
#' By default this parameter is \code{NULL}.
#' @return A \code{vector} with the estimated effort.
#' @export
getEffort = function(data, efforType, effortBy, timeBy, fleeType = NULL) {

  dataBase = data

  #indexacion tipo de flota
  if(!is.null(fleeType)) {dataBase = dataBase[dataBase$fleet %in% fleeType, ]}

  #remove NAs in efforType
  dataBase = dataBase[!is.na(dataBase[efforType]), ]

  effort = dataBase[efforType]
  names(effort) = NULL
  dataBase["effort"] = effort

  if(effortBy %in% "time") {

    if(timeBy == "days") {effortVector = tapply(X = dataBase$effort, INDEX = list(dataBase$dates), FUN = sum, na.rm = TRUE)}
    if(timeBy == "months") {effortVector = tapply(X = dataBase$effort, INDEX = list(dataBase$year, dataBase$month), FUN = sum, na.rm = TRUE) }
    if(timeBy == "years") {effortVector = tapply(X = dataBase$effort, INDEX = dataBase$year, FUN = sum, na.rm = TRUE) }
    if(timeBy == "seasons") {effortVector = tapply(X = dataBase$effort, INDEX = list(dataBase$season), FUN = sum, na.rm = TRUE) }

  }

  if(effortBy %in% "port") {
    effortVector = tapply(X = dataBase$effort, INDEX = list(dataBase$port), FUN = mean, na.rm = TRUE)
    effortVector = effortVector[order(match(names(effortVector), portData$name))]
  }

  return(effort = effortVector)
}

#Funcion para obtener la captura por unidad de esfuerzo (cpue) relativizada
#' Get the relativizated catch per unit effort (cpue)
#' @details  The relativizated cpue is a index used on bitacora report.
#' @param data A \code{data.frame} with information about catchs and fishing effort.
#' @param toTons A logical parameter. \code{TRUE} (dafault) it assume that the catches is in kilograms
#' and converts it into tonnes (divided by 1000). \code{FALSE} it assume the the information is in tons
#' and don't convert the data.
#' @param efforType A \code{character} indicating the fishing effort type. It could be four types:
#' \itemize{
#'   \item travelTime for the effort as a function of the travel time.
#'   \item haulTotal for the effort as a function of the total number of fishing haul.
#'   \item storageCapacity for the effort as a function of the storage capacity.
#'   \item searchTime for the effort as a function of the search time to catch the species.
#' }
#' @param cpueBy Parameter to indicate whether the relativizated cpue will be estimated by
#' time (\code{cpueBy = time}) or by port (\code{cpueBy = port}).
#' @param timeBy If the cpue relativizated is estimated over the time, it could be by days
#'  (\code{timeBy = days}), months (\code{timeBy = months}), years (\code{timeBy = years}) or
#'  by seasons (\code{timeBy = seasons}).
#' @param fleeType The relativizated cpue could be estimated for the whole data given or for a data by fleeType.
#' By default this parameter is \code{NULL}.
#' @return A \code{vector} with the relativizated cpue.
#' @export
getCpue_relativited = function(data, toTons=FALSE, efforType, cpueBy = "time", timeBy, fleeType = NULL) {

  dataBase = data
  dataBase$catch = dataBase$catch / ifelse(isTRUE(toTons), 1000, 1)

  #indexacion tipo de flota
  if(!is.null(fleeType)) {dataBase = dataBase[dataBase$fleet %in% fleeType, ]}

  #remove NAs in the dataBase
  dataBase = dataBase[!is.na(dataBase$catch) & dataBase$catch != 0 &
                        !is.na(dataBase$storageCapacity) & dataBase$storageCapacity != 0, ]
  dataBase = dataBase[!is.na(dataBase[efforType]) & dataBase[efforType] != 0, ]

  cpue = dataBase$catch / dataBase$storageCapacity / dataBase[efforType]
  names(cpue) = NULL
  dataBase["cpue"] = cpue

  if(cpueBy %in% "time"){

    if(timeBy == "days") {cpueVector = tapply(X = dataBase$cpue, INDEX = list(dataBase$dates), FUN = mean, na.rm = TRUE, i)}
    if(timeBy == "months") {cpueVector = tapply(X = dataBase$cpue, INDEX = list(dataBase$year, dataBase$month), FUN = mean, na.rm = TRUE) }
    if(timeBy == "years") {cpueVector = tapply(X = dataBase$cpue, INDEX = dataBase$year, FUN = mean, na.rm = TRUE) }
    if(timeBy == "seasons") {cpueVector = tapply(X = dataBase$cpue, INDEX = list(dataBase$season), FUN = mean, na.rm = TRUE) }
  }

  if(cpueBy %in% "port") {
    cpueVector = tapply(X = dataBase$cpue, INDEX = list(dataBase$port), FUN = mean, na.rm = TRUE)
    cpueVector = cpueVector[order(match(names(cpueVector), portData$name))]
  }

  return(cpue = cpueVector)
}

#Funcion para plotear el esfuerzo por puerto
.plotEffort.bitacora = function(effort1, effort2, labAxis2, labAxis4, colBar = NULL, colLine = NULL,
                                legend = NULL, ...) {

  maxAxis2 = ceiling(1.2*max(effort1))
  maxAxis4 = ceiling(1.2*max(effort2))

  par(mar=c(3.5,4.5,1,4.5))
  #effort 1
  barplot(effort1, cex.names = 0.8, ylim = c(0, maxAxis2), axes = FALSE, ylab = labAxis2, col = colBar, ...)
  axis(side = 2, at = pretty(effort1), las = 1)

  #effort 2
  vectorEffort2 = barplot(effort2, names.arg = seq(from = 1, to = length(effort2), by = 1), plot = FALSE)
  lines(vectorEffort2, effort2*maxAxis2/maxAxis4, type="o", pch=19, lwd=1, col = colLine)

  axis(side = 4, at = pretty(effort2)/maxAxis4*maxAxis2, labels = pretty(effort2), las = 2)
  mtext(text = labAxis4, side = 4, line = 2.5)

  box()

  if(!is.null(legend)){
    legend("topleft", legend = c(legend[1], legend[2]), bty = "n", lty = c(1,4),
           cex = 0.8, lwd = c(5, 2), col = c("gray", "black"))
  }

  return(invisible())
}

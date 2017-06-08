

# Internal function for the class bitacora --------------------------------

#Funcion para obtener la data del programa de bitacoras (PBP)
.getBitacoraData = function(file, colPort, colDates, colTrip, colStorageCapacity, colLat, colHaul,
                            colLon, colCala, colCapCala, capAnch, capSar, capJur, capCab, capBon, ...) {

  dataBase = read.csv(file = file, header = TRUE, na.strings = "", stringsAsFactors = FALSE, ...)

  #Fishing trip
  tripVector = dataBase[, colTrip]
  if(sum(is.na(tripVector)) != 0) {warning("Viajes sin el registro del codigo de viaje")}
  tripVector = tripVector[tripVector != 0 & tripVector != ""]
  tripVector = tripVector[!duplicated(tripVector)]

  #Ports (seguir poniendo a prueba el vector de puertos)
  portsVector = dataBase[, colPort]
  if(sum(is.na(portsVector)) != 0) {warning("Viajes sin el registro del puerto")}

  portsVector = gsub(pattern = "\\([[:print:]]+\\)", replacement = "", x = portsVector, perl = TRUE)
  portList    = getPort(myPorts = portsVector)
  dataBase$puerto = portList$data$name
  dataBase$latitudAux = portList$data$lat

  #Dates
  datesVector = dataBase[, colDates]
  if(sum(is.na(datesVector)) != 0) {warning("Viajes sin el registro de la fecha")}
  datesVector = datesVector[datesVector != "" & !is.na(datesVector)]
  newDatesVector = as.Date(datesVector, format="%d/%m/%Y %H:%M:%S")

  yearVector  = as.numeric(format(newDatesVector, "%Y"))
  monthVector = as.numeric(format(newDatesVector, "%m"))
  dayVector   = as.numeric(format(newDatesVector, "%d"))

  #Fleet
  fleetVector = dataBase[, colStorageCapacity]
  if(sum(is.na(fleetVector)) != 0) {warning("Viajes sin el registro del tipo de flota")}

  dataBase$flota = cut(x  = fleetVector, breaks = c(0, 10, 32.5, 110, Inf),
                       labels = c("Artesanal", "Menor escala", "Industrial madera", "Industrial"))

  info = list(file   =  file,
              trips  =  length(unique(tripVector)),
              ports  =  length(unique(dataBase$puerto[!is.na(dataBase$puerto)])),
              years  =  unique(yearVector),
              months =  length(rle(monthVector)$values),
              fleets =  as.vector(unique(dataBase$flota[!is.na(dataBase$flota)])),
              colTrip = colTrip, colLat  = colLat, colHaul = colHaul, colLon  = colLon, colCala = colCala,
              colCapCala = colCapCala, capAnch = capAnch, capSar = capSar, capJur = capJur, capCab = capCab,
              capBon  = capBon)

  output = list(data = dataBase, info = info)
  class(output) = c("bitacora")
  return(output)

}

#Funcion para obtener los viajes observados
.observedTrip.bitacora = function(object, language) {

  dataBase = object$data
  colTrip  = object$info$colTrip
  dataBase = dataBase[, c(colTrip, "puerto", "flota", "latitudAux") ]
  dataBase = dataBase[!apply(dataBase == 0, 1, FUN = any, na.rm = TRUE),]

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
  colLat   = object$info$colLat
  colHaul  = object$info$colHaul

  dataBase = dataBase[, c(colLat, "latitudAux", colHaul, "flota") ]
  dataBase[, 1] = -abs(dataBase[, 1])
  colnames(dataBase) = c("lat", "latAux", "haul", "fleet")

  if(isTRUE(latByPort)) {
    indexLat = which(is.na(dataBase$lat))
    dataBase$lat[indexLat] = dataBase$latAux[indexLat]
  } else {
    dataBase = dataBase[!is.na(dataBase$lat), ]
  }

  dataBase = dataBase[!is.na(dataBase$haul),]
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
  dataBase = dataBase[, c(object$info$colLat, object$info$colLon, "flota", object$info$colCala,
                          object$info$colCapCala, catchSpecies$catch) ]
  colnames(dataBase)[1:5] = c("lat", "lon", "fleet", "num_haul", "catch_haul")

  #remove null catched
  dataBase = dataBase[!is.na(dataBase$num_haul) & dataBase$num_haul != 0, ]

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
                                       colMap = "khaki1",
                                       cexPointCatch = FALSE, cexPoint = 0.8,
                                       colFleet = c("red", "blue", "green", "black"),
                                       cex.axis = 1.2, cexPorts = 0.9, ...){

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


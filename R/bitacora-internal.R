

# Internal function for the class bitacora --------------------------------

#Funcion para obtener la data del programa de bitacoras (PBP)
.getBitacoraData = function(file, colPort, colDates, colTrip, colStorageCapacity,
                            colLat, colLon, colHaul, colCatchHaul,
                            capAnch, capSar, capJur, capCab, capBon, ...) {

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
              colTrip = colTrip, colLat  = colLat, colLon  = colLon, colHaul = colHaul, colCatchHaul = colCatchHaul,
              capAnch = capAnch, capSar = capSar, capJur = capJur, capCab = capCab, capBon  = capBon)

  output = list(data = dataBase, info = info)
  class(output) = c("bitacora")
  return(output)

}

#Funcion para obtener los viajes observados
.observedTrip.bitacora = function(object, language) {

  dataBase = object$data
  dataBase = dataBase[, c(object$info$colTrip, "puerto", "flota", "latitudAux") ]
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
  dataBase = dataBase[, c(object$info$colLat, "latitudAux", object$info$colHaul, "flota") ]
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

.plotFishingPresence.bitacora = function(x, byGroup = TRUE, group = NULL,
                                         cexPoint = 1, colMap = "khaki1",
                                         cex.axis = 1.2, cexPorts = 0.9,
                                         colSpecies = NULL, colLegend = NULL){

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
       xlab = "", ylab = "", col = colPlot, axes = FALSE, type = "p", pch = 16, cex = cexPoint)
  map(add = TRUE, fill = TRUE, col = colMap)
  box()

  axis(2, at = seq(from = -20, to = -2, by = 2), las = 2, cex.axis = cex.axis,
       labels = paste(seq(from = 20, to = 2, by = -2), "S", sep = " "))

  axis(1, at = seq(from = -82, to = -70, by = 2), cex.axis = cex.axis,
       labels = paste(seq(from = 82, to = 70, by = -2), "W", sep = " "))

  text(x = ports$lon, y = ports$lat, labels = ports$name, adj = -0.1, cex = cexPorts)

  legend("bottomleft", legend = namesLegend, col = colLegend,
         pch = 20, horiz = FALSE, bty = "n", cex = 1)

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
  if(language == "english"){colnames(dataTable)[1] = "Latitude"}

  return(dataTable)

}


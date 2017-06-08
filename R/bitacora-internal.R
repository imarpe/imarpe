

# Internal function for the class bitacora --------------------------------

#Funcion para obtener la data del programa de bitacoras (PBP)
.getBitacoraData = function(file, colPort, colDates, colTrip, colStorageCapacity, ...) {

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
              colTrip = colTrip)

  output = list(data = dataBase, info = info)
  class(output) = c("bitacora")
  return(output)

}

#Funcion para obtener los viajes observados
.observedTrip.bitacora = function(object, language){

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

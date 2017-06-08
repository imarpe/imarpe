

# Internal function for the class bitacora --------------------------------

#Funcion para obtener la data del programa de bitacoras (PBP)

.getBitacoraData = function(file = file,
                            colPort = "PUERTO_SALIDA", colDates = "DIA_SALIDA",
                            colTrip = "CODIGO_VIAJE", colStorageCapacity = "CAPACIDAD_BODEGA_REGISTRADA", ...) {

  dataBase = read.csv(file = file, header = TRUE, na.strings = "", stringsAsFactors = FALSE) # , ...

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
              fleets =  as.vector(unique(dataBase$flota[!is.na(dataBase$flota)])))

  output = list(data = dataBase, info = info)
  class(output) = c("bitacora")
  return(output)

}

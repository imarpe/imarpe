
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

getBitacorasData = function(file, colPort = "PUERTO_SALIDA", colDates = "DIA_SALIDA",
                            colTrip = "CODIGO_VIAJE",
                            colStorageCapacity = "CAPACIDAD_BODEGA_REGISTRADA",
                            colLat = "LATITUD_INICIAL",
                            colHaul = "NUMERO_CALA"){

  dataBase = .getBitacoraData(file = file, colPort = colPort, colDates = colDates, colTrip = colTrip,
                              colStorageCapacity = colStorageCapacity, colLat = colLat, colHaul = colHaul)
  return(dataBase)
}

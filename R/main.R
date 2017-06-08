
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

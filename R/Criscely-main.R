
.getSpeciesComposition.bitacoras = function(object, ...) {
  
  datos = object$data
  datos = datos[, c("anchoveta", "sardina", "jurel", "caballa", "otros")]
  datos = datos[!is.na(datos$anchoveta & datos$sardina & datos$jurel
                       & datos$caballa & datos$otros), ]
  datos = colSums(datos, na.rm=TRUE)
  
  species = tolower(names(datos))
  names(datos) = species
  
  porcentaje = 100*datos/sum(datos, na.rm=TRUE)
  
  #Para la tabla de profundidad
  tabla = data.frame(datos, porcentaje)
  tabla = rbind(tabla, colSums(tabla, na.rm=TRUE))
  colnames(tabla) = c("Capturas", "Porcentaje")
  rownames(tabla) = c(species, "Total")
  
  return(tabla)
}


.getEffort.bitacoras = function(object) {
  
  datos = object$data
  datos = datos[, c("puerto.salida", "dv", "cala", "total.calas", "motivo.viaje.cero")]
  datos = datos[!grepl("2", datos$motivo.viaje.cero), ] #Exonerando motivo de viaje cero
  
  duracionViaje = aggregate(datos$dv, by=list(datos$puerto.salida), FUN=mean, na.rm=TRUE)
  numeroCala = aggregate(datos$total.calas, by=list(datos$puerto.salida), FUN=mean, na.rm=TRUE)
  
  #Para la tabla de esfuerzo
  puerto=duracionViaje[,1]
  tabla = data.frame(duracion=duracionViaje$x, cala=numeroCala$x)
  
  basePuerto = c("paita", "bayobar", "parachique", "chicama", "coishco", "chimbote",
                 "samanco", "huarmey", "supe", "vegueta", "huacho", "chancay", "callao",
                 "tambo de mora", "pisco", "quilca", "la planchada", "mollendo", 
                 "ilo", "morrosama")
  ordenCol = basePuerto[sort(match(puerto, basePuerto), decreasing = FALSE)]
  
  tabla = tabla[match(ordenCol, puerto), ]
  rownames(tabla) = ordenCol

  return(tabla)
}


.getNumberObserver.bitacoras = function(object) {
  
  datos = object$data
  datos = datos[datos$observador != "" & datos$puerto.salida != "", ]
  datos = table(datos$observador, datos$puerto.salida) > 0
  
  numeroObservadores = as.numeric(apply(datos, 2, sum))
  
  #Para la tabla de numero de observadores
  puerto = names(apply(datos, 2, sum))
  tabla = data.frame(numeroObservadores)
    
  basePuerto = c("paita", "bayobar", "parachique", "chicama", "coishco", "chimbote",
                 "samanco", "huarmey", "supe", "vegueta", "huacho", "chancay", "callao",
                 "tambo de mora", "pisco", "quilca", "la planchada", "mollendo", 
                 "ilo", "morrosama")
  ordenCol = basePuerto[sort(match(puerto, basePuerto), decreasing = FALSE)]
  tabla = data.frame(tabla[match(ordenCol, puerto), ])
  colnames(tabla) = "N° de observadores"
  rownames(tabla) = ordenCol
  
  return(tabla)
  
}


.getNumberSet.bitacoras = function(object) {
  
  datos = object$data
  
  latitudes = table(datos$latGf)
  numeroLatitud = names(latitudes)
  latitudes = as.vector(latitudes)
  
  gradoLatitudinal = c(numeroLatitud, "Total")
  numeroCalas = c(latitudes, sum(latitudes))
  
  #Para la tabla de numero de calas
  tabla = data.frame(numeroCalas)
  colnames(tabla) = "N° de calas"
  rownames(tabla) =  gradoLatitudinal
  
  return(tabla)
  
}


.getDepth.bitacoras = function(object) {
  
  datos = object$data
  datos = datos[, c("latGf", "tope.sup", "tope.inf")]
  datos = datos[!is.na(datos$latGf & datos$tope.sup & datos$tope.inf), ]
  
  grado = unique(datos$latGf)
  grado = grado[order(grado, decreasing=FALSE)]
  constante = 1.8288 #constante de conversion
  
  topeSuperior = constante*aggregate(datos$tope.sup, by=list(datos$latGf), FUN=mean,
                                     na.rm=TRUE)
  topeInferior = constante*aggregate(datos$tope.inf, by=list(datos$latGf), FUN=mean,
                                     na.rm=TRUE)
  
  #Para la tabla de numero de calas
  tabla = data.frame(topeSuperior$x, topeInferior$x)
  tabla = cbind(tabla, rowMeans(tabla))
  colnames(tabla) = c("Tope superior", "Tope inferior", "Promedio")
  rownames(tabla) = grado
  
  return(tabla)
  
}


.getSumPorts.landings = function (object, ...) {
  
  datos = object$data
  ports = datos[,c(4:length(colnames(datos)))]
  
  tabla = data.frame(datos[, c(1:3)], apply(ports, 1, sum))
  colnames(tabla) = c("year", "month", "day", "Ports")
  
  return(tabla)
} 


.getPorts.landings = function (object, ...) {
  
  require(Hmisc)
  
  datos = object$data
  
  ports = datos[,c(4:length(colnames(datos)))]
  namesPorts = names(ports)
  
  tabla = data.frame(apply(ports,2,sum), row.names=NULL)
  colnames(tabla) = "Landings"
  rownames(tabla) = capitalize(namesPorts) 
  
  return(tabla)
}


.getMonth.landings = function (object, ...) {
  
  datos = object$data
  
  ports = datos[,c(4:length(colnames(datos)))]
  months = unique(datos$month)
  years = unique(datos$year)
  
  datos = data.frame(datos[, c(1:3)], apply(ports, 1, sum))
  colnames(datos) = c("year", "month", "day", "Ports")
  
  tabla = tapply(datos$Ports, list(datos$month, datos$year),
                 sum, na.remove=FALSE)
  
  monthsTable = row.names(tabla)
  sortMonth = sort(match(monthsTable, months), decreasing=FALSE)
  order = months[sortMonth]
  
  tabla = data.frame(tabla[order, ])
  rownames(tabla) = months
  colnames(tabla) = years
  
  return(tabla)  
}


.getYear.landing = function (object, ...) {
  
  datos = object$data
  
  ports = datos[,c(4:length(colnames(datos)))]
  
  datos = data.frame(datos[, c(1:3)], apply(ports, 1, sum))
  colnames(datos) = c("year", "month", "day", "Ports")
  
  years = unique(datos$year)
  
  tabla = tapply(datos$Ports, list(datos$year), sum, na.remove=FALSE)
  tabla = data.frame(tabla)
  colnames(tabla) = "Landings"
  rownames(tabla) = years
  
  return(tabla)
}


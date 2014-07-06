
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
  tabla = data.frame(Duracion=duracionViaje$x, Cala=numeroCala$x)
  
  basePuerto = c("paita", "bayobar", "parachique", "chicama", "coishco", "chimbote",
                 "samanco", "huarmey", "supe", "vegueta", "huacho", "chancay", "callao",
                 "tambo de mora", "pisco", "quilca", "la planchada", "mollendo", 
                 "ilo", "morrosama")
  ordenCol = basePuerto[sort(match(puerto, basePuerto), decreasing = FALSE)]
  
  tabla = tabla[match(ordenCol, puerto), ]
  rownames(tabla) = ordenCol
  
  return(tabla)
}


.plotEffort.bitacoras = function (x=x, ...) {
  
  datos = .getEffort.bitacoras(x)
  duracionViaje = as.vector(datos[,1])
  duracionViaje = as.numeric(duracionViaje)
  
  lineRange = as.numeric(datos[,2])
  lineValues = (lineRange*35)/ceiling(max(lineRange))
  
  #Para la grafica de esfuerzo
  par(mar=c(4.1, 4.1, 4.1, 4.1), xpd=TRUE)
  
  barplot(duracionViaje, main = "", xlab = "Puertos", 
          ylab = "Duraci\u{F3}n de viaje promedio", col = "red",
          names.arg = rownames(datos), beside = FALSE, xlim = NULL,
          ylim = c(0, ceiling(1.3*max(duracionViaje))))
  
  lines(seq(0.6, 5.6, 1.25), lineValues, col = "dark blue", lwd = 2.5,
        xlab = NA, xlim = NULL, ylab = NULL, yaxs = "i", xaxs = "i",
        ylim = c(0, ceiling(1.3*max(as.numeric(duracionViaje)))) )
  
  axis(side = 4, at = seq(0, ceiling(1.3*max(as.numeric(duracionViaje))),
                          length.out = ceiling(max(lineRange)) + 1),
       labels = seq(0, ceiling(max(lineRange))))
  mtext(side = 4, line = 3, "Cala")
  legend("topleft", legend= c("Duraci\u{F3}n de viaje promedio", "N\u{B0} de calas promedio"),
         col = c("red", "blue"), pch = 15, bty = "n", cex=0.85)
  box()
  
  return(invisible())
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
  colnames(tabla) = "N\u{B0} de observadores"
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
  colnames(tabla) = "N\u{B0} de calas"
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


.plotDepth.bitacoras = function (object, ...) {
  
  datos = object$data
  datos = datos[, c("latGf", "tope.sup", "tope.inf")]
  datos = datos[!is.na(datos$latGf & datos$tope.sup & datos$tope.inf), ]
  
  datosLatitudes = datos[order(datos$latGf), ]
  mean = apply(datosLatitudes[,c(2,3)], 1, mean)
  constante = 1.8288
  datos = data.frame(lat=datosLatitudes[,1], valores = mean*constante)
  
  ylim = c(0, max(datos$valores))
  if(is.null(ylim))
    ylim = rev(range(mean)) else
      ylim = rev(ylim)
  
  boxplot(valores~lat, datos, outline = FALSE, xlab = "Latitud",
          ylab = "Profundidad", ylim = ylim, col = "red", main = "", axes=FALSE)
  axis(2, at=axTicks(2), label=axTicks(2), las=2)
  axis(1, at=axTicks(1), label=coord2text(-unique(datos$lat),"lat"))
  box()
  
  return(invisible())
  
}
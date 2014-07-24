
.getSpeciesComposition.bitacoras = function(object, ...) {
  
  datos = object$data
  datos = datos[, c("anchoveta", "sardina", "jurel", "caballa", "otros")]
  datos = datos[!is.na(datos$anchoveta & datos$sardina & datos$jurel
                       & datos$caballa & datos$otros), ]
  datos = colSums(datos, na.rm=TRUE)
  
  species = tolower(names(datos))
  names(datos) = species
  
  porcentaje = 100*datos/sum(datos, na.rm=TRUE)
  
  # For the table of composition
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

# It needs to be completed automated
.plotEffort.bitacoras = function (x=x,main=NULL,xlab=NULL,ylab=NULL,legend=NULL,col=NULL) {
  
  datos = .getEffort.bitacoras(x)
  duracionViaje = as.vector(datos[,1])
  #duracionViaje = as.numeric(duracionViaje)
  
  lineRange = as.numeric(datos[,2])
  lineValues = (lineRange*ceiling(max(duracionViaje)))/ceiling(max(lineRange))
  
  #Para la grafica de esfuerzo
  opar=par(mar=c(4.1, 4.1, 4.1, 4.1), xpd=TRUE)
  if(is.null(main)) main="Principales Caracter\u{ED}sticas del Esfuerzo Pesquero"
  if(is.null(xlab)) xlab="Puertos"
  if(is.null(ylab)){ ylab="Duraci\u{F3}n promedio de viaje";ylab2=NULL}
  else {ylab2=ylab[2];ylab=ylab[1]}
  if(is.null(col)){ col1="red";col2="dark blue"}
  else {col1=col[1];col2=col[2]}
  barplot(duracionViaje, main = main,
          xlab = xlab, ylab = ylab, col = col1,
          names.arg = .capwords(rownames(datos)), beside = FALSE, xlim = NULL,
          ylim = c(0, ceiling(1.3*max(duracionViaje))))
  
  lines(seq(0.7, 1.2*length(duracionViaje),1.2), lineValues, col = col2, lwd = 2.5,
        xlab = NA, xlim = NULL, ylab = NULL, yaxs = "i", xaxs = "i",
        ylim = c(0, ceiling(1.3*max(as.numeric(duracionViaje)))) )
  
  axis(side = 4, at = seq(0, max(lineRange)*1.3,
                          0.5*(round(max(lineRange)*1.3/4)))*ceiling(max(duracionViaje))/ceiling(max(lineRange)),
       labels = seq(0, max(lineRange)*1.3,0.5*(round(max(lineRange)*1.3/4))),col="blue",col.axis="blue")
  if(is.null(ylab2)) ylab2="Cala"
  mtext(side = 4, line = 3, ylab2,col="blue")
  if(is.null(legend)) legend=c("Duraci\u{F3}n promedio de viaje", "N\u{B0} de calas promedio")
  legend("topleft", legend= legend,
         col = c("black", col2), pch = c(22,46),pt.bg=col1,lty=c(0,1),lwd = c(1,2.5), bty = "n", pt.cex=c(3,0.85),cex=0.85)
  box()
  par(opar)
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

# Getting number of coves by latitude
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
  constante = 1.8288 #constante de conversion: 6 feets = 1.8288 meters
  
  topeSuperior = constante*aggregate(datos$tope.sup, by=list(datos$latGf), FUN=mean,
                                     na.rm=TRUE)
  topeInferior = constante*aggregate(datos$tope.inf, by=list(datos$latGf), FUN=mean,
                                     na.rm=TRUE)
  
  # For the table of Depth
  tabla = data.frame(topeSuperior$x, topeInferior$x)
  tabla = cbind(tabla, rowMeans(tabla))
  colnames(tabla) = c("Tope superior", "Tope inferior", "Promedio")
  rownames(tabla) = grado
  
  return(tabla)
  
}


.plotDepth.bitacoras = function (object,detailed,main=NULL,xlab=NULL,ylab=NULL,col=NULL,...) {
  
  datos = object$data
  datos = datos[, c("latGf", "tope.sup", "tope.inf")]
  datos = datos[!is.na(datos$latGf & datos$tope.sup & datos$tope.inf), ]
  
  datosLatitudes = datos[order(datos$latGf), ]
  mean = apply(datosLatitudes[,c(2,3)], 1, mean)
  constante = 1.8288 #constante de conversion: 6 feets = 1.8288 meters
  datos = data.frame(lat=datosLatitudes[,1], valores = mean*constante)
  
  ylim = c(0, max(datos$valores))
  if(is.null(ylim))
    ylim = rev(range(mean)) else
      ylim = rev(ylim)
  if(is.null(main)) main="Profundidad de Cala seg\u{FA}n Latitud"
  if(is.null(xlab)) xlab="Latitud"
  if(is.null(ylab)) ylab="Profundidad (m)"
  if(is.null(col)) col=rgb(1,0,0,0.3)
  opar=par(xaxs='i')
  BoxInfo=boxplot(valores~lat, datos, outline = FALSE, xlab = xlab,
          ylab = ylab, ylim = ylim, col = col, 
          main = main, axes=FALSE)#,width=as.numeric(table(datos$lat))
  if(detailed==TRUE){
  set.seed(1)
  auxdata=data.frame(order=cumsum(!duplicated(datos$lat)),x=cumsum(!duplicated(datos$lat))+runif(nrow(datos),-0.4,0.4),y=datos$valores)
  auxdata$Low=BoxInfo$stats[1,][auxdata$order]
  auxdata$Up=BoxInfo$stats[5,][auxdata$order]
  auxdata$Out=(auxdata$y<auxdata$Low)|(auxdata$y>auxdata$Up)
  auxdata$x[auxdata$Out]=auxdata$order[auxdata$Out]
  points(auxdata$x[!auxdata$Out],auxdata$y[!auxdata$Out],pch=20,col=rgb(0.2,0.2,0.2,0.3))
  points(auxdata$x[auxdata$Out],auxdata$y[auxdata$Out],pch=20)
  abline(v=1:(length(BoxInfo$n)-1)+0.5)
  }
  axis(2, at=axTicks(2), labels=axTicks(2), las=2)
  axis(1, at=axTicks(1), labels=coord2text(-unique(datos$lat),"lat"))#need to be generalized
  box()
  par(opar)
  return(invisible())
}
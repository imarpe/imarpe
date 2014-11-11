
.getBoats.survey = function(object, ...) {
  
  datos = object$data
  
  nameBarcos = unique(as.character(datos$barco))
  nBarcos = length(nameBarcos)
  
  nCalasbyBarco = numeric(nBarcos)
  minLatbyBarco = numeric(nBarcos)
  maxLatbyBarco = numeric(nBarcos)
  for(i in seq(nBarcos)){
    nCalasbyBarco[i] = length(which(datos$barco == nameBarcos[i])) 
    minLatbyBarco[i] = round(-1 * max(subset(datos, datos$barco == nameBarcos[i])$latitud, na.rm = TRUE), 2) 
    maxLatbyBarco[i] = round(-1 * min(subset(datos, datos$barco == nameBarcos[i])$latitud, na.rm = TRUE), 2) 
  }
  
  tabla = data.frame(barco = nameBarcos, n_calas = nCalasbyBarco, 
                          min.Latitud = minLatbyBarco, max.Latitud = maxLatbyBarco)
  colnames(tabla) = c("Barco", "Cantidad de lances", "Desde la latitud",
                      "Hasta la latitud") 
  rownames(tabla) = as.character(1:nBarcos)
  
  return(tabla)
}


.getNumberTrawl.survey = function(object) {
  
  datos = object$data
  
  
  latitudes = table(ceiling(datos$latitud))
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


.getComposition.survey = function(object) {
  
  datos = object$data
  fSpecie = object$firstSp
  lSpecie = object$lastSp
  
  matrixSp = datos[, (which(names(datos == fSpecie)):
                     which(names(datos == lSpecie)))]
  
  suma = colSums(matrixSp)
  tabla = sort(suma, decreasing = TRUE)[1:5]
  tabla = t(t(tabla))
  colnames(tabla) = c("Peso (kg)")
  
  return(tabla)
}


.getDiversity.survey = function(object) {
  
  datos = object$data
  fSpecie = object$firstSp
  lSpecie = object$lastSp
  
  matrixSp = datos[, (which(names(datos == fSpecie)):
                      which(names(datos == lSpecie)))]
  richness = specnumber(colSums(matrixSp))
  simpson = diversity(colSums(matrixSp), index = "simpson")
  shannon = diversity(colSums(matrixSp), index = "shannon")
  pielou = shannon / log(richness)
  
  tabla = data.frame(richness, simpson, shannon, pielou)
  tabla = t(tabla)
  rownames(tabla) = c("Riqueza", "Ind Simpson", "Ind Shannon",
                      "Equitabilidad de Pielou")
  colnames(tabla) = c("Valor")
  
  return(tabla)
}


#  ------------------------------------------------------------------------


.plotDepth.bitacoras = function (object,detailed,main=NULL,xlab=NULL,ylab=NULL,col=NULL,...) {

  datos = object$data
  datos = datos[, c("barco", "longitud", "latitud")]
  datos = datos[!is.na(datos$barco & datos$latitud & datos$longitud), ]
  
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

#Producto3: N° de viajes
##Input: Base-completa3
require(Hmisc)

file="base-completa_4.csv"

#FUNCION
.getTravelData.bitacoras = function(object, ...) {
  
  #datos = object$data
  datos = read.csv(file, stringsAsFactors = FALSE)
  datos = datos[,c("dia.salida", "puerto.salida")]
  datos = datos[datos$dia.salida != "" & datos$puerto.salida != "", ]
  datos = datos[!is.na(datos$dia.salida), ]
  datos = datos[!is.na(datos$puerto.salida), ]
  
  datosYear = datos$dia.salida
  length(datosYear)
  datosDate = NULL
  for(i in seq_along(datosYear)) {
    datosdate = datosYear[[i]][1]
    datosDate = c(datosDate,datosdate)
  }
  datosDate = as.Date(datosDate, format=c("%d/%m/%Y"))
  years = as.numeric(format(datosDate, "%Y"))
  months = as.numeric(format(datosDate, "%m"))
  
  datos = data.frame(years, months, datos$puerto.salida)
  colnames(datos) = c("year", "months", "output_harbor")
  
  return(datos)
}

########Numero de viajes por puerto de salida
.getTravelPort.bitacoras = function(object, ...) {
  
  datos = .getTravelData.bitacoras(file="base-completa_4.csv")
  #datos = .getTravelData.bitacoras
  years = sort(unique(datos$year))
  tabla = table(datos$output_harbor, datos$year)
  harbor = rownames(tabla)
  baseHarbor = c("paita", "bayovar", "parachique", "chicama", "salaverry", "coishco", "chimbote",
                 "samanco", "casma", "huarmey", "supe", "vegueta", "huacho", "chancay", "callao",
                 "tambo de mora", "pisco", "atico", "quilca", "la planchada", "matarani", "mollendo", 
                 "ilo", "morrosama")
  posicion = sort(match(harbor, baseHarbor), decreasing=FALSE)
  ordenCol = baseHarbor[posicion]
  tabla = tabla[match(ordenCol, harbor), ]
  
  tabla = data.frame(tabla)
  totalHarbor = apply(tabla,1,sum)
  tabla = cbind(tabla,totalHarbor)
  colnames(tabla) = c(years, "Total")
  
  totalYear = apply(tabla,2,sum)
  tabla = rbind(tabla,totalYear)
  rownames(tabla) = c(capitalize(ordenCol), "Total")
  
  return(tabla)
}


#Gráfica: N° de viajes por zona (NC/ S)

tabla = .getTravelPort.bitacoras(file="base-completa_4.csv")

norteCentro = apply(tabla[c(1:match("Pisco",rownames(tabla))),],2,sum)
sur = apply(tabla[c((match("Pisco",rownames(tabla))+1):(length(rownames(tabla))-1)), ],2,sum)
zonas = rbind(norteCentro, sur)
zonas = zonas[, c(1:dim(zonas)[2]) ]
rownames(zonas) = c("NC","S")

par(mar=c(4.3, 4.1, 4.1, 4.3), xpd=TRUE)

barplot(as.matrix(zonas[,1:dim(zonas)[2]-1]), main="Número de viajes por Región",
        xlab="Años", ylab="Número de viajes", col=c("red","blue"), axes=F,
        ylim=c(0,max(zonas[,1:dim(zonas)[2]-1])*1.7))
axis(2, at=seq(0,2000,by=200), labels=seq(0,2000,by=200), las=2)
legend("topleft", c("NC","S"), fill=c("red","blue"), bty = "n",cex=0.7)
box()

#Linea de viajes totales
viajesTotales = read.csv("viajes_total.csv", stringsAsFactors = FALSE)
viajesTotales = viajesTotales[, 2] 
viajesBitacoras = tabla[dim(tabla)[1], c(1:dim(tabla)[2]-1)]

relacion = (viajesBitacoras/viajesTotales)*100
lineRange = as.numeric(relacion) 
lineValues = (lineRange*2000)/ceiling(max(lineRange))

lines(seq(0.7,21,length.out=18),lineValues, col="black", lwd=2.5,
      xlab=NA,ylab=NA,xlim=NA, ylim=c(0,ceiling(max(relacion))), yaxs="i", xaxs="i")
axis(4, at = seq(0, 2000,length.out= 10), labels = seq(0,9), las=2)
mtext(side = 4, line = 3, "% Bitácoras/Viajes Totales ")

#Para la segunda gráfica:
tablaPuerto = tabla[c(1:(dim(tabla)[1]-1)),dim(tabla)[2]]

par(mar=c(5.1, 4.1, 4.1, 2.1)+0.5)
barplot(tablaPuerto, main="Número de viajes por puerto", xlab="Puertos",
        ylab="Número de viajes", col=rep(c("red","blue"), c(17,6)),
        axes=F, ylim=c(0,max(tablaPuerto)*1.2),
        names.arg=FALSE, cex.names=0.7)
axis(1, at=seq(0.7,by=1.2, length.out=23), labels=rownames(tabla)[1:23], las=2,
     cex.axis=0.7)
axis(2, at=seq(0,2800,by=200), labels=seq(0,2800,by=200), las=2, cex.axis=0.7)
legend("topleft", c("NC","S"), fill=c("red","blue"), bty = "n",cex=0.7)
box()


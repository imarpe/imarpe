# functions used for the sisesat class but not exported (available to the user)

.mapa.peru <- function(xlim=c(-84,-70), ylim=c(-21, -3), labelsxy = TRUE, axis4L = FALSE, perfil = FALSE,
                      land.col="khaki1", sea.col="white", cex.Port = 0.65, add = FALSE, ...){
  
  require(kali)
  
  axis.Lon <- paste(abs(seq(xlim[1],xlim[2],by = 2)),"?W")
  axis.Lat <- paste(abs(seq(ylim[1],ylim[2],by = 2)),"?S")
  
  Encoding(axis.Lon) <- "UTF-8"
  Encoding(axis.Lat) <- "UTF-8"
  
  plot.map(axes = F,col="red", cex=1, xlim=xlim, hires = TRUE, ylab = NULL, xlab = NULL, xaxs = "i", yaxs = "i", 
           ylim=ylim, land.col=land.col, sea.col=sea.col, 
           boundaries.col = NA, grid.col = "blue",main="", 
           grid = FALSE, add = add)
  lines(linePeru$lon, linePeru$lat, col="gray40")
  
  if(isTRUE(labelsxy)){
    mtext("Longitud", side=1, line=1.5, cex=0.8)
    mtext("Latitud", side=2, line=1.8, cex=0.8)
  }
  principalP = puertosPeru[c(2,4,5,7,8,10,12,14,16,17,19),]
  text(principalP$lon, principalP$lat, labels = principalP$puertos, cex=cex.Port, pos=4, font=1)
  
  # axis
  axis(2,seq(ylim[1],ylim[2],by = 2), axis.Lat, las=1, cex.axis=0.6, hadj=0.5, tck=-0.010)

  if(!isTRUE(perfil)){
  axis(1,seq(xlim[1],xlim[2],by = 2), tck=-0.01, labels = NA, hadj=0.5)
  axis(1,seq(xlim[1],xlim[2],by = 2), labels = axis.Lon, cex.axis=0.6, line = -0.8, lwd = 0)
  }
  
  if(axis4L == TRUE){
  axis(3,seq(xlim[1],xlim[2],by = 2), tck=-0.01, labels = NA, hadj=0.5)
  axis(3,seq(xlim[1],xlim[2],by = 2),labels = axis.Lon, cex.axis=0.6, line = -0.5, lwd = 0)
  axis(4,seq(ylim[1],ylim[2],by = 2), axis.Lat, las=1, cex.axis=0.6, hadj=0.5, tck=-0.010)
  }
  
  #return(invisible)
}

.mapa.peru.simple <- function(xlim=c(-84,-70), ylim=c(-21, -3), labelsxy = TRUE, axis4L = FALSE, perfil = FALSE,
                             col="khaki1", border = "khaki1", sea.col="white", cex.Port = 0.65, add = FALSE){
  
  require(maps)
  require(mapdata)
  
  axis.Lon <- paste(abs(seq(xlim[1],xlim[2],by = 2)),"?W")
  axis.Lat <- paste(abs(seq(ylim[1],ylim[2],by = 2)),"?S")
  
  Encoding(axis.Lon) <- "UTF-8"
  Encoding(axis.Lat) <- "UTF-8"
  
  map("worldHires",fill=T, myborder = FALSE, col = col, border = border,
      xlim = xlim, ylim = ylim, add = add)
  
  lines(linePeru$lon, linePeru$lat, col="gray40")
  
  if(isTRUE(labelsxy)){
    mtext("Longitud", side=1, line=1.5, cex=0.8)
    mtext("Latitud", side=2, line=1.8, cex=0.8)
  }
  principalP = puertosPeru[c(2,4,5,7,8,10,12,14,16,17,19),]
  text(principalP$lon, principalP$lat, labels = principalP$puertos, cex=cex.Port, pos=4, font=1)
  
  # axis
  axis(2,seq(ylim[1],ylim[2],by = 2), axis.Lat, las=1, cex.axis=0.6, hadj=0.5, tck=-0.010)
  
  if(!isTRUE(perfil)){
    axis(1,seq(xlim[1],xlim[2],by = 2), tck=-0.01, labels = NA, hadj=0.5)
    axis(1,seq(xlim[1],xlim[2],by = 2), labels = axis.Lon, cex.axis=0.6, line = -0.8, lwd = 0)
  }
  
  if(axis4L == TRUE){
    axis(3,seq(xlim[1],xlim[2],by = 2), tck=-0.01, labels = NA, hadj=0.5)
    axis(3,seq(xlim[1],xlim[2],by = 2),labels = axis.Lon, cex.axis=0.6, line = -0.5, lwd = 0)
    axis(4,seq(ylim[1],ylim[2],by = 2), axis.Lat, las=1, cex.axis=0.6, hadj=0.5, tck=-0.010)
  }
  box()
  #return(invisible)
}

.estima_semana <- function(inicio.temp, fin.temp, ...){
  require(lubridate)
  
  diasTemporada = seq.Date(as.Date(inicio.temp), as.Date(fin.temp),by =  "day")
  dias =  weekdays(diasTemporada)
  temporada = data.frame(diasTemporada = diasTemporada, dias = dias)
  nSem = c(rep(0, min(which(dias == "lunes"))-1), sort(rep(1:length(dias[dias=="lunes"]),7)))
  temporada$semana <- nSem[1:length(dias)]  
  return(temporada)
}

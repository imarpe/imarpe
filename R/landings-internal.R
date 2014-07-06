
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

.trimData.landings = function(x, start, end) {
  datos = x
  months = datos$month
  monthsPosition = unique(months)
  dataDate = paste(datos$year,"-",match(months, monthsPosition), "-",datos$day,sep="")
  data = data.frame(dataDate, datos$Ports)
  
  data$date = as.Date(as.character(data$dataDate), format="%Y-%m-%d")
  data = subset(data, data$date >= as.Date(start) & data$date <= as.Date(end))
  
  dataDate = strptime(data$date, format="%Y-%m-%d")
  years = format(dataDate, "%Y")
  months = as.numeric(format(dataDate, "%m"))
  days = format(dataDate, "%d")
  
  monthsName =NULL
  for(i in 1:length(months)) {
    monthsNumber = monthsPosition[months[i]]
    monthsName = c(monthsName, monthsNumber)
  }
  months = monthsName
  
  tabla = data.frame(years, months, days, data$datos.Ports)
  names(tabla) = c("year", "month", "day", "Ports")
  
  return(tabla)
}

.plotDays.landings = function (x, start=NULL, end=NULL, ...) {
  datos = .getSumPorts.landings(x)
  datos = .trimData.landings(datos, start=start, end=end)
  days = paste0(datos$day,"-",capitalize(datos$month))
  daysToPlot = c(1,7,15,21) #dias que seran ploteados
  daysToPlot = which(datos$day %in% daysToPlot) #posicion de los dias que seran ploteados
  daysToPlot = days[daysToPlot] #formato de los dias que seran ploteados
  
  days[! days %in% daysToPlot] = NA
  
  barplot(datos$Ports, main="Daily landing", xlab="Days",
          ylab="Landing (t)", col="blue", names.arg = FALSE,
          ylim=c(0,max(datos$Ports)*1.2), cex.names=0.7, axes=FALSE)
  axis(1, at=seq(1, by=1.2, length.out=length(days)), labels=days, las=2,cex.axis=0.7)
  axis(2, las=2, cex.axis=0.7)
  box()
  
  return(invisible())
  
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

.plotMonths.landings = function (x, ...) {
  
  datos = .getMonth.landings(x)
  years = as.numeric(colnames(datos))
  monthPlot = NULL
  for(i in 1:length(years) ) {
    monthPort = datos[,i]
    monthPlot = c(monthPlot, monthPort)
  }
  monthPlot = monthPlot[!is.na(monthPlot)]
  namesMonthPlot = capitalize(rep(rownames(datos), length.out = length(monthPlot) ))
  
  barplot(monthPlot, main="",
          xlab="Months", ylab="Landings (t)", col="blue", names.arg=FALSE,
          ylim=c(0, max(monthPlot)*1.2), cex.names=0.7, axes=FALSE)
  axis(1, at=seq(0.7, by=1.2, length.out=length(monthPlot)), labels=namesMonthPlot,
       las=1, cex.axis=0.8, line=0)
  axis(1, at=seq(0.7,by=1.2, length.out=length(monthPlot)),
       labels=rep(years,each=12)[1:length(monthPlot)], las=1, cex.axis=0.8, line=1, tick=FALSE)
  axis(2, las=2, cex.axis=0.8)
  box()
  
  return(invisible())
  
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

.plotYears.landings = function (x, ...) {
  
  datos = .getYear.landing(x)
  years = as.numeric(rownames(datos))
  barplot(datos$Landings, main="", xlab="Years",
          ylab="Landings (t)", col="blue", names.arg=FALSE,
          ylim=c(0,max(datos)*1.2), cex.names=0.7, axes=FALSE)
  axis(1, at=seq(0.7, by=1.2, length.out=length(years)), labels=years, las=1,
       cex.axis=0.8)
  axis(2, las=2, cex.axis=0.8)
  box()
  
  return(invisible())
 
}

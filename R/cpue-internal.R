
# Internal functions for the class cpue -----------------------------------

#Funcion para obtener la data para la captura por unidad de esfuerzo (CPUE)
.getCPUEData = function(x, fileName, fleet, toTons, sp, efforType, start, end, port, varType){

  fleeTable = fleet
  dataBase  = .readSegFile(file = x)
  dataBase[is.na(dataBase)] = 0

  #corte en el tiempo
  dataBase = .trimData(x = dataBase, start = start, end = end)

  #corte por puertos
  vectorPorts = colsplit(string = names(dataBase)[-c(1:3)], pattern="_", names=c("part1", "part2"))[,1]
  if(is.null(port)){dataBase = dataBase
  } else {dataBase = dataBase[, c(1:3, c(which(vectorPorts == port)+3))]}

  baseCatch  = dataBase[seq(4, length(colnames(dataBase)), by = 2)]/ifelse(isTRUE(toTons), 1000, 1)
  baseEffort = dataBase[seq(5, length(colnames(dataBase)), by = 2)]
  baseCpue   = baseCatch / baseEffort ; baseCpue[is.na(baseCpue)] = 0

  #CPUE total (promedios sobre los puertos)
  cpueTotal = data.frame(dataBase[1:3], cpue = rowMeans(baseCpue))

  #CPUE by port and by day
  cpuePortDay = baseCpue
  namesPorts = colsplit(string = names(cpuePortDay), pattern="_", names=c("part1", "part2"))[,1]
  colnames(cpuePortDay) = capitalize(namesPorts)
  cpuePortDay = data.frame(dataBase[1:3], cpuePortDay)

  #CPUE by port (promedio sobre todo el tiempo)
  cpuePort = data.frame(colMeans(baseCpue))
  rownames(cpuePort) = capitalize(namesPorts)
  colnames(cpuePort) = ("Cpue")

  ports = rownames(cpuePort)
  portData = getPort(myPorts = ports)
  rownames(cpuePort) = portData$data$name
  cpuePort$lat = portData$data$lat
  cpuePort = cpuePort[with(cpuePort, order(cpuePort$lat, decreasing = TRUE)), ]
  cpuePort$lat = NULL

  #Output
  info = list(file        = fileName,
              records     = nrow(cpueTotal),
              months      = length(rle(cpueTotal$month)$values),
              years       = length(unique(cpueTotal$year)),
              ports       = length(rownames(cpuePort)),
              sp          = sp,
              varType     = varType,
              efforType   = efforType)

  output = list(data = cpueTotal, dataPortDay = cpuePortDay, dataPort = cpuePort,
                info = info, fleeTable = fleeTable, efforType)

  class(output) = "cpue"
  return(output)

}

#Funcion para obtener los cpues promedios por meses
.getMonth.cpue = function (object, language){

  dataBase = object$data

  dataTable = tapply(dataBase[,4], list(dataBase[,2], dataBase[,1]), mean, na.remove=FALSE)
  dataTable[is.na(dataTable)] = 0
  years = colnames(dataTable)

  #order month (by default those are in english)
  monthsTable   = row.names(dataTable)
  vectorMonths  = month.abb

  sortMonth   = sort(match(monthsTable, vectorMonths), decreasing = FALSE)
  order       = vectorMonths[sortMonth]
  dataTable = dataTable[order, ]
  dataTable = as.data.frame(dataTable)
  colnames(dataTable) = unique(years)

  if(language == "english"){
    rownames(dataTable) = month.abb[sortMonth]
    colnames(dataTable) = years
  } else {
    rownames(dataTable) = engToSpa(month.abb[sortMonth])
    colnames(dataTable) = years
  }

  dataTable[is.na(dataTable)] = 0

  return(dataTable)
}

#Funcion para obtener los cpues totales por anhos
.getYear.cpue = function(object){

  dataBase    = object$data
  vectorYears = unique(dataBase[,1])

  dataTable  = tapply(dataBase[,4], list(dataBase[,1]), mean, na.remove=FALSE)
  dataTable  = data.frame(dataTable)
  colnames(dataTable) = "Cpue"

  return(dataTable)
}


#GRAFICAS
#Funcion para plotear el cpue diario
.plotDays.cpue = function(x, main = NULL, xlab = NULL, ylab = NULL, language, col = "blue",
                          daysToPlot, cex.axis = 0.8, cex.names=0.7, cex.main  = 1, ...){

  dataBase = x$data

  if(unique(daysToPlot %in% "all")){
    vectorDays = paste0(as.character(dataBase[,3]),"-", capitalize(as.character(dataBase[,2])))
  } else {
    vectorDays = paste0(as.character(dataBase[,3]),"-", capitalize(as.character(dataBase[,2])))
    daysToPlot = which(as.numeric(dataBase[,3]) %in% daysToPlot)
    daysToPlot = vectorDays[daysToPlot]
    vectorDays[! vectorDays %in% daysToPlot] = NA
  }

  if(is.null(main)){
    if(language == "english"){main = "Daily cpue"} else {main = "Cpue diario"}}

  efforType  = x$info$efforType
  if(is.null(ylab)){
    if(efforType == "viaje") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", viajes^-1, ")"))} else {ylab = expression(paste("Cpue (", "t . ", travels^-1, ")")) }}

    if(efforType == "capacidad_bodega") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", m^-3, ")"))}  else {ylab = expression(paste("Cpue (", "t . ", m^-3, ")")) }}

    if(efforType == "anzuelos") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", anzuelos^-1, ")")) } else {ylab = expression(paste("Cpue (", "t . ", "fish-", hook^-1, ")")) }}

    if(efforType == "embarcaciones") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", embarcaciones^-1, ")")) } else {ylab = expression(paste("Cpue (", "t . ", boats^-1, ")")) }}}

  par(mar = c(4,4.5, 2, 0.5))
  barplot(dataBase$cpue, main=main, xlab=xlab, ylab=ylab, col=col, names.arg = FALSE,
          ylim=c(0,max(dataBase$cpue)*1.2), cex.names=cex.names, axes=FALSE, cex.main = cex.main, ...)
  AxisDate = seq(0.7, by=1.2, length.out=length(vectorDays))
  NonNa =! is.na(vectorDays)
  axis(1, at=AxisDate[NonNa], labels=vectorDays[NonNa], las=2, cex.axis=cex.axis)
  axis(2, las=2, cex.axis=cex.axis)
  box()

  return(invisible())

}

#Funcion para plotear el cpue mensual
.plotMonths.cpue = function(x, main=NULL, xlab=NULL, ylab=NULL, language, col = "blue",
                            cex.axis = 0.8, cex.names=0.7, cex.main = 1, ...){

  dataBase = .getMonth.cpue(object = x, language = language)
  vectorYears = as.numeric(colnames(dataBase))

  monthPlot = as.vector(as.matrix(dataBase[, colnames(dataBase)]))
  monthPlot = monthPlot[!is.na(monthPlot)]
  namesMonthPlot = capitalize(rep(rownames(dataBase), length.out = length(monthPlot)))

  if(is.null(main)){
    if(language == "english"){main = "Montly Cpue"} else {main = "Cpue mensual"}}

  efforType = x$info$efforType
  if(is.null(ylab)){
    if(efforType == "viaje") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", viajes^-1, ")"))} else {ylab = expression(paste("Cpue (", "t . ", travels^-1, ")")) }}

    if(efforType == "capacidad_bodega") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", m^-3, ")"))}  else {ylab = expression(paste("Cpue (", "t . ", m^-3, ")")) }}

    if(efforType == "anzuelos") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", anzuelos^-1, ")")) } else {ylab = expression(paste("Cpue (", "t . ", "fish-", hook^-1, ")")) }}

    if(efforType == "embarcaciones") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", embarcaciones^-1, ")")) } else {ylab = expression(paste("Cpue (", "t . ", boats^-1, ")")) }}}

  par(mar = c(4, 4.5, 2, 0.5))
  barplot(monthPlot, main=main, xlab=xlab, ylab=ylab, col=col, names.arg=FALSE,
          ylim=c(0, max(monthPlot)*1.2), cex.names=cex.names, axes=FALSE, cex.main = cex.main, ...)
  axis(1, at=seq(0.7, by=1.2, length.out=length(monthPlot)), labels=namesMonthPlot,
       las=1, cex.axis=cex.axis, line=0)
  axis(1, at=seq(0.7,by=1.2, length.out=length(monthPlot)),
       labels=rep(vectorYears,each=12)[1:length(monthPlot)], las=1, cex.axis=cex.axis, line=1, tick=FALSE)
  axis(2, las=2, cex.axis=cex.axis)
  box()

  return(invisible())

}

#Funcion para plotear el cpue anual
.plotYears.cpue = function(x, main=NULL, xlab=NULL, ylab=NULL, language, col = "blue",
                           cex.axis = 0.8, cex.names=0.7, cex.main = 1, ...){

  dataBase = .getYear.cpue(object = x)
  vectorYears = as.numeric(rownames(dataBase))

  if(is.null(main)){
    if(language == "english"){main = "Yearly cpue"} else {main = "Cpue anual"}}

  efforType  = x$info$efforType
  if(is.null(ylab)){
    if(efforType == "viaje") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", viajes^-1, ")"))} else {ylab = expression(paste("Cpue (", "t . ", travels^-1, ")")) }}

    if(efforType == "capacidad_bodega") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", m^-3, ")"))}  else {ylab = expression(paste("Cpue (", "t . ", m^-3, ")")) }}

    if(efforType == "anzuelos") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", anzuelos^-1, ")")) } else {ylab = expression(paste("Cpue (", "t . ", "fish-", hook^-1, ")")) }}

    if(efforType == "embarcaciones") {
      if(language == "spanish") {
        ylab = expression(paste("Cpue (", "t . ", embarcaciones^-1, ")")) } else {ylab = expression(paste("Cpue (", "t . ", boats^-1, ")")) }}}

  par(mar = c(2.5, 4.5, 2, 0.5))
  barplot(dataBase$Cpue, main=main, xlab=xlab, ylab=ylab, col=col, names.arg=FALSE,
          ylim=c(0,max(dataBase)*1.2), cex.names=cex.names, axes=FALSE, cex.main = cex.main, ...)
  axis(1, at=seq(0.7, by=1.2, length.out=length(vectorYears)), labels=vectorYears, las=1,
       cex.axis=cex.axis)
  axis(2, las=2, cex.axis=cex.axis)
  box()

  return(invisible())

}


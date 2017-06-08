
# Aditional functions to use helping the clean the bases data used in the package

# Function to change the base format getting a format that allow us to use the package
.convertBase = function(file, sp, fleeType, efforType){

  #lectura de datos
  base            = read.csv(file, na.strings = "")
  colnames(base)  = tolower(colnames(base))
  base$especie    = tolower(base$especie)
  base$tipo_flota = tolower(base$tipo_flota)

  #indexacion sp y tipo de flota
  if(is.null(fleeType)){base = base[base$especie %in% tolower(sp), ]
  } else {base = base[base$especie %in% sp & base$tipo_flota %in% tolower(fleeType), ]}

  #obtencion de vectores captura y esfuerzo
  catch = aggregate(captura ~ anho + mes + dia + puerto, data = base, FUN = sum)

  if(efforType == "viaje"){ #por viaje
    effort = aggregate(puerto ~ anho + mes + dia + puerto, data = base, FUN = table)
    effort  = data.frame(effort[names(effort) %in% c("anho", "mes", "dia")], as.data.frame(effort$puerto))
    effort  = effort[, which(!apply(effort, 2, FUN = function(x){all(x == 0)}))]
  } else { #por otro esfuerzo
    effort = aggregate(as.formula(paste0(efforType, " ~ anho + mes + dia + puerto")), data = base, FUN = sum)
    names(effort)[5] = "effort"
  }

  #generacion de bases de datos finales
  puerto    = unique(catch$puerto)
  newBase   = .getCalendar(unique(catch$anho))

  #base esfuerzo
  if(efforType == "viaje"){
    frameEffort    = merge(newBase, effort, all = TRUE)
    frameEffort    = frameEffort[!names(frameEffort) %in% c("anho", "mes", "dia")]
    colnames(frameEffort) = paste(as.character(puerto),"_e", sep = "")
  } else{
    baseEffort = list()
    for(i in seq_along(puerto)){
      effortPort     = effort[effort$puerto == puerto[i], ]
      effortPort     = merge(newBase, effortPort, all = TRUE)
      dates          = effortPort[names(effortPort) %in% c("anho", "mes", "dia")]
      dates$mes      = tolower(month.abb[dates$mes])
      colnames(dates)= c("year", "month", "day") #ver de poner todo al espanhol
      baseEffort[[i]] = data.frame(effortPort[, "effort"])
      colnames(baseEffort[[i]]) = c(paste(as.character(puerto[i]),"_e", sep = ""))
      frameEffort    = do.call("cbind", baseEffort)
    }
  }

  baseCatch = list()
  for(i in seq_along(puerto)){
    catchPort      = catch[catch$puerto == puerto[i], ]
    catchPort      = merge(newBase, catchPort, all = TRUE)
    dates          = catchPort[names(catchPort) %in% c("anho", "mes", "dia")]
    dates$mes      = tolower(month.abb[dates$mes])
    colnames(dates)= c("year", "month", "day") #ver de poner todo al espanhol
    baseCatch[[i]] = data.frame(catchPort[, "captura"])
    colnames(baseCatch[[i]]) = c(paste(as.character(puerto[i]),"_c", sep = ""))
    frameCatch     = do.call("cbind", baseCatch)
  }

  catcheffort    = data.frame(frameCatch, frameEffort)
  catcheffort    = catcheffort[, order(names(catcheffort))]

  output         = data.frame(dates, catcheffort)
  output[is.na(output)] = 0


  return(base = output)
}
#x = convertBase(file = "perico_row.csv", sp = "perico", fleeType = "artesanal", efforType = "capacidad_bodega")

# Internal function for getData
.fleetData = function(file, varType, fleeType, sp, efforType, toTons=TRUE,
                      landingFun = sum, effortFun = sum, cpueFun = mean,
                      start = start, end = end, port = port){

  #Lectura de base
  dataBase = read.csv(file = file, header = TRUE)
  colnames(dataBase)  = tolower(colnames(dataBase))
  dataBase$especie    = tolower(dataBase$especie)
  dataBase$tipo_flota = tolower(dataBase$tipo_flota)
  dataBase$captura    = dataBase$captura / ifelse(isTRUE(toTons), 1000, 1)

  #Indexacion especie y tipo de flota
  if(is.null(fleeType)){dataBase = dataBase[dataBase$especie %in% tolower(sp), ]
  } else {dataBase = dataBase[dataBase$especie %in% sp & dataBase$tipo_flota %in% tolower(fleeType), ] }

  #trimData
  dataBase$year  = dataBase$anho
  dataBase$month = month2word(dataBase$mes)
  dataBase$day   = dataBase$dia
  dataBase       = .trimData(x = dataBase, start = start, end = end)
  dataBase[, c("year", "month", "day")] = NULL
  rownames(dataBase) = NULL

  #port
  if(is.null(port)){
    dataBase = dataBase
  } else{
    dataBase = dataBase[dataBase$puerto %in% port,]
    rownames(dataBase) = NULL
  }

  #Obtencion de vectores captura, esfuerzo o cpue -> nombre: dataTable
  if(varType == "cpue"){ #cpue
    dataTable = dataBase
    if(efforType != "viaje"){
      dataTable$cpue = dataTable[,"captura"] / dataTable[, efforType]
      dataTable = aggregate(cpue ~ anho + mes + tipo_flota, data = dataTable, FUN = cpueFun)
    } else {
      effort = count(dataTable, vars = c("anho", "mes", "dia", "tipo_flota", "puerto"))
      catch  = aggregate(captura ~ anho + mes + dia + tipo_flota + puerto, data = dataTable, FUN = sum)
      dataTable = merge(effort, catch, by = c("anho", "mes", "dia", "tipo_flota", "puerto"))
      dataTable$cpue = dataTable$captura / dataTable$freq
      dataTable = aggregate(cpue ~ anho + mes + tipo_flota, data = dataTable, FUN = cpueFun)
    }
  } else { #desembarque y esfuerzo
    if(varType == "landing"){ #desembarque
      dataTable = aggregate(captura ~ anho + mes + tipo_flota, data = dataBase, FUN = landingFun)
    } else { #esfuerzo
      if(efforType == "viaje"){
        dataTable = count(dataBase, vars = c("anho", "mes", "dia", "tipo_flota", "puerto"))
        dataTable = aggregate(freq ~ anho + mes + tipo_flota, dataTable, FUN = effortFun)
        colnames(dataTable)[4] = "esfuerzo"

      } else {
        dataTable = aggregate(as.formula(paste0(efforType, " ~ anho + mes + tipo_flota")), data = dataBase, FUN = effortFun)
      }
    }
  }

  #Editando la nueva base de datos en relacion al tipo de flota
  dataTable$tipo_flota = capitalizeFirstLetter(dataTable$tipo_flota)
  dataTable = reshape(dataTable, idvar = c("anho", "mes"), timevar = "tipo_flota", direction = "wide")
  dataTable[is.na(dataTable)] = 0 ; rownames(dataTable) = NULL

  #Editanto la nueva base de datos en relacion a las fechas
  datesVector   = as.Date(paste(dataTable$anho, dataTable$mes,1, sep = "-"), format = "%Y-%m-%d")
  newOrderDates = datesVector[order(datesVector)]
  dataTable = dataTable[match(newOrderDates, datesVector), ]; rownames(dataTable) = NULL

  datesVector = paste(engToSpa(month2word(dataTable$mes)), dataTable$anho, sep=" ")
  rownames(dataTable) = datesVector
  dataTable$anho = NULL
  dataTable$mes  = NULL

  colnames(dataTable) = unlist(lapply(strsplit(colnames(dataTable), split = "\\."), "[[", 2))

  return(dataTable)
}

# Function to use in the .getLandingsData function (landing class)
.readSegFile = function(file){

  output = file
  #output = read.csv(file = file, ...)
  monthVector = tolower(substr(output$month, 1, 3))

  for(i in seq_along(month.abb_spanish)){
    index = which(is.element(monthVector, substr(tolower(month.abb_spanish[i]), 1, 3)))

    if(length(index) < 1){next}
    else{output$month[index] = rep(tolower(month.abb[i]), length(index))}
  }

  output$month = capitalizeFirstLetter(output$month)
  dateVector   = with(output, as.Date(paste(year, word2month(month), day, sep = "-")))

  actualYear   = sort(unique(as.numeric(substr(as.character(dateVector), 1, 4))))

  dateRange    = as.Date(apply(.getCalendar(actualYear), 1, function(x) paste(x, collapse = "-")))
  dateRange    = dateRange[!is.element(dateRange, unique(dateVector))]

  newData = as.data.frame(mat.or.vec(nr = length(dateRange), nc = ncol(output)))
  colnames(newData) = colnames(output)

  newData$year  = as.numeric(substr(dateRange, 1, 4))
  newData$month = month2word(substr(dateRange, 6, 7))
  newData$day   = as.numeric(substr(dateRange, 9, 10))

  output = rbind.data.frame(output, newData)
  output = output[order(output$year, word2month(output$month), output$day),]
  rownames(output) = NULL #

  return(output)
}

# Function to split the data according the time
.trimData = function(x, start, end) {

  if(is.null(start) & is.null(end)){
    months = tolower(x$month)
    vectorDates = as.Date(paste(x$year,match(months, tolower(month.abb)), x$day, sep="-"), format = "%Y-%m-%d")
    start = min(vectorDates)
    end   = max(vectorDates)
  } else{
    months = tolower(x$month)
    start  = start
    end    = end
  }

  datesVector = as.Date(paste(x$year,match(months, tolower(month.abb)), x$day, sep="-"), format = "%Y-%m-%d")
  x = data.frame(date = datesVector, x)
  x = subset(x, x$date >= as.Date(start) & x$date <= as.Date(end))
  x$date = NULL

  return(x)
}

# Function to get data by regions (regionNC and regionS)
.getRegionData = function(x){

  dataBase = x$data
  dataBase = melt(dataBase, id.vars=c("year", "month", "day"))

  portData = getPort(as.vector(as.character(dataBase$variable)))
  dataBase$port   = portData$data$name
  dataBase$region = portData$data$area

  dataNC = dataBase[dataBase$region %in% c("Norte", "Centro"), ]
  dataS  = dataBase[dataBase$region %in% "Sur", ]

  #Ordering data from the NC region
  dataNC = aggregate(value ~ year + month + day, dataNC, sum)
  dataNC = dataNC[order(dataNC$year, word2month(dataNC$month), dataNC$day),]

  #Ordering data from the S region
  dataS = aggregate(value ~ year + month + day, dataS, sum)
  dataS = dataS[order(dataS$year, word2month(dataS$month), dataS$day),]

  #dataNC = .trimData(x = dataNC, start = start, end = end) ; rownames(dataNC) = NULL
  #dataS  = .trimData(x = dataS, start = start, end = end) ; rownames(dataS) = NULL

  output = list(regionNC = dataNC, regionS = dataS)

  return(output)

}

# Function to plot the regions (NC and S)
.plotRegion = function(x, region, byAxis2="default", milesTons=TRUE, textAxis2, textAxis4, cexLab=1.2,
                       daysToPlot, cexAxis24 = 1.1, cexAxis1 = 0.9, cexLegend = 1){

  if(region == "NC"){dataBase = x$regionNC} else {dataBase = x$regionS}

  dataBase$dates = paste(sapply(dataBase$day, function(x) paste0(ifelse(x < 10, 0, ""), x)),
                         sapply(word2month(dataBase$month), function(x) paste0(ifelse(x < 10, 0, ""), x)),
                         dataBase$year, sep = "/")
  xValues = as.Date(paste(dataBase$day, word2month(dataBase$month), dataBase$year, sep="/"), format = "%d/%m/%Y")
  yValues = dataBase$value
  xLim    = range(xValues)

  maxY1 = max(yValues, na.rm = TRUE)
  yLim1 = c(0, maxY1)
  maxY2 = sum(yValues, na.rm = TRUE)
  yLim2 = c(0, maxY2)

  #plot
  par(oma = c(4, 1, 1, 1))

  par(mar = c(5, 5, 1, 5), yaxs = "i")
  plot(1, 1, pch = NA, axes = FALSE, xlim = xLim, ylim = yLim1, xlab = NA, ylab = NA)

  vectorYear = unique(dataBase$year) #code for the polygon
  for(j in seq_along(vectorYear)){
    for(i in seq(2, 12, 2)){
      tempDates = seq(from = as.Date(paste(vectorYear[j], i, "1", sep = "-")), by = "mon", length.out = 2) + c(-1, 1)
      polygon(x = c(tempDates, rev(tempDates)), y = c(-40, -40, 10e7, 10e7), border = NA, col = "gray90") }}

  lines(xValues, yValues, type = "o", pch = 19)
  lines(xValues, cumsum(yValues)/yLim2[2]*yLim1[2], lty = "dashed")
  abline(h = mean(yValues), lty = "dotted", lwd = 2, col = "red")

  #Axis 2 and 4
  if(byAxis2 == "default"){atAxis2 = pretty(dataBase$value)} else {atAxis2 = seq(0, maxY1, byAxis2)}
  axis(side = 2, at = atAxis2, las = 2, cex.axis=cexAxis24)

  yLabs2 = seq(from = 0, to = maxY2, length.out = 10) / ifelse(isTRUE(milesTons), 1e3, 1e6)
  axis(side = 4, at = seq(from = 0, to = maxY1, length.out = length(yLabs2)), labels = round(yLabs2,2), las = 2, cex.axis = cexAxis24)
  box()

  mtext(text = textAxis2, side = 2, line = 4, cex = cexLab)
  mtext(text = textAxis4, side = 4, line = 4, cex = cexLab)

  #Axis1
  if(unique(daysToPlot %in% "all")){index = dataBase$day} else {index = which(dataBase$day %in% daysToPlot)}
  xLabs = dataBase$dates
  index = xLabs[index]
  xLabs[!xLabs %in% index] = NA

  axis(side = 1, at = xValues, labels = xLabs, las = 2, cex.axis = cexAxis1)

  #legend
  par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
  legend("bottom", c("Diario", "Acumulado", "Promedio"), xpd = TRUE,
         horiz = TRUE, inset = c(0, 0), bty = "n",text.width=c(0.25,0.25,0.25),
         lty = c("solid", "dashed", "dotted"), pch = c(19, NA, NA),
         col = c("black", "black", "red"), lwd = c(1, 1, 2), cex = cexLegend)

  return(invisible())
}

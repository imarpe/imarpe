
DownloadPorcenta <- function(directorio, dirUrl, inicio, fin, ...){
  tiempo <- seq(as.Date(inicio), as.Date(fin), by = "day")
  
  tiempo2 <- strftime(tiempo,format="%d%m%Y")
  
  for (i in 1:length(tiempo2)){
    
    tempUrl <- paste0(dirUrl, tiempo2[i], ".xlsx")
    tempUrl <- ifelse(url.exists(tempUrl), tempUrl, paste0(dirUrl, tiempo2[i], "_.xlsx"))
    
    try(download.file(url = tempUrl,
                      destfile = paste0(directorio,"/imarpe_rpelag_porfinal",tiempo2[i],".xlsx"),
                      method = "internal", mode = "wb"))
  }
}

ReadPorcenta <- function(directorio, inicio, fin,...){
  outPorcenta  = list()
  listPorcenta = NULL
  
  tiempo    <- seq(as.Date(inicio), as.Date(fin), by = "day")
  tiempo2 <- strftime(tiempo,format="%d%m%Y")
  
  desembarque     <- NULL
  n.embarcaciones <- NULL
  e.muestreadas   <- NULL
  p.juveniles     <- NULL
  moda            <- NULL
  
  for(i in 1:length(tiempo2))
  {
    file_name <- paste(directorio, paste("/imarpe_rpelag_porfinal",tiempo2[i],".xlsx",sep=""),sep="")
    wb <- NULL
    wb <- tryCatch(expr = loadWorkbook(file_name), 
                   error = function(e) message("Not Found", file_name))
    
    if(!is.null(wb)){
      desemb  <- readWorksheet(object = wb, sheet = "reporte", startRow = 11, endRow = 12, startCol = 3, endCol = 40)
      n.embar <- readWorksheet(object = wb, sheet = "reporte", startRow = 12, endRow = 13, startCol = 3, endCol = 40)
      e.muest <- readWorksheet(object = wb, sheet = "reporte", startRow = 13, endRow = 14, startCol = 3, endCol = 40)
      p.juv   <- readWorksheet(object = wb, sheet = "reporte", startRow = 14, endRow = 15, startCol = 3, endCol = 40)
      mod     <- readWorksheet(object = wb, sheet = "reporte", startRow = 15, endRow = 16, startCol = 3, endCol = 40)
    }else{
      desemb  <- rep(NA, 38)
      n.embar <- rep(NA, 38)
      e.muest <- rep(NA, 38)
      p.juv   <- rep(NA, 38)
      mod     <- rep(NA, 38)
    }
    
    porcenta <- print(tiempo2[i])
    listPorcenta <- rbind(listPorcenta, porcenta)
    
    desemb1 <- t(matrix(as.numeric(desemb)))
    desembarque <- rbind(desembarque, desemb1)
    
    n.embar1 <- t(matrix(as.numeric(n.embar)))
    n.embarcaciones <- rbind(n.embarcaciones, n.embar1)
    
    e.muest1 <- t(matrix(as.numeric(e.muest)))
    e.muestreadas <- rbind(e.muestreadas, e.muest1)
    
    p.juv1 <- t(matrix(as.numeric(p.juv)))
    p.juveniles <- rbind(p.juveniles, p.juv1)
    
    mod1 <- t(matrix(as.numeric(mod)))
    moda <- rbind(moda, mod1)
  }
  
  #desembarque0 <- desembarque
  puertos_porcentas <-  c("Paita","Paita","Parachique","Parachique","Chicama","Chicama",
                          "Chimbote","Chimbote","Samanco","Samanco","Casma","Casma",
                          "Huarmey","Huarmey","Supe","Supe","Vegueta","Vegueta","Huacho","Huacho",
                          "Chancay","Chancay","Callao","Callao","T.Mora","T.Mora",
                          "Pisco","Pisco","Atico","Atico","Planchada","Planchada","Quilca","Quilca",
                          "Mollendo","Mollendo","Ilo","Ilo")
  
  tipo <- c(rep(c("Ind", "Ind Mad"), length(puertos_porcentas)/2))
  puerto <- c("tiempo", as.character(tiempo))
  
  desembarque[is.na(desembarque)] <- 0
  n.embarcaciones[is.na(n.embarcaciones)] <- 0
  e.muestreadas[is.na(e.muestreadas)] <- 0
  p.juveniles[is.na(p.juveniles)] <- 0
  moda[is.na(moda)] <- 0
  ##
  desembarque <- data.frame(desembarque)
  n.embarcaciones <- data.frame(n.embarcaciones)
  e.muestreadas <- data.frame(e.muestreadas)
  p.juveniles <- data.frame(p.juveniles)
  moda <- data.frame(moda)
  ##
  desembarque <- data.frame(rbind(tipo, desembarque), stringsAsFactors = FALSE)
  names(desembarque) <- puertos_porcentas
  
  n.embarcaciones <- data.frame(rbind(tipo,n.embarcaciones), stringsAsFactors = FALSE)
  names(n.embarcaciones) <- puertos_porcentas
  
  e.muestreadas <- data.frame(rbind(tipo,e.muestreadas), stringsAsFactors = FALSE)
  names(e.muestreadas) <- puertos_porcentas
  
  p.juveniles <- data.frame(rbind(tipo,p.juveniles), stringsAsFactors = FALSE)
  names(p.juveniles) <- puertos_porcentas
  
  moda <- data.frame(rbind(tipo,moda), stringsAsFactors = FALSE)
  names(moda) <- puertos_porcentas
  ##
  desembarque <- cbind(puerto, desembarque)
  n.embarcaciones <- cbind(puerto, n.embarcaciones)
  e.muestreadas <- cbind(puerto, e.muestreadas)
  p.juveniles <- cbind(puerto, p.juveniles)
  moda <- cbind(puerto, moda)
  
  outPorcenta$desembarque <- desembarque
  outPorcenta$n.embarcaciones <- n.embarcaciones
  outPorcenta$e.muestreadas <- e.muestreadas
  outPorcenta$p.juveniles <- p.juveniles
  outPorcenta$moda <- moda
  
  return(outPorcenta)
}

# From ruisu. Get harbor information from name
getHarbor <- function(myHarbor){
  
  myHarbor <- chartr(old = "\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1",
                     new = "aeiouun", x = tolower(myHarbor))
  
  output <- NULL
  for(i in seq_along(myHarbor)){
    
    tempHarbor <- myHarbor[i]
    
    harborPos <- which(sapply(harborData$pattern, grepl, x = tempHarbor))
    
    if(length(harborPos) > 1){
      warning(paste(tempHarbor, "matched with more than one pattern at pos =", i,
                    "\nFunction will take the first matched value:", harborData$name[harborPos[1]]))
    }
    
    output <- c(output, harborPos[1])
  }
  
  return(as.list(harborData[output,]))
}

leerData <- function(muestreo = NULL, desembarque = NULL, ...){
  if(is.null(muestreo) & is.null(desembarque)) {
    message("Cargue los dos archivos")
    return(invisible())
  }
  
  baseMuestreo <- read.csv(muestreo, na.strings = c("", " ", NA, "NA"), check.names = FALSE, stringsAsFactors = FALSE)
  colnames(baseMuestreo) <- tolower(colnames(baseMuestreo))
  
  baseMuestreo$especie <- gsub(pattern = " ", replacement = "", x = baseMuestreo$especie, perl = TRUE)
  
  harborNames <- getHarbor(myHarbor = baseMuestreo$puerto)$name
  
  baseMuestreo$puerto <- harborNames
  baseMuestreo$puerto[harborNames == "Coishco"] <- "Chimbote"
  baseMuestreo$puerto[harborNames == "Bay\u00f3var"] <- "Parachique"
  # baseMuestreo$puerto[harborNames == "Tambo de Mora"] <- "t.mora"
  # baseMuestreo$puerto[harborNames == "La Planchada"] <- "Planchada"
  
  allMarks <- seq(2, 20, 0.5)
  index <- which(grepl(x = colnames(baseMuestreo), pattern = "[[:digit:]^]", perl = TRUE))
  for(i in index){
    baseMuestreo[,i] <- suppressWarnings(as.numeric(baseMuestreo[,i]))
  }
  
  baseDesembarque <- read.csv(desembarque, skip = 1, stringsAsFactors = FALSE)
  names(baseDesembarque) <- colnames(read.csv(desembarque))
  names(baseDesembarque)[1] <- "fecha"
  colnames(baseDesembarque) <- tolower(colnames(baseDesembarque))
  
  for(i in 2:ncol(baseDesembarque)){
    baseDesembarque[,i] <- suppressWarnings(as.numeric(baseDesembarque[,i]))
  }
  
  return(list(baseMuestreo = baseMuestreo, baseDesembarque = baseDesembarque))
}

LC_ponderada <- function(data, tallas, especie, umbral, a, b){
  
  dataMuestreo <- filtroMuestreo(data = data$baseMuestreo, tallas = tallas, especie = especie, umbral = umbral)
  dataDesem    <- reordenarDesembarques(data$baseDesembarque)
  colnames(dataDesem)[-ncol(dataDesem)] <- getHarbor(colnames(dataDesem)[-ncol(dataDesem)])$name
  
  capPondBarco <- ponderacion(data = dataMuestreo, tallas = tallas, a = a, b = b)
  
  sumaPuertoDia <- sumPuertoDia(data = capPondBarco, tallas = tallas)
  sumaPuertoDia <- matchDesemPorFechaPuerto(data1 = sumaPuertoDia, data2 = dataDesem)
  
  capPondPuertoDia <- ponderacion2(data = sumaPuertoDia, tallas = tallas, a = a, b = b)
  
  ponderacionPorDia <- aggregate(x = capPondPuertoDia[, -(1:2)], by = list(capPondPuertoDia$fecha), FUN = sum)
  names(ponderacionPorDia)[1] <- "fecha"
  
  ponderacionPorDia <- matchDesemPorFecha(data1 = ponderacionPorDia, data2 = dataDesem)
  output <- ponderacion3(data = ponderacionPorDia, tallas = tallas, a = a, b = b)
  
  captura_ponderada <- sum(apply(output[,-1], 1, FUN = function(x) ((a*as.numeric(names(output[,-1]))^b)*x)))
  
  return(list(ponderacion_diaria = output, captura_ponderada = captura_ponderada))
}

filtroMuestreo <- function(data, tallas, especie, umbral, ...){
  colnames(data) <- tolower(colnames(data))
  if(is.na(sum(data$captura))){
    warning("Existen 'NA' en la variable 'captura'. Las filas que contiene estos valores ser\u00e1n eliminadas.")
  }
  tallas <- as.character(tallas)
  data <- data[data$especie == especie, ]
  data[, tallas][is.na(data[, tallas])] <- 0
  data <- data[!is.na(data$captura), ]
  data$total <- apply(data[,tallas], 1, sum, na.rm = TRUE)
  output <- data[data$total > umbral, ]
  
  return(output)
}

reordenarDesembarques <- function(data, ...){
  puertos <- names(data)[-1]
  puertos <- tolower(unlist(unique(strsplit(puertos, ".1"))))
  
  output <- NULL
  for(i in seq(2, ncol(data)-1, 2)){
    out <- rowSums(data[, c(i, i+1)])
    output <- cbind(output, out)
  }
  
  output <- data.frame(output, stringsAsFactors = FALSE)
  names(output) <- puertos
  output$fecha <- as.Date(data$fecha, format = ifelse(grepl(pattern = "-", x = data$fecha[1]), "%Y-%m-%d", "%d/%m/%Y"))
  
  return(output)
}

ponderacion <- function(data, tallas, a, b, ...){
  tallas <- as.character(tallas)
  
  output <- NULL
  for(i in 1:nrow(data)){
    out <- .ponderacion(as.numeric(tallas), data[i, tallas], a = a, b = b, data$captura[i])
    output <- rbind(output, out)
  }
  
  fecha <- paste(data$dia, "/", data$mes, "/", data$anho, sep = "")
  fecha <- as.Date(fecha, format = "%d/%m/%Y")
  puerto <- tolower(data$puerto)
  output <- cbind(puerto, fecha, output)
  
  return(output)
}

.ponderacion <- function (tallas, frecuencia, a, b, captura){
  peso <- (a * tallas^b) * frecuencia
  freqPonderada <- (captura/sum(peso, na.rm = TRUE)) * peso
  tallasPoderadas <- freqPonderada/(a * tallas^b)
  
  return(tallasPoderadas)
}

sumPuertoDia <- function(data, tallas, ...){
  tallas <- as.character(tallas)
  out <- data
  out$etiqueta <- as.factor(paste(tolower(out$puerto), "-", out$fecha, sep = ""))
  output <- aggregate(x = out[, tallas], by = list(out$etiqueta), FUN = sum)
  
  puerto <- NULL
  fecha <- NULL
  for(i in 1:dim(output)[1]){
    p <- strsplit(as.character(output$Group.1), "-")[[i]][1]
    y <- strsplit(as.character(output$Group.1), "-")[[i]][2]
    m <- strsplit(as.character(output$Group.1), "-")[[i]][3]
    d <- strsplit(as.character(output$Group.1), "-")[[i]][4]
    puerto <- c(puerto, p)
    fecha <- c(fecha, paste(y,"-", m, "-", d, sep=""))
  }
  
  fecha <- as.Date(fecha, format = "%Y-%m-%d")
  output <- cbind(puerto, fecha, output[, tallas], stringsAsFactors = FALSE)
  output$puerto <- getHarbor(output$puerto)$name
  
  return(output)
}

matchDesemPorFechaPuerto <- function(data1, data2, ...){
  captura <- NULL
  for(i in 1:nrow(data1)){
    n <- match(data1$fecha[i], data2$fecha)
    m <- match(data1$puerto[i], colnames(data2))
    captura[i] <- ifelse(is.null(data2[n, m]), NA, data2[n, m])
  }
  
  data1$captura <- captura
  output <- data1
  
  return(output)
}

ponderacion2 <- function(data, tallas, a, b, ...){
  tallas <- as.character(tallas)
  
  output <- NULL
  for(i in 1:nrow(data)){
    out <- .ponderacion(as.numeric(tallas), data[i, tallas], a = a, b = b, data$captura[i])
    output <- rbind(output, out)
  }
  
  fecha <- data$fecha
  fecha <- as.Date(fecha, format = "%d/%m/%Y")
  puerto <- tolower(data$puerto)
  output <- cbind(puerto, fecha, output)
  
  return(output)
}

matchDesemPorFecha <- function(data1, data2, ...){
  captura <- NULL
  for(i in 1:nrow(data1)){
    n <- match(data1$fecha[i], data2$fecha)
    captura[i] <- sum(rev(data2[n, ])[-1], na.rm = T)
  }
  
  data1$captura <- captura
  output <- data1
  
  return(output)
}

ponderacion3 <- function(data, tallas, a, b, ...){
  tallas <- as.character(tallas)
  
  output <- NULL
  for(i in 1:nrow(data)){
    out <- .ponderacion(as.numeric(tallas), data[i, tallas], a = a, b = b, data$captura[i])
    output <- rbind(output, out)
  }
  
  fecha <- data$fecha
  fecha <- as.Date(fecha, format = "%d/%m/%Y")
  output <- cbind(fecha, output)
  
  return(output)
}

guardarPonderacion <- function(data, filename = NULL, ...){
  
  fechas <- t(data[[1]])[1, ]
  tallas <- rownames(t(data[[1]]))[-1]
  
  output <- t(data[[1]])[-1, ]
  colnames(output) <- fechas
  rownames(output) <- tallas
  
  if(is.null(filename)){
    filename <- paste0("ponderaci\u00f3n_al_", as.character(rev(colnames(output)[1])), ".csv")
  }
  
  write.csv(x = output, file = filename, ...)
  
  return(invisible())
}

getInfo <- function(x, millionT = TRUE, nDecimalsBiomass = 2, allMarks, a, b){
  output <- numeric(6)
  
  output[1] <- round(sum(x), 0)
  output[2] <- round(sum(x*a*allMarks^b)*ifelse(isTRUE(millionT), 1e-6, 1), nDecimalsBiomass)
  output[3] <- round(allMarks[findInterval(sum(allMarks*x)/sum(x), allMarks)], 1)
  output[4] <- round(a*output[[3]]^b, 1)
  output[5] <- round(sum(x[allMarks < 12])/sum(x)*100, 1)
  biomassVector <- x*a*allMarks^b
  output[6] <- round(sum(biomassVector[allMarks < 12])/sum(biomassVector)*100, 1)
  
  names(output) <- c("Abundancia (millones ind)", paste0("Biomasa (", ifelse(isTRUE(millionT), "millones ", ""), "t)"),
                     "Talla media (cm)", "Peso medio (g)", "Juv_N (%)", "Juv_B (%)")
  
  return(output)
}


# Pope principal functions ------------------------------------------------

readAtLength = function(file, sp = "anchoveta", ...){
  if(is.null(file)) return(NULL)
  
  base <- read.csv(file, stringsAsFactors = FALSE, ...)
  colnames(base)[1] <- "x"
  specie <- getSpeciesInfo(sp)
  marcas <- .createMarks(specie)
  newBase <- expand.grid(x = marcas)
  base <- merge(base, newBase, all = T)
  base <- as.matrix(base[,-1])
  base[is.na(base)] = 0
  
  return(base)
}

projectPOPE <- function(N, catch, a, b, k, Linf, sizeM, vectorM, freq, sp, Ts){
  
  matrixN    <- array(dim=c(Ts+1, dim(N)))
  matrixB    <- matrix(nrow=Ts+1, ncol=ncol(N))
  matrixBD   <- matrix(nrow=Ts+1, ncol=ncol(N))
  matrixBDR  <- numeric(ncol(N))
  
  for(i in seq_len(ncol(N))){
    
    N0 <- N[, i]
    sim <- .projectPOPE(N=N0, catch=catch, a=a, b=b, k=k, Linf=Linf, sizeM=sizeM, vectorM=vectorM, freq=freq, sp=sp, Ts=Ts)
    
    matrixN[, ,i] <- sim$N
    matrixB[, i]  <- sim$B
    matrixBD[, i] <- sim$BD
    matrixBDR[i]  <- sim$BDR
  }
  
  N    <- apply(matrixN, c(1,2), median)
  B    <- apply(matrixB, 1, median)
  BD   <- apply(matrixBD, 1, median)
  BDR  <- median(matrixBDR)
  
  rawData <- list(N=matrixN, B=matrixB, BD=matrixBD, BDR=matrixBDR)
  
  output <- list(N=N, B=B, BD=BD, BDR=BDR,
                 raw=rawData)
  
  attr(output, which="sp") <-  sp
  attr(output, which="freq") <-  freq
  attr(output, which="Ts") <-  Ts
  
  class(output) <- "surveyProj"
  
  return(output)
}


# Pope internal functions -------------------------------------------------

getSpeciesInfo <- function(sp, data=NULL){
  otros <- NULL
  out <- if(sp %in% rownames(species)) as.list(species[sp, ]) else otros
  return(out)
}

.createMarks <- function(specie, phi=FALSE){
  marks <- seq(from=specie$Lmin, to=specie$Lmax, by=specie$bin)
  if(isTRUE(phi)){
    marks_inf <- marks - 0.5*specie$bin
    marks_sup <- marks + 0.5*specie$bin
    marks <- sort(unique(c(marks_inf, marks_sup)))
  }
  return(marks)
}

.projectPOPE <- function (N0, catch, a, b, k, Linf, sizeM, vectorM, freq, sp, Ts){
  
  A <- lengthProjMatrix(sp=sp, k=k, Linf=Linf, freq=freq)
  M <- naturalMortality(sp=sp, sizeM=sizeM, vectorM=vectorM, freq=freq)
  
  N <- matrix(ncol=length(M), nrow=Ts+1)
  
  N[1, ] <- N0
  
  for(t in seq_len(Ts)){
    nNew <- (N[t,]*exp(-(M/2)) - catch)*exp(-(M/2))
    N[t+1, ] <- as.numeric(nNew %*% A)
  }
  
  species <- getSpeciesInfo(sp)
  marcas <- .createMarks(species)
  
  weights <- a*marcas^b
  
  maturity <- .maturity.ojive(sp)
  
  B   <- N %*% weights
  BD  <- N %*% (maturity*weights)
  BDR <- tail(BD, 1)
  
  return(list(N=N, C=C, B=B, BD=BD, BDR=BDR))
}

lengthProjMatrix <- function(sp, k, Linf, freq){
  
  dt      <- 1/freq
  species <- getSpeciesInfo(sp)
  bin     <- species$bin
  
  marcas     <- .createMarks(species)
  marcas_phi <- .createMarks(species, phi=TRUE)
  marcas_inf <- marcas - 0.5*bin
  marcas_sup <- marcas + 0.5*bin
  
  k    <- k*dt
  Linf <- Linf
  
  l_inf <- brody(marcas_inf, Linf=Linf, k=k)
  l_sup <- brody(marcas_sup, Linf=Linf, k=k)
  
  newMarcas <- cbind(l_inf, l_sup)
  A <- t(apply(newMarcas, 1, .lengthProj, marcas=marcas_phi))
  return(A)
}

naturalMortality <- function(sp, vectorM, sizeM, freq){
  
  dt <- 1/freq
  species <- getSpeciesInfo(sp)
  bin <- species$bin
  
  marcas <- .createMarks(species)
  
  M_table <- data.frame(size = sizeM, vectorM = vectorM)
  
  mPos <- findInterval(marcas, M_table$size)
  
  M <- M_table[mPos, "vectorM"]*dt
  names(M) <- marcas
  
  return(M)
}

.maturity.ojive = function(sp) {
  specieData = getSpeciesInfo(sp)
  marcas = .createMarks(specieData)
  
  out = 1/(1+exp(specieData$mat1+specieData$mat2*marcas))
  return(out)
}

brody <- function(l, Linf, k){
  out <- Linf - (Linf-l)*exp(-k)
  return(out)
}

.lengthProj <- function(l, marcas){
  
  x <- sort(c(l, marcas))
  dif <- diff(x)
  pos <- findInterval(l, marcas) + 1
  pos <- seq(from=pos[1], to=pos[2])
  props <- dif[pos]
  props <- props/sum(props)
  out <- numeric(length(marcas)-1)
  out[pos-1] <- props
  return(out)
}



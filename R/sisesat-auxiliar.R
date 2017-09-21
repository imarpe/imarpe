# auxiliar exported functions

estima_semana <- function(inicio.temp, fin.temp, ...){
  require(lubridate)
  
  diasTemporada = seq.Date(as.Date(inicio_temp, format = "%d/%m/%Y"), as.Date(fin_temp, format = "%d/%m/%Y"),by =  "day")
  dias =  weekdays(diasTemporada)
  temporada = data.frame(diasTemporada = diasTemporada, dias = dias)
  nSem = c(rep(0, min(which(dias == "lunes"))-1), sort(rep(1:length(dias[dias=="lunes"]),7)))
  temporada$semana <- nSem[1:length(dias)]  
  return(temporada)
}


rangeWeek <-function (data, fecha, semana) 
{
  monthE <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", 
              "Ago", "Set", "Oct", "Nov", "Dic")
  rFecha <- tapply(data[[fecha]], data[[semana]], range)
  labelSem <- NULL
  for (i in 1:length(rFecha)) {
    mi = as.numeric(substring(rFecha[[i]][1], 6, 7))
    mf = as.numeric(substring(rFecha[[i]][2], 6, 7))
    if (mi %in% mf) {
      rangeSem <- paste0(paste(substring(rFecha[[i]][1], 
                                         9, 10), substring(rFecha[[i]][2], 9, 10), sep = "-"), 
                         monthE[mi])
    }
    else {
      rangeSem <- paste(paste0(substring(rFecha[[i]][1], 
                                         9, 10), monthE[mi]), paste0(substring(rFecha[[i]][2], 
                                                                               9, 10), monthE[mf]), sep = "-")
    }
    labelSem = rbind(labelSem, rangeSem)
  }
  labelSem <- as.vector(labelSem)
  return(labelSem)
}

# FUNCIONES PARA OBTENER INDICADORES ESPACIO TEMPORALES DEL ESFUERZO --------

##data = Pred_Emi_Todas 
# esta funcion deberia ser input

extrae_data <- function(data, temp, date = "date.GMT", region = "NC", ...){
  require(fenix)
  
  data$Fecha     <- as.Date(substring(data[[date]], 1,10), format = "%Y-%m-%d")
  data$Temporada <- DateTemp(data$Fecha) # asignar temporadas
  
  data <- data[!is.na(data$Temporada),]
  #   data <- data[!data$Temporada %in% c("1999_II"),]
  
  # solo nos quedamos con las emisiones con cala
  data0 = data[data$Temporada == temp,]
  data  = data[data$Temporada == temp & data$Calas == 1,]
  
  # calculamos la distancia a costa
  data$dc    = as.numeric(estima.dc(data$Lon, data$Lat))# calculamos distancia a costa
  
  # removemos viajes que no pertenecen a nc
  Omit_Cod.Viaje.VMS <- unique(data[data$Lat < -16, "Cod.Viaje.VMS"])
  data <- data[!data$Cod.Viaje.VMS %in% Omit_Cod.Viaje.VMS,]
  
  
  # calculamos la etiqueta para el area de pesca
  data$area  = as.numeric(area.pesca(dc = as.numeric(data$dc), lat = data$Lat, grado = 1/12))# etiquetamos las areas de pesca
  
  data$semana = NA
  print(data$Fecha[1])
  for(i in sort(unique(data$Fecha))){
    print(i)
    data[data$Fecha == i, "semana"] = out.semana[out.semana$diasTemporada == i, "semana"]
  }
  
  # #outdata <- list()
  # outdata$data  <- data
  # outdata$data0 <- data0
  
  return(list(data = data, data0 = data0))  
}

## INPUT: data cpue (sisesat y seguimiento por viaje)
effort_cpue_week <- function(data = data, inicio_temp, fin_temp, ...){
  
  out_report <- list()
  
  data$num.dia = as.numeric(as.character(strftime(data$Fecha.Fin, format = "%j")))
  out.semana        = imarpe:::estima_semana(inicio.temp = inicio_temp, fin.temp = fin_temp)
  data$Fecha   = as.Date(substring(data$Fecha.Ini,1,10), format = "%Y-%m-%d")
  
  data$semana = NA
  for(i in sort(unique(data$Fecha))){
    data[data$Fecha == i, "semana"] = out.semana[out.semana$diasTemporada == i, "semana"]
  }
  
  data$cpueDv    = data$CPUE
  data$cpueNcala = data$ANCHOVETA/data$Num.Calas
  data$dia       = as.numeric(as.character(strftime(data$Fecha.Fin, format = "%d")))
  data$mes       = as.numeric(as.character(strftime(data$Fecha.Fin, format = "%m")))
  data$year      = as.numeric(as.character(strftime(data$Fecha.Fin, format = "%Y")))
  labelSEMANA         = rangeWeek(data = out.semana, semana = "semana", fecha = "diasTemporada")
  
  semana = sort(as.numeric(unique(data$semana) + 1))
  
  Dur.viaje <- fun_mean_IC(data$Dur.Viaje, data$semana)
  Num.Calas <- fun_mean_IC(data$Num.Calas, data$semana)
  cpueDv    <- fun_mean_IC(data$cpueDv, data$semana)
  cpueNcala <- fun_mean_IC(data$cpueNcala, data$semana)
  CB_mean   <- fun_mean_IC(data$CB, data$semana)
  semana    <- sort(as.numeric(unique(data$semana) + 1))
  
  Dur.viaje_ind <- fun_mean_IC(data$Dur.Viaje[data$TIPO == "IND"], data$semana[data$TIPO == "IND"])
  Num.Calas_ind <- fun_mean_IC(data$Num.Calas[data$TIPO == "IND"], data$semana[data$TIPO == "IND"])
  cpueDv_ind    <- fun_mean_IC(data$cpueDv[data$TIPO == "IND"], data$semana[data$TIPO == "IND"])
  cpueNcala_ind <- fun_mean_IC(data$cpueNcala[data$TIPO == "IND"], data$semana[data$TIPO == "IND"])
  CB_mean_ind   <- fun_mean_IC(data$CB[data$TIPO == "IND"], data$semana[data$TIPO == "IND"])
  semana_ind    <- sort(as.numeric(unique(data$semana[data$TIPO == "IND"]) + 1))
  
  Dur.viaje_indmad <- fun_mean_IC(data$Dur.Viaje[data$TIPO == "IND MAD"], data$semana[data$TIPO == "IND MAD"])
  Num.Calas_indmad <- fun_mean_IC(data$Num.Calas[data$TIPO == "IND MAD"], data$semana[data$TIPO == "IND MAD"])
  cpueDv_indmad    <- fun_mean_IC(data$cpueDv[data$TIPO == "IND MAD"], data$semana[data$TIPO == "IND MAD"])
  cpueNcala_indmad <- fun_mean_IC(data$cpueNcala[data$TIPO == "IND MAD"], data$semana[data$TIPO == "IND MAD"])
  CB_mean_indmad   <- fun_mean_IC(data$CB[data$TIPO == "IND MAD"], data$semana[data$TIPO == "IND MAD"])
  semana_indmad    <- sort(as.numeric(unique(data$semana[data$TIPO == "IND MAD"]) + 1))
  
  tableReporte = data.frame(semana = semana, Dur.viaje = Dur.viaje$x, Num.Calas = Num.Calas$x, cpueDv = cpueDv$x, cpueNcala = cpueNcala$x, CB_mean = CB_mean$x)
  sdtableReporte = data.frame(semana = semana, Dur.viaje = Dur.viaje$sterr, Num.Calas = Num.Calas$sterr, cpueDv = cpueDv$sterr, cpueNcala = cpueNcala$sterr, CB_mean = CB_mean$sterr)
  
  tableReporte_ind = data.frame(semana = semana_ind, Dur.viaje = Dur.viaje_ind$x, Num.Calas = Num.Calas_ind$x, cpueDv = cpueDv_ind$x, cpueNcala = cpueNcala_ind$x, CB_mean_ind = CB_mean_ind$x)
  sdtableReporte_ind = data.frame(semana = semana_ind, Dur.viaje = Dur.viaje_ind$sterr, Num.Calas = Num.Calas_ind$sterr, cpueDv = cpueDv_ind$sterr, cpueNcala = cpueNcala_ind$sterr, CB_mean_ind = CB_mean_indmad$sterr)
  
  tableReporte_indmad = data.frame(semana = semana_indmad, Dur.viaje = Dur.viaje_indmad$x, Num.Calas = Num.Calas_indmad$x, cpueDv = cpueDv_indmad$x, cpueNcala = cpueNcala_indmad$x, CB_mean_indmad = CB_mean_indmad$x)
  sdtableReporte_indmad = data.frame(semana = semana_indmad, Dur.viaje = Dur.viaje_indmad$sterr, Num.Calas = Num.Calas_indmad$sterr, cpueDv = cpueDv_indmad$sterr, cpueNcala = cpueNcala_indmad$sterr, CB_mean_indmad = CB_mean_indmad$sterr)
  
  out_report$effort_total   <- tableReporte
  out_report$effort_ind     <- tableReporte_ind
  out_report$effort_indmad  <- tableReporte_indmad
  
  out_report$sdt_effort_total  <- sdtableReporte
  out_report$sdt_effort_ind    <- sdtableReporte_ind
  out_report$sdt_effort_indmad <- sdtableReporte_indmad
  
  return(out_report)
}
#  ------------------------------------------------------------------------

## INPUT: data sisesat por calas
# es necesario agrerar las variable tipo de embarcacion y capacidad de bodega
# necesitamos el listado de embarcaciones de cada temporada o año
# data: Sisesat_Point_Pesca_NC

#lista_barcos <- read.csv("F:/reporte/lista_barcos_2016.csv") # para la temporada de 2016

# data <- data_cala$data
# 
# data$Cod.Barco
# list_vessel$COD

type_holdCapacity <- function(data, list_vessel, ...){
  
  out <- lapply(split(data, data$Cod.Barco, drop = TRUE),function(x){
    
    xlen <- dim(table(list_vessel$COD == x$Cod.Barco[1]))
    len  <- length(x$Cod.Barco)
    
    if(xlen == 2){
      
      iTIPO <- list_vessel[list_vessel$COD == x$Cod.Barco[1], "TIPO"]
      iCB   <- list_vessel[list_vessel$COD == x$Cod.Barco[1], "CB"]
      
      tipo <- rep(iTIPO, len)
      cb   <- rep(iCB, len)
      
    }else{
      tipo <- rep(NA, len)
      cb   <- rep(NA, len)
    }
    
    Cod.Barco      <- x$Cod.Barco  
    Cod.Viaje.VMS  <- x$Cod.Viaje.VMS
    date.GMT       <- x$date.GMT
    Dur.Viaje      <- x$Dur.Viaje 
    Puerto.0.Mar.1 <- x$Puerto.0.Mar.1
    Vel.Cal        <- x$Vel.Cal
    Calas          <- x$Calas
    Lon            <- x$Lon
    Lat            <- x$Lat
    Fecha          <- x$Fecha         
    Temporada      <- x$Temporada
    dc             <- x$dc
    area           <- x$area 
    semana         <- x$semana
    
    cbind.data.frame(Cod.Barco, Cod.Viaje.VMS, date.GMT, Dur.Viaje, 
                     Puerto.0.Mar.1, Vel.Cal, Calas, Lon, Lat, Fecha, 
                     Temporada, dc, area, semana, tipo, cb)
  })
  
  out <- do.call(rbind.data.frame,out); row.names(out) <- NULL
  
  # barco que no disponen de la variable tipo o cb son quitados del analisis
  omit_vessel <- unique(out[is.na(out$tipo), "Cod.Barco"]) 
  out <- out[!out$Cod.Barco %in% omit_vessel,]
  
  return(out)
}

#  ------------------------------------------------------------------------
#  ------------------------------------------------------------------------
#x = data0[data0$semana == 1,]
iTemporalSpatial <- function(data, ...){
  require(ineq)
  require(sp)
  
  index <- lapply(split(data, data$semana, drop = TRUE),function(x){
    
    area_pesca        <- length(unique(x$area))*25
    dc_mean           <- mean(x$dc, na.rm = T)
    dc_lower          <- quantile(x$dc, probs = 0.3, na.rm = T)
    dc_upper          <- quantile(x$dc, probs = 0.7, na.rm = T)
    Lat_mean          <- mean(x$Lat, na.rm = T)
    Lat_lower         <- quantile(x$Lat, probs = 0.3, na.rm = T)
    Lat_upper         <- quantile(x$Lat, probs = 0.7, na.rm = T)
    
    # indicadores de autocorrelación espacial  
    vector     <- sort(as.numeric(table(x$area)),decreasing = TRUE)
    Igini      <- Gini(vector, corr = TRUE)
    Irs        <- RS(vector, na.rm = TRUE)
    Itkinson   <- Atkinson(vector, parameter = 0.5, na.rm = TRUE)
    Itheil     <- Theil(vector, parameter = 0, na.rm = TRUE)
    Ikolm      <- Kolm(vector, parameter = 1, na.rm = TRUE)
    Ivar.coeff <- var.coeff(vector, square = FALSE, na.rm = TRUE)
    Ientropy   <- entropy(vector, parameter = 0.5, na.rm = TRUE)
    semana     <- x$semana[1] + 1
    
    cbind.data.frame(semana, area_pesca, dc_mean, dc_lower, dc_upper,
                     Lat_mean, Lat_lower, Lat_upper, 
                     Igini, Irs, Itkinson, Itheil, Ikolm, Ivar.coeff, Ientropy)
  })
  
  index <- do.call(rbind.data.frame,index); row.names(index) <- NULL
  return(index)
}

#


indicators_spatial_week <- function(data, list_vessel, inicio_temp, fin_temp, space.perfil = -5, ...){
  
  data0 = type_holdCapacity(data, list_vessel) # agrega tipo y capacidad de bodega
  
  data_ind    <- data0[data0$tipo == "IND",]
  data_indmad <- data0[data0$tipo == "IND MAD",]
  
  list_data <- list(3)
  list_data[[1]] = data0
  list_data[[2]] = data_ind
  list_data[[3]] = data_indmad
  
  
  require(ineq)
  out <- list(3)
  
  for(i in 1:3){
    out[[i]] <- iTemporalSpatial(data = list_data[[i]])  
  }
  
  out_report <- list()
  
  out_report$spatial_indicators_total   <- out[[1]]
  out_report$spatial_indicators_ind     <- out[[2]]
  out_report$spatial_indicators_indmad  <- out[[3]]
  out_report$fishing_points <- list_data[[1]]
  
  return(out_report)
}

#  ------------------------------------------------------------------------
#  ------------------------------------------------------------------------
indicators_trajectory <- function(data, list_vessel, inicio_temp, fin_temp, ...){
  
  out_report <- list()
  # x = data[data$Cod.Viaje.VMS == "12977-10",]
  require(dplyr)
  indicators <- lapply(split(data, data$Cod.Viaje.VMS, drop = TRUE),function(x){
    
    if(dim(x)[1] > 10){
      
      tipo <- list_vessel[list_vessel$COD == x$Cod.Barco[1], "TIPO"]
      cb   <- list_vessel[list_vessel$COD == x$Cod.Barco[1], "CB"]
      
      # print(x$Cod.Viaje.VMS[1])
      Fecha.ini <- x$date.GMT[1]
      Fecha.fin <- x$date.GMT[length(x$date.GMT)]
      Fecha     <- x$Fecha[1]
      temp      <- x$Temporada[1]
      
      if(sum(x$Calas) > 1){
        xcala = x[x$Calas == 1,]
        distCalas <- NULL
        for(i in 1:(nrow(xcala)-1)){
          dist <- distXY(xcala[i,"Lon"], xcala[i,"Lat"], xcala[i+1,"Lon"], xcala[i+1,"Lat"])
          distCalas <- c(distCalas, dist) 
        }
        
        dist_calas <- sum(distCalas)
        Lon_zarpe  <- rep(x$Lon[1], nrow(xcala))
        Lon_arribo <- rep(x$Lon[length(x$Lat)], nrow(xcala))
        Lat_zarpe  <- rep(x$Lat[1], nrow(xcala))
        Lat_arribo <- rep(x$Lat[length(x$Lat)], nrow(xcala))
        Cod.Viaje.VMS <- rep(x$Cod.Viaje.VMS[1], nrow(xcala))
        Lon <- xcala$Lon
        Lat <- xcala$Lat
        
        hora_zarpe_1cala <- as.numeric(difftime(x$date.GMT[x$Calas == 1][1],x$date.GMT[1], units = "hours"))
      }
      if(sum(x$Calas) == 1){
        dist_calas <- NA
        Lon_zarpe   <- x$Lon[1]
        Lon_arribo  <- x$Lon[length(x$Lat)]
        Lat_zarpe   <- x$Lat[1]
        Lat_arribo  <- x$Lat[length(x$Lat)]
        Cod.Viaje.VMS <- x$Cod.Viaje.VMS[1]
        Lon <- x$Lon[x$Calas == 1]
        Lat <- x$Lat[x$Calas == 1]
        
        hora_zarpe_1cala <- as.numeric(difftime(x$date.GMT[x$Calas == 1][1],x$date.GMT[1], units = "hours"))
        
      }
      if(sum(x$Calas) ==0){
        dist_calas <- NA
        Lon_zarpe   <- x$Lon[1]
        Lon_arribo  <- x$Lon[length(x$Lat)]
        Lat_zarpe   <- x$Lat[1]
        Lat_arribo  <- x$Lat[length(x$Lat)]
        Cod.Viaje.VMS <- x$Cod.Viaje.VMS[1]
        Lon <- NA
        Lat <- NA
        
        hora_zarpe_1cala <- NA
      }
      require(SISESATools)
      
      #SISESATools::puertos
      Puerto_zarpe  <- as.character(puertos[which.min(distAB(Lon_zarpe[1],Lat_zarpe[1], puertos$longitud, puertos$latitud)),"puerto2"])
      Puerto_arribo <- as.character(puertos[which.min(distAB(Lon_arribo[1],Lat_arribo[1], puertos$longitud, puertos$latitud)),"puerto2"])
      
      Puerto_zarpe <- rep(Puerto_zarpe, length(temp))
      Puerto_arribo <- rep(Puerto_arribo, length(temp))
      
      # distancia entre emisiones
      x$distancia_emision <- NA
      x$distancia_emision[2:(length(x[,1]))] <- distORTODROMICA(x$Lon[1:(length(x[,1])-1)],x$Lat[1:(length(x[,1])-1)],x$Lon[2:length(x[,1])],x$Lat[2:length(x[,1])])
      
      # rumbo
      x$rumbo  <- NA
      x$rumbo  <- calcularRumbo(x$Lon,x$Lat)#$vectorRUMBO
      rumbo_inicial <- x$rumbo[1+1]
      rumbo_final   <- x$rumbo[length(x$rumbo)-1]
      
      # angulo
      x$angle  <- NA
      x$angle[2:(length(x$angle)-1)]  <- angle(x$Lon, x$Lat)
      
      angulo_inicial <- x$angle[1+1]
      angulo_final   <- x$angle[length(x$angle)-1]
      
      # 
      sinuosidad1  <- Sinuosidad1(x$angle,x$distancia_emision)
      sinuosidad2  <- Sinuosidad2(x$angle,x$distancia_emision)
      sinuosidad3  <- Sinuosidad3(x$angle)
      
      if(length(cb) == 0){
        cb   <- NA
        tipo <- NA
      }
      
      cbind.data.frame(Fecha.ini, Fecha.fin, Fecha, temp, Cod.Viaje.VMS,Puerto_zarpe, Puerto_arribo, Lon_zarpe, 
                       Lon_arribo, Lat_zarpe, Lat_arribo, Lon, Lat, dist_calas,
                       angulo_inicial, angulo_final, rumbo_inicial, rumbo_final,
                       sinuosidad1 = sinuosidad1, sinuosidad2 = sinuosidad2, sinuosidad3 = sinuosidad3,
                       tipo = tipo, cb = cb)
      
    }  
  })
  indicators <- do.call(rbind.data.frame,indicators); row.names(indicators) <- NULL
  
  out.semana <- imarpe:::estima_semana(inicio.temp = inicio_temp, fin.temp = fin_temp)
  
  indicators$semana = NA
  for(i in sort(unique(indicators$Fecha))){
    indicators[indicators$Fecha == i, "semana"] = out.semana[out.semana$diasTemporada == i, "semana"]
  }
  
  # space = (0:(length(unique(indicators$semana))-1))*space.perfil
  # 
  # indicators00 = indicators
  # for(i in 1:length(unique(indicators00$semana))){
  #   indicators00[indicators00$semana == rev(sort(unique(indicators00$semana)))[i],"Lon"] = indicators00[indicators00$semana == rev(sort(unique(indicators00$semana)))[i],"Lon"] + space[i]
  # }
  
  # total
  semana         <- sort(as.numeric(unique(indicators$semana) + 1))
  dist_calas     <- fun_mean_IC(indicators$dist_calas, indicators$semana)
  angulo_inicial <- fun_mean_IC(indicators$angulo_inicial, indicators$semana)
  angulo_final   <- fun_mean_IC(indicators$angulo_final, indicators$semana)
  rumbo_inicial  <- fun_mean_IC(indicators$rumbo_inicial, indicators$semana)
  rumbo_final    <- fun_mean_IC(indicators$rumbo_final, indicators$semana)
  sinuosidad_1   <- fun_mean_IC(indicators$sinuosidad1, indicators$semana)
  sinuosidad_2   <- fun_mean_IC(indicators$sinuosidad2, indicators$semana)
  sinuosidad_3   <- fun_mean_IC(indicators$sinuosidad3, indicators$semana)
  
  # industrial
  semana_ind            <- sort(as.numeric(unique(indicators$semana[indicators$tipo == "IND"]) + 1))
  dist_calas_ind        <- fun_mean_IC(indicators$dist_calas[indicators$tipo == "IND"], indicators$semana[indicators$tipo == "IND"])
  angulo_inicial_ind    <- fun_mean_IC(indicators$angulo_inicial[indicators$tipo == "IND"], indicators$semana[indicators$tipo == "IND"])
  angulo_final_ind      <- fun_mean_IC(indicators$angulo_final[indicators$tipo == "IND"], indicators$semana[indicators$tipo == "IND"])
  rumbo_inicial_ind     <- fun_mean_IC(indicators$rumbo_inicial[indicators$tipo == "IND"], indicators$semana[indicators$tipo == "IND"])
  rumbo_final_ind       <- fun_mean_IC(indicators$rumbo_final[indicators$tipo == "IND"], indicators$semana[indicators$tipo == "IND"])
  sinuosidad_1_ind      <- fun_mean_IC(indicators$sinuosidad1[indicators$tipo == "IND"], indicators$semana[indicators$tipo == "IND"])
  sinuosidad_2_ind      <- fun_mean_IC(indicators$sinuosidad2[indicators$tipo == "IND"], indicators$semana[indicators$tipo == "IND"])
  sinuosidad_3_ind      <- fun_mean_IC(indicators$sinuosidad3[indicators$tipo == "IND"], indicators$semana[indicators$tipo == "IND"])
  
  # industrial de madera
  semana_indmad            <- sort(as.numeric(unique(indicators$semana[indicators$tipo == "IND MAD"]) + 1))
  dist_calas_indmad     <- fun_mean_IC(indicators$dist_calas[indicators$tipo == "IND MAD"], indicators$semana[indicators$tipo == "IND MAD"])
  angulo_inicial_indmad <- fun_mean_IC(indicators$angulo_inicial[indicators$tipo == "IND MAD"], indicators$semana[indicators$tipo == "IND MAD"])
  angulo_final_indmad   <- fun_mean_IC(indicators$angulo_final[indicators$tipo == "IND MAD"], indicators$semana[indicators$tipo == "IND MAD"])
  rumbo_inicial_indmad  <- fun_mean_IC(indicators$rumbo_inicial[indicators$tipo == "IND MAD"], indicators$semana[indicators$tipo == "IND MAD"])
  rumbo_final_indmad    <- fun_mean_IC(indicators$rumbo_final[indicators$tipo == "IND MAD"], indicators$semana[indicators$tipo == "IND MAD"])
  sinuosidad_1_indmad   <- fun_mean_IC(indicators$sinuosidad1[indicators$tipo == "IND MAD"], indicators$semana[indicators$tipo == "IND MAD"])
  sinuosidad_2_indmad   <- fun_mean_IC(indicators$sinuosidad2[indicators$tipo == "IND MAD"], indicators$semana[indicators$tipo == "IND MAD"])
  sinuosidad_3_indmad   <- fun_mean_IC(indicators$sinuosidad3[indicators$tipo == "IND MAD"], indicators$semana[indicators$tipo == "IND MAD"])
  
  tableResult           <- data.frame(semana = semana, dist_calas = dist_calas$x, angulo_inicial = angulo_inicial$x, angulo_final = angulo_final$x, rumbo_inicial = rumbo_inicial$x, rumbo_final = rumbo_final$x, sinuosidad_1 = sinuosidad_1$x, sinuosidad_2 = sinuosidad_2$x, sinuosidad_3 = sinuosidad_3$x)
  sdtableResult         <- data.frame(semana = semana, dist_calas = dist_calas$sterr, angulo_inicial = angulo_inicial$sterr, angulo_final = angulo_final$sterr, rumbo_inicial = rumbo_inicial$sterr, rumbo_final = rumbo_final$sterr, sinuosidad_1 = sinuosidad_1$sterr, sinuosidad_2 = sinuosidad_2$sterr, sinuosidad_3 = sinuosidad_3$sterr)
  
  tableResult_ind       <- data.frame(semana = semana_ind, dist_calas = dist_calas_ind$x, angulo_inicial = angulo_inicial_ind$x, angulo_final = angulo_final_ind$x, rumbo_inicial = rumbo_inicial_ind$x, rumbo_final = rumbo_final_ind$x, sinuosidad_1 = sinuosidad_1_ind$x, sinuosidad_2 = sinuosidad_2_ind$x, sinuosidad_3 = sinuosidad_3_ind$x)
  sdtableResult_ind     <- data.frame(semana = semana_ind, dist_calas = dist_calas_ind$sterr, angulo_inicial = angulo_inicial_ind$sterr, angulo_final = angulo_final_ind$sterr, rumbo_inicial = rumbo_inicial_ind$sterr, rumbo_final = rumbo_final_ind$sterr, sinuosidad_1 = sinuosidad_1_ind$sterr, sinuosidad_2 = sinuosidad_2_ind$sterr, sinuosidad_3 = sinuosidad_3_ind$sterr)
  
  tableResult_indmad    <- data.frame(semana = semana_indmad, dist_calas = dist_calas_indmad$x, angulo_inicial = angulo_inicial_indmad$x, angulo_final = angulo_final_indmad$x, rumbo_inicial = rumbo_inicial_indmad$x, rumbo_final = rumbo_final_indmad$x, sinuosidad_1 = sinuosidad_1_indmad$x, sinuosidad_2 = sinuosidad_2_indmad$x, sinuosidad_3 = sinuosidad_3_indmad$x)
  sdtableResult_indmad  <- data.frame(semana = semana_indmad, dist_calas = dist_calas_indmad$sterr, angulo_inicial = angulo_inicial_indmad$sterr, angulo_final = angulo_final_indmad$sterr, rumbo_inicial = rumbo_inicial_indmad$sterr, rumbo_final = rumbo_final_indmad$sterr, sinuosidad_1 = sinuosidad_1_indmad$sterr, sinuosidad_2 = sinuosidad_2_indmad$sterr, sinuosidad_3 = sinuosidad_3_indmad$sterr)
  
  out_report$index_trajectory_total           <- tableResult
  out_report$index_trajectory_total_ind       <- tableResult_ind
  out_report$index_trajectory_total_indmad    <- tableResult_indmad
  
  out_report$sd_index_trajectory_total        <- sdtableResult
  out_report$sd_index_trajectory_total_ind    <- sdtableResult_ind
  out_report$sd_index_trajectory_total_indmad <- sdtableResult_indmad
  
  return(out_report)
}


#  ------------------------------------------------------------------------

input_report <- function(object1, object2, object3){
  
  out_report <- list()
  
  out_report$effort_total   <- out_report1$effort_total
  out_report$effort_ind     <- out_report1$effort_ind
  out_report$effort_indmad  <- out_report1$effort_indmad
  
  out_report$sdt_effort_total  <- out_report1$sdt_effort_total
  out_report$sdt_effort_ind    <- out_report1$sdt_effort_ind
  out_report$sdt_effort_indmad <- out_report1$sdt_effort_indmad
  
  out_report$spatial_indicators_total   <- object2$spatial_indicators_total
  out_report$spatial_indicators_ind     <- object2$spatial_indicators_ind
  out_report$spatial_indicators_indmad  <- object2$spatial_indicators_indmad
  out_report$fishing_points             <- object2$fishing_points
  
  out_report$index_trajectory_total        <- object3$index_trajectory_total    
  out_report$index_trajectory_total_ind    <- object3$index_trajectory_total_ind
  out_report$index_trajectory_total_indmad <- object3$index_trajectory_total_indmad
  
  out_report$sd_index_trajectory_total        <- object3$sd_index_trajectory_total    
  out_report$sd_index_trajectory_total_ind    <- object3$sd_index_trajectory_total_ind
  out_report$sd_index_trajectory_total_indmad <- object3$sd_index_trajectory_total_indmad 
  
  return(out_report)
}



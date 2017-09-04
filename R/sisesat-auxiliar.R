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
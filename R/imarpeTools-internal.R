# internal functions

.coord2text = function(coord, type) {
  # write nicely coordinates
  degree = "\U00B0"
  hemi = if(coord==0) {
    rep(degree, 2)
  } else {
    if(coord>0) paste0(degree, c("N","E")) else paste0(degree, c("S","W"))
  }
  
  out = switch(type,
               lat = paste0(abs(coord), hemi[1]),
               lon = paste0(abs(coord), hemi[2]),
               as.character(coord)
  )
  return(out)
}

# Function to add spaces and colon to rownames

.RefineChar4Table = function(RowNames){
  NSpace=max(nchar(RowNames))-nchar(RowNames)
  Vec=1:length(RowNames)
  Z=lapply(Vec,function(y){paste(c(RowNames[y],
                                   rep(" ",NSpace[y]),":"),sep="",collapse="")})
  Z=as.character(Z)
  return(Z)
}

## "Mixed Case" Capitalizing - toupper( every first letter of a word ) :
.capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
{s <- substring(s, 2); if(strict) tolower(s) else s},
sep = "", collapse = " " )
sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

## "Natural Mortality", empirical 
N_mortalidad <- 
          function (Linf = NULL, Klon = NULL, Pinf = NULL ,Kpeso = NULL, Temp = NULL, 
                    IGS = NULL, tmax = NULL, tm = NULL, Phum = NULL, Psec = NULL, 
          metodo = c(1, 2, 3, 4, 5, 6, 7, 8)) 
{
  if (any(metodo == 1) & any(is.null(Linf), is.null(Klon), is.null(Temp))) 
    stop("Método 1 requiere valores de Longitud infinita (Linf), Klon, y Temperatura (Temp)")
  if (any(metodo == 2) & any(is.null(Pinf), is.null(Kpeso), is.null(Temp))) 
    stop("Método 2 requiere valores de Peso infinito (Pinf), Kpeso, y Temperatura (Temp)")
  if (any(metodo == 3) & is.null(IGS)) 
    stop("Método 6 requiere valores de Índice Gonado-Somático (IGS)")
  if (any(metodo == 4) & is.null(tmax)) 
    stop("Método 3 requiere valores de tmax (edad máxima)")
  if (any(metodo == 5) & any(is.null(tmax), is.null(Klon))) 
    stop("Método 4 requiere valores de Klon y tmax (edad máxima)")
  if (any(metodo == 6) & any(is.null(tm), is.null(Klon))) 
    stop("Método 5 requiere valores de Klon y tm (edad de madurez)")
  if (any(metodo == 7) & is.null(Phum)) 
    stop("Método 7 requiere valores de Peso húmedo (Phum)")
  if (any(metodo == 8) & is.null(Psec)) 
    stop("Método 8 requiere valores Peso seco (Psec)")
  n <- length(metodo)
  if (any(metodo == 3)) 
    n <- n + 1
  out <- matrix(NA, n, 1L)
  dimnames(out) <- list(rep(NA, n), c("Mortalidad Natural (M)"))
  wrap <- 0
  if (any(metodo == 1)) {
    wrap <- wrap + 1
    out[wrap, 1] <- round(10^(-0.0066 - 0.279 * log10(Linf) + 0.6543 * log10(Klon) + 0.4634 * log10(Temp)), 2)
    dimnames(out)[[1]][wrap] <- list("Pauly (1980) - M con respecto al crecimiento en Longitud")
    if (Temp <= 5 || Temp > 30) 
      warning("Revisar los límites de Temperatura (Temp). No deben ser menores a 5°C ni mayores a 30°C")
  }
  if (any(metodo == 2)) {
    wrap <- wrap + 1
    out[wrap, 1] <- round(10^(-0.2107 - 0.0824 * log10(Pinf) + 
                               0.6757 * log10(Kpeso) + 0.4687 * log10(Temp)), 2)
    dimnames(out)[[1]][wrap] <- list("Pauly (1980) - M con respecto al crecimiento en Peso")
    if (Temp <= 5 || Temp > 30) 
      warning("Revisar los límites de Temperatura (Temp). No deben ser menores a 5°C ni mayores a 30°C")
  }
  if (any(metodo == 3)) {
    wrap <- wrap + 1
    out[wrap, 1] <- round(0.03 + 1.68 * IGS, 2)
    dimnames(out)[[1]][wrap] <- list("Gunderson y Dygert (1988)")
    if (T < 4 || T > 30) 
      warning("Revisar los límites de Temperatura (Temp) -- <4 or >30")
  }
  if (any(metodo == 4)) {
    if (tmax < 0.5 || tmax > 300) 
      stop("Error: edad máxima es < 0.5 o > 300.")
    wrap <- wrap + 1
    out[wrap, 1] <- round(exp(1.46 - 1.01 * log(tmax)), 2)
    dimnames(out)[[1]][wrap] <- list("Hoenig (1983) - Ecuación para teleósteos")
    wrap <- wrap + 1
    out[wrap, 1] <- round(4.22/(tmax^0.982), 2)
    dimnames(out)[[1]][wrap] <- list("Hoenig (1983) - Ecuación combinada (teleósteos + moluscos + cetaceos)")
  }
  if (any(metodo == 5)) {
    wrap <- wrap + 1
    out[wrap, 1] <- round((3 * Klon)/(exp(Klon * (0.38 * tmax)) - 1), 2)
    dimnames(out)[[1]][wrap] <- list("Alverson y Carney (1975)")
  }
  if (any(metodo == 6)) {
    wrap <- wrap + 1
    out[wrap, 1] <- round((3 * Klon)/(exp(Klon * tm) - 1), 2)
    dimnames(out)[[1]][wrap] <- list("Roff (1984)")
  }
  if (any(metodo == 7)) {
    wrap <- wrap + 1
    out[wrap, 1] <- round(3 * (Phum^-0.288), 2)
    dimnames(out)[[1]][wrap] <- list("Lorenzen (1996)")
  }
  if (any(metodo == 8)) {
    wrap <- wrap + 1
    out[wrap, 1] <- round(1.92 * Psec^-0.25, 2)
    dimnames(out)[[1]][wrap] <- list("Peterson y Wroblewski (1984)")
  }
  return(out)
}


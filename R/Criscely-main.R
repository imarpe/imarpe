#Archivo de Criscely! :D
# Aqui las funciones de Criscely

.getSpeciesComposition.bitacoras = function(object, ...) {
  
  datos = object$data
  datos = datos[, c("anchoveta", "sardina", "jurel", "caballa", "otros")]
  datos = datos[!is.na(datos$anchoveta & datos$sardina & datos$jurel
                       & datos$caballa & datos$otros), ]
  datos = colSums(datos, na.rm=TRUE)
  
  species = tolower(names(datos))
  names(datos) = species
    
  porcentaje = 100*datos/sum(datos, na.rm=TRUE)
  
  #Para la tabla de profundidad
  # nunca hagas una matriz combinando caracteres con numeros!
  # para eso estan los nombres de fila y columna
  tabla = data.frame(datos, porcentaje)
  tabla = rbind(tabla, colSums(tabla, na.rm=TRUE))
  colnames(tabla) = c("Capturas", "Porcentaje")
  rownames(tabla) = c(species, "Total")
   
  return(tabla)
}


.getEffort.bitacoras = function(object) {
  
  datos = object$data
  datos = datos[, c("puerto.salida", "dv", "cala", "total.calas", "motivo.viaje.cero")]
  datos = datos[!grepl("2", tabla1$motivo.viaje.cero), ] #Exonerando motivo de viaje cero
  
  
  duracionViaje = aggregate(datos$dv, by=list(datos$puerto.salida), FUN=mean, na.rm=TRUE)
  numeroCala = aggregate(datos$total.calas, by=list(datos$puerto.salida), FUN=mean, na.rm=TRUE)
  
  #Para la tabla de esfuerzo
  puerto=duracionViaje[,1]
  tabla = data.frame(duracion=duracionViaje$x, cala=numeroCala$x)
  
  basePuerto = c("paita", "bayobar", "parachique", "chicama", "coishco", "chimbote",
                 "samanco", "huarmey", "supe", "vegueta", "huacho", "chancay", "callao",
                 "tambo de mora", "pisco", "quilca", "la planchada", "mollendo", 
                 "ilo", "morrosama")
  ordenCol = basePuerto[sort(match(puerto, basePuerto), decreasing = FALSE)]
<<<<<<< HEAD
  tabla = tabla[match(ordenCol, puerto), ]
  rownames(tabla) = puerto
  
  return(tabla)
=======
  tablaOrdenada = tabla[match(ordenCol, puerto), ]
  rownames(tablaOrdenada) = puerto
  
  return(tablaOrdenada)
>>>>>>> 4a965a06c387f7c0a30f2a4346c66812933ce9bd
}



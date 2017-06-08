# 
# getData =  function(file, type, varType, toTons=TRUE, sp, start=NULL, end = NULL, port = NULL, efforType, ...){
#   
#   output = switch(tolower(type),
#                   fisheryinfo = .getFisheryData(file=file, varType=varType, toTons = toTons, sp=sp,
#                                             start=start, end=end, port=port),
#                   cpue        = .getCPUEData(file=file, toTons = toTons, sp=sp,
#                                              start=start, end=end, port=port, efforType),
#                   read.csv(file = file, stringsAsFactors = FALSE, ...))
#   
#   return(output)
# }

# getData =  function(file, type, varType=NULL, toTons=TRUE, sp, start=NULL, end = NULL, port = NULL,
#                     efforType = "viaje", fleeType, ...){
#   
#   #getdat: adicionar esto
#   dataBase = convertBase(file=file, sp=sp, fleeType=fleeType, efforType=efforType)
#   
#   output = switch(tolower(type),
#                   #fleet      = .getFleetData(file)
#                   fisheryinfo = .getFisheryData(file=dataBase, varType=varType, toTons = toTons, sp=sp,
#                                                 start=start, end=end, port=port),
#                   cpue        = .getCPUEData(file=dataBase, toTons = toTons, sp=sp,
#                                              start=start, end=end, port=port, efforType, varType=varType),
#                   read.csv(file = dataBase, stringsAsFactors = FALSE, ...))
#   
#   return(output)
# }


getData =  function(file, type, varType, toTons=TRUE, sp, start=NULL, end = NULL, port = NULL,
                    efforType = "viaje", fleeType=NULL, ...){
  
  #getdat: adicionar esto
  dataBase = .convertBase(file=file, sp=sp, fleeType=fleeType, efforType=efforType)
  fleet    = .fleetData(file = file, varType = varType, toTons = toTons, sp = sp, efforType=efforType, fleeType = fleeType, ...)
  
  output = switch(tolower(type),
           fisheryinfo = .getFisheryData(x=dataBase, fileName = file, fleet = fleet, varType=varType, 
                                         toTons = toTons, sp=sp, start=start, end=end, port=port),
           cpue        = .getCPUEData(x=dataBase, fileName = file, fleet = fleet,
                                      toTons = toTons, sp=sp, start=start, end=end, port=port, efforType),
           read.csv(file = dataBase, stringsAsFactors = FALSE, ...))
  
  return(output)
}
# auxiliary functions

coord2text = function(coord, type) {
  if(!is.character(type)) type=deparse(substitute(type))
  out = sapply(coord, FUN=.coord2text, type=type)
  return(out)
}

# Function to add spaces and colon to rownames

RefineChar4Table = function(RowNames){
  NSpace=max(nchar(RowNames))-nchar(RowNames)
  Vec=1:length(RowNames)
  Z=lapply(Vec,function(y){paste(c(RowNames[y],
                                   rep(" ",NSpace[y]),":"),sep="",collapse="")})
  Z=as.character(Z)
  return(Z)
}
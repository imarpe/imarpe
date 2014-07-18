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

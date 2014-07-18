# auxiliary functions

coord2text = function(coord, type) {
  if(!is.character(type)) type=deparse(substitute(type))
  out = sapply(coord, FUN=.coord2text, type=type)
  return(out)
}


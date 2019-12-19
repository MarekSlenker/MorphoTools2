# internal

# @param object object of class morphodata
# @param column column where to look for groupName
# @param groupName name of Population or Taxon

removeColumn <- function(object, column, groupName) {
  # obj je triedy morfodata, skontrolovane vyssie

  # groupName moze byt i viac
  toRemove = array(data = NA, dim = 0)
  for (name in groupName) {
    toRemove = c(toRemove, which( unlist(object[column]) %in% groupName) )
  }

  newObject = newMorphodata()
  newObject$ID = droplevels( object$ID[-toRemove] )
  newObject$Population = droplevels( object$Population[-toRemove] )
  newObject$Taxon = droplevels( object$Taxon[-toRemove] )
  newObject$data = object$data[-toRemove, ]

  return(newObject)
}

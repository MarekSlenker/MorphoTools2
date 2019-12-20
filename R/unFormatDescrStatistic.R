# internal

# @param object object of class morphodata
# @param descrStatistic object retrived from mthod descrByGroup()
# @param format form to which will be formated descriptive characters

unFormatDescrStatistic <- function(groups, characters, descrStatistic, format) {

  outputTable = data.frame("characters" = c("Taxon",  characters))

  for (group in groups){
    firstRow = rep(group, dim(descrStatistic[ , , group])[2])
    outputTable = cbind(outputTable, rbind(firstRow, descrStatistic[ , , group]))
  }
  return(outputTable)
}

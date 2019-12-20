# internal

# @param object object of class morphodata
# @param descrStatistic object retrived from mthod descrByGroup()
# @param format form to which will be formated descriptive characters

formatDescrStatistic <- function(groups, characters, descrStatistic, format) {
  outputTable = as.data.frame(matrix(NA, nrow = length(characters) + 2, ncol = length(groups) + 1))

  outputTable[ , 1] = c("format", "N", characters)
  colnames(outputTable)[1] = "group"

  for (group in groups) {
    groupPosition = match(group, groups)

    # name a column as group
    colnames(outputTable)[groupPosition + 1] = group

    # insert "format" - what will appear in the table
    outputTable[ 1 , groupPosition + 1] = gsub("\\$", "", format)

    # insert N of individuals
    outputTable[ 2 , groupPosition + 1] = descrStatistic[ 1, "N",  group]


    # fill row table with strings, according the format string
    for (char in characters){
      columnString = format
      columnString = gsub("\\$MEAN", descrStatistic[ char, "Mean",  group], columnString)
      columnString = gsub("\\$SD", descrStatistic[ char, "SD",  group], columnString)
      columnString = gsub("\\$MIN", descrStatistic[ char, "Min",  group], columnString)
      columnString = gsub("\\$5%", descrStatistic[ char, "5%",  group], columnString)
      columnString = gsub("\\$25%", descrStatistic[ char, "25%",  group], columnString)
      columnString = gsub("\\$MEDIAN", descrStatistic[ char, "Median",  group], columnString)
      columnString = gsub("\\$75%", descrStatistic[ char, "75%",  group], columnString)
      columnString = gsub("\\$95%", descrStatistic[ char, "95%",  group], columnString)
      columnString = gsub("\\$MAX", descrStatistic[ char, "Max",  group], columnString)

      charPosition = match(char, characters)
      outputTable[ charPosition + 2 , groupPosition + 1] = columnString
    }
  }

  return(outputTable)
}

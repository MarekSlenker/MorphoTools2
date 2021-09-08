#' Descriptive statistics
#' @export
descrTaxon <- function(object, format = NULL, decimalPlaces = 3) {
  .checkClass(object, "morphodata")

  # calculate descr stat
  descrStatistic = .descrByGroup(object, "Taxon", decimalPlaces)

  taxa =  levels(object$Taxon)
  characters = colnames(object$data)

  if ( !(is.null(format))){
    # format output according to user specification
    return(.formatDescrStatistic(taxa, characters, descrStatistic, format))
  }
  else{
    # do not format descr stat for export ~ row data
    return(.un.formatDescrStatistic(taxa, characters, descrStatistic, format))
  }
}

#' @rdname descrTaxon
#' @export
descrPopulation <- function(object, format = NULL, decimalPlaces = 3) {
  .checkClass(object, "morphodata")

  # calculate descr stat
  descrStatistic = .descrByGroup(object, "Population", decimalPlaces)

  populs =  levels(object$Population)
  characters = colnames(object$data)

  if ( !(is.null(format))){
    # format output according to user specification
    return(.formatDescrStatistic(populs, characters, descrStatistic, format))
  }
  else{
    # do not format descr stat for export ~ row data
    return(.un.formatDescrStatistic(populs, characters, descrStatistic, format))
  }
}

#' @rdname descrTaxon
#' @export
descrAll <- function(object, format = NULL, decimalPlaces = 3) {
  .checkClass(object, "morphodata")

  object$all = as.factor( rep("all", length(object$Taxon)))

  # calculate descr stat
  descrStatistic = .descrByGroup(object, "all", decimalPlaces)

  #alls =  levels(object$all)
  characters = colnames(object$data)

  if ( !(is.null(format))){
    # format output according to user specification
    return(.formatDescrStatistic("all", characters, descrStatistic, format))
  }
  else{
    # do not format descr stat for export ~ row data
    return(.un.formatDescrStatistic("all", characters, descrStatistic, format))
  }
}



# internal

# @param object object of class morphodata
# @param column Population, Taxon, or whole dataset - levels for calculating descriptive statistics

.descrByGroup <- function(object, column, digits) {
  # obj je triedy morfodata, skontrolovane vyssie

  characters = colnames(object$data)
  descriptors = c("N","Mean","SD","Min","5%","25%","Median","75%","95%","Max")
  groups =  levels(unlist(object[column]))

  descrStatistic = array(numeric(), c(length(characters), length(descriptors), length(groups)), dimnames = list(characters, descriptors, groups))

  # calculate descr statistics
  for (group in groups) {
    groupPositions = which( unlist(object[column]) %in% group)

    # number of observations
    descrStatistic[ , "N", group] = length(groupPositions)

    # mean
    descrStatistic[ , "Mean", group] = sapply(object$data[groupPositions, ], mean, na.rm = TRUE)

    # SD
    descrStatistic[ , "SD", group] = sapply(object$data[groupPositions, ], stats::sd, na.rm=TRUE)

    # Min
    descrStatistic[ , "Min", group] = sapply(object$data[groupPositions, ], stats::quantile, probs=0, na.rm=TRUE)

    # 5%
    descrStatistic[ , "5%", group] = sapply(object$data[groupPositions, ], stats::quantile, probs=0.05, na.rm=TRUE)

    # 25%
    descrStatistic[ , "25%", group] = sapply(object$data[groupPositions, ], stats::quantile, probs=0.25, na.rm=TRUE)

    # Median
    descrStatistic[ , "Median", group] = sapply(object$data[groupPositions, ], stats::quantile, probs=0.5, na.rm=TRUE)

    # 75%
    descrStatistic[ , "75%", group] = sapply(object$data[groupPositions, ], stats::quantile, probs=0.75, na.rm=TRUE)

    # 95%
    descrStatistic[ , "95%", group] = sapply(object$data[groupPositions, ], stats::quantile, probs=0.95, na.rm=TRUE)

    # Max
    descrStatistic[ , "Max", group] = sapply(object$data[groupPositions, ], stats::quantile, probs=1, na.rm=TRUE)
  }

  descrStatistic = round(descrStatistic, digits = digits)
  descrStatistic[which(is.nan(descrStatistic))] = NA

  return(descrStatistic)
}


# internal

# @param object object of class morphodata
# @param descrStatistic object retrived from mthod .descrByGroup()
# @param format form to which will be formated descriptive characters

.formatDescrStatistic <- function(groups, characters, descrStatistic, format) {
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


# internal

# @param object object of class morphodata
# @param descrStatistic object retrived from mthod .descrByGroup()
# @param format form to which will be formated descriptive characters
.un.formatDescrStatistic <- function(groups, characters, descrStatistic, format) {

  outputTable = data.frame("characters" = c("Taxon",  characters))

  for (group in groups){
    characters = rep(group, dim(descrStatistic[ , , group])[2])
    outputTable = cbind(outputTable, rbind(characters, descrStatistic[ , , group]))
  }
  row.names(outputTable) = NULL
  return(outputTable)
}



# internal

# @param object object of class morphodata
# @param column Population, Taxon, or whole dataset - levels for calculating descriptive statistics

descrByGroup <- function(object, column) {
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
    descrStatistic[ , "Mean", group] = sapply(object$data[groupPositions, ], mean, na.rm=T)

    # SD
    descrStatistic[ , "SD", group] = sapply(object$data[groupPositions, ], sd, na.rm=T)

    # Min
    descrStatistic[ , "Min", group] = sapply(object$data[groupPositions, ], quantile, probs=0, na.rm=T)

    # 5%
    descrStatistic[ , "5%", group] = sapply(object$data[groupPositions, ], quantile, probs=0.05, na.rm=T)

    # 25%
    descrStatistic[ , "25%", group] = sapply(object$data[groupPositions, ], quantile, probs=0.25, na.rm=T)

    # Median
    descrStatistic[ , "Median", group] = sapply(object$data[groupPositions, ], quantile, probs=0.5, na.rm=T)

    # 75%
    descrStatistic[ , "75%", group] = sapply(object$data[groupPositions, ], quantile, probs=0.75, na.rm=T)

    # 95%
    descrStatistic[ , "95%", group] = sapply(object$data[groupPositions, ], quantile, probs=0.95, na.rm=T)

    # Max
    descrStatistic[ , "Max", group] = sapply(object$data[groupPositions, ], quantile, probs=1, na.rm=T)
  }

  descrStatistic = round(descrStatistic, digits = 3)
  descrStatistic[which(is.nan(descrStatistic))] = NA

  return(descrStatistic)
}

#' Shapiro-Wilk Normality Test
#' @export
shapiroWilkTest <- function(object, p.value = 0.05) {
  .checkClass(object, "morphodata")


  characters = colnames(object$data)
  groups =  levels(unlist(object$Taxon))

  shapiroWilkStatistic = array(numeric(), c(length(characters), length(groups)), dimnames = list(characters, groups))

  shapiroWilkStatistic = as.data.frame(shapiroWilkStatistic)

  # calculate descr statistics
  for (group in groups) {
    groupPositions = which( unlist(object$Taxon) %in% group)

    shapiroWilkStatistic[group] = apply(object$data[groupPositions, ], 2,
                                        function(x) {
                                          out <- tryCatch(
                                            {
                                              stats::shapiro.test(x)$p.value
                                            },
                                            error=function(cond) {
                                              return(NaN)
                                            }
                                          )
                                          return(out)
                                        }
                                      )
  }

  shapiroWilkStatistic = as.matrix(shapiroWilkStatistic)

  if (! is.na(p.value)) {

    greaterThenP = which(shapiroWilkStatistic > p.value)
    lowerThenP = which(shapiroWilkStatistic <= p.value)

    shapiroWilkStatistic[greaterThenP] = "normally distributed"
    shapiroWilkStatistic[lowerThenP] = "NOT normally distributed"

  }
  shapiroWilkStatistic = as.data.frame(shapiroWilkStatistic)

  return(shapiroWilkStatistic)
}


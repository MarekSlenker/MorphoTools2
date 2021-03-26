

#' Summarize percentage and number of missing characters on the desired grouping level.
#' @importFrom stats aggregate
#' @export
missingCharactersTable <- function(object, level) {
  checkClass(object, "morphodata")

  if (level!="taxon" & level!="pop" & level!="indiv")  stop("Invalid level of grouping. Consider using \"taxon\", \"pop\" or \"indiv\"")

  switch(level,
     taxon={
         aggLevel = list(object$Taxon)
         header = data.frame(unique(object$Taxon))
         colnames(header) = level

      },
     pop={
           aggLevel = list(object$Population)
          header = data.frame(object$Population, object$Taxon)
          header = header[! duplicated(header[,1]),]
           colnames(header) = c(level, "Taxon")
      },
     indiv={
           aggLevel = list(object$ID)
          header = data.frame(object$ID, object$Population, object$Taxon)
          colnames(header) = c(level, "Population", "Taxon")
      }
  )

  missingTable = data.frame(
    stats::aggregate( apply(object$data, 1, function(x) mean(is.na(x))),  # MEAN
                                aggLevel, function(x) round(mean(x), digits = 2)),
    stats::aggregate( apply(object$data, 1, function(x) sum(is.na(x))),   # SUM
                                aggLevel, sum)$x
  )

  t = as.data.frame(table(aggLevel))

  colnames(missingTable) = c(level, "missing.percentage", "numb.of.missing.values")
  colnames(t) = c(level, "N")

  missingTable = merge(t, missingTable, by=level)
  missingTable = merge(header, missingTable, by=level)
  # SORT??
  # missingTable = missingTable[order(-missingTable$missing.percentage),]

  return(missingTable)
}


#' Summarize percentage of missing samples in characters on the desired grouping level.
#' @importFrom stats aggregate
#' @export
missingSamplesTable <- function(object, level) {
  checkClass(object, "morphodata")

  if (level!="taxon" & level!="pop" & level!="indiv")  stop("Invalid level of grouping. Consider using \"taxon\", \"pop\" or \"indiv\"")

  switch(level,
         taxon={
           aggLevel = list(object$Taxon)
         },
         pop={
           aggLevel = list(object$Population)
         },
         indiv={
           aggLevel = list(object$ID)
         }
  )


  missingTable = stats::aggregate( object$data, aggLevel, function(x) round(mean(is.na(x)), digits = 2))

  t = as.data.frame(table(aggLevel))

  colnames(missingTable)[1] = level
  colnames(t) = c(level, "N")

  missingTable = merge(t, missingTable, by=level)

  return(missingTable)
}

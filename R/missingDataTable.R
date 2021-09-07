

#' Summarize percentage and number of missing characters on the desired grouping level.
#' @importFrom stats aggregate
#' @export
missingCharactersTable <- function(object, level) {
  .checkClass(object, "morphodata")

  if (level!="taxon" & level!="pop" & level!="indiv")  stop("Invalid level of grouping. Consider using \"taxon\", \"pop\" or \"indiv\".", call. = FALSE)

  switch(level,
     taxon={
         aggLevel = list(object$Taxon)
         header = data.frame(unique(object$Taxon))
         cname = "Taxon"
         colnames(header) = cname

      },
     pop={
           aggLevel = list(object$Population)
          header = data.frame(object$Population, object$Taxon)
          header = header[! duplicated(header[,1]),]
          cname = "Population"
           colnames(header) = c(cname, "Taxon")
      },
     indiv={
           aggLevel = list(object$ID)
          header = data.frame(object$ID, object$Population, object$Taxon)
          cname = "ID"
          colnames(header) = c(cname, "Population", "Taxon")
      }
  )

  missingTable = data.frame(
    stats::aggregate( apply(object$data, 1, function(x) mean(is.na(x))),  # MEAN
                                aggLevel, function(x) round(mean(x), digits = 2)),
    stats::aggregate( apply(object$data, 1, function(x) sum(is.na(x))),   # SUM
                                aggLevel, sum)$x
  )

  t = as.data.frame(table(aggLevel))

  colnames(missingTable) = c(cname, "missing.percentage", "missing.values")
  colnames(t) = c(cname, "N")

  missingTable = merge(t, missingTable, by=cname)
  missingTable = merge(header, missingTable, by=cname)
  # SORT??
  # missingTable = missingTable[order(-missingTable$missing.percentage),]

  return(missingTable)
}


#' Summarize percentage of missing samples in characters on the desired grouping level.
#' @importFrom stats aggregate
#' @export
missingSamplesTable <- function(object, level) {
  .checkClass(object, "morphodata")

  if (level!="taxon" & level!="pop" & level!="indiv")  stop("Invalid level of grouping. Consider using \"taxon\", \"pop\" or \"indiv\".", call. = FALSE)

  switch(level,
         taxon={
           aggLevel = list(object$Taxon)
           cname = "Taxon"
         },
         pop={
           aggLevel = list(object$Population)
           cname = "Population"
         },
         indiv={
           aggLevel = list(object$ID)
           cname = "ID"
         }
  )


  # missingTable = stats::aggregate( object$data, aggLevel, function(x) round(mean(is.na(x)), digits = 2))


  missingTable = data.frame(
    stats::aggregate( object$data, aggLevel, function(x) round(mean(is.na(x)), digits = 2)),
    "missing.percentage" = stats::aggregate( apply(object$data, 1, function(x) mean(is.na(x))),  # MEAN
                      aggLevel, function(x) round(mean(x), digits = 2))$x,
    "missing.values"= stats::aggregate( apply(object$data, 1, function(x) sum(is.na(x))),   # SUM
                      aggLevel, sum)$x
  )


  t = as.data.frame(table(aggLevel))

  colnames(missingTable)[1] = cname
  colnames(t) = c(cname, "N")

  missingTable = merge(t, missingTable, by=cname)

  return(missingTable)
}

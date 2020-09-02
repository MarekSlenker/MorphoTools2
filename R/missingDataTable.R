

#' Summarize percentage and number of missing characters on the desired grouping level.
#' @export
missingDataTable <- function(object, level) {
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
                    aggregate( apply(object$data, 1, function(x) mean(is.na(x))),  # MEAN
                                aggLevel, mean),
                    aggregate( apply(object$data, 1, function(x) sum(is.na(x))),   # SUM
                                aggLevel, sum)$x

    )

    t = as.data.frame(table(aggLevel))

    colnames(missingTable) = c(level, "missing.percentage", "numb.of.missing.characters")
    colnames(t) = c(level, "N")



    missingTable = merge(t, missingTable, by=level)
    missingTable = merge(header, missingTable, by=level)


    missingTable = missingTable[order(-missingTable$missing.percentage),]

    # missingTable = missingTable[, c(level, "N", "missing.percentage", "numb.of.missing.characters")]

    return(missingTable)
}

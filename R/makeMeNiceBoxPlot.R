# internal

makeMeNiceBoxPlot <- function(object, character, upperWhisker, lowerWhisker) {
  # vyrataj klasicky bxplot
  bxPlot = boxplot(unlist(object$data[character]) ~ object$Taxon, data = object$data, plot = F)

  # a teraz ho zmen
  taxa = levels(object$Taxon)

  # clear bxPlot$out and   bxPlot$group
  bxPlot$out = numeric(0)
  bxPlot$group = numeric(0)

  for (tax in taxa)   {
    dataTaxon = object$data[which( object$Taxon %in% tax), ][character]

    upWhisker = as.numeric( quantile(dataTaxon, probs = upperWhisker, na.rm = T)  )
    loWhisker = as.numeric( quantile(dataTaxon, probs = lowerWhisker, na.rm = T)  )

    bxPlot$stats[1, which(tax == taxa)] = loWhisker
    bxPlot$stats[5, which(tax == taxa)] = upWhisker

    for (individual in  1:nrow(dataTaxon)){
      if (!(is.na(dataTaxon[individual, ]))){
        if (dataTaxon[individual, ] > upWhisker | dataTaxon[individual, ] < loWhisker){

          bxPlot$out = c(bxPlot$out, dataTaxon[individual, ])
          bxPlot$group = c(bxPlot$group, which(tax == taxa))
        }
      }
    }
  }

  return(bxPlot)
}

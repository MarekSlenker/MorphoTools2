# internal

# vector aj symbols su oba vektory. hodnoty vo vektore nahradi postupne symbolmi.
# ak je symbolov menej, tak ich zrecykluje, ak ich je viace, kasle na ne

setValuesForVector <- function(vector, symbols) {

  uniques = unique(vector)

  for (uniq in uniques) {

    levelPos = which( uniques %in% uniq )

    while (levelPos > length(symbols)) {

      # ak uzivatel zada 2 pch pre 3 taxony, tak pre posledny taxon 3 - 2 a pch bude 1
      levelPos = levelPos - length(symbols)
    }

    #symbolsPositions = which( object$objects$Taxon %in% uniq)
    #vector[symbolsPositions == uniq] = symbols[levelPos]

    vector = as.character(vector)
    vector[vector == uniq] = symbols[levelPos]
    vector = as.factor(vector)
  }

  return(as.character(vector))
}





plotLegend <- function(object, legend.pos, bg, ncol) {
  if (length(legend.pos) == 1) legend(legend.pos,
                                      legend = unique(object$objects$Taxon),
                                      pch = unique(object$pch),
                                      col = unique(object$col),
                                      bty="o", pt.bg = bg, ncol = ncol)
  if (length(legend.pos) == 2) legend(legend.pos[1], legend.pos[2], legend = unique(object$objects$Taxon),
                                      pch = unique(object$pch),
                                      col = unique(object$col),
                                      bty="o", pt.bg = bg, ncol = ncol)
}

plot2DLabels <- function(object, axes) {
  text(x = object$objects$scores[ ,axes[1]], y = object$objects$scores[ ,axes[2]],
       labels = object$objects$ID, cex = 0.7, pos = 4, offset = 0.5)

}
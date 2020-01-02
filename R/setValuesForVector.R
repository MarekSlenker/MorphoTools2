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



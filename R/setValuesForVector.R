# internal

# vector aj symbols su oba vektory. hodnoty vo vektore nahradi postupne symbolmi.
# ak je symbolov menej, tak ich zrecykluje, ak ich je viace, kasle na ne

vector = object$objects$Taxon
symbols = c("black", "red", "green", "yellow")
levels(vector)
level = "ph"

setValuesForVector <- function(vector, symbols) {

  levels = levels(vector)

  for (level in levels) {

    levelPos = which( levels %in% level )

    while (levelPos > length(symbols)) {

      # ak uzivatel zada 2 pch pre 3 taxony, tak pre posledny taxon 3 - 2 a pch bude 1
      levelPos = levelPos - length(symbols)
    }

    #symbolsPositions = which( object$objects$Taxon %in% level)
    #vector[symbolsPositions == level] = symbols[levelPos]

    vector = as.character(vector)
    vector[vector == level] = symbols[levelPos]
    vector = as.factor(vector)
  }

  return(as.character(vector))
}



#' Histograms of characters.
#' @export
histCharacter <- function(object, character, taxon = NULL, histogram = TRUE, col = "grey90", densityLine = TRUE, normDistLine = TRUE, ...) {
  checkClass(object, "morphodata")

  if (!(character %in% colnames(object$data))) stop(paste("character", character, "was not found in attached data."), call. = FALSE)

  opar <- par(no.readonly = TRUE)

  if (is.null(taxon)) {
    taxon = levels(object$Taxon)
  }


  histInternal(object, character, taxon, histogram, col, densityLine, normDistLine, ...)


  par(opar)



}

#' @rdname histCharacter
#' @export
histAll <- function(object,  folderName = "histograms", taxon = NULL, histogram = TRUE, col = "grey90", densityLine = TRUE, normDistLine = TRUE, width = 480, height = 480, units = "px", ...) {

  checkClass(object, "morphodata")

  opar <- par(no.readonly = TRUE)

  if (is.null(taxon)) {
    taxon = levels(object$Taxon)
  }

  # check for dir existence. if not, make a new dir
  if (!(dir.exists(paste(getwd(), "/", folderName, sep = ""))))  {
    dir.create(paste(getwd(), "/",folderName, "/", sep = ""), showWarnings = T, recursive = F)
  }

  # hists plot for characters
  for (char in colnames(object$data))
  {
    grDevices::jpeg(filename=paste(getwd(), "/", folderName, "/", char, ".jpg", sep = "" ), width = width, height = height, units = units)

    histInternal(object, char, taxon, histogram, col, densityLine, normDistLine)

    grDevices::dev.off()
  }

  par(opar)


}


# internal
histInternal <- function(object, character, taxon, histogram, col, densityLine, normDistLine, ...) {
  # set mfrow
  dims=c(1,1)
  addRow=FALSE
  while(dims[1]*dims[2]<length(taxon)){
    if (addRow) { # add row
      dims[1] = dims[1]+1
      addRow=FALSE
    } else { # add column
      dims[2] = dims[2]+1
      addRow=TRUE
    }
  }

  par(mfrow=dims)
  par(mar=c(2,1,2,1))

  if (! histogram) {
    lty = "blank"; col = NA
  } else {
    lty = "solid"
  }

  for (tax in taxon)   {
    dataTaxon = as.matrix(object$data[which( object$Taxon %in% tax), ][character])
    dataTaxon = na.omit(dataTaxon)

    graphics::hist(dataTaxon, freq=FALSE, main = paste(character, tax, sep = ": "), ylab="", xlab="", yaxt="n", col = col, lty=lty, ...)
    if (densityLine) { graphics::lines(density(dataTaxon), lwd=2) }
    if (normDistLine) { graphics::curve(dnorm(x, mean=mean(dataTaxon), sd=sqrt(var(dataTaxon))), col="red", lwd=2, add=TRUE) }
  }
}

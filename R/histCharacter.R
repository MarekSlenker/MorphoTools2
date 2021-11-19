#' Histograms of characters.
#' @export
histCharacter <- function(object, character, taxon = levels(object$Taxon), histogram = TRUE, col = "lightgray",  main = NULL,
                          densityLine = TRUE, normDistLine = TRUE, ...) {
  .checkClass(object, "morphodata")

  if (!(character %in% colnames(object$data))) stop(paste("Character \"", character, "\" was not found in attached data.", sep = ""), call. = FALSE)

  oldpar = graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))            

  .histInternal(object, character, taxon, histogram, col, main, densityLine, normDistLine, ...)

}

#' @rdname histCharacter
#' @export
histAll <- function(object,  folderName = "histograms", taxon = levels(object$Taxon), histogram = TRUE, col = "lightgray", main = NULL,
                    densityLine = TRUE, normDistLine = TRUE, width = 480, height = 480, units = "px", ...) {

  .checkClass(object, "morphodata")

  oldpar = graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))            

  # check for dir existence. if not, make a new dir
  if (!(dir.exists(paste(getwd(), "/", folderName, sep = ""))))  {
    dir.create(paste(getwd(), "/",folderName, "/", sep = ""), showWarnings = TRUE, recursive = FALSE)
  }

  # hists plot for characters
  for (character in colnames(object$data))
  {
    grDevices::jpeg(filename=paste(getwd(), "/", folderName, "/", character, ".jpg", sep = "" ), width = width, height = height, units = units)

    .histInternal(object, character, taxon, histogram, col, main, densityLine, normDistLine, ...)

    grDevices::dev.off()
  }

}


# internal
.histInternal <- function(object, character, taxon, histogram, col, main, densityLine, normDistLine, ...) {
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

  graphics::par(mfrow=dims)
  graphics::par(mar=c(2,1,2,1))

  if (! histogram) {
    lty = "blank"; col = NA
  } else {
    lty = "solid"
  }

  setMain = FALSE
  if (is.null(main)) {
    setMain = TRUE
  }

  for (tax in taxon)   {
    dataTaxon = as.matrix(object$data[which( object$Taxon %in% tax), ][character])
    dataTaxon = stats::na.omit(dataTaxon)

    if (setMain) {
      main = paste(character, tax, sep = ": ")
    }

    graphics::hist(dataTaxon, freq=FALSE, main = main, ylab="", xlab="", yaxt="n", col = col, lty=lty, ...)
    if (densityLine) { graphics::lines(stats::density(dataTaxon), lwd=2) }
    if (normDistLine) {
      graphics::lines(seq(min(dataTaxon),max(dataTaxon),(max(dataTaxon)-min(dataTaxon))/1000),
            stats::dnorm(seq(min(dataTaxon),max(dataTaxon),(max(dataTaxon)-min(dataTaxon))/1000), mean=mean(dataTaxon), sd=sqrt(stats::var(dataTaxon))),
            col="red", lwd=2)
     }
  }
}

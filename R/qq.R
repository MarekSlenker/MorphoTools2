# #' @importFrom stats qqnorm
# #' @export

#' Quantile-Quantile Plots
#' @export
qqnormCharacter <- function(object, character, taxon = levels(object$Taxon), main = NULL, ...) {
  .checkClass(object, "morphodata")

  if (!(character %in% colnames(object$data))) stop(paste("Character \"", character, "\" was not found in attached data.", sep = ""), call. = FALSE)

  oldpar = graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(oldpar))            

  .qqnormInternal(object, character, taxon, main, ...)
}

#' @rdname qqnormCharacter
#' @export
qqnormAll <- function(object,  folderName = "qqnormPlots", taxon = levels(object$Taxon), main = NULL, width = 480, height = 480, units = "px", ...) {

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

    .qqnormInternal(object, character, taxon, main, ...)

    grDevices::dev.off()
  }

}







.qqnormInternal <- function(object, character, taxon, main, ...) {

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
  graphics::par(mar=c(4,4,2,1))
  graphics::par(mgp=c(2,0.8,0))

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

    stats::qqnorm(dataTaxon, main = main, bty="n", ...)
    stats::qqline(dataTaxon, lwd=2)
  }




}


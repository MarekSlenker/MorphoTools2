# #' @importFrom stats qqnorm
# #' @export

#' Quantile-Quantile Plots
#' @export
qqnormCharacter <- function(object, character, taxon = levels(object$Taxon), main = NULL, ...) {
  checkClass(object, "morphodata")

  if (!(character %in% colnames(object$data))) stop(paste("character", character, "was not found in attached data."), call. = FALSE)

  opar <- par(no.readonly = TRUE)


  qqnormInternal(object, character, taxon, main, ...)

  par(opar)
}

#' @rdname qqnormCharacter
#' @export
qqnormAll <- function(object,  folderName = "qqnormPlots", taxon = levels(object$Taxon), main = NULL, width = 480, height = 480, units = "px", ...) {

  checkClass(object, "morphodata")

  opar <- par(no.readonly = TRUE)


  # check for dir existence. if not, make a new dir
  if (!(dir.exists(paste(getwd(), "/", folderName, sep = ""))))  {
    dir.create(paste(getwd(), "/",folderName, "/", sep = ""), showWarnings = T, recursive = F)
  }

  # hists plot for characters
  for (character in colnames(object$data))
  {
    grDevices::jpeg(filename=paste(getwd(), "/", folderName, "/", character, ".jpg", sep = "" ), width = width, height = height, units = units)

    qqnormInternal(object, character, taxon, main, ...)

    grDevices::dev.off()
  }

  par(opar)


}







qqnormInternal <- function(object, character, taxon, main, ...) {

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
  par(mar=c(4,4,2,1))
  par(mgp=c(2,0.8,0))

  for (tax in taxon)   {
    dataTaxon = as.matrix(object$data[which( object$Taxon %in% tax), ][character])
    dataTaxon = na.omit(dataTaxon)

    if (is.null(main)) {
      main = paste(character, tax, sep = ": ")
    }

    stats::qqnorm(dataTaxon, main = main, bty="n", ...)
    stats::qqline(dataTaxon, lwd=2)
  }




}

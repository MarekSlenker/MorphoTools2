#' Box Plots
#' @export
boxplotCharacter <- function(object, character, outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", border = "black",
                                      main = character, cex.main = 1.5, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8, horizontal = FALSE,
                                      varwidth = FALSE, ...) {


  .checkClass(object, "morphodata")

  if (!(character %in% colnames(object$data))) stop(paste("Character \"", character, "\" was not found in attached data.", sep = ""), call. = FALSE)

  bxPlot = .giveMeNiceBoxPlot(object, character, upperWhisker = upperWhisker, lowerWhisker = lowerWhisker)

  graphics::bxp(bxPlot, outline = outliers, boxfill = col, border = border, xlab = xlab, ylab = ylab, frame = frame, pch = pch, horizontal = horizontal, varwidth = varwidth, ...)
  graphics::title(main, cex.main = cex.main)
}


#' @rdname boxplotCharacter
#' @export
boxplotAll <- function(object, folderName = "boxplots", outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", border = "black",
                                main = character, cex.main = 1.5, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8, horizontal = FALSE, varwidth = FALSE, width = 480, height = 480, units = "px", ...)
{
  .checkClass(object, "morphodata")

  # check for dir existence. if not, make a new dir
  if (!(dir.exists(paste(getwd(), "/", folderName, sep = ""))))  {
    dir.create(paste(getwd(), "/",folderName, "/", sep = ""), showWarnings = TRUE, recursive = FALSE)
  }

  # box and whisker plot for characters
  for (char in colnames(object$data))
  {

    bxPlot = .giveMeNiceBoxPlot(object, char, upperWhisker = upperWhisker, lowerWhisker = lowerWhisker)

    grDevices::jpeg(filename=paste(getwd(), "/", folderName, "/", char, ".jpg", sep = "" ), width = width, height = height, units = units)

    graphics::bxp(bxPlot, outline = outliers, boxfill = col, border = border, xlab = xlab, ylab = ylab, frame = frame, pch = pch, horizontal = horizontal, varwidth = varwidth, ...)

    graphics::title(char, cex.main = cex.main)

    grDevices::dev.off()
  }
}


# internal
.giveMeNiceBoxPlot <- function(object, character, upperWhisker, lowerWhisker) {
  # vyrataj klasicky bxplot
  bxPlot = graphics::boxplot(unlist(object$data[character]) ~ object$Taxon, data = object$data, plot = FALSE)

  # a teraz ho zmen
  taxa = levels(object$Taxon)

  # clear bxPlot$out and   bxPlot$group
  bxPlot$out = numeric(0)
  bxPlot$group = numeric(0)

  for (tax in taxa)   {
    dataTaxon = object$data[which( object$Taxon %in% tax), ][character]
    if (! is.data.frame(dataTaxon)) {
      eval( parse (text= paste("dataTaxon = data.frame(",character,"= object$data[which( object$Taxon %in% tax), ])",  sep = "") ))

    }

    upWhisker = as.numeric( stats::quantile(dataTaxon, probs = upperWhisker, na.rm = TRUE)  )
    loWhisker = as.numeric( stats::quantile(dataTaxon, probs = lowerWhisker, na.rm = TRUE)  )

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





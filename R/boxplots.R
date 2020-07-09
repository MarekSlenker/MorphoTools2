#' Box Plots
#' @export
boxplot.character <- function(object, character, outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", border = "black",
                              main = character, cex.main = 2, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8, horizontal = FALSE,
                              varwidth = FALSE, ...) {
  UseMethod("boxplot.character")
}

#' @rdname boxplot.character
#' @export
boxplot.all <- function(object, folderName = "boxplots", outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", border = "black",
                        main = character, cex.main = 2, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8, horizontal = FALSE,
                        varwidth = FALSE, ...) {
  UseMethod("boxplot.all")
}

#' @rdname boxplot.character
#' @method boxplot.character morphodata
#' @export
boxplot.character.morphodata <- function(object, character, outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", border = "black",
                                      main = character, cex.main = 2, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8, horizontal = FALSE,
                                      varwidth = FALSE, ...) {


  checkClass(object, "morphodata")

  if (!(character %in% colnames(object$data))) stop(paste("character", character, "was not found in attached data."), call. = F)

  bxPlot = giveMeNiceBoxPlot(object, character, upperWhisker = upperWhisker, lowerWhisker = lowerWhisker)

  graphics::bxp(bxPlot, outline = outliers, boxfill = col, border = border, xlab = xlab, ylab = ylab, frame = frame, pch = pch, horizontal = horizontal, varwidth = varwidth, ...)
  graphics::title(main, cex.main = cex.main)
}


#' @rdname boxplot.character
#' @method boxplot.all morphodata
#' @export
boxplot.all.morphodata <- function(object, folderName = "boxplots", outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", border = "black",
                                main = character, cex.main = 2, xlab = NULL, ylab = NULL, frame = TRUE, pch = 8, horizontal = FALSE,
                                varwidth = FALSE, ...)
{
  checkClass(object, "morphodata")

  # check for dir existence. if not, make a new dir
  if (!(dir.exists(paste(getwd(), "/", folderName, sep = ""))))  {
    dir.create(paste(getwd(), "/",folderName, "/", sep = ""), showWarnings = T, recursive = F)
  }

  # box and whisker plot for characters
  for (char in colnames(object$data))
  {

    bxPlot = giveMeNiceBoxPlot(object, char, upperWhisker = upperWhisker, lowerWhisker = lowerWhisker)

    grDevices::jpeg(filename=paste(getwd(), "/", folderName, "/", char, ".jpg", sep = "" ))

    graphics::bxp(bxPlot, outline = outliers, boxfill = col, border = border, xlab = xlab, ylab = ylab, frame = frame, pch = pch, horizontal = horizontal, varwidth = varwidth, ...)

    graphics::title(char, cex.main = cex.main)

    grDevices::dev.off()
  }
}


# internal
giveMeNiceBoxPlot <- function(object, character, upperWhisker, lowerWhisker) {
  # vyrataj klasicky bxplot
  bxPlot = graphics::boxplot(unlist(object$data[character]) ~ object$Taxon, data = object$data, plot = F)

  # a teraz ho zmen
  taxa = levels(object$Taxon)

  # clear bxPlot$out and   bxPlot$group
  bxPlot$out = numeric(0)
  bxPlot$group = numeric(0)

  for (tax in taxa)   {
    dataTaxon = object$data[which( object$Taxon %in% tax), ][character]

    upWhisker = as.numeric( stats::quantile(dataTaxon, probs = upperWhisker, na.rm = T)  )
    loWhisker = as.numeric( stats::quantile(dataTaxon, probs = lowerWhisker, na.rm = T)  )

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





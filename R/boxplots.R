#' Box Plots
#'
#' @description These functions produce box-and-whisker plot(s) of the given morphological characters.
#'
#' @usage boxplot.character(object, character, outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, ...)
#'
#' @param object 	an object of class 'morphodata'.
#' @param character morphological character used for plotting boxplot
#' @param lowerWhisker percentile, to which the lower whisker is extended
#' @param upperWhisker percentile, to which the upper whisker is extended
#' @param folderName the name of folder, where to save produces boxplot
#' @param ... 	further arguments to be passed to \code{\link{boxplot}} or \code{\link{bxp}} {graphics}.
#'
#' @details These functions are wrappers above classicla R boxplot {graphics} function, allowin user to extend whiskers
#'  to desired percentiles. By default, the  whiskers are extended to the 5\% and 95\% percentiles, because the trimmed range
#'  (without the most extreme 10% of values) use to be used in taxa descriptions, determination keys, etc. Rectangle define 25th and 75th
#'  percentiles, bold horizontal line show median.
#'
#' @details The \strong{boxplot.all} function produce boxplot for each morphological character and save
#'  them to folder named in folderName parameter. If it does not exist, new folder is created.
#'
#' @examples
#' boxplot.character(myMorphoData, "ST", col = "grey", border = "red")
#' boxplot.character(myMorphoData, "ST", outliers = TRUE, pch = 8, lowerWhisker = 0.05, upperWhisker = 0.95)
#' boxplot.character(myMorphoData, "ST", outliers = FALSE, xlab = "Taxa", ylab = "length", main = "Total stem height (cm)")
#' boxplot.character(myMorphoData, "ST", varwidth = T, notch = T, boxwex = 0.4, staplewex = 1.3, horizontal = T)
#'
#' boxplot.all(object, folderName = "boxplots", outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95)
#'
#' @export
boxplot.character <- function(object, ...) {
  UseMethod("boxplot.character")
}

#' @rdname boxplot.character
#' @aliases boxplot.all
#' @export
boxplot.all <- function(object, ...) {
  UseMethod("boxplot.all")
}

#' @rdname boxplot.character
#' @method boxplot.character default
#' @export
boxplot.character.default <- function(object, character, outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", main = character, ...) {
  checkClass(object, "morphodata")

  if (!(character %in% colnames(object$data))) stop(paste("character", character, "was not found in attached data."), call. = F)

  bxPlot = giveMeNiceBoxPlot(object, character, upperWhisker = upperWhisker, lowerWhisker = lowerWhisker)

  bxp(bxPlot, boxfill = col, outline = outliers, ...)
  title(main, cex.main = 2)
}


#' @rdname boxplot.character
#' @method boxplot.all default
#' @export
boxplot.all.default <- function(object, folderName = "boxplots", outliers = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", ...)
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

    jpeg(filename=paste(getwd(), "/", folderName, "/", char, ".jpg", sep = "" ))

    bxp(bxPlot, boxfill = col, outline = outliers, ...)

    title(char, cex.main = 2)

    dev.off()
  }
}


# internal

giveMeNiceBoxPlot <- function(object, character, upperWhisker, lowerWhisker) {
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





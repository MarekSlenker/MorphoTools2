#' Box Plots
#'
#' @description These functions produce box-and-whisker plot(s) of the given morphological characters.
#'
#' @usage boxplot.character(object, character, outline = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, ...)
#'
#' @param object 	an object of class 'morphodata'.
#' @param character morphological character used for plotting boxplot
#' @param lowerWhisker percentile, to which the lower whisker is extended
#' @param upperWhisker percentile, to which the upper whisker is extended
#' @param folderName the name of folder, where to save produces boxplot
#' @param ... 	further arguments to be passed to boxplot or bxp {graphics}.
#'
#' @details These functions are wrappers above classicla R boxplot {graphics} function, allowin user to extend whiskers
#'  to desired percentiles. By default, the  whiskers are extended to the 5\% and 95\% percentiles, because the trimmed range
#'  (without the most extreme 10% of values) use to be used in taxa descriptions, determination keys, etc. Rectangle define 25th and 75th
#'  percentiles, bold horizontal line show median.
#'
#' @examples
#' boxplot.character(myMorphoData, "ST", col = "grey", border = "red")
#' boxplot.character(myMorphoData, "ST", outline = TRUE, pch = 8, lowerWhisker = 0.05, upperWhisker = 0.95)
#' boxplot.character(myMorphoData, "ST", outline = FALSE, xlab = "Taxa", ylab = "length", main = "Total stem height (cm)")
#' boxplot.character(myMorphoData, "ST", varwidth = T, notch = T, boxwex = 0.4, staplewex = 1.3, horizontal = T)
#'
#' @export
boxplot.character <- function(object, character, outline = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", ...) {
  checkMorphodataClass(object)

  if (!(character %in% colnames(object$data))) stop(paste("character", character, "was not found in attached data."), call. = F)



  bxPlot = makeMeNiceBoxPlot(object, character, upperWhisker = upperWhisker, lowerWhisker = lowerWhisker)

  bxp(bxPlot, boxfill = col, outline = outline, ...)
}


#' @rdname boxplot.character
#' @usage boxplot.all(object, folderName = "boxplots", ...)
#'
#' @details The \strong{boxplot.all} function produce boxplot for each morphological character and save
#'  them to folder named in folderName parameter. If it does not exist, new folder is created.
#'
#' @examples
#' boxplot.all(object, folderName = "boxplots", outline = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95)
#'
#' @export
boxplot.all <- function(object, folderName = "boxplots", outline = TRUE, lowerWhisker = 0.05, upperWhisker = 0.95, col = "white", ...)
{
  checkMorphodataClass(object)

  # check for dir existence. if not, make a new dir
  if (!(dir.exists(paste(getwd(), "/", folderName, sep = ""))))  {
    dir.create(paste(getwd(), "/",folderName, "/", sep = ""), showWarnings = T, recursive = F)
  }

  # box and whisker plot for characters
  for (char in colnames(object$data))
  {

    bxPlot = makeMeNiceBoxPlot(object, char, upperWhisker = upperWhisker, lowerWhisker = lowerWhisker)

    jpeg(filename=paste(getwd(), "/", folderName, "/", char, ".jpg", sep = "" ))

    bxp(bxPlot, boxfill = col, outline = outline, ...)

    title(char, cex.main = 3)

    dev.off()
  }
}






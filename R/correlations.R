#' Correlations of characters
#'
#' @description cormat calculates the matrix of the correlation coefficients of the characters.
#'
#' @usage cormat(object, method)
#'
#' @param object an object of class 'morphodata'.
#' @param method a character string indicating which correlation coefficient is to be used for the test.
#'  One of "pearson" (default), "spearman", or "kendall" can be abbreviated.
#'
#' @return object of class 'data.frame'
#'
#' @details This functions return table with pairwise correlation coefficients for each pair of
#' morphological characters. The results are formatted as data frames to allow export with the export.res function.
#'
#' Significance tests are usually unnecessary for morphometric analysis. Anyway,
#' if tests are needed, they can be computed using the cormat.signifTest() function.
#'
#' @examples
#' correlations.p = cormat(myMorphodata, method="pearson")
#' correlations.s = cormat(myMorphodata, method="spearman")
#'
#' export.res(correlations.p, file="correlations.pearson.txt")
#' export.res(correlations.s, file="correlations.spearman.txt")
#'
#' @export
cormat <- function (object, method = "pearson") {
  checkClass(object, "morphodata")

  corelations = cor(object$data, use="pairwise.complete.obs", method = method)
  corelations = round(corelations, digits = 3)
  corelations = data.frame(corelations)

  corelations = data.frame(attr(corelations,"row.names"), corelations ,row.names=NULL)
  if (method=="pearson") names(corelations)[1]<-"Pearson"
  if (method=="spearman") names(corelations)[1]<-"Spearman"
  if (method=="kendall") names(corelations)[1]<-"Kendall"

  return(corelations)
}

#' @rdname cormat
#' @usage cormat.signifTest(object, method, alternative)
#'
#' @param alternative indicates the alternative hypothesis and must be one of "two.sided" (default),
#' "greater" (positive association) or "less" (negative association).
#'
#' @examples
#' correlations.p = cormat.signifTest(myMorphodata, method="pearson", alternative = "two.sided")
#'
#' @export
cormat.signifTest <- function(object, method = "pearson", alternative = "two.sided") {

  table = matrix(data = numeric(), nrow = ncol(object$data), ncol = ncol(object$data))
  colnames(table) = colnames(object$data)
  row.names(table) = colnames(object$data)

  for (row in colnames(object$data)) {
    for (col in colnames(object$data)) {

      cc = cor.test(x = object$data[, row], y = object$data[, col], method = method,
                    use="pairwise.complete.obs", alternative = alternative)

      table[row, col] = paste( round(cc$estimate, digits = 3), "; p-value = ", round(cc$p.value, digits = 3), sep = "" )
    }

    corelations = data.frame(table)

    corelations = data.frame(attr(corelations,"row.names"), corelations ,row.names=NULL)
    if (method=="pearson") names(corelations)[1]<-"Pearson"
    if (method=="spearman") names(corelations)[1]<-"Spearman"
    if (method=="kendall") names(corelations)[1]<-"Kendall"

    return(corelations)
  }
}

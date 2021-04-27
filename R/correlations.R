#' Correlations of characters
#' @export
cormat <- function (object, method = "pearson") {
  .checkClass(object, "morphodata")

  if (! (method %in% c("pearson", "spearman"))) stop(paste("Method", method , "is not supported."), call. = FALSE)

  corelations = stats::cor(object$data, use="pairwise.complete.obs", method = method)
  corelations = round(corelations, digits = 3)
  corelations = data.frame(corelations)

  corelations = data.frame(attr(corelations,"row.names"), corelations ,row.names=NULL)
  if (method == "pearson") names(corelations)[1] = "Pearson"
  if (method == "spearman") names(corelations)[1] = "Spearman"


  return(corelations)
}

#' @rdname cormat
#' @export
cormatSignifTest <- function(object, method = "pearson", alternative = "two.sided") {

  if (! (method %in% c("pearson", "spearman"))) stop(paste("Method", method , "is not supported."), call. = FALSE)

  table = matrix(data = numeric(), nrow = ncol(object$data), ncol = ncol(object$data))
  colnames(table) = colnames(object$data)
  row.names(table) = colnames(object$data)

  for (row in colnames(object$data)) {
    for (col in colnames(object$data)) {

      cc = stats::cor.test(x = object$data[, row], y = object$data[, col], method = method,
                    use="pairwise.complete.obs", alternative = alternative)

      table[row, col] = paste( round(cc$estimate, digits = 3), ";p-value=", round(cc$p.value, digits = 5), sep = "")
    }
  }

    corelations = data.frame(table)

    corelations = data.frame(attr(corelations,"row.names"), corelations ,row.names=NULL)
    if (method == "pearson") names(corelations)[1] = "Pearson"
    if (method == "spearman") names(corelations)[1] = "Spearman"

    return(corelations)



}

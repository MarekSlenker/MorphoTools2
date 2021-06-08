
.calcDistance <- function(object, distMethod, center, scale, binaryChs, nominalChs, ordinalChs) {

  switch (distMethod,
          "euclidean" = {  # moze byt scale alebo nie
            return(
              stats::dist(scale(object$data, center = center, scale = scale), method = distMethod)
            )
          },
          "jaccard" = { # ziadne scale
            if (any(is.na(object$data))) stop("NA values in 'object'.", call. = FALSE)
            return(
              ade4::dist.binary(object$data, method = 1, diag = FALSE, upper = FALSE)
            )
          },
          "simpleMatching" = {
            if (any(is.na(object$data))) stop("NA values in 'object'.", call. = FALSE)
            return(
              ade4::dist.binary(object$data, method = 2, diag = FALSE, upper = FALSE)
            )
          },
          "gower" = {

            if (! is.null(binaryChs)) {
              object$data[binaryChs] = lapply(object$data[binaryChs], as.logical)
            }

            if (! is.null(nominalChs)) {
              object$data[nominalChs] = lapply(object$data[nominalChs], as.character)
            }
            if (! is.null(ordinalChs)) {
              object$data[ordinalChs] = lapply(object$data[ordinalChs], ordered)
            }

            return(
              stats::as.dist(StatMatch::gower.dist(object$data))
            )
          },
          "manhattan" = {
            return(
              stats::dist(object$data, method = "manhattan", diag = FALSE, upper = FALSE)
            )
          },
          "minkowski" = {
            return(
              stats::dist(object$data, method = "minkowski", diag = FALSE, upper = FALSE)
            )
          },
          stop(paste("distMethod", distMethod , "is not supported."), call. = FALSE)

    )

}


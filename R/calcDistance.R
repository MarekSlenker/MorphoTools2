
.calcDistance <- function(object, distMethod, center, scale) {

  switch (distMethod,
          "euclidean" = {  # moze byt scale alebo nie
            return(
              stats::dist(scale(object$data, center = center, scale = scale), method = distMethod)
            )
          },

          "jaccard" = { # ziadne scale
            return(
              ade4::dist.binary(object$data, method = 1, diag = FALSE, upper = FALSE)
            )

          },

          "simpleMatching" = {
            return(
              ade4::dist.binary(object$data, method = 2, diag = FALSE, upper = FALSE)
            )
          },

          "gower" = {
            cat("gower")
          },
          stop(paste("distMethod", distMethod , "is not supported."), call. = FALSE)

    )

}



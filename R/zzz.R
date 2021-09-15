


##' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
  pkgVersion <- packageDescription(pkgname, fields="Version")
  msg <- paste0(pkgname, " v", pkgVersion, "  ",
                "For help: https://github.com/MarekSlenker/MorphoTools2/wiki", "\n\n")

  citation <- paste0("If you use ", pkgname, " in published research, please cite:\nŠlenker, M., Koutecký, P. & Marhold, K. (2021). MorphoTools2: an R package for the multivariate morphometric analysis. https://github.com/MarekSlenker/MorphoTools2 \n" )

  packageStartupMessage(paste0(msg, citation))
}

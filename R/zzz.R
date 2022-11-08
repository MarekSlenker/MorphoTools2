


##' @importFrom utils packageDescription
.onAttach <- function(libname, pkgname) {
  pkgVersion <- packageDescription(pkgname, fields="Version")
  msg <- paste0(pkgname, " v", pkgVersion, ".  ",
                "For help: https://github.com/MarekSlenker/MorphoTools2/wiki", "\n\n")

  citation <- paste0("If you use ", pkgname, " in published research, please cite: \nSlenker, M., Koutecky, P. & Marhold, K. (2022). MorphoTools2: an R package for multivariate morphometric analysis. Bioinformatics 38: 2954-2955. \nhttps://doi.org/10.1093/bioinformatics/btac173 \n"
                 )

  packageStartupMessage(paste0(msg, citation))
}

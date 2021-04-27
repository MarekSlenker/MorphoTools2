#' Non-metric multidimensional scaling (NMDS)
#' @export
nmds.calc <- function(object, distMethod = "euclidean") {


  data(centaurea)
  centaurea = naMeanSubst(centaurea)
  centaurea = deletePopulation(centaurea, populationName = c("LIP", "PREL"))



  d = .calcDistance(centaurea, distMethod = "euclidean", center = TRUE, scale = TRUE)

  pcoa.calc()

  nmds = vegan::monoMDS(d, k = 3, model = "global",#             "local", "linear", "hybrid"),
                        threshold = 0.8, maxit = 200, weakties = FALSE, stress = 1,
                        scaling = FALSE, pc = FALSE                        )

  nmds$stress


  col = as.numeric(centaurea$Taxon);   col[which(col == 1)] = "red";   col[which(col == 2)] = "green";   col[which(col == 3)] = "blue";   col[which(col == 4)] = "black"


  plot3D::polygon3D(nmds$points[,1], nmds$points[,2], nmds$points[,3],col = col, pch = 18)

  plot_e




  metaMDS





}


































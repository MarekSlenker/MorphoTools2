library(ade4)

bin = read.morphodata("clipboard")



calcDistance <- function(object, distance, center, scale) {

  switch (distance,
          "euclidean" = {  # moze byt scale alebo nie
            return(
              stats::dist(scale(object$data, center = center, scale = scale), method = distance)
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
          }
    )

}




# ako syn-tax
plot(hclust(calcDistance(imp, distance = "euclidean", center = FALSE, scale = FALSE), method = "average"), hang = -1) # UPGMA, no standard
plot(hclust(calcDistance(imp, distance = "euclidean", center = TRUE, scale = TRUE), method = "average"), hang = -1) # UPGMA, st dev of vars
plot(hclust(calcDistance(imp, distance = "euclidean", center = FALSE, scale = FALSE), method = "mcquitty"), hang = -1) # WPGMA, no standard
plot(hclust(calcDistance(imp, distance = "euclidean", center = TRUE, scale = TRUE), method = "mcquitty"), hang = -1) # WPGMA, st dev of vars


plot(hclust(calcDistance(bin, distance = "jaccard"), method = "mcquitty"), hang = -1) # UPGMA, no standard

plot(hclust(calcDistance(bin, distance = "simpleMatching"), method = "mcquitty"), hang = -1) # UPGMA, no standard

#x####


pcoaData = cmdscale(calcDistance(imp, distance = "euclidean", center = TRUE, scale = TRUE))
plot(pcoaData[,1]*-1, pcoaData[,2])
pcoaData = cmdscale(calcDistance(imp, distance = "euclidean", center = F, scale = F), eig = T)
plot(pcoaData[,1]*-1, pcoaData[,2]*-1)
{
  pcoaData$eig
  eigenvaluesAsPercent = round(pcoaData$eig/sum(pcoaData$eig), 5)
  eigenvaluesAsPercent
  cumulativePercentageOfEigenvalues = round(cumsum(pcoaData$eig/sum(pcoaData$eig)), 5)
  cumulativePercentageOfEigenvalues
}


pcoaData = cmdscale(calcDistance(bin, distance = "jaccard", center = F, scale = F), eig = F)
plot(pcoaData[,1]*-1, pcoaData[,2]*-1)

pcoaData = cmdscale(calcDistance(bin, distance = "simpleMatching", center = F, scale = F), eig = F)
plot(pcoaData[,1]*-1, pcoaData[,2])












pcaData = pca.calc(imp)
plot(pcaData$objects$scores[,1]*-1, pcaData$objects$scores[,2], col = col, pch = 19)









x = pp
Y = centaurea$data


plot(pr.coo[, plot.axes])




col = as.numeric(centaurea$Taxon)
col[which(col == 1)] = "red"
col[which(col == 2)] = "green"
col[which(col == 3)] = "blue"
col[which(col == 4)] = "black"


ade4::dist.binary(centaurea$data[1:6, 1:6], method = 2, diag = FALSE, upper = FALSE)
bin=data.frame("A" = sample(c(0,1), 5, replace=TRUE),
               "b"=sample(c(0,1), 5, replace=TRUE),
               "c"=sample(c(0,1), 5, replace=TRUE),
               "d"=sample(c(0,1), 5, replace=TRUE),
               "e"=sample(c(0,1), 5, replace=TRUE),
               "f"=sample(c(0,1), 5, replace=TRUE),
               "g"=sample(c(0,1), 5, replace=TRUE),
               "h"=sample(c(0,1), 5, replace=TRUE))

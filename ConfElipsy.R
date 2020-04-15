data = read.morphodata("~/Desktop/Pratensisdata.csv")

cdaRes = cda.calc(data)

plot.points(cdaRes, col = c("blue", "red", "black" ), pch = 15, legend = F)


require(car)

for (level in levels(data$Taxon)) {

  #"2n=16" "2n=46" "2n=48"

  a = cdaRes$objects$scores[which( cdaRes$objects$Taxon %in% "2n=48"), ][,1]
  b = cdaRes$objects$scores[which( cdaRes$objects$Taxon %in% "2n=48"), ][,2]

  npts   <- length(a)
  shape  <- var(cbind(a, b))
  center <- c(mean(a),mean(b))
  rconf  <- sqrt(2 * (npts-1) * qf(0.95, 2, npts-2)/(npts*(npts-2)))
  rpred  <- sqrt(npts+1)*rconf

  conf.elip <- ellipse(center, shape, rconf,draw = FALSE)
  pred.elip <- ellipse(center, shape, rpred,draw = FALSE)

  plot(a,b, xlim = c(-8,5), ylim = c(-5,5) )
  lines(pred.elip, col="red")
  lines(conf.elip, col="black")



  plot(pred.elip, type='l')
}



npts   <- length(a)
shape  <- var(cbind(a, b))
center <- c(mean(a),mean(b))
rconf  <- sqrt(2 * (npts-1) * qf(0.95, 2, npts-2)/(npts*(npts-2)))
rpred  <- sqrt(npts+1)*rconf

conf.elip <- ellipse(center, shape, rconf,draw = FALSE)
pred.elip <- ellipse(center, shape, rpred,draw = FALSE)

lines(pred.elip, col="red")

plot(pred.elip, type='l')
points(a,b)
lines(conf.elip,col="red")

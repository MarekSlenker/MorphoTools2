checkMorphodataClass(object)


data = read.morphodata("./tests/testFiles/samplePlnaMatica.txt")
data = read.morphodata("clipboard")
dtFrame = read.delim("./tests/testFiles/sample.txt")

object = pcaRes
data = object

pops = popul.otu(data)

pcaRes = pca.calc(data)

pcaRes = pca.calc(pops)

warnings()

plot.points(pcaRes, axes = c(1,2), col = c("red", "green", "blue", "red"), pch = c(20, 17, 8, 21), bg = "orange", legend = T, legend.pos = "bottomright", ncol = 2)

legend("topleft", legend = levels(pcaRes$eigenVectors))


labels.points(pcaRes)

unique(pcaRes$objects$Taxon)




plot.labels(pcaRes, axes = c(1,2), cex = 0.8, col = "red", pos = 4, offset = 0.4)

s3_dispatch(plot.labels)

#########################x
   LABELS   generic???
################x
length(pcaRes$eigenValues)

object = pca.calc(data)


plot(object, pch = c(19, 11, 8), col = c("black", "red", "yellow", "green"), cex = 0.8, xlab = "ds")

plot(object)


labels(object, cex = 0.8, pos = 3)

vector = object$objects$Taxon

col = c("green", "red")

plot(x = object$scores[ ,axes[1]], y = object$scores[ ,axes[2]], xlab = xlab, ylab = ylab)



rm(popul.otu)




str(pcaResult)









populs_MK2 = popul.otu(object)

plot(clust(populs_MK))

export.res(object, file = "sa")

objNa = MorphoTools::na.meanSubst(object)

objNa = delete.population(object = objNa, populationName = c("LIP", "PREL"))

export.res(objNa, file = "./tests/testFiles/samplePlnaMatica.txt")

pca = prcomp(object$data)

summary(pca)

str(pp)

str(pca)

methods(summary, "prcomp")

pop = "LIP"




pops_kouta = popul.otu_KOUTA(dtFrame)

View(objNa$data)

data = read.morphodata("./tests/testFiles/sample_Na_celyZnak.txt")

data = read.delim("./tests/testFiles/sample_NaNs.txt")

data = read.morphodata("./tests/testFiles/sample_numericNames.txt")

column = "all"

descr.pop(data, format = "$MEAN + $MAX")

object = data

descr.all(data)

al = descr.all(data,format = "$MEAN")

str(al)


export.res(al, file = "ds")


dd = delete.taxon(data, "hybr")

dim(dd$data)

class(data)
oo = na.meanSubst(object)

write.table(oo$data, "meanSubstMK.txt")

dd = delete.population(data, c("CERV", "DEB", "KOT"))

subData = newObject

object = data
column = "Taxon"
groupName = "hybr"

print(object, file = "rr.txt")

export.res(object, file = "re.txt", kk=9)

for (ch in charecter) {
  if (! (ch %in% colnames(data$data))) stop(paste("charecter", ch , "does not exist"), call. = F)
}

as.character(data$data[1,])

paste(data$data[1,], collapse = " ")


i = "Population"

column = "Population"
groupName = c("RTE", "PREL", "RUS", "HVLT")

column = "Taxon"
groupName = "hybr"




toRemove = array(data = NA, dim = 0)
for (name in groupName) {
  toRemove = c(toRemove, which( unlist(data[column]) == name) )
  #toRemove = paste(toRemove, (which( unlist(data[column]) == name) ))
}

toRemove

data$data[,1]

newdata = newMorphodata()

charecter = c("SN", "SF", "ST", "LL")

for (ch in charecter) {
  toRemove = c(toRemove, which(colnames(data$data) == ch) )
}





newdata$Population = droplevels( data$Population[-toRemove])

newdata$Taxon = ( data$Taxon[-toRemove] )

newdata$Taxon = droplevels( data$Taxon[-toRemove] )


newdata$data = data$data[-toRemove, ]


for (ch in charecter) {
  toRemove = c(toRemove, which( object$data == ch) )
}


droplevels(newdata$Taxon)




x = data$data[-toRemove, ]
class(x)

rm(toRemove)

str(which( unlist(data[column]) == name))

str(data$Population)


is.null(data$ID)




data = read.delim("./tests/testFiles/sample_NaNs.txt")

("ID" %in% colnames(data))


summary(data)


paste(levels(data$Population), collapse = " ")












dim(data$data)[2] -1












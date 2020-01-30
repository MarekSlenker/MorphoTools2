checkMorphodataClass(object)


data = read.morphodata("./tests/testFiles/samplePlnaMatica.txt")

data = read.morphodata("./tests/testFiles/Impatiens_individuals.txt")

data = read.morphodata("clipboard")
individuals = read.delim("./tests/testFiles/samplePlnaMatica.txt")

dataShort = delete.taxon(object = data, taxonName =  c("ph"))
dataXShort = delete.taxon(object = data, taxonName =  c("ph", "hybr"))


groupName = "OLE2"
column = "Population"

round(lda.samp$posterior, digits = 6)

delete.population(object = data, populationName = )

object = ss
class(ss)


ss= classifda.lda(data, crossval = "pop")
ss= classifda.lda(data)

sa = classif.matrix(ss)
classif.matrix(ss, level = "Population")

knn.select(data)
ss = classifda.knn(data, k = 11, crossval = "indiv")

checkClass(ss, "data.frame")

export.res(ss, file = "ddddsa.txt")


k = 8

kselmin[k]

str(knn.samp)

j =1
pop = "BABL"
rm(kselj)

length(train$Population) + length(samp$Population) == length(object$Population)


cdaResult = cda.calc(dataShort, passiveSamples = "hybr")
cdaResultX = cda.calc(dataXShort)

View(cbind(cdaResult$coeffs.std, cdaResultX$coeffs.std))

pcaRes = pca.calc(dataShort)

summary(data)



plot.points(cdaRes, col = c(rgb(0,0,0, alpha=0.6), rgb(1,1,1, alpha=0.6)), legend = T, breaks = seq(-6, 4, 0.2))
plot.points(cdaResult, col = c("green", "orange"), legend = T, breaks = seq(-6, 4, 0.2), pch = c(22, 15, 25, 21))
plot.points(cdaResult, pt.bg = c("green", "red", "navy", "orange"), col = c("black", "red", "black", "red"),  legend = T, pch = c(22, 15, 25, 21))

plot.points(cdaResult, col = c(rgb(0,0,0, alpha=0.6), rgb(1,1,1, alpha=0.6), rgb(1,1,1, alpha=0.6)),  legend = T, breaks = seq(-6,3.5,0.5))

plot.points(pcaRes, col = c("black", "white", "white"),  legend = T)

pca.eigenVectors(pcaRes, n = 100)

warnings()

legendTable = cbind(as.character(cdaResult$objects$Taxon), cdaResult$pt.bg)

unique(legendTable)

aggregate(cdaResult$objects$Taxon, )












labels.points(cdaResult)

plot.points(pcaRes, pt.bg = c("green", "red", "navy", "orange"), col = c("black", "red", "black", "red"),  legend = T, pch = c(22, 15, 25, 21))
labels.points(pcaRes)

plot.characters(pcaRes, labels = F)
labels.characters(pcaRes, cex = 2, pos = 2)







plot.legend(cdaRes, pt.bg = c("green", "orange"), pch = 22)


plot.3D(pcaRes, pch = 22, pt.bg = c("red", "green"), legend = T, type = "h")


length(colnames(cdaRes$objects$scores))

length(pcaRes$eigenValues)
length(colnames(cdaRes$objects$scores))
length(colnames(pcaRes$objects$scores))

e = pca.eigenVectors(pcaRes)

plot.points(pcaRes, legend = T, ncol = 2)


plot.legend(pcaRes)


labels.points(pcaRes)


data.frame("character" = rownames(e), e)


summary(pcaRes)

pcaRes$eigenValues

pca.eigenVectors

class(pcaRes)











cdaResult = cda.calc(data)

plot.points(cdaResult, col = c( rgb(0, 0, 255, max = 255, alpha = 100), rgb(218, 0, 0, max = 255, alpha = 100)), legend = F, labels = T)

  plot(x = cdaResult$objects$scores[ ,axes[1]], y = cdaResult$objects$scores[ ,axes[2]])

cdaResult$rank


plot.characters(dtFrame)

boxplot.()


str(cda$rank)
cda$rank
cda$eigenvalues
cda$canrsq
cda$pct
cda$means
??? cda$factors
cda$coeffs.raw
cda$coeffs.std
cda$structure
cda$scores


4.41939 - 2.00487

2.00487 + 0.36473

2.41452




pcaRes = pca.calc(data)


pcaRes = pca.calc(pops)

prccc = prcomp(x = data$data)

      object = data
object = data
pcaResult = pcaRes
pops = popul.otu(data)











summary(pcaRes)


plot.3D(pcaRes,   col = c("red", "green", "blue", "red"), pch = c(20, 17, 8, 21),  pt.bg = "white", labels = T, cex = 2,
        phi = 10, theta = 110, cex = 1.3, bg.col = "wheat", legend = T)

text3D(x = rep(1, 4),   y = 1:4, z = rep(0, 4),
       labels  = colnames(VADeaths),
       add = TRUE, adj = 1)

plot.points(pcaRes, axes = c(1,2), col = c("red", "green", "blue", "red"), pch = c(20, 17, 8, 21),
            bg = "orange",
            cex = 1,
            legend = F, legend.pos = "bottomright")

plot.legend(pcaRes, col = c("red", "green", "blue", "red"), pch = c(20, 17, 8, 21), pt.bg = "orange")


plot.points(pcaRes, col = c("#E41A1C","#0000FF","#00FF00","#AFA900"))

pcaResult = pcaRes

plot.points(pcaRes, axes = c(1,2),  bg = "orange", legend = T, legend.pos = "bottomright", ncol = 2, cex = 2)

plot.points(pcaRes, axes = c(1,2), col = c("red", "green", "blue", "red"), pch = c(20, 17, 8, 21), bg = "orange", legend = T, legend.pos = "bottomright", ncol = 2)


plot.characters(pcaRes)

legend("topleft", legend = levels(pcaRes$eigenVectors))


labels.points(pcaRes

unique(pcaRes$objects$Taxon)

uniq = "ps"

vector = pcaRes$objects$Taxon

symbols = col

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












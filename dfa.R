
object = read.morphodata("./tests/testFiles/sample.txt")
dtFrame = read.delim("./tests/testFiles/sample.txt")

rm(popul.otu)

populs_MK = popul.otu(morfoData)
pops_kouta = popul.otu_KOUTA(dtFrame)

View(populs_MK$data)

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












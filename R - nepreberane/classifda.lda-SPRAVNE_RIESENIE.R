library(MASS)

data(iris) ##load data

iris.lda = lda(as.formula(paste("iris$Species ~ ", paste(colnames(iris[-5]), collapse="+"))), data=iris)
iris.classif = predict(iris.lda)

res = data.frame("Taxon" = iris$Species, "Classif" = iris.classif$class,
                 round(iris.classif$posterior, digits = 4))

View(res)



data = read.delim("clipboard", header = T)

data.lda = lda(data$Crop ~ V1+V2+V3+V4, data=data)
data.classif = predict(data.lda)

res = data.frame(data$Crop, "Classif" = data.classif$class, round(data.classif$posterior, digits = 4))

View(res)



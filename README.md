# MorphoTools


## TODO list

Running 'testthat.R' [11s] (10.9s)
   Running the tests in 'tests/testthat.R' failed.
   Last 13 lines of output:
     -- 1. Failure: classifSamp.knn (@test.classifMatrix.R#101)  ---------------------------------------
     colnames(m) not equal to c("Taxon", "N", "as.hybr", "as.ph", "as.ps", "as.st").
     Lengths differ: 5 is not 6
     
     -- 2. Failure: classifSamp.knn (@test.classifMatrix.R#106)  ---------------------------------------
     colnames(m) not equal to c("Population", "Taxon", "N", "as.hybr", "as.ph", "as.ps", "as.st").
     Lengths differ: 6 is not 7





### morphodata
**S3** class  implementovane ako list

##### S3 methods

```
summary.morphodata()
```

##### data manipulation

```
delete.population()
delete.taxon()
delete.charecter()

na.meanSubst()
```


### export.res
implemented as generic function
```
export.res.morphodata()
export.res.data.frame()
```

### Descriptive statistics
```
descr.tax(object, format)
descr.pop(object, format)
descr.all(object, format
```

### Box Plots
allowin user to extend whiskers to desired percentiles
```
boxplot.character()
boxplot.all()
```

### Population means
```
popul.otu()
```

### Distribution of characters, transformations
zatial neimplementovane

### Correlations of characters





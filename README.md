# MorphoTools


## TODO list

problem je ked rata CDA a je nejaky znak identicky, vyhodi chybu, ale je to nejaka divna hlaska z vnutra
+  upozorni ze znak je identicky vo vnutri taxonov





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





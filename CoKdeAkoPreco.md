## Data import and control

#### Data structure

vstupny format je ako v MorfoTools 2014. Rozdiel je v tom, ze explicitne vyzadujem, aby uzivatel oznacil prislusne stlpce s popiskami "ID", "Population" a "Taxon".

#### read.morphodata

skontroluje ci ma vstupny subor aspon 3 stlpce a interne volam metodu morphodataFromDataFrame(). Metoda morphodataFromDataFrame() zobere data.frame a urobi z toho objekt triedy morphodata (implementovane ako list). Kontrolujem ci data.frame obsahuje stlpce "ID", "Population" a "Taxon". Este skontolujem ci medzi datami nie su textove retazce = problem desatinnej ciarky a bodky. aby to niekde dalej nespadlo s nejakou divnou chybou. Ak su data chybme, musi to spadnut tu.

#### summary(morphodata)
genericka gunkcia, rekapitulacia struktury dat


## Basic data manipulation

#### delete.population(object, populationName)
#### delete.taxon(object, taxonName)

1. skontrolujem ci objekt je typu 'morphodata'
2. skontrolujem ci populationName alebo taxonName sa nachadza v objekte

3. interne volam removeByColumn(object, column, groupName), kde je implementovana core funkcionalita. Najdem riadky ktore sa maju vyhodit podla definovaneho stlpca. Vratim novy objekt triedy 'morphodata'.


#### delete.charecter(object, charecter)
1. skontrolujem ci objekt je typu 'morphodata'
2. skontrolujem ci charecter existuje
Vratim novy objekt triedy 'morphodata'.

#### na.meanSubst(object)
1. skontrolujem ci objekt je typu 'morphodata'
2. vyratam priemry pre populaciu (sapply)
3. skontrolujem ci boly priemery vyratane. ak je pre nejaky znak cela populacia NA, priemer bude NA. Toto reportujem ako Warning. Ak tam ostanu NA, spradne to dalej, napr v PCA.


#### export.res
genericka funkcia, na export vsetkych typov dat sa bude pouzivat export.res(). 
```
export.res.morphodata()
export.res.data.frame()
export.res.matrix()
export.res.numeric()
export.res.classifdata()
```

## Descriptive statistics

### descr.tax(object, format)
### descr.pop(object, format)
### descr.all(object, format)

1. skontrolujem ci objekt je typu 'morphodata'
2. interne volam descrByGroup(object, column). metoda vyrata descr. stat. podla definovaneho stlpca (Taxon, Population, alebo all (implementovane tak, ze do dat vlozim este jeden stlpec (all) rovnaky pre vsetky polozky = vyrata descr. stat. pre cely dataset)). descrStatistic zaokruhlim na 3 desatinne miesta.

tieto metody maju aj format parameter. The "format" argument brings handy way how to receive only what is wanted and in format what is desired.

ak uzivatel specifikuje format, volam metodu formatDescrStatistic(). ak nie volam unFormatDescrStatistic().
vsetko implementovane raz a volane zo vsetkych descr.tax/pop/all metod.

## Box Plots

#### boxplot.character
#### boxplot.all

fuzy boxplotu sa daju natiahnut podla priania. uzitocne pre hladanie rozdielov v znakoch mezi taxonmi pre identifikacne kluce, etc.  
boxplot.character() spravi boxplot pre jeden znak  
boxplot.all() vyrobi boxploty pre vsetky znaky v datasete, obrazky ulozi ako jpeg do priecinka.  

obe metody interne volaju giveMeNiceBoxPlot() - core implementacia.


## Population means
1. skontrolujem ci objekt je typu 'morphodata'
2. pouzijem metodu aggregate.
3. kontrolujem a vypisem Warning ak data obsahuju NA

## Distribution of characters, transformations
toto zatial nerisim, je to vobec treba? pouziva to niekto?
## normality test
skor pro forma vec, jasne ze su vsetky morfo data "nenormalne" - zatial neimplementovane  
- testuju sa znaky pre kazdy taxon zvlast alebo dokopy?

## Correlations of characters
#### cormat
klasika. v popise v metode spominam okrem Pearson a Spearman aj Kendall, nakolko je to validny vstupny parameter. keby sa v tom niekto rypal, Kendall mu bude fungovat. ak ho necheme, tak to treba osetrit.
#### cormat.signifTest
cormat + significance tests. output je rovnaky, akurat rozsireny o p-value.

## Cluster analysis
### clust
UPGMA; Ward’s; complete linkage - okrm tohto su pre hclust validne aj ine vstupy. uvadzam v dokumentacia. ak ich nechceme, treba to osetrit

## Principal component analysis
#### pca.calc
uzivatelovi nevyhadzujem datas s NA bez jeho vedomia. Radsej nech to spadne s chybovou hlaskou a uzivatel nech si to vyriesi podla lubovôle.  
vrati objekt triedy pcadata  
tento objekt ma vsebe vsetky potrebne data, nie len to co je v prcomp
nepodporujem pasivne premiestanie objektov, ako to bolo mozne v Koutecky 2014. V PCA toto nema zmysel, nakolko su vsetky objekty vykreslene bez informacie o ich prislusnosti a zobrazuje to len variabilitu v celom subore. 


plot.points(pcadata) vykresli scatterplot of objects. Genericka metoda, rovnaka metoda vykresli scatterplot aj pre CDA
labels.points(pcadata) vykrelsli labels k bodom; genericka metoda

plot.characters(pcadata) - vykresli scatterplot of characters
labels.characters(pcadata) - vykrelsli labels ku znakom

plot.legend(pcadata) vykresli legendu k obrazku, genericka metoda 

genericke metody - nie ze by to bolo nutne, je to vsak potrebne pre to aby tato niektore metody mohli byt pomenovane tak ako su. keby plot.legend nebola sama genericka, tak by ju R chapalo ako genericku metodu k plot(). rovnaky pripad je labels.points a labels.characters. keby sami neboli genericke, R by ich chapalo ako gener voci metode labels()

metody labels.points() a plot.legend() su len genericke wrapery pre metody labels.points.internal() a plot.legend.internal(). objekty "pcares" a "cdares" maju ~ rovnaku vnutornu strukturu. data potrebne pre vykreslovanie su ulozene v result$objects$..



summary

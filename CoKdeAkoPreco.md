# Data import and control

### Data structure

vstupny format je ako v MorfoTools 2014. Rozdiel je v tom, ze explicitne vyzadujem, aby uzivatel oznacil prislusne stlpce s popiskami "ID", "Population" a "Taxon".

### read.morphodata

skontroluje ci ma vstupny subor aspon 3 stlpce a interne volam metodu morphodata(). Metoda morphodata() zobere data.frame a urobi z toho objekt triedy morphodata (implementovane ako list). Kontrolujem ci data.frame obsahuje stlpce "ID", "Population" a "Taxon". Este skontolujem ci medzi datami nie su textove retazce = problem desatinnej ciarky a bodky. aby to niekde dalej nespadlo s nejakou divnou chybou. Ak su data chybme, musi to spadnut tu.

### summary(morphodata)
genericka gunkcia, rekapitulacia struktury dat


# Basic data manipulation

### delete.population(object, populationName)
### delete.taxon(object, taxonName)

1. skontrolujem ci objekt je typu 'morphodata'
2. skontrolujem ci populationName alebo taxonName sa nachadza v objekte

3. interne volam removeColumn(object, column, groupName), kde je implementovana core funkcionalita. Najdem riadky ktore sa maju vyhodit podla definovaneho stlpca. Vratim novy objekt triedy 'morphodata'.


### delete.charecter(object, charecter)
1. skontrolujem ci objekt je typu 'morphodata'
2. skontrolujem ci charecter existuje


### na.meanSubst(object)
1. skontrolujem ci objekt je typu 'morphodata'
2. vyratam priemry pre populaciu (sapply)
3. skontrolujem ci boly priemery vyratane. ak je pre nejaky znak cela populacia NA, priemer bude NA. Toto reportujem ako Warning. Ak tam ostanu NA, spradne to dalej, napr v PCA.


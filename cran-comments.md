## Notes


### New submission


#### R CMD check results

0 errors | 1 warnings | 1 notes

* checking sizes of PDF files under ‘inst/doc’ ... **WARNING**
  ‘gs+qpdf’ made some significant size reductions:
     compacted ‘MorphoTools2_tutorial.pdf’ from 1202Kb to 877Kb
  consider running tools::compactPDF(gs_quality = "ebook") on these files
  
> I rebuld this package using "devtools::build(args = c('--resave-data','--compact-vignettes="gs+qpdf"'))"


* checking CRAN incoming feasibility ... **NOTE**: Possibly misspelled words in DESCRIPTION:  Morphometric (3:21)  

> I don't think this is misspelled, as "Morphometric Analysis" is a regular phrase. 

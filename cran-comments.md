## Notes




### Resubmission

This is a resubmission. In the cran-comments.md file, I provide answers to reviewers comments:  

> Is there some reference about the method you can add in the Description field in the form Authors (year) <doi:10.....> or <arXiv:.....>?

For now, there is no published work to reference in the description. When a publication becomes available (likely before the next version of the package), I will update it.

> Please omit the redundant "An R package for" from your description field. 
> Please add more details about the package functionality and implemented methods in your Description text.

Reformulated.

> Please add \\value to .Rd files regarding exported methods and explain the functions results in the documentation. Please write about the structure of the output (class) and also what the output means. (If a function does not return a value, please document that too, e.g. \value{No return value, called for side effects} or similar)  
Missing Rd-tags in up to 25 .Rd files, e.g.: boxMTest.Rd, boxplotCharacter.Rd, characters.Rd, classif.matrix.Rd, descrTaxon.Rd, exportRes.Rd,  ...

Fixed. 


> Some code lines in examples are commented out in exportRes.Rd, read.morphodata.Rd, viewMorphodata.Rd. Please never do that. Ideally find toy examples that can be regularly executed and checked. Lengthy examples (> 5 sec), can be wrapped in \\donttest.

Fixed. 

> Please ensure that your functions do not write by default or in your examples/vignettes/tests in the user's home filespace (including the package directory and getwd()). This is not allowed by CRAN policies. In your examples/vignettes/tests you can write to tempdir().

Fixed. 


> Please make sure that you do not change the user's options, par or working directory. If you really have to do so within functions, please ensure with an *immediate* call of on.exit() that the settings are reset when the function is exited. e.g.: histCharacter.R, qq.R

Fixed. 

> Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos. e.g.: inst/doc/MorphoTools2_tutorial.R

Fixed. 

> You are setting options(warn=-1) in your tests. This is not allowed. Please rather use suppressWarnings() if really needed.

Fixed. 


> Please fix and resubmit.

Done :). I apologize if I missed something or did not provided the expected changes. I'm happy to fix or change whatever is required.










### New submission


#### R CMD check results

0 errors | 0 warnings | 1 notes


* checking CRAN incoming feasibility ... **NOTE**: Possibly misspelled words in DESCRIPTION:  Morphometric (3:21)  

> I don't think this is misspelled, as "Morphometric Analysis" is a regular phrase. 

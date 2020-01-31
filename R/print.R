



print.classifdata <- function(object) {

  attr(object, "class") <- "data.frame"

  print(object)

}

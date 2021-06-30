#' Transformation of character.
#' @export
transformCharacter <- function(object, character, FUN, newName = NULL) {

  .checkClass(object, "morphodata")

  if (!(character %in% colnames(object$data))) stop(paste("Character", character, "was not found in attached data."), call. = FALSE)



  object$data[character] = FUN(object$data[character])
  if (! is.null(newName)) {
    colnames(object$data)[which(colnames(object$data) == character)] = newName
  }

  return(object)
}


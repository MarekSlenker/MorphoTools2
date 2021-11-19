#' Canonical discriminant analysis (CDA)
#' @importFrom candisc candisc
#' @export
cda.calc <- function(object, passiveSamples = NULL) {
  .checkClass(object, "morphodata")

  # matica musi byt plna
  if (any(is.na(object$data))) stop("NA values in 'object'.", call. = FALSE)

  # find and report constant columns
  constantColumns = colnames(object$data)[apply(object$data, 2, function(x) (abs(max(x)-min(x)))==0 )]
  if (length(constantColumns)>0) {
    stop(paste("Characters \"", paste(constantColumns, collapse = "\", \""), "\" are invariant.", sep = ""), call. = FALSE)
  }

  #          characters          samoles  taxa _____________________________________
  if (dim(object$data)[2] >= (length(object$ID) - length(levels(object$Taxon)))) {
    warning("For the number of taxa (g), characters (p) and the total number of samples (n), the following should hold: 0 < p < (n - g);", call. = FALSE)
  }

  for (pasSample in passiveSamples) {
    if (! ((pasSample %in% levels(object$Taxon)) || pasSample %in% levels(object$Population) ) ) stop(paste("Taxon \"", pasSample, "\" was not found in attached data.", sep = ""), call. = FALSE)
  }

  # vypocitaj na zaklade skratenej matice (bez pop alebo taxa)
  # ak NULL, matica sa nezmeni

  objectWithPassiveSamples = object  # povodny objekt
  objectNoPassiveSamples = object

  for (groupName in passiveSamples) {
    if (groupName %in% objectNoPassiveSamples$Taxon) objectNoPassiveSamples = .removeByColumn(objectNoPassiveSamples, "Taxon", groupName)
    if (groupName %in% objectNoPassiveSamples$Population) objectNoPassiveSamples = .removeByColumn(objectNoPassiveSamples, "Population", groupName)
  }

  candisc_MK <- function (mod, term, type = "2", manova, ndim = rank, ...)
  {
    if (!inherits(mod, "mlm"))
      stop("Not an mlm object")
    if (missing(manova))
      manova <- car::Anova(mod, type = as.character(type))
    terms <- manova$terms
    if (missing(term))
      term <- terms[1]
    E <- manova$SSPE
    H <- manova$SSP[[term]]
    dfe <- manova$error.df
    dfh <- manova$df[[term]]
    Sp <- E/dfe

    #tdecomp <- function(m) {
    #    wm <- eigen(m, symmetric = TRUE)  # povodne symmetric T
    #    p <- ncol(m)
    #    wmd <- wm$values

    #    # tam kde sa wmd blizi 0, odmocnina z toho spravi NaN, tak z toho teraz spravim nieco veelmi male (z NaN)
    #    diasq=suppressWarnings(diag(sqrt(wmd)))
    #    malaBlbost = replace(diasq, is.na(diasq), -1.4E-4)  # MK
    
    #    out <- t(wm$vectors %*% malaBlbost)  # MK
    #    #out <- t(wm$vectors %*% diag(sqrt(wmd))) # candisc
    #    out
    #}
    #Tm <- tdecomp(E)

    Tm <- fpc::tdecomp(E)


    eInv <- solve(Tm)

    eHe <- t(eInv) %*% H %*% eInv
    dc <- eigen(eHe, symmetric = TRUE)  # povodne symmetric T
    rank <- min(dfh, sum(dc$values > 0))
    pct <- 100 * dc$values/sum(dc$values)
    if (ndim > rank) {
      warning(paste("You asked for", ndim, "dimensions, but rank is", rank, ". ndim has been reset to", rank))
      ndim <- rank
    }
    coeffs.raw <- eInv %*% dc$vectors * sqrt(dfe)
    coeffs.raw <- as.matrix(coeffs.raw[, 1:ndim])
    rownames(coeffs.raw) <- rownames(H)
    colnames(coeffs.raw) <- cn <- paste("Can", 1:ndim, sep = "")
    coeffs.std <- diag(sqrt(diag(Sp))) %*% coeffs.raw
    rownames(coeffs.std) <- rownames(H)
    colnames(coeffs.std) <- cn
    data <- stats::model.frame(mod)
    Y <- stats::model.response(data)
    Y <- scale(Y, center = TRUE, scale = FALSE)
    scores <- Y %*% coeffs.raw
    scores <- as.matrix(scores[, 1:ndim])
    colnames(scores) <- cn
    all.factors <- data[, sapply(data, is.factor), drop = FALSE]
    factor.names <- unlist(strsplit(term, ":"))
    factors <- data[factor.names]
    means <- stats::aggregate(scores, factors, mean)
    rownames(means) <- do.call(paste, c(means[factor.names],
                                        sep = ":"))
    means <- means[, -(1:length(factor.names))]
    structure <- stats::cor(Y, scores)
    canrsq <- dc$values[1:ndim]/(1 + dc$values[1:ndim])
    #scores <- cbind(model.frame(mod)[predictor.names(mod)], as.data.frame(scores))
    scores <- as.data.frame(scores)
    result <- list(dfh = dfh, dfe = dfe, eigenvalues = dc$values,
                   canrsq = canrsq, pct = pct, rank = rank, ndim = ndim,
                   means = means, factors = factors, term = term, terms = terms,
                   coeffs.raw = coeffs.raw, coeffs.std = coeffs.std, structure = structure,
                   scores = scores)
    class(result) <- "candisc"
    result
  }

  # calculate with objectNoPassiveSamples
  d_NoPassiveSamples = as.matrix(objectNoPassiveSamples$data)
  x_NoPassiveSamples = stats::lm(d_NoPassiveSamples ~ objectNoPassiveSamples$Taxon)

  # cda = candisc::candisc(x_NoPassiveSamples, term="objectNoPassiveSamples$Taxon")
  cda = candisc_MK(x_NoPassiveSamples, term="objectNoPassiveSamples$Taxon")

  newNames = NULL
  for (i in 1:cda$rank) {
    newNames = c(newNames, paste("Can", i, sep = ""))
  }

  names(cda$eigenvalues) = newNames
  names(cda$pct) = newNames

  cdaResult = .newCdadata()

  cdaResult$rank = cda$rank
  cdaResult$eigenvalues = cda$eigenvalues[1:cda$rank]
  cdaResult$canrsq = cda$canrsq
  cdaResult$eigenvaluesAsPercentages = cda$pct[1:cda$rank] / 100

  for (i in cdaResult$rank:1) {
    cdaResult$cumulativePercentageOfEigenvalues[i] = sum(cdaResult$eigenvaluesAsPercentages[1:i])
  }

  cdaResult$coeffs.raw = cda$coeffs.raw
  cdaResult$coeffs.std = cda$coeffs.std
  cdaResult$totalCanonicalStructure = cda$structure


  cdaResult$objects$ID = objectWithPassiveSamples$ID
  cdaResult$objects$Population = objectWithPassiveSamples$Population
  cdaResult$objects$Taxon = objectWithPassiveSamples$Taxon

  # predict na zaklade plnej matice
  # scaleFactor je konstanta, o ktoru treba posunut data
  # scaleFactor = (d_NoPassiveSamples %*% cda$coeffs.raw - cda$scores[,-1])[1,]  // -1 je tiez pozostatok stareho CANDISC
  scaleFactor = (d_NoPassiveSamples %*% cda$coeffs.raw - cda$scores[,])[1,]
  d_WithPassiveSamples = as.matrix(objectWithPassiveSamples$data)
  scoreList = apply(d_WithPassiveSamples %*% cda$coeffs.raw,1, FUN = function(x) {x - scaleFactor})

  scoreList = lapply(scoreList, function(y)as.vector(t(as.matrix(y))))
  scoreList = do.call(rbind, scoreList)
  scoreList = data.frame(scoreList, stringsAsFactors=FALSE)

  cdaResult$objects$scores = scoreList
  colnames(cdaResult$objects$scores) = colnames(cda$scores)
  #colnames(cdaResult$objects$scores) = colnames(cda$scores)[-1]  // stare, ked este som neupravil candisc, tak to tam pchalo jeden stlpec navyse

  #  predict na zaklade plnej matice = novych dat
  cdaResult$groupMeans = stats::aggregate(cdaResult$objects$scores, by = list("Taxon" = cdaResult$objects$Taxon), mean)

  return(cdaResult)
}

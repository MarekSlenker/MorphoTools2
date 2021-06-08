

#' Stepwise discriminant analysis
#' @export
stepdisc.calc <- function(object, F_to_enter = 0.15, F_to_stay = 0.15) {

  # matica musi byt plna
  if (any(is.na(object$data))) stop("NA values in 'object'.", call. = FALSE)

  # find and report constant columns
  constantColumns = colnames(object$data)[apply(object$data, 2, function(x) (abs(max(x)-min(x)))==0 )]
  if (length(constantColumns)>0) {
    stop(paste("Characters", paste(constantColumns, collapse = ", "), "are invariant."), call. = FALSE)
  }

  saturatedModel = FALSE
  X = as.matrix(object$data)     # data matrix
  grouping = object$Taxon       # vector of groupings
  k = length(levels(grouping))   # number of existing groups/populations
  n = nrow(X)                    # number of observations

  Entered = Removed = character()

  if(k < 2) stop("at least two levels required to do anything meaningful", call. = FALSE)
  if(n < 2) stop("n > 1 observations required to do anything meaningful", call. = FALSE)


  # finding the first variable to accept in the model:
  p = ncol(X)                 # number of samples
  enter.Fstat = enter.PrF = enter.R2 = numeric(p)
  for(j in 1:p){
    enterSum = summary(stats::aov(X[,j] ~ grouping))
    enter.Fstat[j] = enterSum[[1]]$`F value`[1]
    enter.PrF[j] = enterSum[[1]]$`Pr(>F)`[1]
    enter.R2[j] = (enterSum[[1]]$`Sum Sq`[1]/sum(enterSum[[1]]$`Sum Sq`))
  }

  if(min(enter.PrF) < F_to_enter) {                       # condition for stopping the forward-selection
    a = which.min(enter.PrF)
    Entered = c(Entered, colnames(X)[a])
    Removed = c(Removed, "")

    # pick variable which seperates the groups most, this is variable a, the one with the smallest p-value
    auswahl = colnames(X)[a]
    weiter = TRUE                     # parameter to report the necessity of another variable selection
    X.mod = as.matrix(X[,auswahl])   # model of the registered variables
    colnames(X.mod) = auswahl
    X = X[, colnames(X) != auswahl]  # data-matrix of the selection of variables
    p = ncol(X)
    Fstat = enter.Fstat[a]           # approx. F-statistic of the Wilks lambda of the selected model / partial Wilks' lambda
    pwert = enter.PrF[a]              # appropriate p-value of Fstat and Fstat2
    R2 = enter.R2[a]

  } else {weiter = FALSE}


  # ---REMOVE VARIABLE ?  ee


  # finding the next variable to accept in the model:
  while(weiter && !is.null(p) && p>0 && ncol(X.mod) < (n-k))
  {
    auswahl = NULL          # vector for the names of the significant variables of the selection
    enter.Fstat = enter.PrF = enter.R2 = numeric(p)
    # ---NAJDI NAJMENSIE F to ENTER----------------MK------- #
    for(j in 1:p){
      enterSum = summary(stats::aov(X[,j] ~ X.mod+grouping))
      if ((!is.null(enterSum[[1]]$`F value`[2])) && (!is.na(enterSum[[1]]$`F value`[2]))) {  # model is not saturated
        enter.Fstat[j] = enterSum[[1]]$`F value`[2]
        enter.PrF[j] = enterSum[[1]]$`Pr(>F)`[2]
        enter.R2[j] = (enterSum[[1]]$`Sum Sq`[2]/ (sum(enterSum[[1]]$`Sum Sq`)-enterSum[[1]]$`Sum Sq`[1]))
      } else saturatedModel = TRUE
    }

    if (saturatedModel) break

    # ---ENTER VARIABLE----------------------------------- #
    a = which.min(enter.PrF)[1]       # MK     # most significant variable a (with the smalles Pr(>F))
    if(enter.PrF[a] < F_to_enter)
    {                 # condition for stopping the forward-selection
      Entered = c(Entered, colnames(X)[a])
      Removed = c(Removed, "")

      namen2 = colnames(X)
      auswahl = namen2[a]              # see a
      namen = colnames(X.mod)
      X.mod = as.matrix(cbind(X.mod, X[,auswahl]))
      colnames(X.mod) = c(namen, auswahl)
      X = as.matrix(X[, namen2 != auswahl])
      p = ncol (X)
      if(p == 1) colnames(X) = namen2[namen2 != auswahl]
      if(p == 0) weiter = FALSE               # the case of selecting all variables: non is left to be selected
      Fstat = c(Fstat, enter.Fstat[a])    # F-value
      pwert = c(pwert, enter.PrF[a])    # appropriate p-values of Fstat
      R2 = c(R2, enter.R2[a])
    } else {weiter = FALSE}

    # ---REMOVE VARIABLE---------------------MK----------- #

    while(TRUE){

      # Statistics for Removal
      rem.Fstat = rem.PrF = rem.R2 = numeric(ncol(X.mod))

      for(j in 1:ncol(X.mod)){
        remSum = summary(stats::aov(X.mod[,j] ~ X.mod[,-j]+grouping))

        rem.Fstat[j] = remSum[[1]]$`F value`[2]
        rem.PrF[j] = remSum[[1]]$`Pr(>F)`[2]
        rem.R2[j] = (remSum[[1]]$`Sum Sq`[2]/ (sum(remSum[[1]]$`Sum Sq`)-remSum[[1]]$`Sum Sq`[1]))
      }

      # ---REMOVE VARIABLE----------------------------------- #
      a = which.max(rem.PrF)[1]
      if(rem.PrF[a] > F_to_stay){
        Entered = c(Entered, "")
        Removed = c(Removed, colnames(X.mod)[a])

        # remove from X.mod = add back to X
        xnames = colnames(X)
        X = cbind(X, X.mod[,a])
        colnames(X) = c(xnames, colnames(X.mod)[a])
        p = ncol (X)
        X.mod = X.mod[,-a]

        Fstat = c(Fstat, rem.Fstat[a])    # F-value
        pwert = c(pwert, rem.PrF[a])    # appropriate p-values of Fstat
        R2 = c(R2, rem.R2[a])
      } else break

    }
  }


  if(!exists("X.mod")) stop("unable to perform required calculations, perhaps not enough observations?", call. = FALSE)

  resDat = data.frame(Entered, Removed, R2, Fstat, pwert)
  colnames(resDat) = c("Entered", "Removed", "Partial R-Square", "F Value", "Pr > F")

  print(resDat)
  cat("\nselected characters:\n")
  cat(paste(Entered[Entered != ""], collapse = ", "))
  cat("\n")
  #cat("\n")
  #cat(paste("\"", paste(Entered[Entered != ""], collapse = "\", \""), "\"\n"), sep = "")
}









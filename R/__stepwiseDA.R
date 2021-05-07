
dd = read.delim("clipboard", header = F, sep = ",")
dd = read.delim("clipboard", header = F, sep = ";")

dim(dd)

# greedy.wilks_MK(dd[,2:24], dd[,1])$results


niveau = 0.15


X=dd[,2:20]; grouping=dd[,1]

X=dd[,2:24]; grouping=dd[,1]


stepdisc(X, grouping)

niveau = 0.15




stepdisc <- function(X, grouping, niveau = 0.15, ...){

  namesset <- FALSE
  saturatedModel = FALSE
  if (is.null(colnames(X))) {colnames(X) <- 1:ncol(X); namesset <- TRUE}

  ### Help Function
  Lambda <- function(X, grouping){
    # function to calculate the eigenvalues of model X with its vector of groupings grouping
    n     <- nrow(X)                      # number of observations
    n.vek <- table(grouping)              # numbers of observations in each group
    C1 <- H <- matrix(0, nrow=n, ncol=n)

    for(i in seq(along=levels(grouping))){
      einsi        <- as.numeric(grouping == levels(grouping)[i])
      C1           <- C1 + (diag(einsi) - (einsi %*% t(einsi)) / n.vek[i])
    }

    W <- t(X) %*% C1 %*% X       # "within-groups" SSP matrix

    H[] <- -1/n
    diag(H) <- 1 - 1/n
    TS <- t(X) %*% H %*%X        # "total" SSP matrix
    B  <- TS-W                   # "between-groups" SSP matrix
    A  <- solve(W) %*% B
    evek <- matrix(eigen(A, symmetric = FALSE)$values, ncol = 1)  # evaluating for the eigenvalues
    evek <- Re(evek[abs(Im(evek)) < .Machine$double.eps^0.5])  # erasing complex eigenvalues (in case of existence)
    evek <- sort(evek[evek > 0], decreasing = TRUE)     # sorting the positive eigenvalues, biggest at first
    lambda <- prod(1/(1 + evek))              # calcutating Wilks' lambda
    return(lambda)                            # Output: Wilks' lambda
  }  ### END Help Function


  gpVar <- deparse(substitute(grouping))
  if(!is.null(ncol(grouping)) && ncol(grouping) > 1)
    stop("only one grouping variable supported")

  X        <- as.matrix(X)               # data matrix
  grouping <- factor(grouping)           # vector of groupings
  k        <- length(levels(grouping))   # number of existing groups/populations
  n        <- nrow(X)                    # number of observations

  Entered = Removed = character()

  if(k < 2)
    stop("at least two levels ", "required to do anything meaningful")
  if(n < 2)
    stop("n > 1 observations ", "required to do anything meaningful")

  # finding the first variable to accept in the model:
  p           <- ncol(X)                 # number of samples
  first.pwert <- first.fstat <- numeric(p)
  for(j in 1:p){
    e1       <- aov(X[,j] ~ grouping)
    e1sum    <- summary(e1)[[1]]
    first.pwert[j] <- e1sum[["Pr(>F)"]][1]
    first.fstat[j] <- e1sum[["F value"]][1]
  }

  if(min(first.pwert) < niveau) {                       # condition for stopping the forward-selection
    a               <- which.min(first.pwert)
    Entered = c(Entered, colnames(X)[a])
    Removed = c(Removed, "")

    # pick variable which seperates the groups most, this is variable a, the one with the smallest p-value
    auswahl         <- colnames(X)[a]
    weiter          <- TRUE                     # parameter to report the necessity of another variable selection
    X.mod           <- as.matrix(X[,auswahl])   # model of the registered variables
    colnames(X.mod) <- auswahl
    X               <- X[, colnames(X) != auswahl]  # data-matrix of the selection of variables
    p               <- ncol(X)
    # wilks           <- Lambda(matrix(X.mod), grouping)  # Wilks' lambda of the selected model
    Fstat <- first.fstat[a]           # approx. F-statistic of the Wilks lambda of the selected model / partial Wilks' lambda
    pwert <- min(first.pwert)                # appropriate p-value of Fstat and Fstat2


  } else {weiter <- FALSE}



  # ---REMOVE VARIABLE----1st-----------------MK----------- #
  # Statistics for Removal

  mk_mod = aov(X.mod ~ grouping)
  mk_mod_sum    <- summary(mk_mod)[[1]]

  mk_mod_sum[["Pr(>F)"]][1]
  mk_mod_sum[["F value"]][1]


  # TODO:   REMORE

  #####################  MK  end



  # finding the next variable to accept in the model:
  while(weiter && !is.null(p) && p>0)
  {
    auswahl     <- NULL          # vector for the names of the significant variables of the selection

    enter.Fstat  <- enter.PrF  <- numeric(p)

    # ---NAJDI NAJMENSIE F to ENTER----------------MK------- #
    for(j in 1:p){
      enterSum = summary(aov(X[,j] ~ X.mod+grouping))
      if (! is.null(enterSum[[1]]$`F value`[2])) {  # model is not saturated
        enter.Fstat[j] <- enterSum[[1]]$`F value`[2]
        enter.PrF[j] <- enterSum[[1]]$`Pr(>F)`[2]
      } else saturatedModel = TRUE

    }

    if (saturatedModel) {
      break
    }


    # ---ENTER VARIABLE----------------------------------- #
    a <- which.min(enter.PrF)[1]       # MK     # most significant variable a (with the smalles Pr(>F))

    if(enter.PrF[a] < niveau)
    {                 # condition for stopping the forward-selection
      Entered = c(Entered, colnames(X)[a])
      Removed = c(Removed, "")

      namen2          <- colnames(X)
      auswahl         <- namen2[a]              # see a
      namen           <- colnames(X.mod)
      X.mod           <- as.matrix(cbind(X.mod, X[,auswahl]))
      colnames(X.mod) <- c(namen, auswahl)
      X               <- as.matrix(X[, namen2 != auswahl])
      p               <- ncol (X)
      if(p == 1) colnames(X) <- namen2[namen2 != auswahl]
      if(p == 0) weiter      <- FALSE               # the case of selecting all variables: non is left to be selected
      #wilks           <- c(wilks, ausw.wilks[a])    # Wilks' lambdas of the selected models
      Fstat           <- c(Fstat, enter.Fstat[a])    # F-value
      #Fstat2          <- c(Fstat2, ausw.Fstat2[a])  # approx. F-statistics of the partial Wilks' lambdas
      pwert           <- c(pwert, enter.PrF[a])    # appropriate p-values of Fstat
      #pwert2          <- c(pwert2, ausw.pwert2[a])  # appropriate p-values of Fstat2
    } else {weiter <- FALSE}

    # ---REMOVE VARIABLE---------------------MK----------- #
    # Statistics for Removal
    rem.Fstat = rem.PrF = numeric(ncol(X.mod))

    for(j in 1:ncol(X.mod)){
      remSum = summary(aov(X.mod[,j] ~ X.mod[,-j]+grouping))

      rem.Fstat[j] <- remSum[[1]]$`F value`[2]
      rem.PrF[j] <- remSum[[1]]$`Pr(>F)`[2]
    }

    # ---REMOVE VARIABLE----------------------------------- #
    a <- which.max(rem.PrF)[1]
    if(rem.PrF[a] > niveau){
      Entered = c(Entered, "")
      Removed = c(Removed, colnames(X.mod)[a])

      # remove from X.mod = add back to X
      xnames = colnames(X)
      X = cbind(X, X.mod[,a])
      colnames(X) = c(xnames, colnames(X.mod)[a])
      p = ncol (X)
      X.mod = X.mod[,-a]

      Fstat           <- c(Fstat, rem.Fstat[a])    # F-value
      pwert           <- c(pwert, rem.PrF[a])    # appropriate p-values of Fstat
    }

    # ZOPAKUJ ESTE RAZ, AZ KYM FALSE

    #####################  MK  end


  }


  if(!exists("X.mod"))
    stop("unable to perform required calculations, perhaps not enough observations?")
  vars <- colnames(X.mod)
  if (namesset) vars<-as.numeric(vars)
  resDat <- data.frame(Entered = Entered, Removed = Removed, F.statistics.overall = Fstat,
                       p.value.overall = pwert)
  return(resDat)
}








#  Constant Variables that have been Excluded











object = read.morphodata("clipboard")

ntax<-length(levels(object$Taxon))
char<-colnames(object$data)

reg = lda(as.numeric( object$Taxon) ~ ., data = object$data)
summary(reg)
stepModel = stepAIC(reg, direction = "both")




data.frame(stepModel$anova$Step, stepModel$anova$AIC)

stepModel$anova



##############  KOUTA

quine.hi <- aov(as.numeric( object$Taxon) ~ ., data = object$data)
summary(quine.hi)
quine.stp <- stepAIC(quine.hi)

quine.stp <- stepAIC(quine.hi),
                     scope = list(upper = ~Eth*Sex*Age*Lrn, lower = ~1),
                     trace = FALSE)
quine.stp$anova


birthwt.glm <- glm(as.numeric( object$Taxon) ~ ., data = object$data)
birthwt.step <- stepAIC(birthwt.glm, trace = TRUE)



quine.stp$anova
############

iris

data <- mtcars[,c("mpg","disp","hp","wt")]
Model1_LM <- lm(mpg ~ ., data = data)
summary(Model1_LM)
fit1_LM <- stepAIC(Model1_LM, direction = "both")
summary(fit1_LM)




###############



library(klaR)

data(iris)
library(MASS)
iris.d <- iris[,1:4]  # the data
iris.c <- iris[,5]    # the classes
sc_obj <- stepclass(iris.d, iris.c, "lda", start.vars = "Sepal.Width")
sc_obj
plot(sc_obj)

## or using formulas:
sc_obj <- stepclass(Species ~ ., data = iris, method = "qda",
                    start.vars = "Sepal.Width", criterion = "AS")  # same as above
sc_obj
## now you can say stuff like
## qda(sc_obj$formula, data = B3)
# }


dd =read.delim("clipboard", header = F, sep = "\t")

stepclass(dd[,2:20], dd[,1], "lda")



greedy.wilks_MK(iris[,1:4], iris[,5])

greedy.wilks_MK(dd[,2:20], dd[,1])

X = dd[,2:20]
grouping = dd[,1]



X = iris[,1:4]
grouping = iris[,5]








##########♣


data(iris)











length(first.fstat)






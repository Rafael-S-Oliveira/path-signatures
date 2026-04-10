# Author: Daniel A.M. Villela
# functions to implpement path signature in R
# and lasso regression with signatures

library("reticulate")

env.instance <- function(x) {
  reticulate::virtualenv_create(x)
  reticulate::use_virtualenv(x, required = TRUE)
  reticulate::py_config()
}

pathsig.env <- function(x) {
 # conda_create("r-reticulate")
  use_python("/usr/bin/python3")
  #virtualenv_remove(packages = "numpy")
  py_install("numpy<2.0")
  py_install("esig")
  #virtualenv_create("r-reticulate")
  
  # install SciPy
  #virtualenv_install("r-reticulate", packages = "numpy")
  # install SciPy
  #virtualenv_install("r-reticulate", packages = "esig")
  
  # import SciPy (it will be automatically discovered in "r-reticulate")
}

getESig <- function(x) {
  esig <- import("esig")
esig  
}

esig <- import("esig")

#pathsig.env()

pathsig.esig <- function(path, m) {
  truncation_level = as.integer(m)
  signature <- esig$stream2sig(path, truncation_level)
  signature
}

pathsig.logsig <- function(path, m) {
  truncation_level = as.integer(m)
  signature <- esig$stream2logsig(path, truncation_level)
  signature
}

pathsig.one <- function(path, m, logT = TRUE) {
  
  if (logT) {
    ssig <- pathsig.logsig(path, m)
  } else {
    ssig <- pathsig.esig(path, m)
  }
  ssig
}

#library(reticulate)

# Import the esig module
#esig <- import("esig")

# Define the path array
# few examples
path <- array(c(1, 1, 3, 10, 20, 30), c(3, 2))

# Define the truncation level
truncation_level <- as.integer(5)  # Set this as needed

# Call the stream2sig function
result <- esig$stream2sig(path, truncation_level)

# Print the result
print(result)

pathsig.logsig(path, 5)

path <- array(c(1,1,3,10,20,30),c(3,2))
esig$stream2sig(path, truncation_level)

path
pathsig.one(path, 2)

path1 <- array(c(1, 1, 2, 3, 5, 7, 8), c(1,7))
path1
pathsig.one(path1, 3)

path <- array(c(1,1,3,10,20,30),c(2,3))
path
pathsig.one(path, 3)

y <- mtcars$hp

x <- data.matrix(mtcars[, c("mpg", "wt", "drat", "qsec")])
x

library(glmnet)
library(ggplot2)

timejoined <- function(X) {
  # Appends the last element of X to the list
  # corrigir <- "ordem transposta "
    X <- rbind(X, X[nrow(X), ])
  l <- list()
  
  for (j in seq(1, 2 * nrow(X) + 1 + 2)) {
    if (j == 1) {
      l <- rbind(l, c(X[1, 1], 0))
      next
    }
    for (i in seq(1, nrow(X) - 1)) {
      if (j == 2 * i) {
        l <- rbind(l, X[i, ])
        break
      }
      if (j == 2 * i + 1) {
        l <- rbind(l, c(X[i + 1, 1], X[i, 2]))
        break
      }
    }
  }
  l <- as.data.frame(l)
  colnames(l) <- c("time", "value")
  return(l)
}

leadlag2 <- function(X) {
  
  x1 <- X[,1]
  y1 <- X[,2]
  
  r1 <- c(x1[1], rep(x1[-1], each=2))
  r2 <- c(rep(head(y1, -1), each =2), y1[length(y1)])
  
  array(c(r1, r2), dim=c(length(r1),2))
}


leadlag <- function(X) {


  #X = array(c(5, 3, 6, 7, 8, 9, 3, 2, 9, 1, 0, 3, 4, 5, 6, 1), dim=c(4,3))
  
  dim1 = dim(X)[1]
  dim2 = dim(X)[2]
  
  rlag <- rbind(X[-dim1,]  %x% c(1,1), X[dim1,])
  rlag
  
  rlead <- rbind(X[1,], X[-1,]  %x% c(1,1))
  rlead
  
  a <- cbind(rlead, rlag)
  
  #  r1 <- c(x1[1], rep(x1[-1], each=2))
#  r2 <- c(rep(head(y1, -1), each =2), y1[length(y1)])
  
  #array(c(r1, r2), dim=c(length(r1),2))
  a
}



Xpath <- matrix(c(2,3,4,5,5,6), ncol=2)
Xpath

leadlag(Xpath)

tj <- timejoined(Xpath)
tj
str(tj)
as.matrix(tj)

plot(Xpath[,1], Xpath[,2])
plot(tj[,1], tj[,2])

cv.model <- cv.glmnet(x, y, alpha=1)

best_lambda <- cv.model$lambda.min
best_lambda

plot(cv.model)

best_model <- glmnet(x,y, alpha = 1, lambda = best_lambda)
coef(best_model)


new = matrix(c(24, 2.5, 3.5, 18.5), nrow = 1, ncol = 4)

predict(best_model, s = best_lambda, newx = new)

y_predicted <- predict(best_model, s = best_lambda, newx = x)

sst <- sum((y-mean(y))^2)
sse <- sum((y_predicted-y)^2)

rsq <- 1-sse/sst

rsq

####

path <- array(c(1, 1, 3, 10, 20, 30, 5, 7, 
                54, 33, 98, 11, 23, 55, 4, 
                7, 6, 1, 2, 3, 2),c(3,7))
path
xx <- pathsig.one(path, 2)
str(xx)
xx

# to apply signatures in a list
pathsig <- function(L, Ltype, m) {

  lL <- length(L)
  if (Ltype =="list") {
    X <- sapply(1:lL, function(x) { pathsig.one(L[[x]], m)}) 
  }
  else {
    X <- sapply(1:lL, function(x) { pathsig.one(L[,x], m)}) 
  }
  x <- t(data.matrix(X))
  x
}

# normalize values
doNorm <- function(L) {
  
  Ll <- length(L)
  
  Ls <- do.call(rbind, L)
  
  mmean <- apply(Ls, 2, mean)
  msd <- apply(Ls, 2, sd)
  
  mlist = lapply(1:Ll, function(x) {(L[[x]] - mmean)/msd})
#  Lsum <- lapply(1:Ll, function(x) {apply(L[[x]], 2, sum)})
#  Ltot <- do.call(rbind, lapply(1:Ll, function(x) {rep(dim(L[[x]])[1], dim(L[[x]])[2])}))
#  dfsum <- do.call(rbind, Lsum)
#  mean <- apply(dfsum, 2, sum)/apply(Ltot, 2, sum)
  list(mlist = mlist, mmean = mmean, sd = msd)
}

lasso.pathsig.util <- function(L, y, m, Ltype = "list", family = "gaussian", norm = TRUE) {
  
  x <- pathsig(L, Ltype, m)
  
  if (norm) {
  meanv = apply(x,2,mean)
  sdv = apply(x,2,sd)
  } else {
    meanv = 0
    sdv = 1
  }
  
  (x-apply(x,2,mean))/apply(x,2,sd)
  df1 = data.frame((x-meanv)/sdv, y=y)
  
  #  df1 %>%
 #   ggplot(aes(x=X5)) + geom_histogram()
  
  interface = "NEW"
  
  if (interface != "OLD") {
    cv.model <- glmnetUtils::cv.glmnet(y ~ ., alpha=1, family = family, data = df1)
   # cv.model <- cv.glmnet(x, y, alpha=1, family = "multinomial", type.multinomial = "grouped")
  } else {
    cv.model <- cv.glmnet(x, y, alpha=1)
  }
  best_lambda <- cv.model$lambda.min
  best_lambda
  
  # plot(cv.model)
  
  if (interface != "OLD") {
    best_model = glmnetUtils::glmnet(y ~ ., family = family, data=df1, alpha =1)
    #best_model <- glmnet(x,y, alpha = 1, lambda = best_lambda, 
    #                     family = family,
    #                    type.multinomial = "grouped")
  } else {
    best_model <- glmnet(x,y, alpha = 1, lambda = best_lambda, 
                         family = family)
  }
  coef = coef(best_model)
  
  list(lambda = best_lambda, coef = coef, model = best_model, 
       sigdegree = m,
       cvmodel = cv.model, sig = x, meanv = meanv, sdv = sdv)
}


lasso.pathsig.bayesian <- function(L, y, m, Ltype = "list", family = "gaussian", norm = TRUE) {
  
  x <- pathsig(L, Ltype, m)
  
  if (norm) {
    meanv = apply(x,2,mean)
    sdv = apply(x,2,sd)
  } else {
    meanv = 0
    sdv = 1
  }
  
  (x-apply(x,2,mean))/apply(x,2,sd)
  df1 = data.frame((x-meanv)/sdv, y=y)
  
  #  df1 %>%
  #   ggplot(aes(x=X5)) + geom_histogram()
  
  interface = "NEW"
  
  out <- rstanarm::stan_glm(y ~., data =df1, family=family, prior = lasso(), 
                            chains = 3, iter=1000)#, prior = lasso)

  best_lambda = 1
  coef = 1
  
  list(lambda = best_lambda, coef = coef, model = out, 
       cvmodel = out, sig = x, meanv = meanv, sdv = sdv)
}



lasso.pathsig <- function(L, y, m, Ltype = "list", family = "gaussian") {
  
  x <- pathsig(L, Ltype, m)

  if (family=="multinomial") {
    cv.model <- cv.glmnet(x, y, alpha=1, type.multinomial = "grouped")
  } else {
  cv.model <- cv.glmnet(x, y, alpha=1)
  }
  best_lambda <- cv.model$lambda.min
  best_lambda
  
  # plot(cv.model)
  
  if (family == "multinomial") {
    best_model <- glmnet(x,y, alpha = 1, lambda = best_lambda, 
                         family = family,
                         type.multinomial = "grouped")
  } else {
  best_model <- glmnet(x,y, alpha = 1, lambda = best_lambda, 
                       family = family)
  }
  coef = coef(best_model)
  
  list(lambda = best_lambda, coef = coef, model = best_model, cvmodel = cv.model, sig = x)
}

path <- array(c(1, 1, 3, 10, 20, 30, 5, 7, 
                54, 33, 98, 11, 23, 55, 4, 
                7, 6, 1, 2, 3, 2),c(3,7))
path
pathsig.one(path, 2)

L <- list(path = path)
L <- replicate(32, array(runif(21), c(3,7)))
L[[1]]

L <- lapply(1:32, function(x) {array(runif(21), c(3,7))})

y <- 3*runif(32)
m<- 2
outm <- lasso.pathsig(L,y, m)
outm

#L <- replicate(32, array(runif(21), c(3,7)))
#pathLasso(L, y, m, Ltype = "array")


predict.pathsig <- function(object, new, s=NULL, Ltype = "list", type="response",
                            conf = FALSE) {

  model <- object$cvmodel
  m <- object$sigdegree
  
  x <- pathsig(new, Ltype = Ltype, m=m)
  df1 <- data.frame((x-object$meanv)/object$sdv)
  if (is.null(s)) {
    if (type =="class") {
    y_predicted <- predict(model, 
                                     s= "lambda.min",
                           newdata = df1,
                                  #s = s, 
                                  # newx = x, 
                           type=type)
    } else {
      y_predicted <- predict(model, 
                             s= "lambda.min",
                             newdata = df1,
                             #s = s, 
                             # newx = x, 
                             type=type)
      y_predicted_conf <- predict(model, 
                             s= "lambda.min",
                             newdata = df1,
                             #s = s, 
                             se.fit = TRUE,
#                             interval = "confidence",
                             # newx = x, 
                             type=type)
      
    }
  } else {
  y_predicted <- predict(model, 
          s = s, 
          newdata = df1, type=type)
  }
  #y_predicted <- predict(best_model, s = best_lambda, newx = x)
  
  if (conf) {
   list(pred = y_predicted, conf = y_predicted_conf)
  } else {
  y_predicted
  }
}

predict.pathsig.bayes <- function(object, new, m, s=NULL, Ltype = "list", type="response") {
  
  model <- object$cvmodel
  
  x <- pathsig(new, Ltype = Ltype, m=m)
  df1 <- data.frame((x-object$meanv)/object$sdv)
  if (is.null(s)) {
    y_predicted <- posterior_predict(model, 
                          # s= "lambda.1se",
                           newdata = df1,
                           #s = s, 
                           # newx = x, 
                           type=type)
  } else {
    y_predicted <- predict(model, 
                           s = s, 
                           newx = x, type=type)
  }
  #y_predicted <- predict(best_model, s = best_lambda, newx = x)
  
  y_predicted
}


X <- sapply(1:32, function(x) { pathsig.one(L[[x]], m)}) 
x <- t(data.matrix(X))

#predict.pathsig(outm, s=outm$lambda, new = L)

# list had paths of different lengths
L <- lapply(1:32, function(x) {if (x!=3) {array(runif(21), c(7,3))} 
  else {array(runif(15), c(5,3))}
  })
L

outm <- lasso.pathsig.util(L,y, m=3)
outm$sig
#predict.pathsig(outm, s=outm$lambda, new = L)


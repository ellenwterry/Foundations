library(pracma)
library(tidyverse)

# this removes the function and walks through the code lines so that you can see how the 
# columns and rows are transformed to get the diagonal matrix





# ------------------- two variable model ----------------------- #


mydata <- read.csv(file="C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Foundations/Ex1LS2.csv", header=TRUE, sep=",")
model <- lm( Y ~ ., mydata)
model$coefficients

X <- mydata[1:2]
X <- mydata[1:2]
X <- as.matrix(X)
X <- as.matrix(cbind(1, X))
X
y <- mydata$Y
y



#------ we won't use the function but it's easier to trace variables 

  nr <- length(y)
  nc <- NCOL(X)
  
  for (j in seq_len(nc))
  {
    print(paste0("beg loop j =  ", j))
    id <- seq.int(j, nr) 
    sigma <- sum(X[id,j]^2) 
    s <- sqrt(sigma) 
    diag_ej <- X[j,j] 
    gamma <- 1.0 / (sigma + abs(s * diag_ej)) 
    kappa <- if (diag_ej < 0) s else -s 
    X[j,j] <- X[j,j] - kappa
    print("before sub-loop X:")
    print(X)
    if (j < nc)
      for (k in seq.int(j+1, nc))
      {
        yPrime <- sum(X[id,j] * X[id,k]) * gamma
        X[id,k] <- X[id,k] - X[id,j] * yPrime
        print(paste0("Sub-Loop k =  ", k))
        print("Sub-Loop X:")
        print(X)
      }
    print("End Sub-Loop X:")
    print(X)
    yPrime <- sum(X[id,j] * y[id]) * gamma
    y[id] <- y[id] - X[id,j] * yPrime # change of y
    X[j,j] <- kappa
    print("End-Loop X:")
    print(X)
    print("End-Loop y:")
    print(y)
  } # end Householder
  
  # residual sum of squares
  rss <- sum(y[seq.int(nc+1, nr)]^2)
  
  # Backsolve
  beta <- rep(NA, nc)
  print("Backsolve X:")
  print(X)
  for (j in seq.int(nc, 1))
  {
    beta[j] <- y[j]
    if (j < nc)
      for (i in seq.int(j+1, nc))
        beta[j] <- beta[j] - X[j,i] * beta[i]
      beta[j] <- beta[j] / X[j,j]
      print(j)
      print(beta)
      print(X)
  }
  
  # set zeros in the lower triangular side of X (which stores) 
  # not really necessary, this is just to return R for illustration
  print("Before Cleanup X:")
  print(X)
  for (i in seq_len(ncol(X)))
    X[seq.int(i+1, nr),i] <- 0
  list(R=X[1:nc,1:nc], y=y, beta=beta, rss=rss)


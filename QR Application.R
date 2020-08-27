suppressMessages(library(pracma))
suppressMessages(library(tidyverse))

QR.regression <- function(y, X)
{
  nr <- length(y)
  nc <- NCOL(X)
  
  for (j in seq_len(nc))
  {
    id <- seq.int(j, nr) 
    sigma <- sum(X[id,j]^2) 
    s <- sqrt(sigma) 
    diag_ej <- X[j,j] 
    gamma <- 1.0 / (sigma + abs(s * diag_ej)) 
    kappa <- if (diag_ej < 0) s else -s 
    X[j,j] <- X[j,j] - kappa
    if (j < nc)
      for (k in seq.int(j+1, nc))
      {
        yPrime <- sum(X[id,j] * X[id,k]) * gamma
        X[id,k] <- X[id,k] - X[id,j] * yPrime
      }
    yPrime <- sum(X[id,j] * y[id]) * gamma
    y[id] <- y[id] - X[id,j] * yPrime
    X[j,j] <- kappa
  } # end Householder
  
  # residual sum of squares
  rss <- sum(y[seq.int(nc+1, nr)]^2)
  
  # Backsolve
  beta <- rep(NA, nc)
  for (j in seq.int(nc, 1))
  {
    beta[j] <- y[j]
    if (j < nc)
      for (i in seq.int(j+1, nc))
        beta[j] <- beta[j] - X[j,i] * beta[i]
      beta[j] <- beta[j] / X[j,j]
  }
  
  # set zeros in the lower triangular side of X (which stores) 
  # not really necessary, this is just to return R for illustration
  for (i in seq_len(ncol(X)))
    X[seq.int(i+1, nr),i] <- 0
  list(R=X[1:nc,1:nc], y=y, beta=beta, rss=rss)
}


# --------- 1 variable model -------------------------------#


mydata <- read.csv(file="C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Foundations/Ex1LS2.csv", header=TRUE, sep=",")
model <- lm( Y ~ X1 ,mydata)
model$coefficients

p <- ggplot(data = mydata, aes(x= X1, y= Y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
p

Y <- mydata$Y
X <- mydata$X1
X <- as.matrix(cbind(1, X))

res <- QR.regression(Y, X)

res$beta

b_1 = res$y[2]/res$R[2,2]
b_0 = (res$y[1] - res$R[1,2]*b_1)/res$R[1,1]

array(c(b_0, b_1))


# ----------------- 2 variable model ---------------------- #

mydata <- read.csv(file="C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Foundations/Ex1LS2.csv", header=TRUE, sep=",")

model <- lm( Y ~ ., mydata)
model$coefficients


X <- mydata[1:2]

p <- ggplot(data = mydata, aes(x= X2, y= Y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) 
p

Y <- mydata$Y
X <- mydata[1:2]
X <- as.matrix(X)
X <- as.matrix(cbind(1, X))

res <- QR.regression(Y, X)
res$beta
res$R
res$y


house <- householder(X)
Q2 <- round(as.matrix(house$Q),5)
R2 <- round(as.matrix(house$R),5)

b_0 = res$y[3]/res$R[3,3]
b_1 = (res$y[2] - (res$R[2,3]*b_0))/res$R[2,2]
b_2 = (res$y[1] - (res$R[1,3]*b_0) - (res$R[1,2]*b_1))/res$R[1,1]

res$beta
array(c(b_2, b_1, b_0))


# ------------------ auto regression ---------------#

Autos <- read.csv(file="C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Foundations/Automobile Price Prediction.csv")
Autos <- select(Autos, wheel.base, engine.size, horsepower, highway.mpg, price )

model <- lm( price ~ ., Autos)

model$coefficients

p <- ggplot(data = Autos, aes(x= horsepower, y= price)) + geom_point() + geom_smooth(method = 'lm', se = FALSE) 
p

Y <- Autos$price
X <- Autos[1:4]
X <- as.matrix(X)
X <- as.matrix(cbind(1, X))

res <- QR.regression(Y, X)
res$beta

R <- res$R
R
  


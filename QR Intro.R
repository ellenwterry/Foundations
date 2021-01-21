library(pracma)
library(tidyverse)

A = matrix(c(3, 0, 4, -2, 3, 4, 3, 5, 4), ncol = 3, nrow = 3)
A
QR <- qr(A) 
R = qr.R(QR) 
R
Q = qr.Q(QR)
Q
Q %*% R


R2 = R[1:2,1:2]
C = R[1:2, 3]
betahat <- backsolve(R2, C)
betahat


# === example 2

setwd("C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Foundations")

mydata <- read.csv(file="Ex1LS.csv", header=TRUE, sep=",")
model <- lm( Y ~ X ,mydata)
model$coefficients

p <- ggplot(data = mydata, aes(x= X, y= Y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
p

Y <- mydata$Y
X <- mydata$X
X <- as.matrix(cbind(1, X))
# or
#
X = model.matrix(Y ~ X, mydata)

A = cbind(X, Y)

QR <- qr(A) 
R = qr.R(QR) 
R2 = R[1:2,1:2]
R2

C = R[1:2, 3]
betahat <- backsolve(R2, C)
betahat



mydata2 = read.csv(file="C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Foundations/Ex1LS2.csv", header=TRUE, sep=",")
model2 = lm( Y ~ ., mydata2)
model2$coefficients

p <- ggplot(data = mydata2, aes(x= X2, y= Y)) + geom_point() + geom_smooth(method = 'lm', se = FALSE)
p


Y <- mydata2$Y
X2 <- select(mydata2, X1, X2)
X2 <- as.matrix(cbind(1, X2))

A = cbind(X2, Y)

QR <- qr(A) 

R = qr.R(QR) 
R2 = R[1:3,1:3]
R2
C = R[1:3, 4]
betahat <- backsolve(R2, C)
betahat


Q = qr.Q(QR)
R = qr.R(QR)
Q %*% R


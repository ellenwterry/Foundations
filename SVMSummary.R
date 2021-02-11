library(tidyverse)

setwd("C:/Users/ellen/Documents/UH/Spring 2019/DA2/Section 1/SVM/Data")

#the hyperplane
tst <- read.csv(file="Optim.csv", header=TRUE, sep=",")
tst$Color[tst$O==1]="red"
tst$Color[tst$O==-1]="black"
p <- ggplot(tst, aes(x=X, y=Y))+geom_point(color=tst$Color) + xlim(-10,10) + ylim(-10,10)
p <- p + geom_abline(intercept = 0, slope = -2, col="blue")
p <- p + geom_hline(yintercept=0) 
p <- p + geom_vline(xintercept=0) 
p <- p + geom_segment(aes(x = 0, y = 0, xend = 2, yend =1 ), color="black", arrow = arrow(length = unit(0.5, "cm")))
p <- p +xlim(-10, 10) + ylim(-10,10)
p

# equation (y=-2x)=0 when x=2 and y=1 
V9 <- data.frame(x=c(0, 2), y=c(0, 1))
#pick a point and extend a vector
V10 <- data.frame(x=c(0, 2), y=c(0,6))
p <- p + geom_segment(aes(x = V10[1,1], y = V10[1,2], xend = V10[2,1], yend =V10[2,2] ), color="black", arrow = arrow(length = unit(0.5, "cm")))
p

#get magnitude of vector
mv10 <- sqrt(((V10[2,1])^2)+((V10[2,2]^2)))
mv9 <- sqrt(((V9[2,1])^2)+((V9[2,2]^2)))

#get dot product
dpB <- (V10[2,1]*V9[2,1])+(V10[2,2]*V9[2,2]) 

#project vector onto othogonal vector
V11<-data.frame(X=c(0, dpB/(mv9^2)*V9[2,1]),Y=c(0, dpB/(mv9^2)*V9[2,2]))
p <- p + geom_segment(aes(x = V11[1,1], y = V11[1,2], xend = V11[2,1], yend =V11[2,2] ), color="red", linetype="dashed", arrow = arrow(length = unit(0.5, "cm")))
p
mv11 <- sqrt(((V11[2,1])^2)+((V11[2,2]^2)))
# (y1-y2)=m(x1-x2) => -2x=.5x+5
p <- p + geom_segment(aes(x =-2 , y = 4, xend = 2, yend =6), color="red", linetype="dashed", arrow = arrow(length = unit(0.5, "cm")))
p <- p + annotate("text", x = 2, y = -2, label = "y = -2x")
p

#Now, lets draw out some parallel lines 
p <- p + geom_abline(intercept = 10, slope = -2, col="blue", linetype="dashed")
p <- p + geom_abline(intercept = -4, slope = -2, col="blue", linetype="dashed")
p
p <- p + annotate("text", x = 3, y = -4, label = "wx-b=0")
p <- p + annotate("text", x = 8, y = -4, label = "wx-b=1")
p <- p + annotate("text", x = -1, y = -4, label = "wx-b=-1")
p

# start over using svm

library(kernlab)
library(tidyverse)

setwd("C:/Users/ellen/Documents/UH/Spring 2019/DA2/Section 1/SVM/Data")

tst <- read.csv(file="Optim.csv", header=TRUE, sep=",")

tst$Color[tst$O==1]="red"
tst$Color[tst$O==-1]="black"
p <- ggplot(tst, aes(x=X, y=Y))+geom_point(color=tst$Color) + xlim(-10,10) + ylim(-10,10)
p <- p + geom_abline(intercept = 0, slope = -2, col="blue")
p <- p + geom_hline(yintercept=0) 
p <- p + geom_vline(xintercept=0) 
p

mTst <- as.matrix(tst[,1:2])
yTst <- as.matrix(tst[,3])

t1 <- ksvm(mTst, yTst, type="C-svc", C=1000, kernel=vanilladot(),scaled=c())

# Extract w and b from the model   
w <- colSums(coef(t1)[[1]] * mTst[SVindex(t1),])
w2 <- colSums(coef(t1)[[1]] * mTst[unlist(alphaindex(t1)),])
b <- b(t1)

p <- p + geom_abline(intercept = b/w[2], slope = -w[1]/w[2], col="red")
p <- p + geom_abline(intercept = (b+1)/w[2], slope = -w[1]/w[2], col="red", linetype="dashed")
p <- p + geom_abline(intercept = (b-1)/w[2], slope = -w[1]/w[2], col="red", linetype="dashed")
p

#non linear

kl <- kernelMatrix(vanilladot(), mTrain)
dim(kl)
kl
dfKl <- data.frame(kl)


# Now lets create a non-linear kernel
# this is a custom kernel function

rbf <- function(x,y) exp(-0.1 * sum((x-y)^2))
class(rbf) <- "kernel"
mTst <- as.matrix(tst[,1:2])
yTst <- as.matrix(tst[,3])

k2 <- kernelMatrix(rbf, mTst)

lookK2 = data.frame(k2)

dim(k2)
dfK2 <- data.frame(k2)

X <- k2[, c(1,2)]
X <- apply(X, 2, as.numeric)
Y <- as.integer(k2[,3])


# model using the rbf kernel   

t2 <- ksvm(mTst, yTst, type="C-svc", C=100, kernel=rbf, scale=c())

plot(t2, data=mTst)

# notice the shape of the margin regions now

result2 <- data.frame(predict(t2, mTrain))
result2 <- cbind(result2, tst$O)
result2$diff <- result2[,1]-result2[,2]
CrudeResult2 <-  round(nrow(result[result2$diff == 0, ])/nrow(result2),2)
CrudeResult2






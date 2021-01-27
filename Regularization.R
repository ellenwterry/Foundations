library(tidyverse)

# this function calculates rmse from a vector of (y - y_hat) values:

rmse <- function(error)
{
  sqrt(mean(error^2))
}


mydata <- read.csv(file="C:/Users/ellen/Documents/UH/Fall 2020/Data/Ex1LS.csv", header=TRUE, sep=",")

model <- lm( formula = Y ~ X, mydata)
modelQ <- lm( formula = Y ~ X + I(X^2), mydata)

modData <- mydata

modData$newYQ <- predict(modelQ, mydata)

modData$newY <- predict(model, mydata)

p <- ggplot(modData, aes(x=X, y=Y))+geom_point()  
p <- p + geom_point(data = modData, aes(x=X, y = newY), color = 'red')
p <- p + geom_smooth(data=modData, aes(X, newY), se=FALSE, color = "red", span = 1.5)
p <- p + geom_smooth(data=modData, aes(X, newYQ), se=FALSE, color = "blue", span = 1.5)
p

x <- mydata$X
y <- mydata$Y
d <- data.frame(x=x,y=y)

# using the quad function above for linear equation 
# and solving using normal equations (go back to regression 1)

m = length(mydata$X) 
x = matrix(c(rep(1,m), mydata$X, mydata$X^2), ncol=3)
n = ncol(x)
y = matrix(mydata$Y, ncol=1)


# set up 3 different levels of regularization term values 0, 1 and 10
# you don't have to do it this way - you can simplify and just try values one at at time


lambda = c(0,1,10)
d = diag(1,n,n)
d[1,1] = 0 
# set reg term to 0 for intercept
# remember we set the intercept = 1 and backsolve
th = array(0,c(n,length(lambda)))

for (i in 1:length(lambda)) {
  th[,i] = solve(t(x) %*% x + (lambda[i] * d)) %*% (t(x) %*% y)
  print(i)
}
# I'm just using a loop to go through a range of values to see how it goes
# let's break this down 
# recall normal equations:

betaHat <- solve(t(x)%*%x) %*% t(x) %*%y
# compare to lm
modelQ
as.numeric(betaHat)

# but now, betaHat, not penalized is:

lambda[1]*d
# and
betaHat <- solve(t(x) %*% x + (lambda[1] * d)) %*% (t(x) %*% y)
# so no change, which is what we would expect.
# now for reg term = 1
betaHat <- solve(t(x) %*% x + (lambda[2] * d)) %*% (t(x) %*% y)
# now for reg term = 10
betaHat <- solve(t(x) %*% x + (lambda[3] * d)) %*% (t(x) %*% y)

# if you wanted to just plug in your own value, say 5:
betaHat <- solve(t(x) %*% x + (5 * d)) %*% (t(x) %*% y)
# you still need the diagonal to multiply by x, which has 3 columns
# also remember that this is element multiplication, not matrix (dot product)




# ------------ create seq for smooth visual --------------- #

# generate x sequence (just for visual)
nwx = seq(1, 4, len=50)

# extend x to include poly term
x = matrix(c(rep(1,length(nwx)), nwx, nwx^2), ncol=3)

# multiply by 3 different beta vectors
newData <- as.data.frame(nwx)
newData$th1 <- (x %*% th[,1])
newData$th2 <- (x %*% th[,2])
newData$th3 <- (x %*% th[,3])

# notice how the thetas are reduced in magnitude

X <- newData$nwx

p <- ggplot(mydata, aes(x=X, y=Y))+geom_point() + geom_smooth(method = 'lm', se=FALSE, color = 'black') 
p <- p+ geom_smooth(data = newData, aes(x=nwx, y = th1), color = 'blue')
p <- p+ geom_smooth(data = newData, aes(x=nwx, y = th2), color = 'red')
p <- p+ geom_smooth(data = newData, aes(x=nwx, y = th3), color = 'green')
p


# ----------- end of visual sequence ----------------- #

# now let's check the errors

x = matrix(c(rep(1, nrow(mydata)), mydata$X, mydata$X^2), ncol = 3)

# OR (you'll want to use this where possible)

x = model.matrix(Y ~ X, mydata)
xq = model.matrix(Y ~ X + I(X^2), mydata)

# recompute unpenalized betas #

betaHat <- solve(t(x)%*%x) %*% t(x) %*%y
betaHatQ <- solve(t(xq)%*%xq) %*% t(xq) %*%y

# and compute errors

# just for comparison

rmse((x %*% betaHat)- mydata$Y)  

# just for comparison, here's how lm calucalates rmse:

k=length(model$coefficients)-1 #Subtract one to ignore intercept
SSE=sum(model$residuals^2)
n=length(model$residuals)
sqrt(SSE/(n-(1+k))) 

# the problem here is that there's no data and df = 1, so huge variance in methods
# we'll get into all this later

# for now, let's compare the quadratic models

# --- blue (using both theta sources)
rmse((xq %*% betaHatQ)- mydata$Y)  
rmse((xq %*% th[,1])- mydata$Y)  
# --- red
rmse((xq %*% th[,2])- mydata$Y)  
# --- green
rmse((xq %*% th[,3])- mydata$Y)  

# so the blue has the lowest error


# so now let's create some new data:

newData = data.frame(X = c(1, 2, 3, 4), Y = c(5, 6, 6.5, 9))
p = p + geom_point(data = newData, aes(X, Y), color = "red", size = 3)  
p

# calculate error

xq2 = model.matrix(Y ~ X + I(X^2), newData)

# --- blue (using both theta sources)
rmse((xq2 %*% betaHatQ)- newData$Y)  
rmse((xq2 %*% th[,1])- newData$Y)  
# --- red
rmse((xq2 %*% th[,2])- newData$Y)  
# --- green
rmse((xq2 %*% th[,3])- newData$Y)  

# and now the lowest error is green. WHY?

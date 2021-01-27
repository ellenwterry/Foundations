suppressMessages(library(tidyverse))

rmse = function(error)
{
  sqrt(mean(error^2))
}

#setwd("/home/ellen/Documents/Spring2020/DA2/Section I/Introduction/data")
setwd("C:/Users/ellen/OneDrive/Documents/GitHub/EllenwTerry/Foundations")


mydata <- read.csv(file="Ex1LS.csv", header=TRUE, sep=",")


model <- lm( formula = Y ~ X, mydata)
modelQ <- lm( formula = Y ~ X + I(X^2), mydata)

modData <- mydata
modData$newY <- predict(model, mydata)
modData$newYQ <- predict(modelQ, mydata)

rmse(modData$newY - modData$Y)
rmse(modData$newYQ - modData$Y)

p <- ggplot(modData, aes(x=X, y=Y))+geom_point()  
p <- p + geom_smooth(data=modData, aes(X, newY), se=FALSE, color = "red", span = 1.5)
p <- p + geom_smooth(data=modData, aes(X, newYQ), se=FALSE, color = "blue", span = 1.5)
p


newData = data.frame(X = c(1, 2, 3, 4), Y = c(5, 6, 6.5, 9))
p = p + geom_point(data = newData, aes(X, Y), color = "red")  
p

newData$newY = predict(model, newData)
newData$newYQ = predict(modelQ, newData)

rmse(newData$newY - newData$Y)
rmse(newData$newYQ - newData$Y)

x <- mydata$X
y <- mydata$Y

m = length(mydata$X) 
x = matrix(c(rep(1,m), mydata$X, mydata$X^2), ncol=3)
n = ncol(x)
y = matrix(mydata$Y, ncol=1)

### --------------- Recall NE

# Normal Equations Solution - refer to Python file and Slides

vY <- as.matrix(dplyr::select(Advertising, Sales)) # set up y values in matrix                        
mX <- as.matrix(cbind(1, dplyr::select(Advertising, TV, Radio))) # set up x values in matrix
vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations                    
vBeta

str(Advertising)
# Predictions using normal equations

vBeta2 <- as.numeric(vBeta)
Advertising$neY <- t(vBeta2%*%t(mX)) # 3 columns on left * 3 rows on right (after transpose)


### ------------------------


d = diag(1,n,n) # create so we can multiply theta * X
d[1,1] = 0 # this leaves the intercept alone
th1= array(0, c(3, 1))

1*d

# from NE vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations 

# penalty: start with 1 and go to 5 and 10
penalty = 10

th1 = solve(t(x) %*% x + (penalty * d)) %*% (t(x) %*% y)

# just to check 
newData$th1 <- (x %*% th1)

p <- p + geom_smooth(data=newData, aes(X, th1), se=FALSE, color = "green", span = 1.5)
p

rmse(newData$newY - newData$Y)
rmse(newData$newYQ - newData$Y)
rmse(newData$th1 - newData$Y)

# lowest with 1
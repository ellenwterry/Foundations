library(tidyverse)

# Recall our search for parameters. OLS is actually pretty rare in that it has a closed form solution:
# i.e. its tractable and expressed in terms of elementary functions (the answer provided is unique and definite):

# normal equations

Advertising = read_csv("C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Foundations/Advertising.csv")


vY = Advertising$Sales
mX <- as.matrix(cbind(1, dplyr::select(Advertising, TV))) # set up x values in matrix
vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations                    
vBeta

vBeta <- as.numeric(vBeta)

Advertising$neY <- t(vBeta%*%t(mX)) # 3 columns on left * 3 rows on right (after transpose)

ggplot(Advertising, aes(x = TV, y = neY)) + geom_point()

# it is an optimization though, and can be estimated with optimization methods:

# linear likelihood function
linear.lik <- function(theta, y, X){
  n      <- nrow(X)
  k      <- ncol(X)
  beta   <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e      <- y - X%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) ) # log SS
  return(-logl)
}

# create some linear data

y = as.numeric(Advertising$Sales)
x = model.matrix(Sales ~ TV, data = Advertising)

linear.MLE <- optim(fn=linear.lik, par=c(1,1,1), lower = c(-Inf, -Inf, 1e-8), 
                    upper = c(Inf, Inf, Inf), hessian=TRUE, 
                    y=y, X=x, method = "L-BFGS-B")



linear.MLE$par[1]
vBeta[1]
linear.MLE$par[2]
vBeta[2]

# it doesn't always work out this close - actually, it's rare.
# note that this utilizes two concepts: maximum likelihood 
# in this case, the ML equation is least squares - vs. a density function
# derivative solutions: 

# Duplicating with LA

dfDefault <- Default

glm.fit <- glm(default ~ balance, data = dfDefault, family = binomial)
summary(glm.fit)

dfDefault$Prob <- predict(glm.fit, type = "response")

# glm uses ML

alpha <- glm.fit$coefficients[1]
beta <- glm.fit$coefficients[2]
tst1 = dfDefault$balance


dfDefault$tmProb <- exp(alpha[1] + t(beta%*%t(tst1)))/(1+exp(alpha[1] + t(beta%*%t(tst1))))

# looks like just as much effort, but it's not when you're working!!
p = 
  ggplot(dfDefault, aes(x=balance, y=Prob)) + 
  geom_point(color = "red")   
p

# Logistic Regression doesn't have a closed form solution

vY = as.numeric(dfDefault$default)-1
mX = model.matrix(default ~ balance, data = dfDefault)

logit = function(mX, vBeta) {
  return(exp(mX %*% vBeta)/(1+ exp(mX %*% vBeta)) )
}

# stable parametrisation of the log-likelihood function
# Note: The negative of the log-likelihood is being returned, since we will be
# /minimising/ the function.

logLikelihoodLogitStable = function(vBeta, mX, vY) {
  return(-sum(
    vY*(mX %*% vBeta - log(1+exp(mX %*% vBeta)))
    + (1-vY)*(-log(1 + exp(mX %*% vBeta)))
  ) 
  ) 
}


# initial set of parameters
vBeta0 = c((alpha - 1), 0)

# minimise the (negative) log-likelihood to get the logit fit
optimLogit = optim(vBeta0, logLikelihoodLogitStable,
                   mX = mX, vY = vY, method = 'BFGS', 
                   hessian=TRUE)


optimLogit$par[1]
alpha
optimLogit$par[2]
beta


dfDefault$mlProb <- exp(optimLogit$par[1] + t(optimLogit$par[2]%*%t(mX[,2])))/(1+exp(optimLogit$par[1] + t(optimLogit$par[2]%*%t(mX[,2]))))
# looks like just as much effort, but it's not when you're working!!
p = p + geom_point(data = dfDefault, aes(x = balance, y = mlProb), color = "blue") 
p





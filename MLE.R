library(tidyverse)
library(plotly)

# recall from above how a binomial distribution works

manBinom <- function(n, p, h)
{
  x <- factorial(n)/(factorial(h)*factorial(n-h))
  x*(p)^h*(1-p)^(n-h)
}

manBinom(10, .4, 2)
manBinom(10, .4, 4)
manBinom(10, .4, 6)

ggplot(data = data.frame(h = c(2, 4, 6), 
                         p = c(manBinom(10, .4, 2), manBinom(10, .4, 4), manBinom(10, .4, 6))))+
        geom_histogram(aes(x = h, y = p), stat = 'identity') 
  

# note that the top portion of the pdf above does not include p, 
# so, if we're only interested in finding p, we don't need it 

# the likelihood is not the density or the probability, but it is proportionate to those. 
# It is constructed to measure p  - what's the likelihood of a probability!!
# for example: .1, .4 and .9. It is derived from the binomial density function
# but the probabilty function above can be simplified if we hold k and n constant 

L = function(p,k,n) p^k*(1-p)^(n-k) 

L(.1,4,10)
L(.4,4,10)
L(.9,4,10)

# read logmodels2 in blackboard
# then go to the Probability and Statistics Cheatsheet 
# and get the equations for binomial and normal distributions 
# pmf for binomial is: p^k (1-p)^(n-k)
# therefore, the log of the pmf is: k*log(p) + (n-k)*log(1-p)

N = 10 
K = 4 

# The likelihood function 
L = function(p,k,n) p^k*(1-p)^(n-k) 
# The log-likelihood function 
l = function(p,k,n) k*log(p) + (n-k)*log(1-p) 



mu = seq(0,1,0.001) 

# Plotting the Likelihood function 
# bNomLikePlot <- dLogL2<-data.frame(mu, L(mu, K, N))

bNomLikePlot = data.frame(x = mu, y = L(mu, K, N))

p <- plot_ly (data = bNomLikePlot, x = ~ x, y = ~y, type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = list(title = "mu"), yaxis = list(title = "Likelihood"))
p

  
# Plotting the Log Likelihood function 

bNomLogLikePlot = data.frame(x = mu, y = l(mu, K, N))

p <- plot_ly (data = bNomLogLikePlot, x = ~x, y = ~y, type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = list(title = "mu"), yaxis = list(title = "Log Likelihood"))
p


# The optimization functions in R finds the minimum, not the maximum. We  
# therefore must create new functions that return the negavive likelihood and  
# log-likelihood, and then minimize these:  
# Minus likelihood:

mL = function(p,k,n) -p^k*(1-p)^(n-k) 

# minus log-likelihood: 

ml = function(p,k,n) -(k*log(p) + (n-k)*log(1-p)) 

# Using 'optimize' 
#  simpler version of optim for one parameter. 
#  we will use optim in the next exercise

mLO <- optimize(mL, interval = c(0,1), k=K, n=N) 
mlO <- optimize(ml, interval = c(0,1), k=K, n=N) 

mLO$minimum
mlO$minimum



# ------------normal distr 2 parameters

x <- sort(rnorm(1000, 10, 2))
df2 <- density(x)
x2 <- data.frame(x = df2$x, y = df2$y)

p <- plot_ly (x = ~x2$x, y = ~ x2$y, type = 'scatter', mode = 'lines') %>% 
  layout(xaxis = list(title = "mu"), yaxis = list(title = "Likelihood"))
p

testData = data.frame(x = x)
# testData


# ggplot(x2, aes(x,y)) + geom_line()

# capMu <- matrix( nrow = 0, ncol = 3)

NLL <- function(theta,data) { 
  mu = theta[1] 
  sigma = theta[2] 
  n = length(data)
  #  t <- n
  NLL = -(n/2)*log(2*pi) - (n/2)*log(sigma**2) 
  tmp = 0 
  for (i in 1:n) { 
    tmp = tmp + (data[i]-mu)**2 
  }
  NLL = NLL + -(1/(2*(sigma**2)))*tmp 
  #  capMu <<- rbind(capMu, c(NLL, mu, sigma))
  -NLL 
}

out = optim(par=c(9,1), fn=NLL, data=x) 
out$par



NLL(c(9,1), testData$x)


mu1 = seq(8, 12, length.out = 10)
sigma1 = seq(1, 3, length.out = 10)

minLL = matrix( nrow = 0, ncol = 3)
for (i in 1:length(mu1)) { 
  mu = mu1[i]
  for(j in 1:length(sigma1)){
    sigma = sigma1[j]
    ml = NLL(c(mu, sigma), x)
    minLL <<- rbind(minLL, c(mu, sigma, ml))
  }
}
dfSurface <- data.frame(minLL)


tst = matrix(dfSurface$X3, nrow = 10)
x <- mu1
y <- sigma1

plot_ly() %>% add_surface(x = ~x, y = ~y, z = ~tst) %>%
  layout(
    title = "Minimum Log Likelihood",
    scene = list(
      xaxis = list(title = "mu"),
      yaxis = list(title = "sigma"),
      zaxis = list(title = "ml")
    ))



# -------------- mle applied to regression -----------------#

# linear likelihood function
linear.lik <- function(theta, y, X){
  n      <- nrow(X)
  k      <- ncol(X)
  beta   <- theta[1:k]
  sigma2 <- theta[k+1]^2
  e      <- y - X%*%beta
  logl   <- -.5*n*log(2*pi)-.5*n*log(sigma2) - ( (t(e) %*% e)/ (2*sigma2) )
  return(-logl)
}

# create some linear data

data.x <- rnorm(n = 100, mean = 10, sd = 2)

a.true <- 3
b.true <- 8

true.y <- data.x * a.true + b.true

err.sd.true <- 1  # Set noise sd
noise <- rnorm(n = 100, mean = 0, sd = 2)  # Generate noise

data.y <- true.y + noise  # Add noise to true (latent) responses

data <- data.frame(cbind(x = data.x, y = data.y))

lmData <- data
mod <- lm(data = lmData, y ~ x)
summary(mod)

linear.MLE <- optim(fn=linear.lik, par=c(1,1,1), lower = c(-Inf, -Inf, 1e-8), 
                    upper = c(Inf, Inf, Inf), hessian=TRUE, 
                    y=data$y, X=cbind(1, data$x), method = "L-BFGS-B")



# Compare lm with MLE
linear.MLE$par[1]
mod$coefficients[1]
linear.MLE$par[2]
mod$coefficients[2]

plot_ly(data = data, x = ~ x) %>% 
  add_markers(y = ~y) %>% 
  add_lines(x = ~x, y = fitted(mod))


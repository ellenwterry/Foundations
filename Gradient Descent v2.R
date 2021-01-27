library(tidyverse)
library(plotly)


Intercept <- 3.5
b_1 <- 1.4
X = 3
y = Intercept + (b_1 * X)

# We're holding X  and Intercept constant and working with just b_1 (slope)

# squared error cost function - Cost2 forces an intercept
cost2 <- function(X, y, theta) {
  sum ((((X %*% theta)+Intercept) - y)^2 ) / (2*length(y))
}


# set up the cost function

thetaV <- seq(0,3, by=.1)

FunchartPts <- NULL
for(i in 1:length(thetaV)){
  rwt <- cost2(X, y, thetaV[i])
  FunchartPts <- rbind(FunchartPts, c(thetaV[i], rwt)) 
}

dfFunChartPts <- data.frame(FunchartPts)
colnames(dfFunChartPts)[1] <- "Theta2"
colnames(dfFunChartPts)[2] <- "Cost"

p = plot_ly(dfFunChartPts, x = ~Theta2, y = ~Cost, type = 'scatter', mode = 'lines+markers', name = 'function') 
p

# just for reference in case plotly not installed

#p = ggplot(dfFunChartPts, aes(x = Theta2, y = Cost)) +
#  geom_point(color = "blue") +
#  geom_line(color = "blue") +
#  theme(panel.background = element_rect(fill = "white")) 
#p           


# set up the gradient descent


# now increase the threshold
alpha <- 0.2
# start with .001, increase to .01 and .1 and finally, .2

# holding alpha at .2
epsilon <- 1
# start with .001, increase to .1 and 1 and finally, 2


num_iters <- 1000
theta <- 0
chartPts <- NULL

tracerMat <- matrix(numeric(0), nrow = num_iters, ncol = 6)
colnames(tracerMat) <- c("i", "error", "delta", "theta", "cost", "sqrt(sum(delta^2))")
for (i in 1:num_iters) {
  error <- ((X %*% theta)+Intercept - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - (alpha * delta)
  cost <- cost2(X, y, theta)
  chartPts <- rbind(chartPts, c(theta, cost)) 
  if ((sqrt(sum(delta^2))) < epsilon) {break} 
  tracerMat[i,] <- c(i, error, delta, theta, cost, sqrt(sum(delta^2)))
}

dfChartPts <- data.frame(chartPts)
colnames(dfChartPts)[1] <- "Theta2"
colnames(dfChartPts)[2] <- "Cost"

round(theta,1)

p2 = p %>%
  add_trace(data = dfChartPts, y = ~Cost, mode = 'markers', name = 'GD pts')
p2

#p = p + geom_point(data = dfChartPts, aes(y = Cost), color = "red") 
#p



library(tidyverse)
library(plotly)

n = 10000
genSample = function()
{
  track = matrix( nrow = n+1, ncol = 3)
  for (i in 1:n) { 
    track[i, 1] = i
    track[i, 2] = rnorm(1, 10, 2)
    track[i, 3] = mean(track[,2], na.rm =T)
  }
  return(track)
}


p = plot_ly(data = data.frame(genSample()), x = ~X1, y = ~X3, type = 'scatter', mode = 'lines', name = 'sample 1') %>%
  add_trace(data = data.frame(genSample()), y = ~X3, mode = 'lines', name = 'sample 2') %>%
  add_trace(data = data.frame(genSample()), y = ~X3, mode = 'lines', name = 'sample 3')
p


data = rnorm(1000, mean = 10, sd = 2)

# NLL from maximum likelihood

NLL <- function(theta,data) { 
  mu = theta[1] 
  sigma = theta[2] 
  n = length(data)
  NLL = -(n/2)*log(2*pi) - (n/2)*log(sigma**2) 
  tmp = 0 
  for (i in 1:n) { 
    tmp = tmp + (data[i]-mu)**2 
  }
  NLL = NLL + -(1/(2*(sigma**2)))*tmp 
  -NLL 
}

proposalfunction <- function(param, t){
  return(runif(1,(param-t), (param+t)))   
}

run_metropolis_MCMC <- function(startvalue, iterations, t){
  chain = array(dim = c(iterations+1,2))
  chain[1,2] = startvalue
  sigma = 2 # isolate the mean for demo purposes
  for (i in 1:iterations){
    chain[i, 1] = i
    proposal = proposalfunction(chain[i,2], t)
    if (NLL(c(chain[i,2],sigma), data) < (NLL(c(proposal,sigma), data))){
      chain[i+1,2] = chain[i,2]
    }else{
      chain[i+1,2] = proposal
    }
  }
  return(chain)
}

iterations = 100
chain = run_metropolis_MCMC(9, iterations, 1)


p = plot_ly(data = data.frame(run_metropolis_MCMC(6, 50, 1)), x = ~X1, y = ~X2, type = 'scatter', mode = 'lines', name = 'chain 1') %>%
  add_trace(data = data.frame(run_metropolis_MCMC(12, 50, 1)), y = ~X2, mode = 'lines', name = 'chain 2') %>%
  add_trace(data = data.frame(run_metropolis_MCMC(20, 50, 1)), y = ~X2, mode = 'lines', name = 'chain 3')
p

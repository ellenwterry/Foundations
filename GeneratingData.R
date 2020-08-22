library(tidyverse)

N = 50
Beta = 2

Data = data.frame(X = sample(seq(from = 10, to =40, by = 1), N, replace = TRUE))
Data = Data %>% mutate(Y =  (X*Beta))

p = ggplot (aes(x = X, y = Y), data = Data) + 
  geom_point()
p

# now add normal randomness

Data = Data %>% mutate(Y =  (X*Beta) + rnorm(Data$Y,  0 , 10))

p = ggplot (aes(x = X, y = Y), data = Data) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F)
p

mFit <- lm(Y ~ X, data = Data)
mFit$coefficients
Data$yhat <- predict(mFit, Data)
Data$yhat2  = Data$X * 2


p = ggplot (aes(x = X, y = Y), data = Data) + 
  geom_point() + 
  geom_smooth(method = "lm", se = F) +
  geom_point(aes(x = X, y = yhat), color = "red") +
  geom_point(aes(x = X, y = yhat2), color = "green")
p


# now generate with categorical (discrete variables)

N = N*3
# the columns generated have to be multiples

Data = data.frame(
  Model = c("Model1", "Model2", "Model3"), 
  MEffect = c(0, 10, 20), 
  X = sample(seq(from = 10, to =40, by = 1), N, replace = TRUE))

Data = Data %>% mutate(Y =  Data$MEffect + (X*Beta)) 
Data = Data %>% mutate(Y = Y + rnorm(Data$Y,  0 , 10))

mFit <- lm(Y ~ Model + X, data = Data)

mFit$coefficients
Data$yhat <- predict(mFit, Data)
Data$yhat2  = Data$MEffect + Data$X * 2


p = ggplot (aes(x = X, y = Y, color = Model), data = Data) + 
  geom_point(alpha = .2) + 
  geom_smooth(method = "lm", se = F)
p

p = ggplot (aes(x = X, y = Y, color = Model), data = Data) + 
  geom_point(alpha = .2) + 
  geom_line(aes(x = X, y = yhat2))
p


vY = as.matrix(select(Data, Y)) 
mX = model.matrix(Y ~ Model + X,  Data)
vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations                    
vBeta


vBeta2 <- as.numeric(vBeta)
Data$neY <- t(vBeta2%*%t(mX)) # 3 columns on left * 3 rows on right (after transpose)
# transpose different than pyhon bc lm stores coef in vector vs hoz array

# compare predictions using NE vs lm - should be the same

p = p + 
  geom_point(aes(x = X, y = neY), data = Data)
p



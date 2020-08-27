library(tidyverse)

# repeat from Quick LA Demo.R ----------------- 

Advertising = read_csv("C:/Users/ellen/OneDrive/Documents/GitHub/EllenwTerry/Foundations/Advertising.csv")
Advertising = select(Advertising, TV, Radio, Sales)

mFit <- lm(Sales ~ TV + Radio, data = Advertising)
mFit$coefficients
Advertising$yhat <- predict(mFit, Advertising)

p = ggplot (aes(x = TV, y = Sales), data = Advertising) + 
  geom_point(aes(x = TV, y = yhat))
p


# 

vY <- as.matrix(dplyr::select(Advertising, Sales)) # set up y values in matrix                        
mX <- as.matrix(cbind(1, dplyr::select(Advertising, TV, Radio))) # set up x values in matrix
vBeta <- solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations                    
vBeta

str(Advertising)
# Predictions using normal equations

vBeta2 <- as.numeric(vBeta)
Advertising$neY <- t(vBeta2%*%t(mX)) # 3 columns on left * 3 rows on right (after transpose)
# transpose different than pyhon bc lm stores coef in vector vs hoz array

# compare predictions using NE vs lm - should be the same

p = p + 
  geom_point(data = Advertising, aes(x = TV, y = neY), color = "red")
p


# ----------- Categorical Data ------------------ #

Autos <- read.csv(file="C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Foundations/Automobile Price Prediction.csv")
Autos <- select(Autos, make, horsepower, price )
Autos = filter(Autos, make %in% c("audi", "bmw", "honda"))
model <- lm( price ~ ., Autos)
model$coefficients

vY = as.matrix(select(Autos, price)) 
mX = model.matrix(price ~ make + horsepower,  Autos)
vBeta = solve(t(mX)%*%mX, t(mX)%*%vY) # solve using normal equations                    
as.numeric(model$coefficients)
    
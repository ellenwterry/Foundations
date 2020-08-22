library(tidyverse)


Advertising = read_csv("C:/Users/ellen/OneDrive/Documents/GitHub/EllenwTerry/Foundations/Advertising.csv")
Advertising = select(Advertising, TV, Radio, Sales)

mFit <- lm(Sales ~ TV + Radio, data = Advertising)
mFit$coefficients
Advertising$yhat <- predict(mFit, Advertising)


mFit <- lm(Sales ~ TV + Radio, data = Advertising)
mFit$coefficients
Advertising$yhat <- predict(mFit, Advertising)


p = ggplot (aes(x = TV, y = Sales), data = Advertising) + 
  geom_point(aes(x = TV, y = yhat))
p


# Normal Equations Solution - refer to Python file and Slides

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
      geom_point(aes(x = TV, y = neY), data = Advertising, color = "red")
    p



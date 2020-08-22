library(tidyverse)

Advertising = read_csv("C:/Users/ellen/OneDrive/Documents/GitHub/EllenwTerry/Foundations/Advertising.csv")
Advertising = select(Advertising, TV, Radio, Sales)

mFit <- lm(Sales ~ TV + Radio, data = Advertising)
mFit$coefficients
Advertising$yhat <- predict(mFit, Advertising)


mFit = lm(Sales ~ TV + Radio, data = Advertising)
mFit$coefficients
Advertising$yhat = predict(mFit, Advertising)

sample = sample_n(Advertising, 4)
sample

vBeta <- as.numeric(mFit$coefficients)
mX <- as.matrix(cbind(1, select(sample, TV, Radio))) # set up x values in matrix

vBeta %*% mX 
# this doesn't work because mX is 4x3 and vBeta is 3x1 (3 columns on left <> 1 column on right)
# the number of columns on the left must equal the number of rows on the right... EXACTLY in that order, so
vBeta%*%t(mX) # works, but let's transpose it so we can see it better
t(vBeta%*%t(mX)) # gets us there

t(vBeta*t(mX)) 
# keep in mind that we can multiply the elements, but that won't solve the equation
# we want to use %*% because we want: 
(vBeta[1] * t(mX)[1,1]) + (vBeta[2] * t(mX)[2,1]) + (vBeta[3] * t(mX)[3,1]) 
# tie back to slide

tmX = t(mX)


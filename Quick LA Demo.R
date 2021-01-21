library(tidyverse)

setwd("C:/Users/ellen/Documents/UH/Fall 2020")

set.seed(0118)

mX = matrix(c(1, 8, 2, 6), nrow = 2, ncol = 2)
B = c(1,3)
B*mX
# or
B = matrix(c(1, 3, 2, 2), nrow = 2, ncol = 2)
# or
B = c(1,3)
B*mX
# and
B*t(mX)

solve(mX)

# inverse matrix
# proof
mX %*% solve(mX) %*% mX

# solve is also used to solve systems of equations
# for example, elimination from the ls problem
a = matrix(c(8,20,20,60),nrow=2,ncol=2)
b = matrix(c(56,154),nrow=2,ncol=1)
solve(a, b)
# still want you to do the elimination manually, but you can check with this
# and lm


B*t(mX)
# is not
B%*%t(mX)

# but think if mX as data and B as coefficients, so
B%*%t(mX)
# think of this as a linear equation where col 1, 2, etc. are dimensions
# so the first y_hat would be: (1*1) +  (3*2) = 7
# and tbe second y_hat would be (1*8) + (3*6) = 26
# SOOOO, that's why we use this ALL the time - it gives us a regression equation

# Now, what if we have more observations:
#mX = matrix(c(1, 8, 2, 6), nrow = 2, ncol = 2)
mX <- rbind(mX, c(5, 3)) 
B%*%t(mX)
# and tbe third row y_hat would be (1*5) + (3*3) = 14
# see?

# Also
B%*%t(mX)

# is not
t(B)%*%mX
# and it won't work because for a couple of reasons:
# 1. it's row * column AND the number of columns (2) on the LEFT must equal the number of rows (3) on the RIGHT
# picky picky picky
# 2. and btw, t(vector) doesn't do anything
# so
t(B)%*%t(mX)
# works but that's stupid


# OK with more complex data


Advertising = read_csv("C:/Users/ellen/OneDrive/Documents/GitHub/EllenwTerry/Foundations/Advertising.csv")
Advertising = select(Advertising, TV, Radio, Sales)

mFit <- lm(Sales ~ TV + Radio, data = Advertising)
mFit$coefficients
Advertising$yhat <- predict(mFit, Advertising)

sample = sample_n(Advertising, 4)
sample



vBeta <- as.numeric(mFit$coefficients)
str(mFit$coefficients) # this is a list
str(vBeta) # this is a vector
mX <- as.matrix(cbind(1, select(sample, TV, Radio))) # set up x values in matrix
mX

vBeta %*% mX 
# this doesn't work because mX is 4x3 and vBeta is 1x3 (3 columns on left <> 4 rows on right)
# the number of columns on the left must equal the number of rows on the right... EXACTLY in that order, so

vBeta%*%t(mX) # works, but let's transpose it so we can see it better

round(t(vBeta%*%t(mX)),1) # gets us there
# and compare

round(sample,1)

t(vBeta*t(mX)) 
# keep in mind that we can multiply the elements, but that won't solve the equation
# we want to use %*% because we want: 
round((vBeta[1] * t(mX)[1,1]) + (vBeta[2] * t(mX)[2,1]) + (vBeta[3] * t(mX)[3,1]),1) 
# tie back to slide


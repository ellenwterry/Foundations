library(tidyverse)
library(splines)
library(mgcv)
library(rms)

setwd("C:/Users/ellen/OneDrive/Documents/Spring 2020/DA2/Section 1/Regression/Data")
Auto <- read.csv(file="Automobile Price Prediction.csv")

p <- ggplot(Auto, aes(x=horsepower, y=price))+geom_point() 
p

modelBS <- lm(data = Auto, price ~  bs(horsepower, 3))
Auto$BSPrice <- predict(modelBS, data = Auto)

p <- ggplot(data=Auto) + geom_point(aes(x=horsepower, y = price), color = 'black')
p <- p + geom_smooth(aes(x=horsepower, y = price), se=FALSE, color = "blue")
p

Intercept <- modelBS$coefficients[1]
mb1 <- modelBS$coefficients[2]
mb2 <- modelBS$coefficients[3]
mb3 <- modelBS$coefficients[4]
rng <- range(Auto$horsepower)
div <- ((rng[2]-rng[1])/3)

## coefficients of the first model
a1 <- seq(rng[1], rng[1]+div, length.out = 10)
b1 <- seq(Intercept, Intercept+mb1, length.out = 10)
a2 <- seq(rng[1]+div, rng[1]+div+div, length.out = 10)
b2 <- seq(Intercept+mb1, Intercept+mb2, length.out = 10)
a3 <- seq(rng[1]+div+div, rng[2], length.out = 10)
b3 <- seq(Intercept+mb2, Intercept+mb3, length.out = 10)
tst <- data.frame(a1, b1, a2, b2, a3, b3)


p <- p + geom_line(data = tst, aes(x=a1, y = b1), color  = "red")
p <- p + geom_line(data = tst, aes(x=a2, y = b2), color  = "red")
p <- p + geom_line(data = tst, aes(x=a3, y = b3), color  = "red")
p


modelGAM <- gam(price ~ s(horsepower), data = Auto, family = gaussian(link = identity))
Auto$GAMPrice <- predict(modelGAM, data = Auto)
p <- p + geom_smooth(data = Auto, aes(x=horsepower, y = GAMPrice), se=FALSE, color = "red")
p

summary(modelGAM)
summary(modelBS)


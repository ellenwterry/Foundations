library(tidyverse)
library(caret)
library(ISLR)
library(klaR)
library(car)

rmse <- function(error)
{
  sqrt(mean(error^2))
}


# ---------------------- Stratified Sampling in tidyverse -----------------------#


homeSales <- read_csv(file="C:/Users/ellen/Documents/UH/Fall 2020/Class Materials/Class 5/Resampling/Data/HomeSales2.csv")

homeSales$ZIP <- as.factor(homeSales$ZIP) # Zip Codes are not numberical - there's no meaning to the quantity
homeSales %>% dplyr::count(ZIP) # we can see that we'll have problems
xTrain <- sample_frac(homeSales, .5)
xTest <- anti_join(homeSales, xTrain, by = "LISTID")
model <- lm( SALE_PRICE~ZIP + SQF + YEAR_BUILT, xTrain)
xTest$PREDSALEPRICE <- predict(model, xTest)


# so we have to stratify so that the training and test set are balanced

by_Zip <- homeSales %>% group_by(ZIP) %>% mutate(cnt = n()) %>% filter(cnt > 2) 
xTrain <- sample_frac(by_Zip, .5)
xTest <- anti_join(by_Zip, xTrain, by = "LISTID")
model <- lm( SALE_PRICE~ZIP + SQF + YEAR_BUILT, xTrain)
xTest$PREDSALEPRICE <- predict(model, xTest)

p <- ggplot(data=xTest, aes(SQF, PREDSALEPRICE)) + geom_point(alpha = 0.2)
p <- p + geom_smooth(data=xTest, aes(SQF, PREDSALEPRICE), se=FALSE)
p

summary(model)
rmse(model$residuals)

# so we have to be aware of the balance within variables 
# as well as classes in classification
# you can use dplyr to balance multiple variables too:

Auto <- read_csv("C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Archive/Data_Files/Automobile Price Prediction.csv")

Auto$`body-style` <- as.factor(Auto$`body-style`) # factors easier than character
Auto$make <- as.factor(Auto$make)
Auto %>% group_by(make, `body-style`) %>% count(make) 

# we can see that we'll have problems
# anytime obs < folds or groups, you have a problem!!!

Auto <- rowid_to_column(Auto, var="SampleID") # this creates a primary key (you have to be careful with rownames)
by_MakeStyle <- Auto %>% group_by(make, `body-style`) %>% dplyr::mutate(cnt = n()) %>% filter(cnt > 2) 
xTrain <- sample_frac(by_MakeStyle, .5)
xTest <- anti_join(by_MakeStyle, xTrain, by = "SampleID")
model <- lm(price ~ make + `body-style`+ horsepower, xTrain)
xTest$PREDSALEPRICE <- predict(model, xTest)

p <- ggplot(data=xTest, aes(horsepower, PREDSALEPRICE, color = make)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm", se=FALSE)
p

summary(model)
rmse(model$residuals)

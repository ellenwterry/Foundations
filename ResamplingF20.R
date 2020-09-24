library(tidyverse)
library(caret)
library(MLmetrics)
library(klaR)
library(ISLR)
library(psych)
library(e1071)
library(randomForest)
library(lubridate)
library(stringr)

rmse <- function(error)
{
  sqrt(mean(error^2))
}

set.seed(1015)

dfMPG = mpg 
dfMPG %>% group_by(manufacturer) %>% summarise(Cnt  = n())

dfMPG %>% data.matrix() %>% cor() %>% 
  data.frame() %>% rownames_to_column("Factor") %>%
  dplyr::select(Factor, hwy) %>%
  filter(abs(hwy) > .2) %>%
  dplyr::select(Factor)
# take out city - we'll just predict hwy

dfMPG$trans  = factor(dfMPG$trans)  
dfMPG$drv = factor(dfMPG$drv)
dfMPG$class = factor(dfMPG$class)

dfMPG <- rowid_to_column(dfMPG, var="SampleID")
by_ClassTrans = dfMPG %>% group_by(trans, drv, class) 
xTrain <- sample_frac(by_ClassTrans, .6)
xTest <- anti_join(by_ClassTrans, xTrain, by = "SampleID")

model <- lm(hwy ~ displ + cyl + drv + class, xTrain)
rmse(predict(model, xTest) - xTest$hwy)

control <- trainControl(method="repeatedcv", number = 5, repeats = 5) 
# number is K (# folds, so we create 5 folds and run each through 5 times)
# train the model

modelLmCV <- train(hwy ~ displ + cyl + drv + class, 
                   data = by_ClassTrans, method="lm", 
                   preProcess="scale", 
                   trControl=control,
                   metric = "RMSE")


modelLmCV$finalModel
modelLmCV$results
# now test against our test data (always a good policy)
rmse(predict(modelLmCV, xTest) - xTest$hwy)

modelSVM <- svm(hwy ~ displ + cyl + drv + class, data = xTrain)
rmse(predict(modelSVM, xTest) - xTest$hwy)
modelSVMCV <- train(hwy ~ displ + cyl + drv + class, 
                    data = by_ClassTrans, 
                    method="svmRadial", 
                    preProcess="scale", 
                    trControl=control)
rmse(predict(modelSVMCV, xTest) - xTest$hwy)
modelSVMCV$finalModel

modelRF <- randomForest(hwy ~ displ + cyl + drv + class, data = xTrain)
rmse(predict(modelRF, xTest) - xTest$hwy)
modelRFCV <- train(hwy ~ displ + cyl + drv + class, 
                   data = by_ClassTrans, method="rf", 
                   trControl=control)
rmse(predict(modelRFCV, xTest) - xTest$hwy)
modelRFCV$finalModel

modelGLM <- glm(hwy ~ displ + cyl + trans + drv + class, data = dfMPG, 
                family = gaussian())
rmse(predict(modelGLM, xTest) - xTest$hwy)

modelGLMCV <- train(hwy ~ displ + cyl + drv + class, 
                    data = by_ClassTrans, method="glm", 
                    trControl=control,
                    preProcess="scale", 
                    metric = "RMSE")

rmse(predict(modelGLMCV, xTest) - xTest$hwy)
# notice that CV doesn't do a lot - high bias model
# but also an interpretable model:
modelGLMCV$finalModel


results <- matrix(ncol = 2, nrow=3)
results[1,1] <- 'glm'
results[2,1] <- 'svmPoly'
results[3,1] <- 'rf'
cntrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
i <- 1
for(i in 1:nrow(results))
{
  caretMod <- train(hwy ~ displ + cyl + drv + class, 
                    data = by_ClassTrans,  method = results[i,1],
                    preProcess="scale",
                    metric = "RMSE")
  results[i,2] <- round(rmse(predict(caretMod, xTest) - xTest$hwy),3)
}

# if you get a rank deficiency warning, it generally due to
# lack of observations vs. number of dimensions
# often happens with smaller folds (or greater number of folds)

results %>% data.frame() %>% arrange(X2)


# Different data

ProductSales = read_csv("C:/Users/ellen/Documents/UH/Fall 2020/Class Materials/Regression/ProductSalesv2.csv")
ProductSales$WkBeg = mdy(ProductSales$WkBeg)
ProductSales = pivot_longer(ProductSales, 3:5, names_to = "Product", values_to = "Sales")

xTrain = ProductSales %>% filter(WkBeg < "2015-01-01")
xTest  =  ProductSales %>%  filter(WkBeg >= "2015-01-01")

control <- trainControl(method = "repeatedcv", number = 5, repeats = 5)

i <- 1
for(i in 1:nrow(results))
{
  caretMod <- train(Sales ~ Product + Wk, data = xTrain,  method = results[i,1])
  results[i,2] <- round(rmse(predict(caretMod, xTest) - xTest$Sales),3)
}

results %>% data.frame() %>% arrange(X2)

glmPoly <- train(Sales ~ Product + Wk + I(Wk^2), data = xTrain,  method = "glm")
round(rmse(predict(glmPoly, xTest) - xTest$Sales),3)
# recall our regularization lesson!


# now for some classification

QuoteData = read_csv("C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Applications/QuoteData2.csv")
QuoteData$QuoteDiff <- QuoteData$QuoteDiff/1000
QuoteData$Result = factor(QuoteData$Result)
QuoteData = QuoteData %>% rownames_to_column("SampleID")
xTrain <- sample_n(QuoteData, nrow(QuoteData)-100)
xTest <- QuoteData %>% anti_join(xTrain, by = "SampleID")

modelGLMC <- glm(Result ~ RSF + QuoteDiff + RFPDiff + ATPDiff, 
                 data = xTrain, family = binomial)
summary(modelGLMC)

xTest$Prob <- predict(modelGLMC, type = "response", newdata = xTest)
xTest$Class <- ifelse(xTest$Prob < .5, 0, 1)
confusionMatrix(factor(xTest$Class) , factor(xTest$Result), positive = '1')

metric = "Accuracy"
caretMod <- train(Result ~ RSF + QuoteDiff + RFPDiff + ATPDiff, 
                  data = QuoteData,  
                  method="rf", 
                  metric=metric, 
                  trControl=control)

CM = confusionMatrix(predict(caretMod, newdata = xTest), xTest$Result, positive = '1')
as.numeric(CM$byClass[11])
as.numeric(CM$byClass[1])

results <- matrix(ncol = 2, nrow=5)
results[1,1] <- 'glm'
results[2,1] <- 'nb'
results[3,1] <- 'lda'
results[4,1] <- 'svmRadial'
results[5,1] <- 'rf'

i <- 1
for(i in 1:nrow(results)){
  caretMod = train(Result ~ RSF + QuoteDiff + RFPDiff + ATPDiff, 
                    data = QuoteData,  
                    metric=metric, 
                    trControl=control,
                    method = results[i,1])
  CM = confusionMatrix(predict(caretMod, newdata = xTest), xTest$Result, positive = '1')
  results[i,2] = round(as.numeric(CM$byClass[11]),2)
}

results %>% data.frame() %>% arrange(desc(X2))

# ===================== now for harder datasets.
# remember that sensitivity is the metric we're interested in with Defaults
# using the same results matrix from above

xTest = sample_n(Default, 1000)

i <- 1
for(i in 1:nrow(results)){
  caretMod = train(default ~ student + balance + income,  
                   data = Default,  
                   metric=metric, 
                   trControl=control,
                   method = results[i,1])
  CM = confusionMatrix(predict(caretMod, newdata = xTest), xTest$default, positive = "Yes")
  results[i,2] = round(as.numeric(CM$byClass[1]),2)
}

results %>% data.frame() %>% arrange(desc(X2))

rfMod = train(default ~ student + balance + income,  
                 data = Default,  
                 metric=metric, 
                 trControl=control,
                 method = 'rf')

confusionMatrix(predict(rfMod, newdata = xTest), xTest$default, positive = "Yes")


# another

Emp_Turn <- read.csv("C:/Users/ellen/OneDrive/Documents/UH/Spring 2020/DA2/Section 1/Classification and SVM/Homework/EmpTurn2.csv")
Emp_Turn$Left  = factor(Emp_Turn$Left)
Emp_Turn$Work_Accident  = factor(Emp_Turn$Work_Accident)
Emp_Turn$Promotion  = factor(Emp_Turn$Promotion)
Emp_Turn$Dept  = factor(Emp_Turn$Dept)
Emp_Turn$Salary  = factor(Emp_Turn$Salary)

xTest = sample_n(Emp_Turn, 1000)


results <- matrix(ncol = 2, nrow=2)
results[1,1] <- 'nb'
results[2,1] <- 'rf'

i <- 1
for(i in 1:nrow(results)){caretMod = train(Left ~ 
                   Satisfaction +   
                   Last_Eval +
                   Number_Projects +
                   Avg_Mo_Hrs + 
                   Tenure +
                   Work_Accident +
                   Left +
                   Promotion +
                   Dept + 
                   Salary,
                   data = Emp_Turn,  
                   metric=metric, 
                   trControl=control,
                   method = results[i,1])
  CM = confusionMatrix(factor(predict(caretMod, newdata = xTest)), xTest$Left, positive = '1')
  results[i,2] = round(as.numeric(CM$byClass[11]),2)
}

results %>% data.frame() %>% arrange(desc(X2))


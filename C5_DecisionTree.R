library(tidyverse)
library(rpart.plot)
library(RColorBrewer)
library(ISLR)
library(rpart)

set.seed(913)


dfDefault <- Default

p = ggplot(dfDefault, aes(balance, fill = default)) +
  geom_histogram(binwidth = 500)
p

dfDefault <- dfDefault %>% rownames_to_column("SampleID")
xTrain <- sample_n(dfDefault, round(nrow(dfDefault)*.6))
xTest <- dfDefault %>% anti_join(xTrain, by = "SampleID")


fit <- rpart(default ~ student + balance + income,  
              data = xTrain,
             method="class")

xTest$pred = predict(fit, type = "class", newdata = xTest)  # factor

confusionMatrix(factor(xTest$pred), (xTest$default),  positive = "Yes")

# Recursive Partitioning and Regression Trees

rpart.plot(fit, box.palette="RdBu", shadow.col="gray", nn=TRUE)


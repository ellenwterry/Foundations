library(tidyverse)
library(rpart.plot)
library(RColorBrewer)
library(ISLR)
library(caret)
library(e1071)

set.seed(913)

dfDefault <- Default

p <- ggplot(dfDefault, aes(balance, fill = default)) +
  geom_histogram(binwidth = 500) +
  facet_wrap(~student) +
  theme(panel.background = element_rect(fill = "white")) 
p

dfDefault <- dfDefault %>% rownames_to_column("SampleID")
xTrain <- sample_n(dfDefault, round(nrow(dfDefault)*.6,0))
xTest <- dfDefault %>% anti_join(xTrain, by = "SampleID")

svmMod <- svm(default ~ student + balance + income, data = xTrain)
summary(svmMod)
xTest$pred <- predict(svmMod, xTest)

confusionMatrix(factor(xTest$pred), xTest$default,  positive = "Yes")











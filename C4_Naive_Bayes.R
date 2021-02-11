library(tidyverse)
library(ISLR)
library(e1071)
library(caret)

set.seed(913)


dfDefault <- Default

#tst <- subset(dfDefault, default == 'No')
#dfDefault <- Default
#ProbDef = nrow(filter(dfDefault, default == "Yes"))/nrow(dfDefault)

p = ggplot(dfDefault, aes(balance, fill = default)) +
  geom_histogram(binwidth = 500)
p


dfDefault <- dfDefault %>% rownames_to_column("SampleID")
xTrain <- sample_n(dfDefault, round(nrow(dfDefault)*.6))
xTest <- dfDefault %>% anti_join(xTrain, by = "SampleID")

model <- naiveBayes(default ~ student + balance + income,  data = xTrain)
xTest$pred <- predict(model, xTest, prob = TRUE)

confusionMatrix(factor(xTest$pred), (xTest$default),  positive = "Yes")

# ROC Curve https://en.wikipedia.org/wiki/Receiver_operating_characteristic
# it measures the true positive vs false positive *rate* 

probs <- predict(model, xTest, type = 'raw')

library(ROCR)

pred <- prediction(probs[, "Yes"], xTest$default)
perf_nb <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf_nb)
auc<- performance(pred,"auc")
auc

# compare 

pred <- prediction(probs[, "No"], xTest$default)
perf_nb <- performance(pred, measure = 'tpr', x.measure = 'fpr')
plot(perf_nb)
auc<- performance(pred,"auc")
auc


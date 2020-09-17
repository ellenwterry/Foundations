library(tidyverse)
library(ISLR)
library(caret)
library(e1071)
library(MASS)
library(rpart)

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

smoteData <- SMOTE(default ~ student + balance + income, data = Default, 
                   perc.over = 350, perc.under=130) 
# SMOTE only works with factors, so be careful
prop.table(table(smoteData$default))


# LDA

lda.fit <- lda(default ~ student + balance + income, smoteData) 
lda.pred <- predict(lda.fit, xTest)

xTest$LDASmote =  lda.pred$class

confusionMatrix(xTest$LDASmote,  factor(xTest$default), positive = "Yes")



library(tidyverse)
library(gridExtra)
library(lubridate)
library(caret)


set.seed(116)

quoteData = read_csv("C:/Users/ellen/Documents/UH/Fall 2020/Github Staging/EllenwTerry/Archive/Data_Files/quoteData.csv")

quoteData <- filter(quoteData, Result %in% c(0, 1))
quoteData <- quoteData %>% rownames_to_column("SampleID")
quoteData$SampleID  <- as.numeric(quoteData$SampleID)
quoteData$QuoteDiff <- quoteData$QuoteDiff/1000
quoteData$RSF <- factor(quoteData$RSF)
train <- sample_n(quoteData, nrow(quoteData)-100)
test <- quoteData %>% anti_join(train, by = "SampleID")

glm.fit <- glm(Result ~ RSF + QuoteDiff + RFPDiff + ATPDiff, data = train, family = binomial)
summary(glm.fit)

testPred <- predict(glm.fit, type = "response", newdata = test, se.fit = T)

test$Prob <- testPred$fit
test$lcl <- test$Prob - testPred$se.fit
test$ucl <- test$Prob + testPred$se.fit


ggplot(test, aes(x=QuoteDiff, y=Prob)) + geom_point() + 
  geom_smooth(method="glm", method.args=list(family=quasibinomial)) +
  geom_smooth(aes(x = QuoteDiff, y = lcl), method="glm", method.args=list(family=quasibinomial)) +
  geom_smooth(aes(x = QuoteDiff, y = ucl), method="glm", method.args=list(family=quasibinomial)) +
  theme(panel.background = element_rect(fill = "white")) 


glm.fit$coefficients

# ------------- this gives you the confidence intervals for the coefficients ------------------ #

confint(glm.fit) # this uses profile likelihood to compute CIs
confint.default(glm.fit) # this uses likelihood to compute Wald CIs - I'll use Wald (traditional symmetric)

GLMParamEst <- data.frame(mean = glm.fit$coefficients, sdEst = 
  (confint.default(glm.fit)[,2]-glm.fit$coefficients)/1.96)
GLMParamEst <- rownames_to_column(GLMParamEst, "Param")
PlotData <- data.frame(Param = GLMParamEst$Param, 
  x = rnorm(700, GLMParamEst$mean, GLMParamEst$sdEst))

ggplot(PlotData, aes(x = x, color = Param)) + 
  geom_density(bw = .5) +
  scale_x_continuous(limits = c(-6, 6)) +
  theme(panel.background = element_rect(fill = "white")) 


# ------------- developing matrix algebra equations  ------------------ #

test$Prob <- predict(glm.fit, type = "response", newdata = test)

tst1 <- model.matrix(Result ~ RSF + QuoteDiff + RFPDiff + ATPDiff,  data = test)
bet1 <- as.numeric(glm.fit$coefficients)
test$tmProb2 <- exp( t(bet1%*%t(tst1)))/(1+exp(t(bet1%*%t(tst1))))

# show that equation gets same result as glm
sum(round(test$Prob - test$tmProb2,0))


# score results
test$PResult <- ifelse(test$Prob < .5, 0, 1)
# check metrics
confusionMatrix(factor(test$PResult) , factor(test$Result))


